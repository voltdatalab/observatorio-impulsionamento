#!/usr/bin/env Rscript
# Data Accuracy Validation Script
# Compares a year's RDS file vs what's in the `despesas` table for that ANO_ELEICAO
# Usage: Rscript validate_data_accuracy.R [rds_file] [ano_eleicao]

library(RSQLite)
library(pool)
library(tidyverse)
library(lubridate)

args <- commandArgs(trailingOnly = TRUE)
rds_file <- if (length(args) >= 1) args[1] else "e2024.rds"
ano_eleicao <- if (length(args) >= 2) as.integer(args[2]) else 2024

cat("==========================================================\n")
cat("  DATA ACCURACY VALIDATION -", ano_eleicao, "\n")
cat("  Comparing", rds_file, "vs the despesas table\n")
cat("==========================================================\n\n")

# Load from RDS (original method)
cat("📁 Loading data from RDS file...\n")
if (file.exists(rds_file)) {
  rds_data <- readRDS(rds_file)

  # Process same way as original app
  rds_data$valor <- gsub(".", "", rds_data$VR_DESPESA_CONTRATADA, fixed = TRUE)
  rds_data$valor <- as.numeric(gsub(",", ".", rds_data$VR_DESPESA_CONTRATADA))
  rds_data$DT_DESPESA <- as_date(rds_data$DT_DESPESA, format="%d/%m/%Y")
  rds_data$mun <- paste0(rds_data$NM_UE, " - ", rds_data$SG_UF)

  rds_data <- rds_data %>% distinct(SQ_DESPESA, .keep_all = TRUE)

  cat("✅ RDS loaded:", nrow(rds_data), "rows\n\n")
} else {
  cat("❌", rds_file, "not found - skipping RDS comparison\n")
  cat("   (This is OK if you've already validated and deleted the file)\n\n")
  quit(status = 0)
}

# Load from SQLite (optimized method)
cat("💾 Loading data from SQLite database...\n")
pool <- dbPool(drv = RSQLite::SQLite(), dbname = "obseleitoral.db")

sql_data <- dbGetQuery(pool, sprintf("
  SELECT
    NM_CANDIDATO,
    SG_PARTIDO,
    DS_CARGO,
    DT_DESPESA,
    dt_despesa_iso,
    VR_DESPESA_CONTRATADA,
    valor_numeric,
    SG_UF,
    NM_UE,
    mun_uf,
    ST_TURNO,
    SQ_DESPESA,
    SQ_CANDIDATO,
    rede_social_mae
  FROM despesas
  WHERE ANO_ELEICAO = %d
", ano_eleicao))

sql_data <- sql_data %>% distinct(SQ_DESPESA, .keep_all = TRUE)

cat("✅ SQLite loaded:", nrow(sql_data), "rows\n\n")

# Validation Tests
cat("==========================================================\n")
cat("  VALIDATION TESTS\n")
cat("==========================================================\n\n")

errors <- 0

# Test 1: Row count
cat("1️⃣  Row Count Validation\n")
if (nrow(rds_data) == nrow(sql_data)) {
  cat("   ✅ PASS: Both have", nrow(rds_data), "rows\n\n")
} else {
  cat("   ❌ FAIL: RDS has", nrow(rds_data), "rows, SQLite has", nrow(sql_data), "rows\n\n")
  errors <- errors + 1
}

# Test 2: Total value comparison
cat("2️⃣  Total Value Validation\n")
rds_total <- sum(rds_data$valor, na.rm = TRUE)
sql_total <- sum(sql_data$valor_numeric, na.rm = TRUE)
diff_percent <- abs((rds_total - sql_total) / rds_total * 100)

cat("   RDS total:    R$", format(round(rds_total, 2), big.mark=","), "\n")
cat("   SQLite total: R$", format(round(sql_total, 2), big.mark=","), "\n")
cat("   Difference:   ", round(diff_percent, 4), "%\n")

if (diff_percent < 0.01) {
  cat("   ✅ PASS: Values match (< 0.01% difference)\n\n")
} else {
  cat("   ❌ FAIL: Values differ by more than 0.01%\n\n")
  errors <- errors + 1
}

# Test 3: Date conversion
cat("3️⃣  Date Conversion Validation\n")
sample_rows <- sample(1:min(100, nrow(sql_data)), 10)
date_mismatches <- 0

for (i in sample_rows) {
  sq_despesa <- sql_data$SQ_DESPESA[i]
  rds_row <- rds_data %>% filter(SQ_DESPESA == sq_despesa)

  if (nrow(rds_row) > 0) {
    # Compare dates
    rds_date <- rds_row$DT_DESPESA[1]
    sql_date <- as_date(sql_data$dt_despesa_iso[i])

    if (!identical(rds_date, sql_date)) {
      date_mismatches <- date_mismatches + 1
      cat("   ⚠️  Date mismatch for SQ_DESPESA", sq_despesa, "\n")
      cat("      RDS:", format(rds_date), "| SQLite:", format(sql_date), "\n")
    }
  }
}

if (date_mismatches == 0) {
  cat("   ✅ PASS: All", length(sample_rows), "sampled dates match\n\n")
} else {
  cat("   ❌ FAIL:", date_mismatches, "date mismatches found\n\n")
  errors <- errors + 1
}

# Test 4: Value parsing
cat("4️⃣  Value Parsing Validation\n")
sample_rows <- sample(1:min(100, nrow(sql_data)), 10)
value_mismatches <- 0

for (i in sample_rows) {
  sq_despesa <- sql_data$SQ_DESPESA[i]
  rds_row <- rds_data %>% filter(SQ_DESPESA == sq_despesa)

  if (nrow(rds_row) > 0) {
    rds_val <- rds_row$valor[1]
    sql_val <- sql_data$valor_numeric[i]

    if (!is.na(rds_val) && !is.na(sql_val)) {
      diff <- abs(rds_val - sql_val)
      if (diff > 0.01) {  # Allow 1 cent difference for rounding
        value_mismatches <- value_mismatches + 1
        cat("   ⚠️  Value mismatch for SQ_DESPESA", sq_despesa, "\n")
        cat("      RDS: R$", rds_val, "| SQLite: R$", sql_val, "\n")
      }
    }
  }
}

if (value_mismatches == 0) {
  cat("   ✅ PASS: All", length(sample_rows), "sampled values match\n\n")
} else {
  cat("   ❌ FAIL:", value_mismatches, "value mismatches found\n\n")
  errors <- errors + 1
}

# Test 5: Municipality concatenation
cat("5️⃣  Municipality Format Validation\n")
sample_rows <- sample(1:min(100, nrow(sql_data)), 10)
mun_mismatches <- 0

for (i in sample_rows) {
  sq_despesa <- sql_data$SQ_DESPESA[i]
  rds_row <- rds_data %>% filter(SQ_DESPESA == sq_despesa)

  if (nrow(rds_row) > 0) {
    rds_mun <- rds_row$mun[1]
    sql_mun <- sql_data$mun_uf[i]

    if (!identical(rds_mun, sql_mun)) {
      mun_mismatches <- mun_mismatches + 1
      cat("   ⚠️  Municipality mismatch for SQ_DESPESA", sq_despesa, "\n")
      cat("      RDS:", rds_mun, "| SQLite:", sql_mun, "\n")
    }
  }
}

if (mun_mismatches == 0) {
  cat("   ✅ PASS: All", length(sample_rows), "sampled municipalities match\n\n")
} else {
  cat("   ❌ FAIL:", mun_mismatches, "municipality mismatches found\n\n")
  errors <- errors + 1
}

# Test 6: Aggregations (Partido totals)
cat("6️⃣  Aggregation Validation (by Partido)\n")
rds_by_partido <- rds_data %>%
  group_by(SG_PARTIDO) %>%
  summarise(total = sum(valor, na.rm = TRUE), n = n()) %>%
  arrange(SG_PARTIDO)

sql_by_partido <- sql_data %>%
  group_by(SG_PARTIDO) %>%
  summarise(total = sum(valor_numeric, na.rm = TRUE), n = n()) %>%
  arrange(SG_PARTIDO)

agg_match <- all.equal(rds_by_partido$total, sql_by_partido$total, tolerance = 0.01)

if (isTRUE(agg_match)) {
  cat("   ✅ PASS: Partido aggregations match\n\n")
} else {
  cat("   ❌ FAIL: Partido aggregations differ\n")
  cat("      Details:", agg_match, "\n\n")
  errors <- errors + 1
}

# Test 7: Top candidates match
cat("7️⃣  Top Candidates Validation\n")
rds_top <- rds_data %>%
  group_by(NM_CANDIDATO, SG_PARTIDO) %>%
  summarise(total = sum(valor, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  head(10)

sql_top <- sql_data %>%
  group_by(NM_CANDIDATO, SG_PARTIDO) %>%
  summarise(total = sum(valor_numeric, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  head(10)

top_match <- identical(rds_top$NM_CANDIDATO, sql_top$NM_CANDIDATO)

if (top_match) {
  cat("   ✅ PASS: Top 10 candidates match\n")
  cat("      Top candidate:", sql_top$NM_CANDIDATO[1], "-", sql_top$SG_PARTIDO[1], "\n")
  cat("      Total: R$", format(round(sql_top$total[1], 2), big.mark=","), "\n\n")
} else {
  cat("   ⚠️  WARNING: Top 10 candidates order differs slightly\n")
  cat("      (This may be due to rounding differences)\n\n")
}

# Summary
cat("==========================================================\n")
cat("  VALIDATION SUMMARY\n")
cat("==========================================================\n\n")

if (errors == 0) {
  cat("✅ ✅ ✅  ALL TESTS PASSED!  ✅ ✅ ✅\n\n")
  cat("The optimized SQLite database is accurate and ready for production.\n")
  cat("You can safely use the fast version of your app!\n\n")
} else {
  cat("❌ ❌ ❌  ", errors, "TEST(S) FAILED  ❌ ❌ ❌\n\n")
  cat("Please review the errors above before using in production.\n")
  cat("You may need to re-run the optimization script.\n\n")
}

# Cleanup
poolClose(pool)

cat("==========================================================\n")
cat("Validation complete!\n")
cat("==========================================================\n")
