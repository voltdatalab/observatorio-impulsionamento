#!/usr/bin/env Rscript
# Update the SQLite database with a new election year's data
# Usage: Rscript update_database.R <path_to_rds_or_csv> <ano_eleicao> [--replace]

library(RSQLite)
library(tidyverse)

TABLE_NAME <- "despesas"

REQUIRED_COLS <- c(
  "NM_CANDIDATO", "SG_PARTIDO", "DS_CARGO", "DT_DESPESA", "SG_UF", "NM_UE",
  "ST_TURNO", "DT_PRESTACAO_CONTAS", "NR_CNPJ_PRESTADOR_CONTA", "DS_TIPO_FORNECEDOR",
  "NM_FORNECEDOR", "NM_FORNECEDOR_RFB", "DS_ORIGEM_DESPESA", "DS_DESPESA",
  "VR_DESPESA_CONTRATADA", "SQ_DESPESA", "SQ_CANDIDATO", "rede_social_mae"
)

# Adds the computed columns the app filters on (dt_despesa_iso, valor_numeric, mun_uf)
# and stamps the election year, so every load produces rows shaped exactly like `despesas`.
prepare_data <- function(data, ano_eleicao) {
  missing_cols <- setdiff(REQUIRED_COLS, names(data))
  for (col in missing_cols) data[[col]] <- NA

  data %>%
    mutate(
      dt_despesa_iso = as.character(as_date(DT_DESPESA, format = "%d/%m/%Y")),
      valor_numeric = as.numeric(gsub(",", ".", gsub(".", "", VR_DESPESA_CONTRATADA, fixed = TRUE), fixed = TRUE)),
      mun_uf = if_else(is.na(NM_UE), NA_character_, paste0(NM_UE, " - ", SG_UF)),
      ANO_ELEICAO = as.integer(ano_eleicao)
    ) %>%
    select(all_of(REQUIRED_COLS), dt_despesa_iso, valor_numeric, mun_uf, ANO_ELEICAO)
}

create_indexes <- function(con) {
  cat("Creating indexes...\n")
  idx <- function(name, cols) dbExecute(con, sprintf("CREATE INDEX IF NOT EXISTS %s ON %s(%s)", name, TABLE_NAME, cols))
  idx("idx_despesas_ano", "ANO_ELEICAO")
  idx("idx_despesas_dt_despesa_iso", "dt_despesa_iso")
  idx("idx_despesas_sg_partido", "SG_PARTIDO")
  idx("idx_despesas_sg_uf", "SG_UF")
  idx("idx_despesas_ds_cargo", "DS_CARGO")
  idx("idx_despesas_st_turno", "ST_TURNO")
  idx("idx_despesas_sq_despesa", "SQ_DESPESA")
  idx("idx_despesas_sq_candidato", "SQ_CANDIDATO")
  idx("idx_despesas_rede_social", "rede_social_mae")
  idx("idx_despesas_nm_candidato", "NM_CANDIDATO")
  idx("idx_despesas_nm_ue", "NM_UE")
  idx("idx_despesas_mun_uf", "mun_uf")
  idx("idx_despesas_valor_numeric", "valor_numeric")
  idx("idx_despesas_uf_mun", "SG_UF, NM_UE")
  idx("idx_despesas_partido_cargo", "SG_PARTIDO, DS_CARGO")
  idx("idx_despesas_date_partido", "dt_despesa_iso, SG_PARTIDO")
  idx("idx_despesas_partido_valor", "SG_PARTIDO, valor_numeric")
  idx("idx_despesas_uf_valor", "SG_UF, NM_UE, valor_numeric")
  idx("idx_despesas_candidato_partido", "NM_CANDIDATO, SG_PARTIDO, valor_numeric")
  idx("idx_despesas_ano_partido", "ANO_ELEICAO, SG_PARTIDO")
  dbExecute(con, "ANALYZE")
  cat("Indexes created successfully\n")
}

read_data <- function(file_path) {
  if (grepl("\\.rds$", file_path, ignore.case = TRUE)) {
    readRDS(file_path)
  } else if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    read.csv(file_path, fileEncoding = "UTF-8", header = TRUE, sep = ",", stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file type. Please provide .rds or .csv file")
  }
}

# Appends new records for one election year, skipping SQ_DESPESA values already present
update_data <- function(file_path, ano_eleicao) {
  cat("Loading data from:", file_path, "\n")
  data <- prepare_data(read_data(file_path), ano_eleicao)

  con <- dbConnect(RSQLite::SQLite(), Sys.getenv("OBS_DB_PATH", "obseleitoral.db"))
  on.exit(dbDisconnect(con))

  if (dbExistsTable(con, TABLE_NAME)) {
    existing_sq <- dbGetQuery(con, sprintf(
      "SELECT SQ_DESPESA FROM %s WHERE ANO_ELEICAO = %d", TABLE_NAME, as.integer(ano_eleicao)
    ))$SQ_DESPESA

    new_data <- data %>% filter(!SQ_DESPESA %in% existing_sq)
    cat("Found", nrow(new_data), "new records to add (of", nrow(data), "in the file)\n")

    if (nrow(new_data) > 0) {
      dbWriteTable(con, TABLE_NAME, new_data, append = TRUE, row.names = FALSE)
      cat("Successfully added", nrow(new_data), "records\n")
    }
  } else {
    dbWriteTable(con, TABLE_NAME, data, overwrite = FALSE, row.names = FALSE)
    cat("Created table", TABLE_NAME, "with", nrow(data), "records\n")
  }

  create_indexes(con)
  cat("Database updated successfully!\n")
}

# Replaces one election year's rows entirely, leaving other years untouched
replace_year <- function(file_path, ano_eleicao) {
  cat("Replacing", ano_eleicao, "data with:", file_path, "\n")
  data <- prepare_data(read_data(file_path), ano_eleicao)

  con <- dbConnect(RSQLite::SQLite(), Sys.getenv("OBS_DB_PATH", "obseleitoral.db"))
  on.exit(dbDisconnect(con))

  # Transacional: o app pode estar lendo este arquivo de outro container
  # (volume compartilhado) - não pode ver o ano deletado sem as linhas novas
  dbBegin(con)
  if (dbExistsTable(con, TABLE_NAME)) {
    dbExecute(con, sprintf("DELETE FROM %s WHERE ANO_ELEICAO = %d", TABLE_NAME, as.integer(ano_eleicao)))
  }
  dbWriteTable(con, TABLE_NAME, data, append = TRUE, row.names = FALSE)
  dbCommit(con)
  cat("Replaced", ano_eleicao, "with", nrow(data), "records\n")

  create_indexes(con)
  cat("Database updated successfully!\n")
}

if (interactive()) {
  cat("Usage examples:\n")
  cat("  update_data('e2026.rds', 2026)\n")
  cat("  replace_year('e2026.rds', 2026)\n")
} else {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 2) {
    cat("Usage: Rscript update_database.R <file_path> <ano_eleicao> [--replace]\n")
    quit(status = 1)
  }

  file_path <- args[1]
  ano_eleicao <- args[2]
  replace_mode <- "--replace" %in% args

  if (!file.exists(file_path)) {
    cat("Error: File not found:", file_path, "\n")
    quit(status = 1)
  }

  if (replace_mode) {
    replace_year(file_path, ano_eleicao)
  } else {
    update_data(file_path, ano_eleicao)
  }
}
