#!/usr/bin/env Rscript
# Substitui as linhas de um ano na tabela `totais` (gasto total de campanha por
# candidato, gerada por load_totais.R). Sempre replace - a tabela e um agregado,
# nao faz sentido append incremental.
# Usage: Rscript update_totais.R <totais_ANO.rds> <ano>

library(RSQLite)
suppressMessages(library(tidyverse))

TABLE_NAME <- "totais"
REQUIRED_COLS <- c("ANO_ELEICAO", "SQ_CANDIDATO", "NM_CANDIDATO", "SG_PARTIDO",
                   "DS_CARGO", "SG_UF", "NM_UE", "mun_uf", "ST_TURNO",
                   "total_geral", "n_despesas")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  cat("Usage: Rscript update_totais.R <totais_ANO.rds> <ano>\n")
  quit(status = 1)
}
file_path <- args[1]
ano <- as.integer(args[2])
if (!file.exists(file_path)) stop("File not found: ", file_path)

d <- readRDS(file_path)
missing <- setdiff(REQUIRED_COLS, names(d))
if (length(missing) > 0) stop("Colunas ausentes no rds: ", paste(missing, collapse = ", "))
d <- d %>% select(all_of(REQUIRED_COLS))

con <- dbConnect(RSQLite::SQLite(), Sys.getenv("OBS_DB_PATH", "obseleitoral.db"))
on.exit(dbDisconnect(con))

# Transacional: o app compartilha este arquivo (volume CapRover) e não pode
# enxergar o ano deletado sem as linhas novas
dbBegin(con)
if (dbExistsTable(con, TABLE_NAME)) {
  dbExecute(con, sprintf("DELETE FROM %s WHERE ANO_ELEICAO = %d", TABLE_NAME, ano))
}
dbWriteTable(con, TABLE_NAME, d, append = TRUE, row.names = FALSE)
dbCommit(con)

idx <- function(name, cols) dbExecute(con, sprintf("CREATE INDEX IF NOT EXISTS %s ON %s(%s)", name, TABLE_NAME, cols))
idx("idx_totais_ano", "ANO_ELEICAO")
idx("idx_totais_ano_uf", "ANO_ELEICAO, SG_UF")
idx("idx_totais_ano_partido", "ANO_ELEICAO, SG_PARTIDO")
idx("idx_totais_ano_cargo", "ANO_ELEICAO, DS_CARGO")
idx("idx_totais_ano_mun", "ANO_ELEICAO, mun_uf")
idx("idx_totais_ano_candidato", "ANO_ELEICAO, NM_CANDIDATO")
dbExecute(con, "ANALYZE")

cat(sprintf("Tabela %s: ano %d substituido com %d linhas\n", TABLE_NAME, ano, nrow(d)))
