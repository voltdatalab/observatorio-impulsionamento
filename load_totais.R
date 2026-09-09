#!/usr/bin/env Rscript
## Agrega o gasto TOTAL de campanha por candidato (todas as despesas, nao so as
## de impulsionamento) a partir do MESMO zip do TSE usado por load_data_<ANO>.R.
## Alimenta a tabela `totais`, denominador do KPI "% do total gasto pelas
## campanhas foi com impulsionamento" no app.
##
## Usage: TSE_ZIP_PATH=/caminho/do.zip Rscript load_totais.R <ANO>
## Next:  Rscript update_totais.R totais_<ANO>.rds <ANO>

suppressMessages(library(tidyverse))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: TSE_ZIP_PATH=... Rscript load_totais.R <ANO>")
ANO <- as.integer(args[1])
ZIP_PATH <- Sys.getenv("TSE_ZIP_PATH", unset = "temp.zip")
if (!file.exists(ZIP_PATH)) stop("Zip nao encontrado: ", ZIP_PATH)

csv_name <- grep(sprintf("despesas_contratadas_candidatos_%d_BRASIL.csv$", ANO),
                 unzip(ZIP_PATH, list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
if (length(csv_name) == 0) stop("CSV BRASIL de despesas contratadas nao encontrado em ", ZIP_PATH)

needed <- c("SQ_CANDIDATO", "NM_CANDIDATO", "SG_PARTIDO", "DS_CARGO", "SG_UF",
            "NM_UE", "ST_TURNO", "SQ_DESPESA", "VR_DESPESA_CONTRATADA")

csv_file <- unzip(ZIP_PATH, files = csv_name)
raw <- readr::read_csv2(csv_file,
                        locale = readr::locale(encoding = "ISO-8859-1"),
                        col_select = tidyselect::all_of(needed),
                        col_types = readr::cols(.default = "c"),
                        progress = FALSE)

missing <- setdiff(needed, names(raw))
if (length(missing) > 0) {
  stop("TSE mudou o schema do CSV - colunas ausentes: ", paste(missing, collapse = ", "))
}

d <- raw %>%
  distinct(SQ_DESPESA, .keep_all = TRUE) %>%  # mesmo de-dup por SQ_DESPESA que o app aplica
  mutate(valor = as.numeric(gsub(",", ".", gsub(".", "", VR_DESPESA_CONTRATADA, fixed = TRUE), fixed = TRUE))) %>%
  group_by(SQ_CANDIDATO, NM_CANDIDATO, SG_PARTIDO, DS_CARGO, SG_UF, NM_UE, ST_TURNO) %>%
  summarise(total_geral = sum(valor, na.rm = TRUE), n_despesas = n(), .groups = "drop") %>%
  mutate(mun_uf = if_else(is.na(NM_UE) | NM_UE == "", NA_character_, paste0(NM_UE, " - ", SG_UF)),
         ANO_ELEICAO = ANO)

saveRDS(d, sprintf("totais_%d.rds", ANO), compress = "xz")
unlink(csv_file)

cat(sprintf("totais %d: %d linhas (candidato x turno), R$ %s no total geral\n",
            ANO, nrow(d), format(round(sum(d$total_geral)), big.mark = ".")))
cat(sprintf("Next: Rscript update_totais.R totais_%d.rds %d\n", ANO, ANO))
