## ETL: TSE 2026 candidate campaign expenses -> e2026.rds
##
## cdn.tse.jus.br blocks sandboxed/datacenter traffic, so this is meant to be run
## either (a) on a normal machine/network, where the download step below works, or
## (b) by pointing ZIP_PATH at a copy of the zip you already downloaded by hand from
## https://dadosabertos.tse.jus.br/dataset/prestacao-de-contas-eleitorais-2026
## (the file is prestacao_de_contas_eleitorais_candidatos_2026.zip - NOT the
## "prestacao_contas_anual_partidaria" dataset, which is party-level and has no
## per-candidate expense/supplier detail).
##
## Election-year filings are partial until after the election (1st round Oct 4 2026,
## 2nd round Oct 25 2026) - re-run this as new data is filed.

library(lubridate)
library(httr)
suppressMessages(library(tidyverse))

ANO_ELEICAO <- 2026
ZIP_PATH <- Sys.getenv("TSE_ZIP_PATH", unset = "temp.zip")
URL <- "https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2026.zip"

if (!file.exists(ZIP_PATH)) {
  cat("Downloading", URL, "\n")
  httr::GET(
    URL,
    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"),
    httr::write_disk(ZIP_PATH, overwrite = TRUE)
  )
}

zipped_csv_names <- grep(
  sprintf('despesas_contratadas_candidatos_%d_BRASIL.csv$', ANO_ELEICAO),
  unzip(ZIP_PATH, list = TRUE)$Name,
  ignore.case = TRUE, value = TRUE
)

if (length(zipped_csv_names) == 0) {
  stop("Couldn't find the BRASIL expenses CSV inside ", ZIP_PATH,
       " - check the zip actually contains 'despesas_contratadas_candidatos_",
       ANO_ELEICAO, "_BRASIL.csv' (TSE's new Conta+JE system may have renamed it).")
}

raw <- read.csv(unzip(ZIP_PATH, files = zipped_csv_names), fileEncoding = "ISO-8859-1", header = TRUE, sep = ";")

expected_cols <- c(
  "NM_CANDIDATO", "SG_PARTIDO", "DS_CARGO", "DT_DESPESA", "SG_UF", "NM_UE",
  "ST_TURNO", "DT_PRESTACAO_CONTAS", "NR_CNPJ_PRESTADOR_CONTA", "DS_TIPO_FORNECEDOR",
  "NM_FORNECEDOR", "NM_FORNECEDOR_RFB", "DS_ORIGEM_DESPESA", "DS_DESPESA",
  "VR_DESPESA_CONTRATADA", "SQ_DESPESA", "SQ_CANDIDATO"
)
missing <- setdiff(expected_cols, names(raw))
if (length(missing) > 0) {
  stop("TSE changed the CSV schema - missing columns: ", paste(missing, collapse = ", "),
       ". Update the column list in load_data_2026.R before proceeding.")
}

# \\bX BRASIL: fronteira de palavra para não casar "EBANX BRASIL" (processador
# de pagamento) - o X real entra como "X BRASIL..." no início do nome.
# "TIK TOK" (com espaço) aparece assim em DS_DESPESA de prestações reais.
# (?i:...) só nas variantes de TikTok: prestações trazem "Tik tok", "TIK TOK"
# etc. em caixa mista; case-insensitive geral seria perigoso (ex.: "meta")
PLATAFORMAS <- "FACEBOOK|YOUTUBE|INSTAGRAM|(?i:TIKTOK|TIK TOK|TOKTOK|TOK TOK)|BYTEDANCE|GOOGLE|KWAI|META|WHATSAPP|X CORP|TWITTER|\\bX BRASIL|JOYO TECNOLOGIA"

d <- raw %>%
  filter(
    str_detect(NM_FORNECEDOR, PLATAFORMAS) |
      str_detect(NM_FORNECEDOR_RFB, PLATAFORMAS) |
      str_detect(DS_DESPESA, PLATAFORMAS) |
      str_detect(DS_ORIGEM_DESPESA, paste0(PLATAFORMAS, "|Despesa com Impulsionamento de Conteúdos"))
  ) %>%
  select(all_of(expected_cols))

d <- d %>%
  mutate(rede_social_mae = case_when(
    str_detect(NM_FORNECEDOR, "META|FACEBOOK|INSTAGRAM|WHATSAPP|THREADS") |
      str_detect(NM_FORNECEDOR_RFB, "META|FACEBOOK|INSTAGRAM|WHATSAPP|THREADS") |
      str_detect(DS_DESPESA, "META|FACEBOOK|INSTAGRAM|WHATSAPP") |
      str_detect(DS_ORIGEM_DESPESA, "META|FACEBOOK|INSTAGRAM|WHATSAPP") ~ "Meta",
    str_detect(NM_FORNECEDOR, "BYTEDANCE|(?i:TIKTOK|TIK TOK|TOKTOK|TOK TOK)") |
      str_detect(NM_FORNECEDOR_RFB, "BYTEDANCE|(?i:TIKTOK|TIK TOK|TOKTOK|TOK TOK)") |
      str_detect(DS_DESPESA, "BYTEDANCE|(?i:TIKTOK|TIK TOK|TOKTOK|TOK TOK)") |
      str_detect(DS_ORIGEM_DESPESA, "BYTEDANCE|(?i:TIKTOK|TIK TOK|TOKTOK|TOK TOK)") ~ "ByteDance",
    str_detect(NM_FORNECEDOR, "JOYO|KWAI") |
      str_detect(NM_FORNECEDOR_RFB, "JOYO|KWAI") |
      str_detect(DS_DESPESA, "JOYO|KWAI") |
      str_detect(DS_ORIGEM_DESPESA, "JOYO|KWAI") ~ "Kwai",
    str_detect(NM_FORNECEDOR, "GOOGLE|YOUTUBE") |
      str_detect(NM_FORNECEDOR_RFB, "GOOGLE|YOUTUBE") |
      str_detect(DS_DESPESA, "GOOGLE|YOUTUBE") |
      str_detect(DS_ORIGEM_DESPESA, "GOOGLE|YOUTUBE") ~ "Google",
    # \\bX BRASIL: nao casa "EBANX BRASIL" (o X vem colado no N); casa o
    # fornecedor real "X BRASIL ..." com o X no inicio de palavra
    str_detect(NM_FORNECEDOR, "X CORP|TWITTER|\\bX BRASIL") |
      str_detect(NM_FORNECEDOR_RFB, "X CORP|TWITTER|\\bX BRASIL") |
      str_detect(DS_DESPESA, "X CORP|TWITTER|\\bX BRASIL") |
      str_detect(DS_ORIGEM_DESPESA, "X CORP|TWITTER|\\bX BRASIL") ~ "X (Twitter)",
    TRUE ~ "Não informado"
  ))

saveRDS(d, sprintf("e%d.rds", ANO_ELEICAO), compress = "xz")

unlink("*.csv")
unlink("*.pdf")
if (ZIP_PATH == "temp.zip") unlink(ZIP_PATH)

cat(sprintf(
  "ETL 2026 encerrado às %s - %d linhas, R$%s\n",
  Sys.time(), nrow(d), format(round(sum(as.numeric(gsub(",", ".", gsub(".", "", d$VR_DESPESA_CONTRATADA, fixed = TRUE)))), 0), big.mark = ".")
))
cat(sprintf("Next: Rscript update_database.R e%d.rds %d\n", ANO_ELEICAO, ANO_ELEICAO))
