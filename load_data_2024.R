library(googlesheets4)
library(lubridate)
library(httr)
library(tidyverse)

setwd("/Users/spagnuolo/data/eleicoes/observatorio-impulsionamento")

# sheets - https://docs.google.com/spreadsheets/d/1ZClRWrYFGsSHF-ONn6U5G4l2jcVQt5-3y33bw7FNG80/edit#gid=0
voltutils::autenticar_gsheets()
id <- googlesheets4::as_sheets_id("1FV13HOm5dAqZR7koVphyAhcgLRSLGmV1sLx3m1WQf9s")
raw <- googlesheets4::read_sheet(id)

# temp <- tempfile()
# download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2022.zip", temp)
# 
## URL <- "https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2022.zip"
## temp <- getURL(URL)

# -------------------------------------------------------------------------

url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2024.zip"

httr::GET(
  url,
  httr::user_agent("Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"),
  httr::write_disk("temp.zip", overwrite = TRUE)
)

zipped_csv_names <- grep('despesas_contratadas_candidatos_2024_BRASIL.csv$', unzip("temp.zip", list=TRUE)$Name, 
                         ignore.case=TRUE, value=TRUE)

d <- read.csv(unzip("temp.zip", files=zipped_csv_names),fileEncoding = "ISO-8859-1", header = T, sep = ";") %>% 
  filter(str_detect(NM_FORNECEDOR, "FACEBOOK|YOUTUBE|INSTAGRAM|TIKTOK|BYTEDANCE|GOOGLE|KWAI|META|WHATSAPP|	
JOYO TECNOLOGIA") |
           str_detect(NM_FORNECEDOR_RFB, "FACEBOOK|YOUTUBE|INSTAGRAM|TIKTOK|BYTEDANCE|GOOGLE|KWAI|META|WHATSAPP|	
JOYO TECNOLOGIA") |
           str_detect(DS_DESPESA, "FACEBOOK|YOUTUBE|INSTAGRAM|TIKTOK|BYTEDANCE|GOOGLE|KWAI|META|WHATSAPP|	
JOYO TECNOLOGIA") |
           str_detect(DS_ORIGEM_DESPESA, "FACEBOOK|YOUTUBE|INSTAGRAM|TIKTOK|BYTEDANCE|GOOGLE|KWAI|META|WHATSAPP|	
JOYO TECNOLOGIA|Despesa com Impulsionamento de Conteúdos")) %>%
  select(NM_CANDIDATO, 
         SG_PARTIDO, 
         DS_CARGO, 
         DT_DESPESA, 
         SG_UF,
         NM_UE,
         ST_TURNO, 
         DT_PRESTACAO_CONTAS, 
         NR_CNPJ_PRESTADOR_CONTA, 
         DS_TIPO_FORNECEDOR, 
         NM_FORNECEDOR, 
         NM_FORNECEDOR_RFB, 
         DS_ORIGEM_DESPESA, 
         DS_DESPESA, 
         VR_DESPESA_CONTRATADA,
         SQ_DESPESA,
         SQ_CANDIDATO
  )

d2 <- d %>% # Create the new column with main company names
  mutate(rede_social_mae = case_when(
    str_detect(NM_FORNECEDOR, "META|FACEBOOK|INSTAGRAM|WHATSAPP") | 
      str_detect(NM_FORNECEDOR_RFB, "META|FACEBOOK|INSTAGRAM|WHATSAPP") | 
      str_detect(DS_DESPESA, "META|FACEBOOK|INSTAGRAM|WHATSAPP") | 
      str_detect(DS_ORIGEM_DESPESA, "META|FACEBOOK|INSTAGRAM|WHATSAPP") ~ "Meta",
    str_detect(NM_FORNECEDOR, "BYTEDANCE|TIKTOK") | 
      str_detect(NM_FORNECEDOR_RFB, "BYTEDANCE|TIKTOK") | 
      str_detect(DS_DESPESA, "BYTEDANCE|TIKTOK") | 
      str_detect(DS_ORIGEM_DESPESA, "BYTEDANCE|TIKTOK") ~ "ByteDance",
    str_detect(NM_FORNECEDOR, "JOYO|KWAI") | 
      str_detect(NM_FORNECEDOR_RFB, "JOYO|KWAI") | 
      str_detect(DS_DESPESA, "JOYO|KWAI") | 
      str_detect(DS_ORIGEM_DESPESA, "JOYO|KWAI") ~ "Kwai",
    str_detect(NM_FORNECEDOR, "GOOGLE|YOUTUBE") | 
      str_detect(NM_FORNECEDOR_RFB, "GOOGLE|YOUTUBE") | 
      str_detect(DS_DESPESA, "GOOGLE|YOUTUBE") | 
      str_detect(DS_ORIGEM_DESPESA, "GOOGLE|YOUTUBE") ~ "Google",
    TRUE ~ "Não informado"  # Assign NA if none of the conditions match
  ))

# View the resulting data frame
write_sheet(d2, ss = "https://docs.google.com/spreadsheets/d/1FV13HOm5dAqZR7koVphyAhcgLRSLGmV1sLx3m1WQf9s/edit?gid=0#gid=0", sheet = "dados")

unlink("*.csv", recursive = FALSE, force = FALSE)
unlink("*.pdf", recursive = FALSE, force = FALSE)
unlink("*.zip", recursive = FALSE, force = FALSE)

print(paste("ETL encerrado às", Sys.time()))
