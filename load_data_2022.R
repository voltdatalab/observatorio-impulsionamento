library(googlesheets4)
library(lubridate)
library(httr)
library(tidyverse)

#setwd("/home/ec2-user/app-eleicoes/")

# sheets - https://docs.google.com/spreadsheets/d/1ZClRWrYFGsSHF-ONn6U5G4l2jcVQt5-3y33bw7FNG80/edit#gid=0
voltutils::autenticar_gsheets()
id <- googlesheets4::as_sheets_id("1ZClRWrYFGsSHF-ONn6U5G4l2jcVQt5-3y33bw7FNG80")
raw <- googlesheets4::read_sheet(id)

# temp <- tempfile()
# download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2022.zip", temp)
# 
# # URL <- "https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2022.zip"
# # temp <- getURL(URL)

# -------------------------------------------------------------------------

url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2022.zip"

httr::GET(
  url,
  httr::user_agent("Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"),
  httr::write_disk("temp.zip", overwrite = TRUE)
)

zipped_csv_names <- grep('despesas_contratadas_candidatos_2022_BRASIL.csv$', unzip("temp.zip", list=TRUE)$Name, 
                         ignore.case=TRUE, value=TRUE)

d <- read.csv(unzip("temp.zip", files=zipped_csv_names),fileEncoding = "ISO-8859-1", header = T, sep = ";") %>% 
  filter(str_detect(NM_FORNECEDOR, "FACEBOOK|YOUTUBE|INSTAGRAM|TIKTOK|BYTEDANCE|TWITTER|GOOGLE") |
           str_detect(NM_FORNECEDOR_RFB, "FACEBOOK|YOUTUBE|INSTAGRAM|TIKTOK|BYTEDANCE|TWITTER|GOOGLE") |
           str_detect(DS_DESPESA, "FACEBOOK|YOUTUBE|INSTAGRAM|TIKTOK|BYTEDANCE|TWITTER|GOOGLE") |
           str_detect(DS_ORIGEM_DESPESA, "FACEBOOK|YOUTUBE|INSTAGRAM|TIKTOK|BYTEDANCE|TWITTER|GOOGLE|Despesa com Impulsionamento de Conteúdos")) %>%
  select(NM_CANDIDATO, 
         SG_PARTIDO, 
         DS_CARGO, 
         DT_DESPESA, 
         SG_UF, 
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

write_sheet(d, ss = "https://docs.google.com/spreadsheets/d/1ZClRWrYFGsSHF-ONn6U5G4l2jcVQt5-3y33bw7FNG80/edit#gid=0", sheet = "dados")

unlink("*.csv", recursive = FALSE, force = FALSE)
unlink("*.pdf", recursive = FALSE, force = FALSE)
unlink("*.zip", recursive = FALSE, force = FALSE)

print(paste("ETL encerrado às", Sys.time()))

d <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQw_zM_7VqGQ0tuxEvmYVBOR1hK702jiThBeKc9NmNWOQGKKD78f-4OKLmLJqRQwD37snObfdEW4jzl/pub?gid=0&single=true&output=csv", header = T)

d$dado <-as.numeric(gsub(",", ".", d$VR_DESPESA_CONTRATADA))

d %>% summarise(total = sum(dado))
                