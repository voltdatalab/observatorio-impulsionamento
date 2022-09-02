library(googlesheets4)
library(lubridate)
#library(RCurl)

# sheets - https://docs.google.com/spreadsheets/d/1ZClRWrYFGsSHF-ONn6U5G4l2jcVQt5-3y33bw7FNG80/edit#gid=0
voltutils::autenticar_gsheets()
id <- googlesheets4::as_sheets_id("1ZClRWrYFGsSHF-ONn6U5G4l2jcVQt5-3y33bw7FNG80")
raw <- googlesheets4::read_sheet(id)

temp <- tempfile()
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2022.zip", temp)

# URL <- "https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2022.zip"
# temp <- getURL(URL)

# -------------------------------------------------------------------------


zipped_csv_names <- grep('despesas_contratadas_candidatos_2022_BRASIL.csv$', unzip(temp, list=TRUE)$Name, 
                         ignore.case=TRUE, value=TRUE)

d <- read.csv(unzip(temp, files=zipped_csv_names),fileEncoding = "ISO-8859-1", header = T, sep = ";") %>% 
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
         VR_DESPESA_CONTRATADA)

write_sheet(d, ss = "https://docs.google.com/spreadsheets/d/1ZClRWrYFGsSHF-ONn6U5G4l2jcVQt5-3y33bw7FNG80/edit#gid=0", sheet = "dados")