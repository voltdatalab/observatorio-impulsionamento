library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(htmlwidgets)
library(DT)

#desabilita notação científica para números
options(scipen = 999)
options(shiny.sanitize.errors = FALSE)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(collapsed = TRUE),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      )
    ),
    fluidRow(
      column(4,  
             column(6,uiOutput('estados')),
             column(6,uiOutput('legenda')),
             column(6,uiOutput('cargos')),
             column(6,uiOutput('politicos')),
             column(6,
                    textInput(inputId = "valor_custom",
                              label = tags$div(icon("money-bill", class = "icons"),
                                               'Valor mínimo', tags$br(),
                                               tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Filtre por valor acima de R$1")),
                              value = "",
                              placeholder = "Apenas números")),
             column(6,
                    selectInput(inputId = "rede",
                                label = tags$div(icon("share-alt-square", class = "icons"),
                                                 'Redes', tags$br(),
                                                 tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Selecione uma rede")),
                                choices = c("Todas", 
                                            "Facebook", 
                                            "Google", 
                                            "ByteDance (TikTok)" = "ByteDance", 
                                            "YouTube"),
                                selected = "Todas"
                    )
             ),
             column(12,
                    dateRangeInput(inputId = "data",
                                   label = tags$div(icon("calendar", class = "icons"),
                                                    'Datas (dd/mm/aa)',tags$br(),
                                                    tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Desde início de agosto")),
                                   start = "2022-08-04",  end = Sys.Date(),
                                   min = "2022-08-04",   max = Sys.Date(),
                                   format = "dd/mm/yyyy", weekstart = 0,
                                   language = "pt",       separator = " ATÉ ",
                                   width = NULL,          autoclose = TRUE),
             )
             
      ),
      valueBox(textOutput("n_gastos"), "gastos com rubrica \"impulsionamento\"", icon = icon("list-alt")),
      valueBox(textOutput("n_candidatos"), "candidatos impulsionaram conteúdo", icon = icon("users")),
      valueBox(textOutput("total_gasto"), "gastos com rubrica \"impulsionamento\"", icon = icon("money-bill")),
      valueBox(textOutput("media_gasto"), "foi a média de gastos", icon = icon("grip-lines")),
      valueBox(textOutput("maior_gasto"), "foi o maior gasto", icon = icon("sort-up")),
      valueBox(textOutput("menor_gasto"), "foi o menor gasto", icon = icon("sort-down")),
      tags$div(style="padding:20px",
              DT::DTOutput("table"),
               
      )
    )
    
    
  )
)

server <- function(input, output) { 
  
  dt <- reactive({
    d <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQw_zM_7VqGQ0tuxEvmYVBOR1hK702jiThBeKc9NmNWOQGKKD78f-4OKLmLJqRQwD37snObfdEW4jzl/pub?gid=0&single=true&output=csv", header = T)
    
    d <- d %>% 
      filter(DS_ORIGEM_DESPESA == "Despesa com Impulsionamento de Conteúdos")
    
    d$valor <- gsub(".", "", d$VR_DESPESA_CONTRATADA)
    d$valor <- as.numeric(gsub(",", ".", d$VR_DESPESA_CONTRATADA)) 
    d$DT_DESPESA <- as_date(d$DT_DESPESA,format="%d/%m/%Y")
    #d$DT_DESPESA <- sub("^0+", "", d$DT_DESPESA)
    
    d <- d %>%
      select(NM_CANDIDATO, SG_PARTIDO, DS_CARGO, DT_DESPESA,valor, SG_UF, ST_TURNO, NR_CNPJ_PRESTADOR_CONTA, DS_TIPO_FORNECEDOR, NM_FORNECEDOR, NM_FORNECEDOR_RFB, DS_DESPESA, DS_ORIGEM_DESPESA) %>%
      rename(Candidato = NM_CANDIDATO, 
             Partido = SG_PARTIDO,
             Cargo = DS_CARGO,
             "Turno" = ST_TURNO,
             UF = SG_UF,
             "Valor" = valor,
             "Descrição" = DS_DESPESA,
             "Data da despesa" = DT_DESPESA,
             "CNPJ prestador" = NR_CNPJ_PRESTADOR_CONTA,
             "Tipo de fornecedor" = DS_TIPO_FORNECEDOR,
             "Nome do fornecedor" = NM_FORNECEDOR,
             "Nome do prestador" = NM_FORNECEDOR_RFB,
             "Origem da despesa" = DS_ORIGEM_DESPESA
      )
    
    return(d)
  })
  
  output$estados <- renderUI({
    
    d <- dt()
    
    selectizeInput(inputId = "uf",
                   #multiple = TRUE,
                   label = tags$div(icon("map-marker-alt", class = "icons"),
                                    'UF', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Escolha um cargo.")),
                   choices  = c("Todas", as.list(unique(d$UF))),
                   selected = "Todas")
  })
  
  output$legenda <- renderUI({
    
    d <- dt()
    
    selectizeInput(inputId = "partido",
                   #multiple = TRUE,
                   label = tags$div(icon("paste", class = "icons"),
                                    'Partidos', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Escolha um partido")),
                   choices  = c("Todos", as.list(unique(d$Partido))),
                   selected = "Todos")
  })
  
  output$cargos <- renderUI({
    
    d <- dt()
    
    selectizeInput(inputId = "cargo",
                   #multiple = TRUE,
                   label = tags$div(icon("suitcase", class = "icons"),
                                    'Cargo', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Escolha um ou mais cargos")),
                   choices  = c("Todos", as.list(unique(d$Cargo))),
                   selected = "Todos")
  })
  
  output$politicos <- renderUI({
    
    d <- dt()
    
    d <- d %>% arrange(Candidato)
    
    selectizeInput(inputId = "politico",
                   #multiple = TRUE,
                   label = tags$div(icon("user", class = "icons"),
                                    'Candidato', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Escolha um candidato")),
                   choices  = c("Todos", as.list(unique(d$Candidato))),
                   selected = "Todos",
                   multiple = FALSE)
  })
  
  
  dados <- reactive({
    d <- dt()
    
    d <- d %>% filter(`Data da despesa` >= input$data[1] & `Data da despesa` <= input$data[2])
    
    if (input$partido != "Todos") {
      d <- d %>% filter(Partido == input$partido)
    }
    if(input$uf != "Todas"){
      d <- d %>% filter(UF == input$uf)
    }
    if(input$cargo != "Todos"){
      d <- d %>% filter(Cargo == input$cargo)
    }
    if(input$politico != "Todos"){
      d <- d %>% filter(Candidato == input$politico)
    }
    if(input$valor_custom != ""){
      d <- d %>% filter(Valor >= input$valor_custom)
    }
    if(input$rede != "Todas"){
      d <- d %>% filter(str_detect(Descrição, regex(input$rede, ignore_case = TRUE)) | 
                         str_detect(`Nome do prestador`, regex(input$rede, ignore_case = TRUE)) | 
                          str_detect(`Nome do fornecedor`, regex(input$rede, ignore_case = TRUE))
        )
    }
    
    return(d)
  })
  
  # STATS
  output$n_gastos <- renderText({
    d <- dados()
    
    d <- d %>%
      tally()
    
    paste0(format(round(d$n, 1), big.mark=".", small.mark = ","))
  })
  
  output$n_candidatos <- renderText({
    d <- dados()
    
    d <- d %>%
      n_distinct(na.rm = FALSE)
    
    paste0(format(round(d), big.mark=".", small.mark = ","))
  })
  
  output$total_gasto <- renderText({
    d <- dados()
    
    d <- d %>%
      summarise(t = sum(Valor))
    
    paste0("R$", format(round(d$t, 0), big.mark=".", small.mark = ","))
  })
  
  output$media_gasto <- renderText({
    d <- dados()
    
    d <- d %>%
      summarise(t = mean(Valor))
    
    paste0("R$", format(round(d$t, 0), big.mark=".", small.mark = ","))
  })
  
  output$maior_gasto <- renderText({
    d <- dados()
    
    d <- d %>%
      summarise(t = max(Valor))
    
    paste0("R$", format(round(d$t, 0), big.mark=".", small.mark = ","))
  })
  
  output$menor_gasto <- renderText({
    d <- dados()
    
    d <- d %>%
      summarise(t = min(Valor))
    
    paste0("R$", format(round(d$t, 0), big.mark=".", small.mark = ","))
  })
  
  output$table <- DT::renderDT({
    
    # Importa os dados principais e filtra pelas datas do input$date
    main_table <- dados()
    
    # Gera a tabela principal
    
    main_table
    
  }, escape = FALSE,
  filter = "top",
  callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Busca" )'),
  extensions = c("Buttons", "Scroller"), 
  rownames = FALSE,
  # CONFIGURACOES GERAIS DA TABELA
  options = list(
    #language = list(searchPlaceholder = "Busca por palavra-chave...",
    #              zeroRecords = "Não há resultados para a sua busca.",
    #             sSearch = ""),
    scrollY = 500, scroller = TRUE, scrollX = T,
    pageLength = 50,
    lengthMenu = list( c(10, 50, 100, -1) # declare values
                       , c(10, 50, 100, "Todos") # declare titles
    ),
    dom = 'fBlrtip',
    buttons = 
      list('copy', list(
        extend = 'collection',
        buttons = c('csv', 'excel'),
        text = 'Baixe os dados',
        exportOptions = list(
          modifiers = list(selected = TRUE)
        )
      )),
    language = list(
      lengthMenu = "Mostrando _MENU_ registros",
      buttons = list(copy = 'Copiar tabela', 
                     copyTitle = "Tabela copiada com sucesso", 
                     copySuccess = "%d linhas copiadas"),
      info = 'FONTE: TSE/Análise Núcleo Jornalismo',
      paginate = list(previous = 'Anterior', `next` = 'Próxima'),
      processing = "CARREGANDO OS DADOS...",
      searchPlaceholder = "Busque em todas as colunas",
      search = "",
      emptyTable = "INICIE SUA BUSCA POR TERMOS DE PESQUISA",
      zeroRecords = "SEM RESULTADOS PARA MOSTRAR, FAÇA NOVA BUSCA"),
    info = TRUE
  )
  
  # Fecha DT::datatable
  )
  
}

shinyApp(ui, server)