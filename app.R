library(shiny)
library(shinydashboard)
library(lubridate)
library(htmlwidgets)
library(DT)
library(ggthemes)
library(shinyWidgets)
library(plotly)
library(scales)
library(ggbeeswarm)
suppressMessages(library(tidyverse))
library(shinybusy)
library(RSQLite)
library(pool)

### FUNCOES
# source("funcoes.R")

#desabilita notação científica para números
options(scipen = 999)
options(shiny.sanitize.errors = FALSE)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(collapsed = TRUE),
  dashboardBody(
    tags$head(
      tags$script(src = "resizer.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    ),
    # add_busy_spinner(
    #   spin = "double-bounce",
    #   color = "#2ADD90",
    #   timeout = 100,
    #   position = "full-page",
    #   onstart = TRUE,
    #   margins = c(10, 10),
    #   height = "150px",
    #   width = "150px"           # Make the spinner 3 times larger than the default size
    # ),
    # Custom loading text element
    # tags$div(
    #   class = "loading-text",
    #   "Carregando..."  # The text to be shown with the spinner
    # ),
    tags$img(src = "header_impulsionamento.png", class = "app-header", alt = "Observatório de Impulsionamento Eleitoral"),
    fluidRow(
      column(6, offset = 3,
             tags$div(class="warning",
                      tags$p("⚠️ As descrições de impulsionamento são autodeclaradas e muitas vezes não é possível determinar o destino dos gastos sob essas rubricas. Veja as notas abaixo para esclarecimentos.")
             ),
             tags$div(class = "update-note", textOutput("ultima_atualizacao"))
      )
    ),
    fluidRow(
      column(4, tags$div(class = "filters-card",
             column(6,uiOutput('eleicao')),
             column(6,uiOutput('estados')),
             column(6,uiOutput('mun')),
             column(6,uiOutput('legenda')),
             column(6,uiOutput('cargos')),
             column(6,uiOutput('politicos')),
             column(6,uiOutput('turno')),
             column(6,
                    textInput(inputId = "valor_custom",
                              label = tags$div(icon("money-bill", class = "icons"),
                                               'Valor mínimo', tags$br(),
                                               tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Filtre por valor")),
                              value = "",
                              placeholder = "Apenas números")),
             column(6,
                    selectInput(inputId = "rede",
                                label = tags$div(icon("share-alt-square", class = "icons"),
                                                 'Redes', tags$br(),
                                                 tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Selecione uma rede")),
                                choices = c("Todas", 
                                            "Meta (Facebook, Instagram e WhatsApp)" = "Meta", 
                                            "Google e YouTube" = "Google", 
                                            "ByteDance (TikTok)" = "ByteDance", 
                                            "Kwai"),
                                selected = "Todas"
                    )
             ),
             column(12, uiOutput('periodo')),
             column(12, style="margin-top:20px",
                    prettyRadioButtons(inputId = "valores",
                                       label = tags$div(icon("line-chart", class = "icons"),
                                                        '',tags$br(),
                                                        tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "")),
                                       choices = c("Ver gráficos em valores R$" = "Valores R$", "Ver gráficos em número de gastos" = "Contagem"),
                                       shape = c("curve"),
                                       fill = TRUE,
                                       inline = TRUE,
                    )
             ),

      )),
      column(8, class = "kpi-row",
        valueBox(textOutput("n_gastos"), "gastos com rubrica \"impulsionamento\"", icon = icon("list-alt"), width = 6),
        valueBox(textOutput("n_candidatos"), "candidatos impulsionaram conteúdo", icon = icon("users"), width = 6),
        valueBox(textOutput("total_gasto"), "gastos com rubrica \"impulsionamento\"", icon = icon("money-bill"), width = 6),
        valueBox(textOutput("media_gasto"), "foi a média dos gastos", icon = icon("grip-lines"), width = 6),
        valueBox(textOutput("maior_gasto"), "foi o maior gasto", icon = icon("sort-up"), width = 6),
        valueBox(textOutput("pct_impulsionamento"), "do total gasto pelas campanhas foi com impulsionamento", icon = icon("percent"), width = 6)
      )
    ),
    fluidRow(
      column(6, tags$div(class = "chart-card",
                    conditionalPanel("input.valores == 'Contagem'",
                                     tags$h2("NÚMERO DE IMPULSIONAMENTOS"),
                                     tags$h4("Total de despesas com impulsionamento de conteúdo nas redes sociais contratadas por políticos nas eleições")
                    ),
                    conditionalPanel("input.valores == 'Valores R$'",
                                     tags$h2("VALOR DE IMPULSIONAMENTOS"),
                                     tags$h4("Valor (R$) acumulado de despesas com impulsionamento de conteúdo nas redes sociais contratadas por políticos nas eleições")
                    ),
                    plotlyOutput("graf_gastos"))
             ),
             column(6, tags$div(class = "chart-card",
                    conditionalPanel("input.valores == 'Contagem'",
                                     tags$b(tags$h2("IMPULSIONAMENTOS DE PARTIDOS")),
                                     tags$h4("Número despesas, por partido, com impulsionamento de conteúdo nas redes sociais contratadas por políticos nas eleições")
                    ),
                    conditionalPanel("input.valores == 'Valores R$'",
                                     tags$h2("VALOR DE IMPULSIONAMENTOS DE PARTIDO"),
                                     tags$h4("Valor (R$) acumulado, por partido, de despesas com impulsionamento de conteúdo nas redes sociais contratadas por políticos")
                    ),
                    plotlyOutput("graf_partidos"))
      )
    ),
    fluidRow(class = "m-hide",
      column(6, tags$div(class = "chart-card",
                             conditionalPanel("input.valores == 'Contagem'",
                                              tags$b(tags$h2("POLÍTICOS QUE MAIS IMPULSIONARAM NAS REDES")),
                                              tags$h4("Número despesas, por político, com impulsionamento de conteúdo nas redes sociais contratadas por políticos nas eleições")
                             ),
                             conditionalPanel("input.valores == 'Valores R$'",
                                              tags$h2("POLÍTICOS QUE MAIS IMPULSIONARAM NAS REDES"),
                                              tags$h4("Valor (R$) acumulado, por político, de despesas com impulsionamento de conteúdo nas redes sociais contratadas por políticos")
                             ),
                             plotlyOutput("graf_politicos_top"))
      ),
      column(6, tags$div(class = "chart-card",
                             conditionalPanel("input.valores == 'Contagem'",
                                              tags$b(tags$h2("IMPULSIONAMENTOS POR REDE SOCIAL")),
                                              tags$h4("Número de despesas, por rede social, com impulsionamento de conteúdo contratadas por políticos nas eleições")
                             ),
                             conditionalPanel("input.valores == 'Valores R$'",
                                              tags$h2("VALOR DE IMPULSIONAMENTOS POR REDE SOCIAL"),
                                              tags$h4("Valor (R$) acumulado, por rede social, de despesas com impulsionamento de conteúdo contratadas por políticos")
                             ),
                             plotlyOutput("graf_redes"))
      )
    ),
    fluidRow(class = "m-hide",
      column(6, tags$div(class = "chart-card",
                             conditionalPanel("input.valores == 'Contagem'",
                                              tags$b(tags$h2("IMPULSIONAMENTOS DE POLÍTICOS")),
                                              tags$h4("Número despesas, por político, com impulsionamento de conteúdo nas redes sociais contratadas por políticos nas eleições")
                             ),
                             conditionalPanel("input.valores == 'Valores R$'",
                                              tags$h2("VALOR DE IMPULSIONAMENTOS DE POLÍTICOS"),
                                              tags$h4("Valor (R$) acumulado, por político, de despesas com impulsionamento de conteúdo nas redes sociais contratadas por políticos")
                             ),
                             plotlyOutput("graf_politicos"))
      ),
      column(6, tags$div(class = "chart-card",
                             conditionalPanel("input.valores == 'Contagem'",
                                              tags$b(tags$h2("IMPULSIONAMENTOS POR MUNICÍPIO")),
                                              tags$h4("Número despesas, por local, com impulsionamento de conteúdo nas redes sociais contratadas por políticos nas eleições")
                             ),
                             conditionalPanel("input.valores == 'Valores R$'",
                                              tags$h2("VALOR DE IMPULSIONAMENTOS POR MUNICÍPIO"),
                                              tags$h4("Valor (R$) acumulado, por local, de despesas com impulsionamento de conteúdo nas redes sociais contratadas por políticos")
                             ),
                             plotlyOutput("graf_ufs"))
      )
    ),
    fluidRow(
      column(12,
             tags$div(class = "table-card",
                      DT::DTOutput("table")
             )
      )
    )
    
    
  )
)

server <- function(input, output, session) {

  # Connection pool for better performance
  pool <- dbPool(
    drv = RSQLite::SQLite(),
    # OBS_DB_PATH permite apontar para o .db num volume compartilhado (CapRover):
    # o ETL escreve no mesmo arquivo a partir de outro container
    dbname = Sys.getenv("OBS_DB_PATH", "obseleitoral.db"),
    maxSize = 10,
    onCreate = function(conn) {
      # WAL lets concurrent shinyapps.io sessions read without blocking each other
      DBI::dbExecute(conn, "PRAGMA journal_mode=WAL;")
      DBI::dbExecute(conn, "PRAGMA busy_timeout=5000;")
    }
  )

  # Close pool when session ends
  onStop(function() {
    poolClose(pool)
  })

  tema <- function(base_size = 14 , base_family = "Barlow"){(
    
    theme_foundation(base_size = base_size, base_family = base_family) +
      theme(
        plot.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
        panel.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
        text = element_text(colour = "#000000"),
        
        axis.text = element_text(size = rel(0.8), margin=margin(0,40,0,0)),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = rel(0.9), colour = "#000000"),
        
        legend.text = element_text(size=rel(0.9), angle = 0),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#eeeeee", colour = "#eeeeee", size = 0.5, linetype='dashed'),
        legend.key.width = unit(0.6, "cm"),
        legend.position = "top",
        legend.justification = c(-0.05, 0),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.margin = (margin=margin(0,0,0,0)),
        legend.box = NULL,
        
        panel.border = element_rect(colour = "#eeeeee", fill=NA, size=2),
        panel.grid.major = element_line(colour = "#cbcbcb"),
        panel.grid.minor = element_line(colour = "#cbcbcb"),
        panel.grid.minor.x = element_line(colour = "#cbcbcb"),
        
        plot.title = element_text(hjust = 0, size = rel(1.3), face = "bold", colour = "#231f20"),
        plot.title.position = "plot",
        strip.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
        plot.subtitle = element_text(hjust = 0, margin=margin(0,0,40,0),size = rel(1), lineheight = 1),
        plot.caption = element_text(size = rel(0.75), hjust = 1, margin=margin(20,0,0,0), colour = "#555555", lineheight = 1),
        plot.margin = unit(c(1, 1, 1, 0), "lines")
      )
  )
  }
  
  # Cache filter data - load once and reuse
  filter_cache <- reactiveVal(NULL)

  observe({
    if (is.null(filter_cache())) {
      periodo <- dbGetQuery(pool, "SELECT MIN(dt_despesa_iso) as min_d, MAX(dt_despesa_iso) as max_d FROM despesas")

      filter_cache(list(
        anos = dbGetQuery(pool, "SELECT DISTINCT ANO_ELEICAO FROM despesas ORDER BY ANO_ELEICAO DESC")$ANO_ELEICAO,
        ufs = dbGetQuery(pool, "SELECT DISTINCT SG_UF FROM despesas ORDER BY SG_UF")$SG_UF,
        municipios = dbGetQuery(pool, "SELECT DISTINCT mun_uf FROM despesas WHERE mun_uf IS NOT NULL ORDER BY mun_uf")$mun_uf,
        partidos = dbGetQuery(pool, "SELECT DISTINCT SG_PARTIDO FROM despesas ORDER BY SG_PARTIDO")$SG_PARTIDO,
        cargos = dbGetQuery(pool, "SELECT DISTINCT DS_CARGO FROM despesas ORDER BY DS_CARGO")$DS_CARGO,
        turnos = dbGetQuery(pool, "SELECT DISTINCT ST_TURNO FROM despesas ORDER BY ST_TURNO")$ST_TURNO,
        data_min = as_date(periodo$min_d),
        data_max = as_date(periodo$max_d)
      ))
    }
  })

  # Helper function to build SQL WHERE clause from filters (using optimized columns)
  # Filtros que descrevem o universo de candidatos - colunas que existem tanto em
  # `despesas` quanto em `totais` (gasto total de campanha), para o KPI de % poder
  # aplicar exatamente o mesmo recorte nos dois lados da divisão.
  build_universe_parts <- function() {
    parts <- c()

    # Eleição (ano) filter - sempre restringe a um ano; somar eleições
    # diferentes mistura valores nominais (inflação)
    parts <- c(parts, sprintf("ANO_ELEICAO = %s", as.integer(input$ano_eleicao)))

    # Partido filter
    if (!is.null(input$partido) && input$partido != "Todos") {
      parts <- c(parts, sprintf("SG_PARTIDO = '%s'", gsub("'", "''", input$partido)))
    }

    # UF filter
    if (!is.null(input$uf) && input$uf != "Todas") {
      parts <- c(parts, sprintf("SG_UF = '%s'", gsub("'", "''", input$uf)))
    }

    # Municipio filter - use computed column
    if (!is.null(input$mun) && input$mun != "Todos") {
      parts <- c(parts, sprintf("mun_uf = '%s'", gsub("'", "''", input$mun)))
    }

    # Cargo filter
    if (!is.null(input$cargo) && input$cargo != "Todos") {
      parts <- c(parts, sprintf("DS_CARGO = '%s'", gsub("'", "''", input$cargo)))
    }

    # Turno filter
    if (!is.null(input$turno) && input$turno != "Todos") {
      parts <- c(parts, sprintf("ST_TURNO = %s", input$turno))
    }

    # Candidato filter
    if (!is.null(input$politico) && input$politico != "Todos") {
      parts <- c(parts, sprintf("NM_CANDIDATO = '%s'", gsub("'", "''", input$politico)))
    }

    parts
  }

  build_where_clause <- function() {
    where_parts <- build_universe_parts()

    # Date filter - use ISO format column
    if (!is.null(input$data)) {
      start_date <- format(input$data[1], "%Y-%m-%d")
      end_date <- format(input$data[2], "%Y-%m-%d")
      where_parts <- c(where_parts, sprintf("dt_despesa_iso >= '%s' AND dt_despesa_iso <= '%s'", start_date, end_date))
    }

    # Valor mínimo filter - use computed numeric column
    if (!is.null(input$valor_custom) && input$valor_custom != "") {
      valor_min <- as.numeric(input$valor_custom)
      if (!is.na(valor_min)) {
        where_parts <- c(where_parts, sprintf("valor_numeric >= %f", valor_min))
      }
    }

    # Rede social filter
    if (!is.null(input$rede) && input$rede != "Todas") {
      where_parts <- c(where_parts, sprintf("rede_social_mae LIKE '%%%s%%'", gsub("'", "''", input$rede)))
    }

    if (length(where_parts) > 0) {
      return(paste("WHERE", paste(where_parts, collapse = " AND ")))
    }
    return("")
  }

  # Lazy data loading with debouncing - only fetches filtered data when needed
  dados <- reactive({
    # Espera o seletor de ano chegar do cliente: evita uma query inicial
    # sem filtro de ano (agregando todas as eleições)
    req(input$ano_eleicao)
    # Debounce to avoid excessive queries
    where_clause <- build_where_clause()

    query <- sprintf("
      SELECT
        ANO_ELEICAO as Eleição,
        NM_CANDIDATO as Candidato,
        SG_PARTIDO as Partido,
        DS_CARGO as Cargo,
        dt_despesa_iso as 'Data da despesa',
        valor_numeric as Valor,
        SG_UF as UF,
        mun_uf as 'Município - UF',
        ST_TURNO as Turno,
        NR_CNPJ_PRESTADOR_CONTA as 'CNPJ prestador',
        DS_TIPO_FORNECEDOR as 'Tipo de fornecedor',
        NM_FORNECEDOR as 'Nome do fornecedor',
        NM_FORNECEDOR_RFB as 'Nome do prestador',
        DS_DESPESA as 'Descrição',
        DS_ORIGEM_DESPESA as 'Origem da despesa',
        SQ_DESPESA as 'Código da Despesa',
        SQ_CANDIDATO as 'Código da candidato',
        rede_social_mae as 'Rede social'
      FROM despesas
      %s
    ", where_clause)

    d <- dbGetQuery(pool, query)

    # Convert ISO date to Date object
    d$`Data da despesa` <- as_date(d$`Data da despesa`)

    # Remove duplicates
    d <- d %>% distinct(`Código da Despesa`, .keep_all = TRUE)

    return(d)
  }) %>% debounce(500)  # Wait 500ms before executing query
  
  # Eleição (ano) filter - uses cached filter data
  output$eleicao <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    selectizeInput(inputId = "ano_eleicao",
                   label = tags$div(icon("calendar-check", class = "icons"),
                                    'Eleição', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Escolha um ano")),
                   choices  = filters$anos,
                   selected = max(filters$anos))
  })

  # Período (datas) filter - bounds derived live from the data, uses cached filter data
  output$periodo <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    dateRangeInput(inputId = "data",
                   label = tags$div(icon("calendar", class = "icons"),
                                    'Datas (dd/mm/aa)',tags$br(),
                                    tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Selecione um período")),
                   start = filters$data_min,  end = filters$data_max,
                   min = filters$data_min,    max = Sys.Date(),
                   format = "dd/mm/yyyy", weekstart = 0,
                   language = "pt",       separator = " ATÉ ",
                   width = NULL,          autoclose = TRUE)
  })

  # Updated 'mun' UI - uses cached filter data
  output$mun <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    selectizeInput(inputId = "mun",
                   label = tags$div(icon("map-marker-alt", class = "icons"),
                                    'Município - UF', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Escolha um município")),
                   choices  = c("Todos", filters$municipios),
                   selected = "Todos",
                   options = list(maxOptions = 5000))
  })

  # Updated 'estados' UI - uses cached filter data
  output$estados <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    selectizeInput(inputId = "uf",
                   label = tags$div(icon("map-marker-alt", class = "icons"),
                                    'UF', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Selecione uma UF")),
                   choices  = c("Todas", filters$ufs),
                   selected = "Todas")
  })

  output$legenda <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    selectizeInput(inputId = "partido",
                   label = tags$div(icon("paste", class = "icons"),
                                    'Partidos', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Escolha um partido")),
                   choices  = c("Todos", filters$partidos),
                   selected = "Todos")
  })

  output$turno <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    selectizeInput(inputId = "turno",
                   label = tags$div(icon("suitcase", class = "icons"),
                                    'Turno', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Selecione um turno")),
                   choices  = c("Todos", filters$turnos),
                   selected = "Todos")
  })

  output$cargos <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    selectizeInput(inputId = "cargo",
                   label = tags$div(icon("suitcase", class = "icons"),
                                    'Cargo', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Escolha um ou mais cargos")),
                   choices  = c("Todos", filters$cargos),
                   selected = "Todos")
  })

  # Server-side selectize for candidates (39K+ options)
  output$politicos <- renderUI({
    selectizeInput(inputId = "politico",
                   label = tags$div(icon("user", class = "icons"),
                                    'Candidato', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Escolha um candidato")),
                   choices  = NULL,
                   selected = NULL,
                   multiple = FALSE,
                   options = list(
                     placeholder = 'Digite para buscar...',
                     maxOptions = 50
                   ))
  })

  # Server-side search for candidates (loads on demand)
  updateSelectizeInput(session, "politico",
    choices = c("Todos" = "Todos"),
    server = TRUE,
    options = list(
      load = I('function(query, callback) {
        if (!query.length) return callback();
        Shiny.setInputValue("candidate_search", query, {priority: "event"});
      }')
    )
  )

  # Handle candidate search
  observeEvent(input$candidate_search, {
    query <- input$candidate_search
    if (!is.null(query) && nchar(query) >= 2) {
      results <- dbGetQuery(pool, sprintf(
        "SELECT DISTINCT NM_CANDIDATO FROM despesas WHERE NM_CANDIDATO LIKE '%%%s%%' ORDER BY NM_CANDIDATO LIMIT 50",
        gsub("'", "''", query)
      ))
      choices <- c("Todos", results$NM_CANDIDATO)
      updateSelectizeInput(session, "politico", choices = choices, server = TRUE)
    }
  })

  # Data mais recente de registro no TSE (DT_PRESTACAO_CONTAS) para o ano
  # selecionado - é a data que vem nos próprios dados, não a hora do ETL.
  # (DT_DESPESA não serve: tem datas futuras digitadas errado nas prestações.)
  output$ultima_atualizacao <- renderText({
    req(input$ano_eleicao)
    m <- dbGetQuery(pool, sprintf(
      "SELECT MAX(substr(DT_PRESTACAO_CONTAS,7,4) || '-' || substr(DT_PRESTACAO_CONTAS,4,2) || '-' || substr(DT_PRESTACAO_CONTAS,1,2)) AS m
       FROM despesas WHERE ANO_ELEICAO = %d AND length(DT_PRESTACAO_CONTAS) = 10",
      as.integer(input$ano_eleicao)))$m
    if (is.null(m) || is.na(m)) return("")
    paste0("Dados registrados no TSE até ", format(as_date(m), "%d/%m/%Y"))
  })

  ##########################################
  ############## BIG NUMBERS
  output$n_gastos <- renderText({
    d <- dados()
    
    d <- d %>%
      tally()
    
    paste0(format(round(d$n, 1), big.mark=".", decimal.mark = ","))
  })
  
  output$n_candidatos <- renderText({
    d <- dados()
    
    d <- d %>%
      distinct(`Código da candidato`, .keep_all = TRUE) %>%
      tally()
    #n_distinct(Candidato, na.rm = FALSE)
    
    paste0(format(round(d), big.mark=".", decimal.mark = ","))
  })
  
  output$total_gasto <- renderText({
    d <- dados()
    
    d <- d %>%
      summarise(t = sum(Valor))
    
    paste0("R$", format(round(d$t, 0), big.mark=".", decimal.mark = ","))
  })
  
  output$media_gasto <- renderText({
    d <- dados()
    
    d <- d %>%
      summarise(t = mean(Valor))
    
    paste0("R$", format(round(d$t, 0), big.mark=".", decimal.mark = ","))
  })
  
  output$maior_gasto <- renderText({
    d <- dados()
    
    d <- d %>%
      summarise(t = max(Valor))
    
    paste0("R$", format(round(d$t, 0), big.mark=".", decimal.mark = ","))
  })
  
  # % do gasto total de campanha que foi para impulsionamento. Usa só os filtros
  # de universo (ano/UF/município/partido/cargo/turno/candidato) - datas, rede e
  # valor mínimo não se aplicam à tabela `totais`, que é agregada por candidato.
  # GROUP BY SQ_DESPESA espelha o de-dup do reactive dados().
  output$pct_impulsionamento <- renderText({
    req(input$ano_eleicao)
    w <- paste("WHERE", paste(build_universe_parts(), collapse = " AND "))

    imp <- dbGetQuery(pool, sprintf(
      "SELECT COALESCE(SUM(valor_numeric), 0) AS v FROM (
         SELECT valor_numeric FROM despesas %s GROUP BY SQ_DESPESA)", w))$v
    tot <- dbGetQuery(pool, sprintf(
      "SELECT COALESCE(SUM(total_geral), 0) AS v FROM totais %s", w))$v

    if (is.na(tot) || tot <= 0) return("–")
    paste0(format(round(100 * imp / tot, 1), decimal.mark = ",", nsmall = 1), "%")
  })
  
  
  ##########################################
  ############## GRAFICOS
  output$graf_gastos <- renderPlotly({
    d <- dados()
    d$data_c <- as.POSIXct(d$`Data da despesa`,format="%d/%m/%Y")
    
    if(input$valores == 'Contagem'){
      d <- d %>%
        group_by(data_c) %>%
        count()
      
      labs <- labs(x="", y = "n despesas")
      escala <- scale_y_continuous(
        labels = scales::dollar_format(scale = 1, prefix = "", suffix = ""),
        #labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }else{
      d <- d %>%
        group_by(data_c) %>%
        summarise(n = sum(Valor))
      
      labs <- labs(x="", y = "")
      escala <- scale_y_continuous(
        labels = scales::dollar_format(scale = .000001, prefix = "R$", suffix = " mi"),
        #labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }
    
    graf <- ggplot(d, aes(data_c,n)) +
      geom_bar(aes(text = paste('<b>DATA:</b>', format(data_c, format = "%d/%m/%Y"),
                                '<br><b>MONTANTE:</b>', format(round(n, 1), big.mark = ",", decimal.mark = "."))), stat = "identity", fill="#FF8C42") + 
      scale_x_datetime(
        breaks = scales::pretty_breaks(n = 6),
        labels = date_format("%d/%m\n%Y")) +
      # scale_y_continuous(
      #   labels = function(x) format(x, big.mark = ",", scientific = FALSE),
      #   breaks = scales::pretty_breaks(n = 6), limits = c(0, NA)) +
      escala +
      labs + 
      tema()
    
    #graf <- plotly::ggplotly(graf)
    
    ggplotly(graf, tooltip = "text")
  })
  
  output$graf_partidos <- renderPlotly({
    d <- dados()
    #d$data_c <- as.POSIXct(d$`Data da despesa`,format="%d/%m/%Y")
    
    if(input$valores == 'Contagem'){
      d <- d %>%
        group_by(Partido) %>%
        count() %>% 
        arrange(desc(n))
      
      labs <- labs(x="", y = "despesas")
      escala <- scale_y_continuous(
        labels = scales::dollar_format(scale = 1, prefix = "", suffix = ""),
        #labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }else{
      d <- d %>%
        group_by(Partido) %>%
        summarise(n = sum(Valor)) %>% 
        arrange(desc(n))
      
      labs <- labs(x="", y = "")
      escala <- scale_y_continuous(
        labels = scales::dollar_format(scale = .000001, prefix = "R$", suffix = " mi"),
        #labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }
    
    d <- d %>% head(10)
    
    graf <- ggplot(d, aes(reorder(Partido, n), n)) +
      geom_bar(aes(text = paste('<b>PARTIDO:</b>', Partido,
                                '<br><b>MONTANTE:</b>', format(round(n, 1), big.mark = ",", decimal.mark = "."))), stat = "identity", fill="#FF8C42") + 
      # scale_x_datetime(
      #   breaks = scales::pretty_breaks(n = 6),
      #   labels = date_format("%d/%m\n%Y")) +
      escala +
      labs  + 
      tema() + 
      coord_flip()
    
    #graf <- plotly::ggplotly(graf)
    ggplotly(graf, tooltip = "text")
    
  })
  
  output$graf_politicos_top <- renderPlotly({
    d <- dados()
    #d$data_c <- as.POSIXct(d$`Data da despesa`,format="%d/%m/%Y")
    
    if(input$valores == 'Contagem'){
      d <- d %>%
        group_by(`Município - UF`, Cargo, Partido, Candidato) %>%
        count() %>% 
        arrange(desc(n))
      
      labs <- labs(x="", y = "despesas")
      escala <- scale_y_continuous(
        labels = scales::dollar_format(scale = 1, prefix = "", suffix = ""),
        #labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }else{
      d <- d %>%
        group_by(`Município - UF`, Cargo, Partido, Candidato) %>%
        summarise(n = sum(Valor)) %>% 
        arrange(desc(n))
      
      labs <- labs(x="", y = "")
      escala <- scale_y_continuous(
        labels = scales::dollar_format(scale = .000001, prefix = "R$", suffix = " mi"),
        #labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }
    
    d <- d %>% head(10)
    
    graf <- ggplot(d, aes(reorder(Candidato, n), n)) +
      geom_bar(aes(text = paste('<b>CANDIDATO:</b>', Candidato,
                                '<br><b>CARGO:</b>', Cargo,
                                '<br><b>LOCAL:</b>', `Município - UF`,
                                '<br><b>PARTIDO:</b>', Partido,
                                '<br><b>MONTANTE:</b>', format(round(n, 0), big.mark = ".", decimal.mark = ","))), stat = "identity", fill="#FF8C42") + 
      # scale_x_datetime(
      #   breaks = scales::pretty_breaks(n = 6),
      #   labels = date_format("%d/%m\n%Y")) +
      escala +
      labs  + 
      tema() + 
      coord_flip()
    
    #graf <- plotly::ggplotly(graf)
    ggplotly(graf, tooltip = "text")
    
  })
  
  output$graf_redes <- renderPlotly({
    d <- dados()
    # 2022 rows never had rede_social_mae computed (NULL); show them honestly
    d$rede <- coalesce(d$`Rede social`, "Não informado")

    if(input$valores == 'Contagem'){
      d <- d %>%
        group_by(rede) %>%
        count() %>%
        arrange(desc(n))

      labs <- labs(x="", y = "despesas")
      escala <- scale_y_continuous(
        labels = scales::dollar_format(scale = 1, prefix = "", suffix = ""),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }else{
      d <- d %>%
        group_by(rede) %>%
        summarise(n = sum(Valor)) %>%
        arrange(desc(n))

      labs <- labs(x="", y = "")
      escala <- scale_y_continuous(
        labels = scales::dollar_format(scale = .000001, prefix = "R$", suffix = " mi"),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }

    graf <- ggplot(d, aes(reorder(rede, n), n)) +
      geom_bar(aes(text = paste('<b>REDE:</b>', rede,
                                '<br><b>MONTANTE:</b>', format(round(n, 0), big.mark = ".", decimal.mark = ","))), stat = "identity", fill="#FF8C42") +
      escala +
      labs  +
      tema() +
      coord_flip()

    ggplotly(graf, tooltip = "text")

  })

  output$graf_politicos <- renderPlotly({
    d <- dados()
    
    if(input$valores == 'Contagem'){
      d <- d %>%
        group_by(Candidato, Cargo, Partido, `Município - UF`) %>%
        count() %>% 
        arrange(desc(n))
      
      labs <- labs(x="", y = "n despesas")
      escala <- scale_x_continuous(
        labels = scales::dollar_format(scale = .000001, prefix = "", suffix = ""),
        #labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }else{
      d <- d %>%
        group_by(Candidato, Cargo, Partido, `Município - UF`) %>%
        summarise(n = sum(Valor)) %>% 
        arrange(desc(n))
      
      labs <- labs(x="", y = "R$")
      escala <- scale_x_continuous(
        labels = scales::dollar_format(scale = .001, prefix = "R$", suffix = " mi"),
        #labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }
    
    d <- d %>% head(250) 
    
    graf <- ggplot(d, aes(x = Partido, y = n, color = Partido, text = paste(
      '<b>CANDIDATO:</b>', Candidato,
      '<br><b>CARGO:</b>', Cargo,
      '<br><b>LOCAL:</b>', `Município - UF`,
      '<br><b>PARTIDO:</b>', Partido,
      '<br><b>MONTANTE:</b>', format(round(n, 0), big.mark = ".", decimal.mark = ",")))) + 
      geom_quasirandom(
        size = 1.3, alpha = 0.5,
        priority = "density", colour="#FF8C42") + 
      # scale_color_brewer(palette = "Set1") +
      #escala +
      labs + 
      tema() + coord_flip() + theme(legend.position = "none")
    
    ggplotly(graf, tooltip = "text")
    
  })
  
  output$graf_politicos_backup <- renderPlotly({
    d <- dados()
    #d$data_c <- as.POSIXct(d$`Data da despesa`,format="%d/%m/%Y")
    
    if(input$valores == 'Contagem'){
      d <- d %>%
        group_by(Candidato) %>%
        count() %>% 
        arrange(desc(n))
      
      labs <- labs(x="", y = "n despesas")
    }else{
      d <- d %>%
        group_by(Candidato) %>%
        summarise(n = sum(Valor)) %>% 
        arrange(desc(n))
      
      labs <- labs(x="", y = "R$")
    }
    
    d <- d %>% head(30) 
    
    graf <- ggplot(d, aes(reorder(Candidato, n), n)) +
      geom_bar(aes(text = paste('<b>CANDIDATO:</b>', Candidato,
                                '<br><b>MONTANTE:</b>', format(round(n, 1), big.mark = ",", decimal.mark = "."))), stat = "identity", fill="#FF8C42") + 
      # scale_x_datetime(
      #   breaks = scales::pretty_breaks(n = 6),
      #   labels = date_format("%d/%m\n%Y")) +
      scale_y_continuous(
        labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 6), limits = c(0, NA)) +
      labs  + tema() + theme(axis.text.y = element_text(size = rel(0.5), margin=margin(0,40,0,0))) + coord_flip() + expand_limits(x = 0)
    
    #graf <- plotly::ggplotly(graf)
    ggplotly(graf, tooltip = "text") %>% layout(height = 650)
    
  })
  
  output$graf_ufs <- renderPlotly({
    d <- dados()
    #d$data_c <- as.POSIXct(d$`Data da despesa`,format="%d/%m/%Y")
    
    if(input$valores == 'Contagem'){
      d <- d %>%
        group_by(`Município - UF`) %>%
        count() %>%
        arrange(desc(n))
      
      labs <- labs(x="", y = "n despesas")
      labs <- labs(x="", y = "")
      escala <- scale_y_continuous(
        labels = scales::dollar_format(scale = 1, prefix = "", suffix = ""),
        #labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }else{
      d <- d %>%
        group_by(`Município - UF`) %>%
        summarise(n = sum(Valor)) %>%
        arrange(desc(n))
      
      labs <- labs(x="", y = "n despesas")
      escala <- scale_y_continuous(
        labels = scales::dollar_format(scale = .000001, prefix = "R$", suffix = " mi"),
        #labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 4), limits = c(0, NA))
    }
    
    d <- d %>% head(30) %>% arrange(desc(n))
    
    graf <- ggplot(d, aes(reorder(`Município - UF`, n), n)) +
      geom_bar(aes(text = paste('<b>LOCAL:</b>', `Município - UF`,
                                '<br><b>MONTANTE:</b>', format(round(n, 1), big.mark = ",", decimal.mark = "."))), stat = "identity", fill="#FF8C42") + 
      # scale_x_datetime(
      #   breaks = scales::pretty_breaks(n = 6),
      #   labels = date_format("%d/%m\n%Y")) +
      # scale_y_continuous(
      #   labels = function(x) format(x, big.mark = ",", scientific = FALSE),
      #   breaks = scales::pretty_breaks(n = 6), limits = c(0, NA)) +
      escala +
      labs  + 
      tema() + 
      coord_flip()
    
    #graf <- plotly::ggplotly(graf)
    ggplotly(graf, tooltip = "text") %>% layout(height = 650)
    
  })
  
  ##########################################
  ############## TABELA
  output$table <- DT::renderDT({
    
    # Importa os dados principais e filtra pelas datas do input$date
    main_table <- dados()
    
    # Gera a tabela principal
    
    main_table
    
  }, escape = FALSE,
  filter = "top",
  callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "" )'),
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