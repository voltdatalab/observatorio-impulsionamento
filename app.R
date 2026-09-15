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
      # Workaround selectize: na instância recriada pelo updateSelectizeInput
      # (busca server-side de candidato), o input de digitação fica preso em
      # isInputHidden=true e as teclas não entram. Garante showInput() ao focar.
      tags$script(HTML("
        $(document).on('focusin click', '#politico + .selectize-control .selectize-input', function() {
          var el = document.getElementById('politico');
          if (el && el.selectize && el.selectize.isInputHidden) el.selectize.showInput();
        });
      "))
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
                      tags$p("⚠️ As descrições de impulsionamento são autodeclaradas e muitas vezes não é possível determinar o destino dos gastos sob essas rubricas. Veja as notas abaixo para esclarecimentos."),
                      tags$p(class = "update-note", icon("clock", class = "icons"), textOutput("ultima_atualizacao", inline = TRUE))
             )
      )
    ),
    fluidRow(
      column(4, offset = 4, class = "col-eleicao", uiOutput('eleicao'))
    ),
    fluidRow(
      column(4, class = "col-filtros", tags$div(class = "filters-card",
             column(12,
                    selectInput(inputId = "origem",
                                label = tags$div(icon("filter", class = "icons"),
                                                 tags$span(style="font-weight:300;font-size:0.75em;line-height:1.45em;display:block;margin-top:4px",
                                                           '"Busca ampla" joga uma rede maior para tentar encontrar impulsionamentos que não foram marcados na rubrica correta, mas pode incluir outros tipos de gastos. Ao selecionar "Apenas Despesa com Impulsionamento de Conteúdos", o usuário força o filtro de busca apenas a essa rubrica.'),tags$br()),
                                choices = c("Busca ampla" = "ampla",
                                            "Apenas Despesa com Impulsionamento de Conteúdos" = "rubrica"),
                                selected = "ampla")
             ),
             column(6,uiOutput('estados')),
             column(6,uiOutput('mun')),
             column(6,uiOutput('legenda')),
             column(6,uiOutput('cargos')),
             column(6,uiOutput('politicos')),
             column(6,uiOutput('turno')),
             column(6,
                    textInput(inputId = "valor_custom",
                              label = tags$div(icon("money-bill", class = "icons"), 'Valor mínimo'),
                              value = "",
                              placeholder = "Apenas números")),
             column(6,
                    selectInput(inputId = "rede",
                                label = tags$div(icon("share-alt-square", class = "icons"), 'Redes'),
                                choices = c("Todas", 
                                            "Meta (Facebook, Instagram e WhatsApp)" = "Meta", 
                                            "Google e YouTube" = "Google", 
                                            "ByteDance (TikTok)" = "ByteDance", 
                                            "Kwai"),
                                selected = "Todas"
                    )
             ),
             # Filtro de datas oculto por decisão editorial (set/2026). Para
             # reativar, basta descomentar - o render do server e o
             # build_where_clause voltam a funcionar sozinhos.
             # column(12, uiOutput('periodo')),
             column(12, style="margin-top:20px",
                    prettyRadioButtons(inputId = "valores",
                                       label = tags$div(icon("line-chart", class = "icons"), style="visibility:hidden"),
                                       choices = c("Gráficos em R$" = "Valores R$", "Gráficos em nº de registros" = "Contagem"),
                                       shape = c("curve"),
                                       fill = TRUE,
                                       inline = TRUE,
                    )
             ),
             column(12, style="margin: 6px 0 12px",
                    downloadButton("download_dados", "Baixar dados filtrados (CSV)", class = "btn-download")
             ),

      )),
      column(8, class = "kpi-row",
        # dois "heros" em largura total; os demais em grade 2x2 - a coluna de
        # KPIs fica da altura do card de filtros, sem gap embaixo
        valueBox(textOutput("total_gasto"), "é o volume total gasto com rubrica impulsionamento", icon = icon("money-bill"), width = 12),
        valueBox(textOutput("n_candidatos"), "é o número de candidatos que impulsionaram conteúdo", icon = icon("users"), width = 12),
        valueBox(textOutput("n_gastos"), "nº total de rubricas de impulsionamento", icon = icon("list-alt"), width = 6),
        valueBox(textOutput("media_gasto"), "foi a média dos gastos com impulsionamento", icon = icon("grip-lines"), width = 6),
        valueBox(textOutput("maior_gasto"), "foi o maior gasto com impulsionamento", icon = icon("sort-up"), width = 6),
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
        plot.background = element_rect(colour="#ffffff", fill="#ffffff"),
        panel.background = element_rect(colour="#ffffff", fill="#ffffff"),
        text = element_text(colour = "#000000"),
        
        axis.text = element_text(size = rel(0.8), margin=margin(0,40,0,0)),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = rel(0.9), colour = "#000000"),
        
        legend.text = element_text(size=rel(0.9), angle = 0),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#ffffff", colour = "#ffffff", size = 0.5, linetype='dashed'),
        legend.key.width = unit(0.6, "cm"),
        legend.position = "top",
        legend.justification = c(-0.05, 0),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.margin = (margin=margin(0,0,0,0)),
        legend.box = NULL,
        
        panel.border = element_rect(colour = "#ffffff", fill=NA, size=2),
        panel.grid.major = element_line(colour = "#cbcbcb"),
        panel.grid.minor = element_line(colour = "#cbcbcb"),
        panel.grid.minor.x = element_line(colour = "#cbcbcb"),
        
        plot.title = element_text(hjust = 0, size = rel(1.3), face = "bold", colour = "#231f20"),
        plot.title.position = "plot",
        strip.background = element_rect(colour="#ffffff", fill="#ffffff"),
        plot.subtitle = element_text(hjust = 0, margin=margin(0,0,40,0),size = rel(1), lineheight = 1),
        plot.caption = element_text(size = rel(0.75), hjust = 1, margin=margin(20,0,0,0), colour = "#555555", lineheight = 1),
        plot.margin = unit(c(1, 1, 1, 1), "lines")
      )
  )
  }
  
  # Carimbo de fonte + data em todos os gráficos - importante para o gráfico
  # não circular descontextualizado em prints. Sem botão de export de imagem
  # (o rasterizador do plotly ignora o CSS da página e o resultado sai torto);
  # o download oferecido é o dos DADOS filtrados (CSV), no card de filtros.
  FONTE_STACK <- "Barlow, Arial, sans-serif"

  fonte_plotly <- function(p) {
    rodape <- paste0(
      "Fonte: Observatório de Impulsionamento Eleitoral/Núcleo Jornalismo, com dados do TSE",
      if (!is.null(ultima_atualizacao_data())) paste0("<br>Dados registrados no TSE até ", ultima_atualizacao_data()) else ""
    )

    p %>%
      plotly::layout(
        margin = list(b = 115),
        font = list(family = FONTE_STACK),
        xaxis = list(tickfont = list(family = FONTE_STACK), title = list(font = list(family = FONTE_STACK))),
        yaxis = list(tickfont = list(family = FONTE_STACK), title = list(font = list(family = FONTE_STACK))),
        annotations = list(list(
          # ancorado em pixels abaixo do eixo (yshift), não em fração da altura:
          # em gráficos altos a fração cai fora da margem e o rodapé some no export
          x = 1, y = 0, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "top", yshift = -80, align = "right",
          text = rodape, showarrow = FALSE,
          font = list(size = 11, color = "#777777", family = FONTE_STACK)
        ))
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("toImage", "zoom2d", "pan2d", "select2d", "lasso2d",
                                      "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
                                      "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")
      )
  }

  # Download dos dados filtrados em CSV (padrão brasileiro: ";" e vírgula
  # decimal, com BOM para o Excel abrir acentos corretamente)
  output$download_dados <- downloadHandler(
    filename = function() {
      paste0("observatorio-impulsionamento-", input$ano_eleicao, "-", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      readr::write_excel_csv2(dados(), file)
    }
  )

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

    # Candidato filter ("" acontece transitoriamente enquanto o selectize
    # server-side recarrega após troca de ano - não pode virar filtro)
    if (!is.null(input$politico) && !input$politico %in% c("Todos", "")) {
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

    # Origem da despesa: "rubrica" restringe à rubrica oficial do TSE; o default
    # ("ampla") mantém a rede maior por fornecedor/descrição (grafia idêntica
    # nos 3 anos, verificado em set/2026)
    if (!is.null(input$origem) && input$origem == "rubrica") {
      where_parts <- c(where_parts, "DS_ORIGEM_DESPESA = 'Despesa com Impulsionamento de Conteúdos'")
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
                   label = tags$div(icon("calendar-check", class = "icons"), 'Eleição'),
                   choices  = filters$anos,
                   selected = max(filters$anos))
  })

  # Período (datas) filter - bounds derived live from the data, uses cached filter data
  output$periodo <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    dateRangeInput(inputId = "data",
                   label = tags$div(icon("calendar", class = "icons"), 'Datas (dd/mm/aa)'),
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
                   label = tags$div(icon("map-marker-alt", class = "icons"), 'Município - UF'),
                   choices  = c("Todos", filters$municipios),
                   selected = "Todos",
                   options = list(maxOptions = 5000))
  })

  # Updated 'estados' UI - uses cached filter data
  output$estados <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    selectizeInput(inputId = "uf",
                   label = tags$div(icon("map-marker-alt", class = "icons"), 'UF'),
                   choices  = c("Todas", filters$ufs),
                   selected = "Todas")
  })

  output$legenda <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    selectizeInput(inputId = "partido",
                   label = tags$div(icon("paste", class = "icons"), 'Partidos'),
                   choices  = c("Todos", filters$partidos),
                   selected = "Todos")
  })

  output$turno <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    selectizeInput(inputId = "turno",
                   label = tags$div(icon("suitcase", class = "icons"), 'Turno'),
                   choices  = c("Todos", filters$turnos),
                   selected = "Todos")
  })

  output$cargos <- renderUI({
    req(filter_cache())
    filters <- filter_cache()

    selectizeInput(inputId = "cargo",
                   label = tags$div(icon("suitcase", class = "icons"), 'Cargo'),
                   choices  = c("Todos", filters$cargos),
                   selected = "Todos")
  })

  # Server-side selectize for candidates (39K+ options)
  output$politicos <- renderUI({
    selectizeInput(inputId = "politico",
                   label = tags$div(icon("user", class = "icons"), 'Candidato'),
                   choices  = NULL,
                   selected = NULL,
                   multiple = FALSE,
                   options = list(
                     placeholder = 'Digite para buscar...',
                     maxOptions = 50
                   ))
  })

  # Busca server-side NATIVA do Shiny (server = TRUE): o cliente manda a query
  # a cada tecla e o Shiny filtra em R - sem load() customizado, que recriava o
  # campo no meio da digitação e quebrava a busca. A lista é só do ano
  # selecionado (troca de ano recarrega e volta para "Todos").
  observeEvent(input$ano_eleicao, {
    nomes <- dbGetQuery(pool, sprintf(
      "SELECT DISTINCT NM_CANDIDATO FROM despesas WHERE ANO_ELEICAO = %d ORDER BY NM_CANDIDATO",
      as.integer(input$ano_eleicao)))$NM_CANDIDATO
    updateSelectizeInput(session, "politico",
                         choices = c("Todos", nomes), selected = "Todos",
                         server = TRUE,
                         options = list(placeholder = 'Digite 3 ou mais letras...',
                                        maxOptions = 50,
                                        # gate de 3+ caracteres via score: abaixo disso nada é
                                        # exibido (o selectize 0.15.2 do Shiny NÃO suporta a
                                        # opção shouldLoad, então o gate é só na exibição)
                                        score = I('function(search) {
                                          var scorer = this.getScoreFunction(search);
                                          if (search.length > 0 && search.length < 3) return function() { return 0; };
                                          return scorer;
                                        }')))
  })

  # Data mais recente de registro no TSE (DT_PRESTACAO_CONTAS) para o ano
  # selecionado - é a data que vem nos próprios dados, não a hora do ETL.
  # (DT_DESPESA não serve: tem datas futuras digitadas errado nas prestações.)
  ultima_atualizacao_data <- reactive({
    req(input$ano_eleicao)
    m <- dbGetQuery(pool, sprintf(
      "SELECT MAX(substr(DT_PRESTACAO_CONTAS,7,4) || '-' || substr(DT_PRESTACAO_CONTAS,4,2) || '-' || substr(DT_PRESTACAO_CONTAS,1,2)) AS m
       FROM despesas WHERE ANO_ELEICAO = %d AND length(DT_PRESTACAO_CONTAS) = 10",
      as.integer(input$ano_eleicao)))$m
    if (is.null(m) || is.na(m)) return(NULL)
    format(as_date(m), "%d/%m/%Y")
  })

  output$ultima_atualizacao <- renderText({
    d <- ultima_atualizacao_data()
    if (is.null(d)) return("")
    paste0("Dados registrados no TSE até ", d)
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

    # Série histórica limitada ao período de campanha: começa em 15/ago (início
    # da propaganda eleitoral) e vai até 1º/nov - ou até hoje, no ano corrente.
    # Despesas contratadas fora dessa janela existem, mas não entram neste gráfico.
    ano <- as.integer(input$ano_eleicao)
    ini <- as.POSIXct(sprintf("%d-08-15", ano), tz = "UTC")
    fim <- min(as.POSIXct(sprintf("%d-11-01", ano), tz = "UTC"), as.POSIXct(Sys.Date()))
    d <- d %>% filter(data_c >= ini, data_c <= fim)

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
        limits = c(ini, fim),
        breaks = scales::pretty_breaks(n = 6),
        labels = date_format("%d/%m\n%Y")) +
      # scale_y_continuous(
      #   labels = function(x) format(x, big.mark = ",", scientific = FALSE),
      #   breaks = scales::pretty_breaks(n = 6), limits = c(0, NA)) +
      escala +
      labs + 
      tema()
    
    #graf <- plotly::ggplotly(graf)
    
    fonte_plotly(ggplotly(graf, tooltip = "text"))
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
    fonte_plotly(ggplotly(graf, tooltip = "text"))
    
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
    fonte_plotly(ggplotly(graf, tooltip = "text"))
    
  })
  
  output$graf_redes <- renderPlotly({
    d <- dados()
    # 2022 rows never had rede_social_mae computed (NULL); show them honestly
    d$rede <- coalesce(d$`Rede social`, "Não informado")
    # X (Twitter) fora do gráfico (valores residuais); segue nos dados/CSV
    d <- d %>% filter(rede != "X (Twitter)")

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

    fonte_plotly(ggplotly(graf, tooltip = "text"))

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
    
    fonte_plotly(ggplotly(graf, tooltip = "text"))
    
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
    fonte_plotly(ggplotly(graf, tooltip = "text")) %>% layout(height = 650)
    
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
    fonte_plotly(ggplotly(graf, tooltip = "text")) %>% layout(height = 650)
    
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