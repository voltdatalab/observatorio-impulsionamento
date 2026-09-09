#########################################################################################
#########################################################################################
###########################                                   ###########################
###########################     FUNCOES DO SCIENCE PULSE      ###########################
###########################                                   ###########################

#########################################################################################

### POPULAR NO PULSE

# Entre os tweets da nossa amostra, quais tiveram o maior n. de RTs (considerando)
# RTs dados pelo universo de usuarios do Twitter? Conta inclui RT de usuarios
# fora do Science Pulse. Contudo, os tweets originais foram postados somente
# por contas monitoradas pela plataforma.

# assuntos_24h <- function(dataset){
#   show_dataset <- dataset
#
#   return(show_dataset)
# }

popular_within_pulse <- function(dataset){
  
  show_dataset <- dataset
  
  if(nrow(show_dataset) == 0) {
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      # Seleciona somente o tweet com mais RT de cada usuario
      group_by(screen_name) %>%
      arrange(desc(interacoes), created_at) %>%
      #slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com mais RTs, eliminadas as repeticoes
      arrange(desc(interacoes), created_at) %>%
      slice(1:20) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="max-width:90%">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

youtube_embed <- function(dataset){
  
  show_dataset <- dataset
  
  if(nrow(show_dataset) == 0) {
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      # Seleciona somente o tweet com mais RT de cada usuario
      group_by(channel) %>%
      arrange(desc(views), created_at) %>%
      #slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com mais RTs, eliminadas as repeticoes
      arrange(desc(views), created_at) %>%
      slice(1:10) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/',
                           videoid, '" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

tiktok_embed <- function(dataset){
  
  show_dataset <- dataset
  
  if(nrow(show_dataset) == 0) {
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      # Seleciona somente o tweet com mais RT de cada usuario
      group_by(authorMeta.name) %>%
      arrange(desc(playCount), createTimeStamp) %>%
      #slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com mais RTs, eliminadas as repeticoes
      arrange(desc(playCount), createTimeStamp) %>%
      slice(1:10) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<iframe width="500" height="350" src="https://www.tiktok.com/@', authorMeta.name, '/video/', id, '"?is_copy_url=1&is_from_webapp=v1"></iframe>'
      ))
    select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### RT-RATIO

# Entre os tweets autorais da amostra, quais tem a maior proporcao de RT/seguidores?
# Contagem inclui RTs de perfis fora do Pulse. Excluem tweets com menos de 2 RTs,
# para evitar que conteudo irrelevante de usuarios com poucos seguidores apareca.

rising_popularity <- function(dataset){
  
  show_dataset <- dataset %>%
    # Filtra tweets autorais com +1 RT e calcula o ratio
    filter(shares > 1)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(show_dataset) == 0) {
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      # Seleciona somente o tweet com maior ratio de cada usuario
      group_by(screen_name) %>%
      arrange(desc(ratio), created_at) %>%
      #slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com maior ratio, eliminadas as repeticoes
      arrange(desc(ratio), created_at) %>%
      slice(1:20) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="max-width:90%">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### DESEMPENHO EM ALTA

# Identifica os posts com maior numero de interacoes do que seria tradicionalmente
# observado por aquela conta. Isto e, ele considera a soma de RTs e curtidas, com os
# devidos pesos (curtidas valem menos que RTs), de cada post com a media dos ultimos
# posts daquela conta. Tambem existe uma "punicao" para contas com menos seguidores,
# para evitar que o resultado seja artificio de um pequeno numero de tweets.
# Baseado na medida de Overperforming do CrowdTangle.

overperforming <- function(dataset){
  
  show_dataset <- dataset %>%
    # Filtra tweets autorais com +1 RT
    filter(shares > 2)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(show_dataset) == 0) {
    
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      # Seleciona somente o tweet com maior final_score de cada usuario
      group_by(screen_name) %>%
      arrange(desc(final_score), created_at) %>%
      #slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com maiores overperform, eliminadas as repeticoes
      arrange(desc(final_score), created_at) %>%
      slice(1:20) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="max-width:90%">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### SORTEIA TWEETS COM MAIS DE UM RT

# Sorteia, aleatoriamente, 5 tweets com + de 1 RT

sample_more_than_one <- function(dataset){
  
  dataset %>%
    # Filtra tweets autorais com +1 RT
    filter(is_retweet == F,
           retweet_count > 1) %>%
    arrange(desc(retweet_count)) %>%
    # Cria o embed e seleciona somente essa coluna
    mutate(text = paste0('<blockquote class="twitter-tweet" style="max-width:90%">',
                         text, '</p>&mdash;',
                         name, '(@',
                         screen_name, ') <a href="https://twitter.com/',
                         screen_name, '/status/',
                         status_id, '">',
                         created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
    )) %>%
    select(text) %>%
    mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
    rename(" " = text)
  
}

#########################################################################################

### USUARIOS MAIS ATIVOS

# Usuarios monitorados pelo Science Pulse com o maior n. de tweets nas ultimas 12h

active_users <- function(dataset){
  
  dataset %>%
    group_by(screen_name) %>%
    count(sort = T) %>%
    ungroup() %>%
    slice(1:5) %>%
    # Cria o embed e seleciona somente essa coluna
    mutate(screen_name = paste0("@<a href='https://twitter.com/", screen_name, "' target='_blank' style='color: #4b31dd'>", screen_name, "</a>")) %>%
    select(screen_name) %>%
    rename("<i class='fas fa-users'></i>" = screen_name)
  
}

#########################################################################################

### HASHTAGS MAIS USADAS

# Hashtags mais usadas por contas monitoradas pelo Science Pulse nas ultimas 12h

most_hashtags <- function(dataset){
  
  dataset %>%
    mutate(hashtag = toupper(hashtag)) %>%
    filter(hashtag != "NA") %>%
    count(hashtag, sort = T) %>%
    slice(1:5) %>%
    # Cria o embed e seleciona somente essa coluna
    select(hashtag) %>%
    mutate(hashtag = paste0("#<a href='https://twitter.com/hashtag/", hashtag, "' target='_blank' style='color: #4b31dd'>", hashtag, "</a>")) %>%
    rename("<i class='fas fa-hashtag'></i>" = hashtag)
  
}

#########################################################################################

### TAMBEM POPULARES NO PULSE

also_popular <- function(dataset){
  
  # Seleciona somente tweets autorais
  own_sample_trends <- dataset %>%
    filter(is_retweet == F)
  
  # Aplica um algoritmo de kmeans em 4 grupos, para identificar o segundo grupo com mais RT
  set.seed(12345) # para manter uniformidade entre grupos todas as vezes que usarmos o algoritmo
  # Pega o nome de cada grupo para ficarem em ordem
  centers <- sort(kmeans(as.numeric(own_sample_trends$retweet_count),
                         centers = 4, nstart = 1000)$centers)
  # Aplica o algoritmo com os nomes ordenados dos grupos
  own_sample_trends$cluster <- kmeans(as.numeric(own_sample_trends$retweet_count),
                                      centers = centers)$cluster
  
  # Filtra somente o segundo grupo com mais RTs
  own_sample_trends <- own_sample_trends %>%
    filter(cluster == 2)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(own_sample_trends) == 0){
    not_enough_tweets_pt()
    
  } else {
    
    own_sample_trends %>%
      # Seleciona somente o tweet com mais RT de cada usuario
      group_by(screen_name) %>%
      arrange(desc(retweet_count), created_at) %>%
      #slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com mais RTs, eliminadas as repeticoes
      arrange(desc(retweet_count), created_at) %>%
      slice(1:5) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### RADAR PULSE

# Amostra aleatoria de 5 tweets entre aqueles bastante populares no Pulse, mas
# que nao chegam ao topo do ranking abosluto de RTs.

pulse_radar <- function(dataset){
  
  # Seleciona somente tweets autorais
  own_sample_trends <- dataset %>%
    filter(is_retweet == F)
  
  # Aplica um algoritmo de kmeans em 4 grupos, para identificar o segundo grupo com mais RT
  set.seed(12345) # para manter uniformidade entre grupos todas as vezes que usarmos o algoritmo
  # Pega o nome de cada grupo para ficarem em ordem
  centers <- sort(kmeans(as.numeric(own_sample_trends$retweet_count),
                         centers = 4, nstart = 1000)$centers)
  # Aplica o algoritmo com os nomes ordenados dos grupos
  own_sample_trends$cluster <- kmeans(as.numeric(own_sample_trends$retweet_count),
                                      centers = centers)$cluster
  
  # Filtra somente o segundo grupo com mais RTs
  cluster2 <- own_sample_trends %>%
    filter(cluster == 2)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(cluster2) < 5){
    not_enough_tweets_pt()
    
  } else {
    
    cluster2 %>%
      # Sorteia cinco tweets aleatoriamente
      sample_n(5) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')) %>%
      select(text) %>%
      mutate(text = paste0("<strong>//</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### POPULAR ENTRE CIENTISTAS

# Entre os posts que foram retuitados por membros do Pulse, quais foram mais RTs,
# considerando somente os retuites feitos por contas monitoradas pelo Pulse.
# Eles incluem tweets de qualquer conta do Twitter. Contudo, eles aparecem segundo
# o n. de vezes que o tweet original apareceu em nossa amostra (ja que cada
# RT e uma linha do banco original).

popular_among_scientists <- function(dataset){
  
  show_dataset <- dataset %>%
    # Filtra tweets nao-autorais com +1 RT
    filter(is_retweet == T) %>%
    group_by(retweet_status_id) %>%
    mutate(numero = n()) %>%
    ungroup() %>%
    filter(numero > 1)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(show_dataset) == 0) {
    
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      select(text, retweet_name, retweet_screen_name,
             retweet_status_id, retweet_created_at, numero) %>%
      distinct() %>%
      # Seleciona somente o tweet mais vezes RT de cada usuario
      group_by(retweet_screen_name) %>%
      arrange(desc(numero), retweet_status_id) %>%
      #slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 mais vezes RT, eliminadas as repeticoes
      arrange(desc(numero), retweet_status_id) %>%
      slice(1:5) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%">',
                           text, '</p>&mdash;',
                           retweet_name, '(@',
                           retweet_screen_name, ') <a href="https://twitter.com/',
                           retweet_screen_name, '/status/',
                           retweet_status_id, '">',
                           retweet_created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### OUTROS TWEETS POPULARES

# Entre os tweets que aparecem na amostra, quais foram mais RT por todo o
# universo de usuarios do Twitter? A contagem inclui RTs tanto de usuarios
# monitorados pelo Pulse, como de outras contas. Contudo, eles precisam
# ter sido RT ao menos uma vez por contas do Pulse.

other_popular_tweets <- function(dataset){
  
  show_dataset <- dataset %>%
    # Filtra RTs com 1+ RTs
    filter(is_retweet == T,
           retweet_count > 1)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(show_dataset) == 0) {
    
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      select(text, retweet_name, retweet_screen_name, retweet_status_id, retweet_created_at, retweet_count) %>%
      distinct() %>%
      # Seleciona somente o tweet com mais RT de cada usuario
      group_by(retweet_screen_name) %>%
      arrange(desc(retweet_count), retweet_status_id) %>%
      #slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com mais RT, eliminadas as repeticoes
      arrange(desc(retweet_count), retweet_status_id) %>%
      slice(1:5) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%">',
                           text, '</p>&mdash;',
                           retweet_name, '(@',
                           retweet_screen_name, ') <a href="https://twitter.com/',
                           retweet_screen_name, '/status/',
                           retweet_status_id, '">',
                           retweet_created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong>", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### NAO EXISTEM TWEETS SUFICIENTES

# Mensagem que indica nao existirem tweets suficiente para criar aquela coluna

not_enough_tweets_pt <- function(){
  data.frame(variable = "Desculpe, no momento não existem tweets suficientes para esta métrica.\nPor favor, verifique novamente em breve!") %>%
    rename(" " = variable)
}

selecionar_name <- function(mensagem){
  
  grafico_vazio <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 4, y = 40, size = 5,
                      label = mensagem,
                      family = "Barlow") +
    scale_y_continuous(limits = c(0,50)) +
    tema() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.minor.x = element_blank())
  
  return(grafico_vazio)
}

#########################################################################################

### SPINNERS

# Spinners que devem aparecer enquanto dados carregam. Sao diferentes tipos:

# Coluna Grande: spinner circular
include_spinner_large_column <- function(output){
  
  withSpinner(tableOutput(output),
              type = getOption("spinner.type", default = 3),
              color = getOption("spinner.color", default = "#BAF8AF"),
              size = getOption("spinner.size", default = 1),
              color.background = getOption("spinner.color.background", default = "#162E1F"),
              custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(output))) NULL else "300px")
  
}

# Coluna Fina: spinner retangular
include_spinner_thin_column <- function(output){
  
  withSpinner(tableOutput(output),
              type  = getOption("spinner.type",  default = 1),
              color = getOption("spinner.color", default = "#BAF8AF"),
              size  = getOption("spinner.size",  default = 1),
              color.background = getOption("spinner.color.background", default = "#BAF8AF"),
              custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(output))) NULL else "300px")
  
}

# Spinner pequeno: textos, circular pequeno
include_spinner_small <- function(output){
  
  withSpinner(textOutput(output),
              type = getOption("spinner.type", default = 7),
              color = getOption("spinner.color", default = "#BAF8AF"),
              size = getOption("spinner.size", default = 0.4),
              color.background = getOption("spinner.color.background", default = "#BAF8AF"),
              custom.css = FALSE, proxy.height = "20px")
  
}

# Spinner tabelas: circular grande
include_spinner_tables <- function(output){
  
  withSpinner(DT::dataTableOutput(output),
              type = getOption("spinner.type", default = 6),
              color = getOption("spinner.color", default = "#BAF8AF"),
              size = getOption("spinner.size", default = 1),
              color.background = getOption("spinner.color.background", default = "#BAF8AF"),
              custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", DT::dataTableOutput(output))) NULL else "300px")
  
}

########################################################################################

## TEMA GRAFICO

tema <- function(base_size = 14 , base_family = "Barlow"){(
  
  theme_foundation(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(colour="#162E1F", fill="#162E1F"),
      panel.background = element_rect(colour="#162E1F", fill="#162E1F"),
      text = element_text(colour = "#ffffff"),
      
      axis.text = element_text(size = rel(0.8), margin=margin(0,40,0,0)),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title = element_text(size = rel(0.9), colour = "#ffffff"),
      
      legend.text = element_text(size=rel(0.9), angle = 0),
      legend.title = element_blank(),
      legend.key = element_rect(fill = "#162E1F", colour = "#162E1F", size = 0.5, linetype='dashed'),
      legend.key.width = unit(0.6, "cm"),
      legend.position = "top",
      legend.justification = c(-0.05, 0),
      legend.background = element_blank(),
      legend.direction = "horizontal",
      legend.margin = (margin=margin(0,0,0,0)),
      legend.box = NULL,
      
      panel.border = element_rect(colour = "#162E1F", fill=NA, size=2),
      panel.grid.major = element_line(colour = "#e4e4e4"),
      panel.grid.minor = element_line(colour = "#e6e6e6"),
      panel.grid.minor.x = element_line(colour = "#e4e4e4"),
      
      plot.title = element_text(hjust = 0, size = rel(1.3), face = "bold", colour = "#231f20"),
      plot.title.position = "plot",
      strip.background = element_rect(colour="#162E1F", fill="#162E1F"),
      plot.subtitle = element_text(hjust = 0, margin=margin(0,0,40,0),size = rel(1), lineheight = 1),
      plot.caption = element_text(size = rel(0.75), hjust = 1, margin=margin(20,0,0,0), colour = "#555555", lineheight = 1),
      plot.margin = unit(c(1, 1, 1, 0), "lines")
    )
)
}


####

# Funcoes
minmax <- function(x){
  valor <- (x- min(x)) /(max(x)-min(x))
  return(valor)
}

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}


calcular_elementos_temperatura <- function(banco){
  
  banco <- banco %>%
    mutate(min_qtd_tweets = min(qtd_tweets),
           max_qtd_tweets = max(qtd_tweets),
           m_qtd_tweets = minmax(qtd_tweets),
           min_eng = min(engajamento),
           max_eng = max(engajamento),
           m_engajamento = minmax(engajamento),
           min_qtd_users = min(qtd_users),
           max_qtd_users = max(qtd_users),
           m_qtd_users = minmax(qtd_users))
  return(banco)
  
}

# mod_conteudo cards
create_card_fb <- function(post, id) {
  text <- ifelse(is.na(post$message), "", post$message)
  shiny::div(
    style = "padding: 8px 12px;",
    shiny::p(stringr::str_c(id, "º //"), style = "font-weight: 700"),
    shiny::div(
      style = "padding: 12px 18px;border: 1px solid #cbcbcb;border-radius:10px;margin: 6px 0 ;background-color:#fff",
      shiny::a(
        href = post$link,
        target = "_blank",
        style = "color:#000000; text-decoration: none",
        shiny::div(
          style = "max-width: 600px",
          shiny::div(
            style = "max-width: 600px; ",
            shiny::div(
              # shiny::img(
              #   src = post$photo, width = "12%",
              #   style = stringr::str_c(
              #     "vertical-align: middle; border-radius: 50%; ",
              #     "display: inline-block; min-width: 40px; "
              #   )
              # ),
              shiny::div(
                shiny::div(
                  tags$h3(post$name), #verified(post$verified),
                  style = "font-size: 1.2em; font-weight: 500"
                ),
                shiny::div(tags$em(post$created_at)),
                style = "display: inline-block; vertical-align: middle;"
              )
            )
          ),
          tags$br(),
          shiny::p(text, style = "font-size: 0.95em;text-align:left"),
          #media_img(post$photo),
          tags$br(),
          shiny::HTML(
            glue::glue(
              "<br>
            <p style='border: 1px solid rgb(207, 217, 222);'></p>
            <p><i class='fas fa-smile-wink'></i> <strong>{post$likeCount + post$loveCount}</strong> reações &bull; ",
              "<i class='fas fa-comment'></i> <strong>{post$commentCount}</strong> comentários  &bull; ",
              "<i class='fas fa-share'></i> <strong>{post$shareCount}</strong> compart.</p>"
            )
          )
        )
      )
    )
  )
}

create_card_ig <- function(post, id) {
  text <- ifelse(is.na(post$message), "", post$message)
  shiny::div(
    style = "padding: 8px 12px;",
    shiny::p(stringr::str_c(id, "º //"), style = "font-weight: 700"),
    shiny::div(
      style = "padding: 12px 18px;border: 1px solid #cbcbcb;border-radius:10px;margin: 6px 0 ;background-color:#fff",
      shiny::a(
        href = post$link,
        target = "_blank",
        style = "color:#000000; text-decoration: none",
        shiny::div(
          style = "max-width: 600px",
          shiny::div(
            style = "max-width: 600px; ",
            shiny::div(
              # shiny::img(
              #   src = post$photo, width = "12%",
              #   style = stringr::str_c(
              #     "vertical-align: middle; border-radius: 50%; ",
              #     "display: inline-block; min-width: 40px; "
              #   )
              # ),
              shiny::div(
                shiny::div(
                  tags$h3(post$name), #verified(post$verified),
                  style = "font-size: 1.2em; font-weight: 500"
                ),
                shiny::div(tags$em(post$created_at)),
                style = "display: inline-block; vertical-align: middle;"
              )
            )
          ),
          tags$br(),
          media_img(post$photo),
          tags$br(),
          shiny::p(text, style = "font-size: 0.95em;text-align:left"),
          tags$br(),
          tags$br(),
          shiny::HTML(
            glue::glue(
              "<p style='border: 1px solid rgb(207, 217, 222);'></p>
            <p><i class='fas fa-smile-wink'></i> <strong>{post$likeCount}</strong> reações &bull; ",
              "<i class='fas fa-comment'></i> <strong>{post$commentCount}</strong> comentários  &bull; "
            )
          )
        )
      )
    )
  )
}



# mod_conteudo embeds
create_embed_fb <- function(post, id) {
  text <- ifelse(is.na(post$message), "", post$message)
  shiny::div(
    style = "padding: 8px 12px;",
    shiny::p(stringr::str_c(id, "º //"), style = "font-weight: 700"),
    HTML('
              <div id="fb-root"></div>
          <script async defer src="https://connect.facebook.net/en_US/sdk.js#xfbml=1&version=v3.3"></script>
         <div class="fb-post fb_iframe_widget" data-href="', post$link, '"
         data-width=""></div>
         '
    )
  )
}

create_iframe_fb <- function(post, id) {
  text <- ifelse(is.na(post$message), "", post$message)
  shiny::div(
    style = "padding: 8px 12px;",
    shiny::p(stringr::str_c(id, "º //"), style = "font-weight: 700"),
    HTML('
    <iframe src="https://www.facebook.com/plugins/post.php?href=', post$link, '%2F&width=auto&show_text=true" width="100%" height="460" style="border:none;overflow:show" scrolling="yes" frameborder="0" allowfullscreen="true" allow="autoplay; clipboard-write; encrypted-media; picture-in-picture; web-share" style="width: 100%;"></iframe>
         '
    )
  )
}

create_embed_ig <- function(post, id) {
  text <- ifelse(is.na(post$message), "", post$message)
  shiny::div(
    style = "padding: 8px 12px;",
    shiny::p(stringr::str_c(id, "º //"), style = "font-weight: 700"),
    HTML(paste0('
         <blockquote class="instagram-media" data-instgrm-captioned data-instgrm-permalink="', post$link, '?utm_source=ig_embed&amp;utm_campaign=loading" data-instgrm-version="14" style=" background:#FFF; border:0; border-radius:3px; box-shadow:0 0 1px 0 rgba(0,0,0,0.5),0 1px 10px 0 rgba(0,0,0,0.15); margin: 1px; max-width:540px; min-width:326px; padding:0; width:99.375%; width:-webkit-calc(100% - 2px); width:calc(100% - 2px);"><div style="padding:16px;"> <a href="', post$link, '?utm_source=ig_embed&amp;utm_campaign=loading" style=" background:#FFFFFF; line-height:0; padding:0 0; text-align:center; text-decoration:none; width:100%;" target="_blank"> <div style=" display: flex; flex-direction: row; align-items: center;"> <div style="background-color: #F4F4F4; border-radius: 50%; flex-grow: 0; height: 40px; margin-right: 14px; width: 40px;"></div> <div style="display: flex; flex-direction: column; flex-grow: 1; justify-content: center;"> <div style=" background-color: #F4F4F4; border-radius: 4px; flex-grow: 0; height: 14px; margin-bottom: 6px; width: 100px;"></div> <div style=" background-color: #F4F4F4; border-radius: 4px; flex-grow: 0; height: 14px; width: 60px;"></div></div></div><div style="padding: 19% 0;"></div> <div style="display:block; height:50px; margin:0 auto 12px; width:50px;"><svg width="50px" height="50px" viewBox="0 0 60 60" version="1.1" xmlns="https://www.w3.org/2000/svg" xmlns:xlink="https://www.w3.org/1999/xlink"><g stroke="none" stroke-width="1" fill="none" fill-rule="evenodd"><g transform="translate(-511.000000, -20.000000)" fill="#000000"><g><path d="M556.869,30.41 C554.814,30.41 553.148,32.076 553.148,34.131 C553.148,36.186 554.814,37.852 556.869,37.852 C558.924,37.852 560.59,36.186 560.59,34.131 C560.59,32.076 558.924,30.41 556.869,30.41 M541,60.657 C535.114,60.657 530.342,55.887 530.342,50 C530.342,44.114 535.114,39.342 541,39.342 C546.887,39.342 551.658,44.114 551.658,50 C551.658,55.887 546.887,60.657 541,60.657 M541,33.886 C532.1,33.886 524.886,41.1 524.886,50 C524.886,58.899 532.1,66.113 541,66.113 C549.9,66.113 557.115,58.899 557.115,50 C557.115,41.1 549.9,33.886 541,33.886 M565.378,62.101 C565.244,65.022 564.756,66.606 564.346,67.663 C563.803,69.06 563.154,70.057 562.106,71.106 C561.058,72.155 560.06,72.803 558.662,73.347 C557.607,73.757 556.021,74.244 553.102,74.378 C549.944,74.521 548.997,74.552 541,74.552 C533.003,74.552 532.056,74.521 528.898,74.378 C525.979,74.244 524.393,73.757 523.338,73.347 C521.94,72.803 520.942,72.155 519.894,71.106 C518.846,70.057 518.197,69.06 517.654,67.663 C517.244,66.606 516.755,65.022 516.623,62.101 C516.479,58.943 516.448,57.996 516.448,50 C516.448,42.003 516.479,41.056 516.623,37.899 C516.755,34.978 517.244,33.391 517.654,32.338 C518.197,30.938 518.846,29.942 519.894,28.894 C520.942,27.846 521.94,27.196 523.338,26.654 C524.393,26.244 525.979,25.756 528.898,25.623 C532.057,25.479 533.004,25.448 541,25.448 C548.997,25.448 549.943,25.479 553.102,25.623 C556.021,25.756 557.607,26.244 558.662,26.654 C560.06,27.196 561.058,27.846 562.106,28.894 C563.154,29.942 563.803,30.938 564.346,32.338 C564.756,33.391 565.244,34.978 565.378,37.899 C565.522,41.056 565.552,42.003 565.552,50 C565.552,57.996 565.522,58.943 565.378,62.101 M570.82,37.631 C570.674,34.438 570.167,32.258 569.425,30.349 C568.659,28.377 567.633,26.702 565.965,25.035 C564.297,23.368 562.623,22.342 560.652,21.575 C558.743,20.834 556.562,20.326 553.369,20.18 C550.169,20.033 549.148,20 541,20 C532.853,20 531.831,20.033 528.631,20.18 C525.438,20.326 523.257,20.834 521.349,21.575 C519.376,22.342 517.703,23.368 516.035,25.035 C514.368,26.702 513.342,28.377 512.574,30.349 C511.834,32.258 511.326,34.438 511.181,37.631 C511.035,40.831 511,41.851 511,50 C511,58.147 511.035,59.17 511.181,62.369 C511.326,65.562 511.834,67.743 512.574,69.651 C513.342,71.625 514.368,73.296 516.035,74.965 C517.703,76.634 519.376,77.658 521.349,78.425 C523.257,79.167 525.438,79.673 528.631,79.82 C531.831,79.965 532.853,80.001 541,80.001 C549.148,80.001 550.169,79.965 553.369,79.82 C556.562,79.673 558.743,79.167 560.652,78.425 C562.623,77.658 564.297,76.634 565.965,74.965 C567.633,73.296 568.659,71.625 569.425,69.651 C570.167,67.743 570.674,65.562 570.82,62.369 C570.966,59.17 571,58.147 571,50 C571,41.851 570.966,40.831 570.82,37.631"></path></g></g></g></svg></div><div style="padding-top: 8px;"> <div style=" color:#3897f0; font-family:Arial,sans-serif; font-size:14px; font-style:normal; font-weight:550; line-height:18px;">Ver essa foto no Instagram</div></div><div style="padding: 12.5% 0;"></div> <div style="display: flex; flex-direction: row; margin-bottom: 14px; align-items: center;"><div> <div style="background-color: #F4F4F4; border-radius: 50%; height: 12.5px; width: 12.5px; transform: translateX(0px) translateY(7px);"></div> <div style="background-color: #F4F4F4; height: 12.5px; transform: rotate(-45deg) translateX(3px) translateY(1px); width: 12.5px; flex-grow: 0; margin-right: 14px; margin-left: 2px;"></div> <div style="background-color: #F4F4F4; border-radius: 50%; height: 12.5px; width: 12.5px; transform: translateX(9px) translateY(-18px);"></div></div><div style="margin-left: 8px;"> <div style=" background-color: #F4F4F4; border-radius: 50%; flex-grow: 0; height: 20px; width: 20px;"></div> <div style=" width: 0; height: 0; border-top: 2px solid transparent; border-left: 6px solid #f4f4f4; border-bottom: 2px solid transparent; transform: translateX(16px) translateY(-4px) rotate(30deg)"></div></div><div style="margin-left: auto;"> <div style=" width: 0px; border-top: 8px solid #F4F4F4; border-right: 8px solid transparent; transform: translateY(16px);"></div> <div style=" background-color: #F4F4F4; flex-grow: 0; height: 12px; width: 16px; transform: translateY(-4px);"></div> <div style=" width: 0; height: 0; border-top: 8px solid #F4F4F4; border-left: 8px solid transparent; transform: translateY(-4px) translateX(8px);"></div></div></div> <div style="display: flex; flex-direction: column; flex-grow: 1; justify-content: center; margin-bottom: 24px;"> <div style=" background-color: #F4F4F4; border-radius: 4px; flex-grow: 0; height: 14px; margin-bottom: 6px; width: 224px;"></div> <div style=" background-color: #F4F4F4; border-radius: 4px; flex-grow: 0; height: 14px; width: 144px;"></div></div></a><p style=" color:#c9c8cd; font-family:Arial,sans-serif; font-size:14px; line-height:17px; margin-bottom:0; margin-top:8px; overflow:hidden; padding:8px 0 7px; text-align:center; text-overflow:ellipsis; white-space:nowrap;"><a href="', post$link, '?utm_source=ig_embed&amp;utm_campaign=loading" style=" color:#c9c8cd; font-family:Arial,sans-serif; font-size:14px; font-style:normal; font-weight:normal; line-height:17px; text-decoration:none;" target="_blank">Uma publicação compartilhada por ', post$name, ' (@', post$handle, 'jairmessiasbolsonaro)</a></p></div></blockquote> <script async src="//www.instagram.com/embed.js"></script>
         '))
  )
}

media_img <- function(media) {
  if (media != "sem mídia") {
    return(
      shiny::img(
        src = media, style = "max-width: 98%;border-radius:10px"
      )
    )
  } else {
    return(NULL)
  }
}

verified <- function(verificado) {
  if (verificado) {
    return(shiny::icon("check-circle fa-sm", style = "color: rgb(1, 110, 252)"))
  } else {
    return(NULL)
  }
}


