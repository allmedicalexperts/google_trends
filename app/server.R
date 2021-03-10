# 1.0 Server App -----

# 1.1 Object -----

# Application that the public will be able to perform direct search by Google trend

# 2.0 Packages ----

library(shiny)
library(gtrendsR)
library(reshape2)
library(ggplot2)
library(plotly)
library(geobr)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(plyr)
library(fs)
library(utf8)
library(text2vec)

# 3.0 Make server.R ----


server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fad fa-sign-out"), "Sair", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      dashboardSidebar(
        
        br(),
        
        
        h6(" Termo(s) de Pesquisa",style="text-align:center;color:#FFA319;font-size:150%"),
        
        helpText("Forneça uma ou até 05 (cinco) palavras chaves, que deseja que mostrando a tendência 
             e os temas relacionados. Use vírgula para separar os termos.", 
                 style="text-align:justify; padding-left: 10px; padding-right: 10px"),
        
        textInput('terms',''),
        
        
        #selectInput("geography", 
        #            label = tags$h4(strong(em("Localização")),style="text-align:center;color:#FFA319;font-size:150%"),
        #            choices = c("Brasil" = "BR",
        #                        "Estados Unidos"= "US"),
        #            selected = "Brasil"),
        
        
        selectInput("period", 
                    label = tags$h4(strong(em("Período de tempo")),style="text-align:center;color:#FFA319;font-size:150%"),
                    choices = c("2004 - Presente" = "all",
                                "Nos Últimos 5 anos"= "today+5-y",
                                "Últimos 12 meses"="today 12-m",
                                "Últimos 90 dias" = "today 3-m",
                                "Últimos 30 dias"= "today 1-m",
                                "Últimos 7 dias"= "now 7-d",
                                "Ontem" = "now 1-d",
                                "Nas Últimas 4 horas"="now 4-H",
                                "Na Última hora" = "now 1-H"),
                    selected = "2004 - Presente"),
        
        checkboxInput("Smooth", 
                      label = strong("Suavidade",style="text-align:center;color:#FFA319;font-size:150%")),
        br(),
        
        tags$h1(actionButton('btn_plot', 'Atualizar'),style="text-align:center"),
        helpText(style="text-align:justify; padding-left: 10px; padding-right: 10px",
                 "Para obter resultado, clique no botão Atualizar.
             "),
        
        br(),
        br(),
        br(),
        br(),
        br(),
        br()
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      
      
      
      #####
      ##  Main Panel
      #### help ====        
      dashboardBody(    
        fluidRow(
          br(),
          h5(em(strong("Análise de Tendência", style="color:darkblue")),align = "left"),
          
          h5(tabsetPanel(type = "tabs",
                         
                         
                         
                         tabPanel("Temas Relacionados",
                                  HTML("<br><br>"),
                                  h5("Os 10 Temas relacionados para cada palavra chave."),
                                  plotOutput("graph_related_topics")
                         ),
                         
                         
                         
                         
                         
                         #                  tabPanel("Regionalidade",
                         #                           HTML("<br><br>"),
                         #                           h5("Gráfico por tema, e frequência por estados brasileiro."),
                         #                           plotOutput("graph_region")
                         #                           ),
                         tabPanel("Tendências",
                                  HTML("<br><br>"),
                                  h5("Gráfico de Tendência das palavras."),
                                  #                           numericInput("num", 
                                  #                                        h5("De 5 a 30 termos"),
                                  #                                        min = 5,
                                  #                                        max = 30,
                                  #                                        step = 5,
                                  #                                        value = 10),
                                  plotOutput("graph_TS")
                         )),
             style="padding-left: 20px;"
          )
          
          
        )
      )
      
      
      
      
    }
    else {
      loginpage
    }
  })
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  output$results2 <-  DT::renderDataTable({
    datatable(mtcars, options = list(autoWidth = TRUE,
                                     searching = FALSE))
  })
  
  
  # <-- Gráfico de TendÊncia Série Temporal -->
  observeEvent(input$btn_plot, {
    search_terms <- space_tokenizer(input$terms, sep =",")
    gtrends_lst <- search_terms %>%  gtrends(geo = "BR", 
                                             category = 0, time =input$period)
    output$graph_TS <- renderPlot({
      gtrends_lst <- gtrends_lst %>%
        pluck("interest_over_time") %>%
        mutate(hits = as.numeric(hits)) %>%
        as_tibble() %>%
        ggplot(aes(date, hits, color =  keyword)) +
        geom_line() +
        theme_tq() +
        scale_color_tq() +
        labs(color = "Palavras Chaves:") 
      
      ifelse(input$period == "all",
             gtrends_lst <- gtrends_lst +
               labs(title = paste0("Tendência - Brasil - ","2004 - Presente")),
             ifelse(input$period == "today+5-y",
                    gtrends_lst <- gtrends_lst +
                      labs(title = paste0("Tendência - BR - ","Nos Últimos 5 anos")),
                    
                    
                    ifelse(input$period == "today 12-m",
                           gtrends_lst <- gtrends_lst +
                             labs(title = paste0("Tendência - BR - ","Últimos 12 meses")),
                           ifelse(input$period == "today 3-m",
                                  gtrends_lst <- gtrends_lst +
                                    labs(title = paste0("Tendência - BR - ","Últimos 90 dias")),
                                  ifelse(input$period == "today 1-m",
                                         gtrends_lst <- gtrends_lst +
                                           labs(title = paste0("Tendência - BR - ","Últimos 30 dias")),
                                         ifelse(input$period == "now 7-d",
                                                gtrends_lst <- gtrends_lst +
                                                  labs(title = paste0("Tendência - BR - ","Últimos 7 dias")),
                                                ifelse(input$period == "now 1-d",
                                                       gtrends_lst <- gtrends_lst +
                                                         labs(title = paste0("Tendência - BR - ","Ontem")),
                                                       ifelse(input$period == "now 4-H",
                                                              gtrends_lst <- gtrends_lst +
                                                                labs(title = paste0("Tendência - BR - ","Nas Últimas 4 horas")),
                                                              
                                                              gtrends_lst <- gtrends_lst +
                                                                labs(title = paste0("Tendência - BR - ","Na Última hora"))
                                                       ))))))))
      
      ifelse(isTRUE(input$Smooth),
             gtrends_lst <- gtrends_lst +
               geom_smooth(span = 0.3, se = TRUE),
             gtrends_lst)
      
      
      gtrends_lst
      
      
    })
    
    # <---->
    
    
    
    
    
    
    # <-- Gráfico de Palavras relacionadas pelo tema -->
    
    output$graph_related_topics <- renderPlot({
      
      n_terms <- 10
      
      top_n_related_searches_tbl <- gtrends_lst %>%
        pluck("related_queries") %>%
        as_tibble() %>%
        filter(related_queries == "top") %>%
        mutate(interest = as.numeric(subject)) %>%
        
        select(keyword, value, interest) %>%
        group_by(keyword) %>%
        arrange(desc(interest)) %>%
        slice(1:n_terms) %>%
        ungroup() %>%
        
        mutate(value = as_factor(value) %>% fct_reorder(interest))
      
      
      x <- top_n_related_searches_tbl$value
      x <- as.character(x)
      Encoding(x) <- c("UTF-8", "UTF-8", "bytes")
      
      x <- as_utf8(x)
      
      x <- as.data.frame(x)
      
      colnames(x) <- c('Temas')
      
      top_n_related_searches_tbl$Temas<- x$Temas
      
      top_n_related_searches_tbl$Temas<- factor(as.character(top_n_related_searches_tbl$Temas),
                                                levels = top_n_related_searches_tbl$Temas[order(top_n_related_searches_tbl$interest)])
      
      
      
      top_n_related_searches_tbl %>%
        ggplot(aes(Temas, interest, color = keyword)) +
        geom_segment(aes(xend = Temas, yend = 0)) +
        geom_point() +
        coord_flip() +
        facet_wrap(~ keyword, nrow = 1, scales = "free_y") +
        labs(color = "Palavras chaves",
             y = NULL,
             X = NULL) + 
        theme_tq() +
        scale_color_tq()
      
      
    })
    
    
    # <---->
    
    
    
  })
  
  
  
}


# <-- Run APP -->

# runApp(list(ui = ui, server = server), launch.browser = TRUE)