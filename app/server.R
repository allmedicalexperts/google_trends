# 1.0 Server App -----

# 1.1 Object -----

# Application that the public will be able to perform direct search by google trend

# 2.0 Packages ----

library(shiny)
library(gtrendsR)
library(reshape2)
library(ggplot2)
library(plotly)
library(geobr)
library(ggspatial)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(plyr)
library(fs)
library(utf8)
library(text2vec)




# 3.0 Make Server.R ----

server <- function(input, output, session){
   

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
         
            ifelse(isTRUE(input$corr),
            gtrends_lst <- gtrends_lst +
            geom_smooth(span = 0.3, se = TRUE),
            gtrends_lst)
            
         
         gtrends_lst
         
         
      })
      
      # <---->
      
      
      
      
      
      # <-- Gráfico de Regionalidade -->
      
      output$graph_region <- renderPlot({
      
      region_gtrens <- gtrends_lst$interest_by_region
      
      region_gtrens$location <- mapvalues(region_gtrens$location,
                                          from = levels(as.factor(region_gtrens$location)),
                                          to = c("Distrito Federal",
                                                 "Acre",     
                                                 "Alagoas",        
                                                 "Amapá",              
                                                 "Amazonas", 
                                                 "Bahia",      
                                                 "Ceará",              
                                                 "Espirito Santo",
                                                 "Goiás",              
                                                 "Maranhão",      
                                                 "Mato Grosso",    
                                                 "Mato Grosso Do Sul", 
                                                 "Minas Gerais",   
                                                 "Pará",            
                                                 "Paraíba",            
                                                 "Paraná",         
                                                 "Pernambuco",      
                                                 "Piauí",              
                                                 "Rio De Janeiro",  
                                                 "Rio Grande Do Norte",
                                                 "Rio Grande Do Sul",  
                                                 "Rondônia",         
                                                 "Roraima",         
                                                 "Santa Catarina",     
                                                 "São Paulo",       
                                                 "Sergipe",         
                                                 "Tocantins"))
      
      
      
      state_trends_tbl <- region_gtrens %>%
         right_join(states_tbl, by = c('location' = "name_state")) %>%
         as_tibble()
      
      
      
      
      # Make Map Geografic
      
      tema_mapa <-
         theme_bw() + # Escolhe o tema. Eu gosto do theme_bw() por ser bem simples/limpo
         
         # Os códigos abaixo são referentes à estética do tema,
         # como o tamanho da fonte, direção do texto,
         # linhas ao fundo, etc.
         
         theme(
            axis.text.y = element_text(
               angle = 90,
               hjust = 0.5,
               size = 8
            ),
            axis.text.x = element_text(size = 8),
            axis.title.y = element_text(size = rel(0.8)),
            axis.title.x = element_text(size = rel(0.8)),
            panel.grid.major = element_line(
               color = gray(0.9),
               linetype = "dashed",
               size = 0.1
            ),
            panel.background = element_rect(fill = "white") +
               annotation_scale(location = "br", width_hint = 0.30)
         )
      
      
      ggplot(state_trends_tbl, aes(geometry = geom)) + 
         geom_sf(aes(fill = hits)) +
         scale_fill_viridis_c() +
         theme_tq() +
         facet_wrap(~keyword,  nrow = 1) +
         labs(title = "Tendências dos temas - Brasil") +
         labs(fill = "Frequência:") 
      
      
      })
      
         
      # <---->
      
      
      
      
      
      # <-- Gráfico de Palavras relacionadas pelo tema -->
      
      output$graph_related_topics <- renderPlot({
         
#         n_terms <- input$num
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
