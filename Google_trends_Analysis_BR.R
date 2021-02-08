# All Medical Expert

# Project: Google Trends Automation ----


# 1.0 Libraries ---


# Install Package 

install.packages('tidyquant')


# ------ Load Package ------

library(gtrendsR)
library(geobr)
library(ggspatial)
library(ggplot2)

# Core -----
library(tidyverse)
library(lubridate)
library(tidyquant)
library(plyr)

# File System
library(fs)


# 2.0 Google Trends API ---

search_terms <- c( 'AWS', 'Azure','Google Cloud')

# Read Search Terms -----
gtrends_lst <- search_terms %>%
  gtrends(geo = 'BR', #Opções: "news","images", "froogle", "youtube"
          #category = 0,     #Zero é defaul, uma lista de categorias pode ser conferida abaixo
          time ='all')


# 3.0 Inspect Trends ----

gtrends_lst %>%
  pluck("interest_over_time") %>%
  mutate(hits = as.numeric(hits)) %>%
  as_tibble() %>%
  ggplot(aes(date, hits, color =  keyword)) +
  geom_line() +
  geom_smooth(sapn = 0.3, se = TRUE) +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Keyword Trends - BR - Over Time")

# 3.2 Trends by Geography ----

gtrends_lst %>%
  pluck("interest_by_region") %>%
  as_tibble()

states_tbl <- read_state(code_state = 'all')

# 4.0 Data Clean

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
  labs(title = "Tendências dos temas - Brasil")




gtrends_lst %>% names()
gtrends_lst %>% pluck("interest_by_dma") %>% as_tibble() %>% View()
gtrends_lst %>% pluck("related_queries") %>% as_tibble() %>% View()

# 3.3 Related Queries ----
gtrends_lst %>% pluck("related_queries") %>% DataExplorer::plot_bar()

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


top_n_related_searches_tbl %>%
  ggplot(aes(value, interest, color = keyword)) +
  geom_segment(aes(xend = value, yend = 0)) +
  geom_point() +
  coord_flip() +
  facet_wrap(~ keyword, nrow = 1, scales = "free_y") +
  theme_tq() +
  scale_color_tq()













