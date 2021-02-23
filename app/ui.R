# 1.0 ui App -----

# 1.1 Object -----

# Application that the public will be able to perform direct search by google trend

# 2.0 Packages ----

library(shinydashboard)
library(ggplot2)
library(plotly)


# 3.0 Make UI.R ----

ui <- dashboardPage(
  dashboardHeader(title="All Medical Expert"),
  
  
  dashboardSidebar(
    br(),
    
    
    h6(" Termo(s) de Pesquisa",style="text-align:center;color:#FFA319;font-size:150%"),
    
    helpText("Forneça uma ou até 05 (cinco) palavras chaves que deseja que mostrando a tendência 
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
    
    checkboxInput("corr", 
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
    
  ),
  
  
  #####
  ##  Main Panel
  #### help ====        
  dashboardBody(    
        fluidRow(
      br(),
      h5(em(strong("Análise de Tendência", style="color:darkblue;font-size:210%")),align = "center"),
      
      h5(tabsetPanel(type = "tabs",
                  tabPanel("Tendência",
                           HTML("<br><br>"),
                           h5("Gráfico de Tendência das palavras."),
                           plotOutput("graph_TS")
                  ),
                  tabPanel("Regionalidade",
                           HTML("<br><br>"),
                           h5("Gráfico por tema, e frequência por estados brasileiro."),
                           plotOutput("graph_region")
                           ),
                  tabPanel("Temas Relacionados.",
                           HTML("<br><br>"),
                           h5("Escolha a quantidade de Temas relacionados"),
                           numericInput("num", 
                                        h5("De 5 a 30 termos"),
                                        min = 5,
                                        max = 30,
                                        step = 5,
                                        value = 10),
                           plotOutput("graph_related_topics")
                  )),
                  style="padding-left: 20px;"
                  )
      
      
    )
  ))