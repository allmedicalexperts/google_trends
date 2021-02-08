# start user interface ----

library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title="By Fish"),
  
  dashboardSidebar(
    br(),
    
    
    h6(" Search Term(s)",style="text-align:center;color:#FFA319;font-size:150%"),
    
    helpText("Give one or more terms that you want R to retrieve data from the Google Trends API.
             Use comma to separate terms", style="text-align:center"),
    
    textInput('terms',''),
    
    
    selectInput("geography", 
                label = tags$h4(strong(em("Geography")),style="text-align:center;color:#FFA319;font-size:150%"),
                choices = c("Worldwide","Afghanistan","Albania","Algeria","Angola","Argentina","Armenia","Australia","Austria",  "Azerbaijan","Bahamas","Bahrain","Bangladesh","Belarus","Belgium","Botswana", "Brazil","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Canada","Chad","Chile","China","Colombia","Cuba","Cyprus","Czech Republic","Denmark","Djibouti","Ecuador","Egypt","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Finland","France","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Hong Kong","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kiribati","Korea (North)","Korea (South)","Kuwait","Kyrgyzstan","Lebanon","Liberia","Libya","Macedonia","Madagascar","Malawi","Malaysia","Mali","Malta","Mexico","Morocco","Mozambique","Namibia","Nepal","Netherlands","New Zealand","Niger","Nigeria","Norway","Oman","Pakistan","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Romania","Russian Federation","Rwanda","Saudi Arabia","Senegal","Serbia","Sierra Leone","Singapore","Somalia","South Africa","Spain","Sudan","Swaziland","Sweden","Switzerland","Syria","Taiwan","Tajikistan","Tanzania","Thailand","Togo","Tunisia","Turkey","Turkmenistan","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uzbekistan","Venezuela","Viet Nam","Yemen","Zaire","Zambia","Zimbabwe"),
                selected = "Worldwide"),           
    selectInput("period", 
                label = tags$h4(strong(em("Time Period")),style="text-align:center;color:#FFA319;font-size:150%"),
                choices = c("2004-present",
                            "Past30Days",
                            "Past90Days",
                            "Past12Months",
                            "2011",
                            "2012",
                            "2013",
                            "2014",
                            "2015"
                ),
                selected = "2004-present"),
    
    checkboxInput("corr", 
                  label = strong("Correlation",style="text-align:center;color:#FFA319;font-size:150%")),
    br(),
    
    tags$h1(submitButton("Update!"),style="text-align:center"),
    helpText("To get results, click the 'Update!' button",style="text-align:center"),
    
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
      h5(em(strong("Google Trends Analytics", style="color:darkblue;font-size:210%")),align = "center"),
      
      plotOutput("myplot"),
      br(),
      plotOutput("myplot3"),
      plotOutput("myplot2")
      
      
    )
  ))