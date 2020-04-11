library(shiny)

shinyUI(
  
  
  fluidPage(
    
    use_sever(),
    
    navbarPage(
      
      "Novel Coronavirus (COVID-19) Situation",
      theme = shinytheme("cyborg"),
      
      
      
      tabPanel(
        
        "Global",
        
        
        sidebarLayout(
          sidebarPanel(width = 4,
                       
                       p(paste("Updated: ", format(x = Sys.Date(), format("%d, %B %Y")))),
                       
                       dataTableOutput(
                         outputId = "fulldata"
                       ),
                       
                       br(),
                       
                       p("Created by", a("Ahmad Husain Abdullah", href = "https://github.com/ahmadhusain"), "."),
                       
                       img(src = "http://www.gravatar.com/avatar/faa60428b46ecc30beeef02974e49d47?s=64", width = "70px", height = "70px")
          ),
          
          mainPanel(
            includeCSS(path = "adminlte.css"),
            includeCSS(path = "shinydashboard.css"),
            
            infoBox(
              value = tags$p(style = "font-size: 20px;",  comma(sum(dat %>% 
                                                                      pull(Confirmed), na.rm = T), digits = 0)),
              title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Cases"),
              icon = icon("user-check"),
              color = "black",
              fill = TRUE
            ),
            
            infoBox(
              value = tags$p(style = "font-size: 20px;", comma(sum(dat %>% 
                                                                     pull(Recovered), na.rm = T), digits = 0)),
              title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Recovered"),
              icon = icon("user-plus"),
              color = "black",
              fill = TRUE
            ),
            
            infoBox(
              value = tags$p(style = "font-size: 20px;", comma(sum(dat %>% 
                                                                     pull(Deaths), na.rm = T), digits = 0)),
              title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Deaths"),
              icon = icon("user-alt-slash"),
              color = "black",
              fill = TRUE
            ),
            
            br(),
            br(),
            
            
            column(
              width = 4,
              selectInput(
                inputId = "country", 
                label = "Select Country",
                choices = levels(ts_deaths_long$country), 
                multiple = TRUE,
                selected = selected
              )
            ),
            
            column(
              width = 4,
              
              dateRangeInput("dateSelector",
                             label = "Observation Period",
                             start = ymd("2020-01-01"),
                             end = ymd(Sys.Date() + 1),
                             separator = "to"
              )
            ),
            
            column(
              width = 4,
              
              radioButtons(
                inputId = "option",
                label = "Choose plot",
                inline = TRUE,
                choices = c("confirmed", "death", "recovered")
                
              )
            ),
            
            br(),
            
            highchartOutput(
              outputId = "plot"
            ),
            
            hr(),
            
            highchartOutput(
              outputId = "map", height = "600px"
            )
          )
        )
      ),
      
      
      # Indonesia -----------
      
      tabPanel(
        
        "Indonesia",
        
        
        sidebarLayout(
          sidebarPanel(width = 4,
                       
                       p(paste("Updated: ", format(x = Sys.Date(), format("%d, %B %Y")))),
                       
                       dataTableOutput(
                         outputId = "indodata"
                       ),
                       
                       br()
          ),
          
          mainPanel(
            includeCSS(path = "adminlte.css"),
            includeCSS(path = "shinydashboard.css"),
            
            infoBox(
              value = tags$p(style = "font-size: 20px;",  comma(pull(infobox_ts_indo, confirmed), digits = 0)),
              title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Cases"),
              icon = icon("user-check"),
              color = "black",
              fill = TRUE
            ),
            
            infoBox(
              value = tags$p(style = "font-size: 20px;", comma(pull(infobox_ts_indo, recovered), digits = 0)),
              title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Recovered"),
              icon = icon("user-plus"),
              color = "black",
              fill = TRUE
            ),
            
            infoBox(
              value = tags$p(style = "font-size: 20px;", comma(pull(infobox_ts_indo, deaths), digits = 0)),
              title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Deaths"),
              icon = icon("user-alt-slash"),
              color = "black",
              fill = TRUE
            ),
            
            br(),
            
            highchartOutput(
              outputId = "plotindo"
            ),
            
            br(),
            hr(),
            
            
            
            highchartOutput(
              outputId = "mapindo", height = "600px"
         )
        )
      )
     )
    )
  )
)