

shinyUI(
  
  
  fluidPage(
    
    tags$head(
      tags$style(
        HTML(
          "
          table.dataTable tbody th, table.dataTable tbody td {
    padding: 8px 10px;
    background: white;
    font-size: 12px;
}"
        )
      )
    ),
    
    use_sever(),
    useShinyalert(),
    
    navbarPage(
      
      "Novel Coronavirus (COVID-19) Situation",
      theme = shinytheme("united"),
      
      
      
      tabPanel(
        
        "Global",
        
        
        sidebarLayout(
          sidebarPanel(width = 4,
                       
                      
                       p(paste("Updated: ",format(x = Sys.time() %>% ymd_hms() %>% with_tz(tzone = "Asia/Jakarta"),
                                                  format("%A, %d %B %Y %H:%M:%S")))),
                       
                       p("Source: api.kawalcorona.com"),
                       
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
              color = "red",
              fill = TRUE
            ),
            
            infoBox(
              value = tags$p(style = "font-size: 20px;", comma(sum(dat %>% 
                                                                     pull(Recovered), na.rm = T), digits = 0)),
              title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Recovered"),
              icon = icon("user-plus"),
              color = "red",
              fill = TRUE
            ),
            
            infoBox(
              value = tags$p(style = "font-size: 20px;", comma(sum(dat %>% 
                                                                     pull(Deaths), na.rm = T), digits = 0)),
              title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Deaths"),
              icon = icon("user-alt-slash"),
              color = "red",
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
            ) %>% withSpinner(type = 4, color = "#C5C889", size = 0.5) ,
            
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
                       
                       p(paste("Updated: ", format(x = Sys.time() %>% ymd_hms() %>% with_tz(tzone = "Asia/Jakarta"),
                                                   format("%A, %d %B %Y %H:%M:%S")))),
                       
                       p("Source: api.kawalcorona.com"),
                       
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
              color = "red",
              fill = TRUE
            ),
            
            infoBox(
              value = tags$p(style = "font-size: 20px;", comma(pull(infobox_ts_indo, recovered), digits = 0)),
              title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Recovered"),
              icon = icon("user-plus"),
              color = "red",
              fill = TRUE
            ),
            
            infoBox(
              value = tags$p(style = "font-size: 20px;", comma(pull(infobox_ts_indo, deaths), digits = 0)),
              title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Deaths"),
              icon = icon("user-alt-slash"),
              color = "red",
              fill = TRUE
            ),
            
            br(),
            
            highchartOutput(
              outputId = "plotindo"
            ) %>% withSpinner(type = 4, color = "#C5C889", size = 0.5),
            
            br(),
            hr(),
            
            
            
            highchartOutput(
              outputId = "mapindo", height = "600px"
         )
        )
      )
     ),
     
     
     tabPanel(
       
       "News",
       
       sidebarLayout(
         
         sidebarPanel(
           
           p("You can provide keywords to find the news about this pandemic in Indonesia. Press the 'summary' button to get a resume. The process of text summarization using extractive method with the TextRank algorithm."),
           
           textInput(
             inputId = "getnews",
             label = "what's are you looking for?", 
             value = "Corona",
           ),
           
           radioButtons(
             inputId = "type",
             label = "Please choose this following types of news:",
             inline = TRUE,
             choices = c("finance", "health", "news", "sport")
             
           ),
           
           actionButton("goButton", "Get!", icon = icon("refresh"))
           
         ),
         mainPanel(
           column(
             
             dataTableOutput(
               outputId = "newstable"
             ) %>% withSpinner(type = 4, color = "#C5C889", size = 0.5),
             width = 12,
             style = "height:600px; overflow-y: scroll;"
           )
         )
       )
     )
    )
  )
)