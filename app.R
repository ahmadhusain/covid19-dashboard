
library(shiny)
library(shinydashboard)
library(rvest)
library(tidyverse)
library(gganimate)
library(shinythemes)
library(highcharter)
library(formattable)
library(lubridate)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(sever)
library(jsonlite)

options(scipen = 999)


mapdata <- read_csv("mapdata.csv")


### table global

path_to_file <- "https://pkgstore.datahub.io/core/covid-19/countries-aggregated/archive/2302a6fc05d900f73e31531248cbec9b/countries-aggregated.csv"

timeseries_update <- read.csv(url(path_to_file)) %>%
  mutate(Date = ymd(Date)) %>% 
  group_by(Country) %>% 
  arrange(desc(Date)) %>% 
  distinct_at(vars("Country"), .keep_all = TRUE) %>% 
  pull(Date) %>% 
  head(1)

dat <- read.csv(url(path_to_file)) %>%
  mutate(Date = ymd(Date)) %>% 
  group_by(Country) %>% 
  arrange(desc(Date)) %>% 
  distinct_at(vars("Country"), .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(Country = as.character(Country)) %>% 
  select(-Date)

dat_map <- dat %>% 
  mutate(country = case_when(
    Country == "US" ~ "United States of America",
    Country == "Korea, South" ~ "South Korea",
    Country == "Czechia" ~ "Czech Republic",
    TRUE ~ Country
  )) %>% 
  left_join(mapdata, by = c("Country" = "name"))

#### confirmed

ts_confirmed <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))

date_seq <- seq(from = ymd("2020-01-22"), 
                length.out = ncol(ts_confirmed) - 4,
                by = "day")

colnames(ts_confirmed) <- c("province_state", "country", "lat", "long", as.character(date_seq))

ts_confirmed_long <- pivot_longer(
  ts_confirmed,
  cols = -c(province_state, country, lat, long),
  names_to = "datetime",
  values_to = "confirmed"
)


#### recovered

ts_recovered <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))

date_seq_recov <- seq(from = ymd("2020-01-22"), 
                length.out = ncol(ts_recovered) - 4,
                by = "day")

colnames(ts_recovered) <- c("province_state", "country", "lat", "long", as.character(date_seq_recov))



ts_recovered_long <- pivot_longer(
  ts_recovered,
  cols = -c(province_state, country, lat, long), 
  names_to = "datetime", 
  values_to = "recovered"
)



#### deaths


ts_deaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))


date_seq_deaths <- seq(from = ymd("2020-01-22"), 
                      length.out = ncol(ts_deaths) - 4,
                      by = "day")


colnames(ts_deaths) <- c("province_state", "country", "lat", "long", as.character(date_seq_deaths))



ts_deaths_long <- pivot_longer(
  ts_deaths, 
  cols = -c(province_state, country, lat, long),
  names_to = "datetime",
  values_to = "deaths"
)


ui <- fluidPage(
  
  use_sever(),
  
  navbarPage(
    
    "Novel Coronavirus (COVID-19) Situation",
    theme = shinytheme("cyborg"),
    
    
    
    tabPanel(
      
      "Global",
      
      
      sidebarLayout(
        sidebarPanel(width = 4,
                     
                     p(paste("Updated: ", format(x = timeseries_update, format("%d, %B %Y")))),
                     
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
              selected = c("US","Spain", "Italy", "China")
            )
          ),
          
          column(
            width = 4,
            
            dateRangeInput("dateSelector",
                           label = "Observation Period",
                           start = ymd("2020-01-01"),
                           end = ymd(Sys.Date()),
                           separator = "to"
            )
          ),
          
          column(
            width = 4,
            
            radioButtons(
              inputId = "option",
              label = "Choose plot",
              inline = TRUE,
              choices = c("case", "death", "recovered")
              
            )
          ),
          
          br(),
          
          highchartOutput(
            outputId = "plot", 
          ),
          
          hr(),
          
          highchartOutput(
            outputId = "map", 
          )
        )
      )
    )
  )
  
  
)
  
  
  

server <- function(input, output) {
  
  sever()

  output$plot <- renderHighchart({
    
    if (input$option == "death") {
      ts_deaths_long %>% 
        mutate(datetime = ymd(datetime)) %>% 
        filter(country %in% input$country) %>% 
        group_by(country, datetime) %>% 
        summarise(deaths = sum(deaths)) %>% 
        filter(datetime >= input$dateSelector[1] & datetime < input$dateSelector[2]) %>% 
        hchart(.,
               type = "line",
               hcaes(
                 x = datetime,
                 y = deaths,
                 group = country
               )
        ) %>% 
        hc_title(
          text = "The Total Number of Deaths"
        ) %>% 
        hc_subtitle(
          text = "The data is compiled by the Johns Hopkins University Center"
        ) %>% 
        hc_plotOptions(
          line = list(
            lineWidth = 4,
            allowPointSelect = TRUE,
            marker= list(
              enabled = FALSE,
              radius=1,
              symbol="circle"
            )
          )
        ) %>% 
        hc_add_theme(
          hc_theme_db()
        ) %>% 
        hc_exporting(enabled = TRUE)
    } 
    
    else if (input$option == "recovered") {
      
      ts_recovered_long %>% 
        mutate(datetime = ymd(datetime)) %>% 
        filter(country %in% input$country) %>% 
        group_by(country, datetime) %>% 
        summarise(recovered = sum(recovered)) %>% 
        filter(datetime >= input$dateSelector[1] & datetime < input$dateSelector[2]) %>% 
        hchart(.,
               type = "line",
               hcaes(
                 x = datetime,
                 y = recovered,
                 group = country
               )
        ) %>% 
        hc_title(
          text = "The Total Number of Recovered"
        ) %>% 
        hc_subtitle(
          text = "The data is compiled by the Johns Hopkins University Center "
        ) %>% 
        hc_plotOptions(
          line = list(
            lineWidth = 4,
            allowPointSelect = TRUE,
            marker= list(
              enabled = FALSE,
              radius=1,
              symbol="circle"
            )
          )
        ) %>% 
        hc_add_theme(
          hc_theme_db()
        ) %>% 
        hc_exporting(enabled = TRUE)
      
    } 
    
    else {
      
      ts_confirmed_long %>% 
        mutate(datetime = ymd(datetime)) %>% 
        filter(country %in% input$country) %>% 
        group_by(country, datetime) %>% 
        summarise(confirmed = sum(confirmed)) %>% 
        filter(datetime >= input$dateSelector[1] & datetime < input$dateSelector[2]) %>% 
        hchart(.,
               type = "line",
               hcaes(
                 x = datetime,
                 y = confirmed,
                 group = country
               )
        ) %>% 
        hc_title(
          text = "The Total Number of Confirmed Cases"
        ) %>% 
        hc_subtitle(
          text = "The data is compiled by the Johns Hopkins University Center "
        ) %>% 
        hc_plotOptions(
          line = list(
            lineWidth = 4,
            allowPointSelect = TRUE,
            marker= list(
              enabled = FALSE,
              radius=1,
              symbol="circle"
            )
          )
        ) %>% 
        hc_add_theme(
          hc_theme_db()
        ) %>% 
        hc_exporting(enabled = TRUE)
      
    }
    

  })

  output$fulldata <- renderDataTable({
    
    
    dat %>% 
      mutate_if(is.numeric, ~comma(.,digits = 0)) %>% 
      arrange(desc(Confirmed)) %>% 
    datatable(caption = 'Table 1: Top 20 Confirmed Cases, Deaths, Recovered by Country',
              options = list(dom = "ft",
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"), 
                             scrollX = TRUE,
                             pageLength = 20), 
              rownames = T) %>% 
      formatStyle(names(dat),
                  backgroundColor = "black",
                  background = "black",
                  target = "row",
                  fontSize = "100%") %>% 
      formatCurrency(mark = ",", columns = 2:9, interval = 3, currency = "", digits = 0)
    
  })
  
  output$map <- renderHighchart({
    
    
    hcmap(map = "custom/world.js", data = dat_map, value = "Confirmed",
          joinBy = c("iso-a3"), name = "Total Cases",
          dataLabels = list(enabled = TRUE, format = '{point.name}'),
          borderColor = "black", borderWidth = 0.1,
          tooltip = list(valueDecimals = 0)) %>% 
      hc_add_theme(
        hc_theme_db()
      ) %>% 
      hc_colorAxis(minColor = "#C5C889", maxColor = "#434348") %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_title(
        text = "Mapping Cumulative Confirmed Cases by Country"
      ) %>% 
      
      hc_subtitle(
        text = paste("Updated:", Sys.time())
      )
  })
  
  
  
}

shinyApp(ui, server)
