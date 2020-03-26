
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

options(scipen = 999)


mapdata <- read_csv("mapdata.csv")


### table global


list_dat <- read_html("https://www.worldometers.info/coronavirus") %>% 
  html_nodes(css = "td") %>% 
  html_text()

tot_country <- (length(list_dat)/10)-1


dat <- tibble(
  country = list_dat[c(seq(from = 1, to = (tot_country*10)-9, by = 10))],
  total_cases = list_dat[c(seq(from = 2, to = (tot_country*10)-8, by = 10))],
  new_cases = list_dat[c(seq(from = 3, to = (tot_country*10)-7, by = 10))],
  total_deaths = list_dat[c(seq(from = 4, to = (tot_country*10)-6, by = 10))],
  new_deaths = list_dat[c(seq(from = 5, to = (tot_country*10)-5, by = 10))],
  total_recovered = list_dat[c(seq(from = 6, to = (tot_country*10)-4, by = 10))],
  active_cases = list_dat[c(seq(from = 7, to = (tot_country*10)-3, by = 10))],
  serious_critical = list_dat[c(seq(from = 8, to = (tot_country*10)-2, by = 10))],
  tot_cases_per_pop = list_dat[c(seq(from = 9, to = (tot_country*10)-1, by = 10))],
  tot_death_per_pop = list_dat[c(seq(from = 10, to = (tot_country*10)-0, by = 10))]
) %>% 
  mutate(
    total_cases = str_remove_all(total_cases, pattern = ",") %>% as.numeric(),
    new_cases = str_remove_all(new_cases, pattern = "\\+") %>% as.numeric(),
    new_deaths = str_remove_all(new_deaths, pattern = "\\+") %>% as.numeric(),
    total_deaths = str_remove_all(total_deaths, pattern = ",") %>% as.numeric(),
    total_recovered = str_remove_all(total_recovered, pattern = ",") %>% as.numeric(),
    active_cases = str_remove_all(active_cases, pattern = ",") %>% as.numeric(),
    serious_critical = str_remove_all(serious_critical, pattern = ",") %>% as.numeric(),
    tot_cases_per_pop = str_remove_all(tot_cases_per_pop, pattern = ",") %>% as.numeric(),
    tot_death_per_pop = str_remove_all(tot_death_per_pop, pattern = ",") %>% as.numeric(),
    country = str_squish(country)
  )

dat_map <- dat %>% 
  mutate(country = case_when(
    country == "USA" ~ "United States of America",
    country == "UK" ~ "United Kingdom",
    country == "UAE" ~ "United Arab Emirates",
    country == "S. Korea" ~ "South Korea",
    country == "Czechia" ~ "Czech Republic",
    TRUE ~ country
  )) %>% 
  left_join(mapdata, by = c("country" = "name"))

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
                     
                     p(paste("Updated: ", Sys.time()),
                       p("Source: https://www.worldometers.info/coronavirus/")),
                     
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
            value = tags$p(style = "font-size: 20px;",  comma(sum(dat$total_cases, na.rm = T), digits = 0)),
            title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Cases"),
            icon = icon("user-check"),
            color = "black",
            fill = TRUE
          ),
          
          infoBox(
            value = tags$p(style = "font-size: 20px;", comma(sum(dat$total_recovered, na.rm = T), digits = 0)),
            title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Recovered"),
            icon = icon("user-plus"),
            color = "black",
            fill = TRUE
          ),
          
          infoBox(
            value = tags$p(style = "font-size: 20px;", comma(sum(dat$total_deaths, na.rm = T), digits = 0)),
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
              selected = c("China","Iran", "Italy")
            )
          ),
          
          column(
            width = 4,
            
            dateRangeInput("dateSelector",
                           label = "Observation Period",
                           start = ymd("2020-01-01"),
                           end = ymd("2020-04-01"),
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
      distinct(country, .keep_all = TRUE) %>% 
      mutate_all(~replace_na(data = ., replace = 0)) %>% 
      filter(country != "Total:") %>% 
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
    
    
    hcmap(map = "custom/world.js", data = dat_map, value = "total_cases",
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
