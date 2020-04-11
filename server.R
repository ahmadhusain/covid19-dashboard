shinyServer(
  
  function(input, output) {
    
    sever()
    
    output$plotindo <- renderHighchart({
      
      ts_indo_longer %>% 
        mutate(datetime = ymd(datetime)) %>% 
        hchart(.,
               type = "line",
               hcaes(
                 x = datetime,
                 y = count,
                 group = variable
               )
        ) %>% 
        hc_title(
          text = "Statistics COVID-19 Outbreak in Indonesia"
        ) %>% 
        hc_subtitle(
          text = "Source: https://kawalcorona.com/api/"
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
      
    })
    
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
                                 scrollY = "850px",
                                 pageLength = 185), 
                  rownames = T) %>% 
        formatStyle(names(dat),
                    backgroundColor = "black",
                    background = "black",
                    target = "row",
                    fontSize = "90%") %>% 
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
        ) %>% 
        hc_mapNavigation(enabled = TRUE)
    })
    
    output$indodata <- renderDataTable({
      
      
      
      dat_indo %>%
        datatable(options = list(dom = "ft",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                   "}"), 
                                 scrollX = TRUE,
                                 pageLength = 34), 
                  rownames = T) %>% 
        formatStyle(names(dat_indo),
                    backgroundColor = "black",
                    background = "black",
                    target = "row",
                    fontSize = "90%") %>% 
        formatCurrency(mark = ",", columns = 2:5, interval = 3, currency = "", digits = 0)
      
    })
    
    output$mapindo <- renderHighchart({
      
      hcmap(map = "countries/id/id-all", 
            data = map_indo, 
            value = "kasus",
            name = "Confirmed Cases",
            dataLabels = list(enabled = TRUE, format = '{point.name}'),
            borderColor = "black", borderWidth = 0.1,
            tooltip = list(valueDecimals = 0)) %>% 
        hc_add_theme(
          hc_theme_db()
        ) %>% 
        hc_colorAxis(minColor = "#C5C889", maxColor = "#434348") %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_title(
          text = "Mapping Cumulative Confirmed Cases in Indonesia"
        ) %>% 
        hc_subtitle(
          text = paste("Updated:", Sys.time())
        ) %>% 
        hc_mapNavigation(enabled = TRUE)
      
      
      
    })
    
  }
  
)