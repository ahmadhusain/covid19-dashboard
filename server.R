

shinyServer(
  
  function(input, output) {
    
    sever()
    
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }
    
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
        hc_add_theme(hc_theme_google()) %>% 
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
          hc_add_theme(hc_theme_google()) %>% 
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
          hc_add_theme(hc_theme_google()) %>% 
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
          hc_add_theme(hc_theme_google()) %>% 
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
                                   "$(this.api().table().header()).css({'background-color': 'white', 'color': 'black'});",
                                   "}"), 
                                 scrollX = TRUE,
                                 scrollY = "850px",
                                 pageLength = 185), 
                  rownames = T) %>% 
        formatStyle(names(dat),
                    backgroundColor = "white",
                    background = "white",
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
        hc_add_theme(hc_theme_google()) %>% 
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
                                   "$(this.api().table().header()).css({'background-color': 'white', 'color': 'black'});",
                                   "}"), 
                                 scrollX = TRUE,
                                 pageLength = 34), 
                  rownames = T) %>% 
        formatStyle(names(dat_indo),
                    backgroundColor = "white",
                    background = "white",
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
        hc_add_theme(hc_theme_google()) %>% 
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
    
    values <- reactiveValues(
      action = NULL
    )
    
    observeEvent(input$goButton, {
      values$action <- 'refresh'
    })
    
    googlenews <- reactive({
    
 
        
        search.term <- gsub(' ', '%20', input$getnews)
        
        
        link <- NULL
        title <- NULL
        url <- NULL
        
        type <- switch (input$type,
                        finance = 29,
                        sport = 27,
                        news = 3,
                        health = 55
        )
        
        for (i in 1) {
          
          
          
          url[i] <- paste0("https://www.detik.com/search/searchall?query=",search.term,"&siteid=",type,"&sortby=time&page=",i)
          
          link_temp <- read_html(url[i]) %>% 
            html_nodes("a") %>% 
            html_attr("href") %>% 
            as.character()
          
          link <- c(link, link_temp[7:15])
          
          
          title_temp <- read_html(url[i]) %>% 
            html_nodes("span .title") %>% 
            html_text() %>% 
            as.character()
          
          title <- c(title, title_temp)
          
          detik_dummy <- data.frame("link" = link, "title" = title) %>% 
            mutate(link = as.character(link),
                   title = as.character(title))
        }
        
        news <- NULL
        
        for (i in 1:nrow(detik_dummy)) {
          news[i] <- read_html(detik_dummy$link[i]) %>% 
            html_nodes("p") %>% 
            html_text() %>% 
            paste(collapse = " ") %>% 
            str_squish() %>% 
            str_remove_all("[\"]")
          
          dat_text <- data.frame("text" = news)
        }
        
        bind_cols(detik_dummy, dat_text) %>% filter(text != "")
      
      

      
    })
    
    output$newstable <- renderDataTable({
      
      
      shiny::validate(
        need(
          input$getnews != "",
          message = "Waiting your Keyword.."
        )
      )
      
      if (is.null(values$action)) {
        return(NULL)
      } 
    
      
      else if (values$action == 'refresh') {
      
      googlenews() %>% 
        mutate( Actions = shinyInput(actionButton, nrow(googlenews()), 'button_', label = "Summary", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )) %>% 
        select(title, text, Actions)
        
      }
    }, escape = FALSE)
    
    value <- reactiveValues(summary = '')
    
    observeEvent(input$select_button, {
      
      selected_news <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      
      article_sentences <- tibble(text = googlenews()[selected_news,"text"]) %>%
        unnest_tokens(sentence, text, token = "sentences") %>%
        mutate(sentence_id = row_number()) %>%
        select(sentence_id, sentence)
      

      article_words <- article_sentences %>%
        unnest_tokens(word, sentence)

      article_summary <- textrank_sentences(data = article_sentences, 
                                            terminology = article_words)

      value <- article_summary[["sentences"]] %>%
        arrange(desc(textrank)) %>% 
        slice(1:3) %>%
        pull(sentence) %>% 
        paste(collapse = " ")
      
      shinyalert("Summary", text = value, type = "info")
      
    })
    
    

  }
  
)