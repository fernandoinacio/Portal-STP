
server <- function(input,output){
  
  NewCases_tbl$NewActive <- NewCases_tbl$NewConfirmed-NewCases_tbl$NewRecovered-NewCases_tbl$NewDeaths
  
  
    corona_1 <- reactive({
    if (input$country=="São Tomé e Principe")(
      NewCases_tbl %>%  filter(between(date, input$date_range[1], input$date_range[2]))
    ) else(
      NewCases_tbl %>%  filter(between(date, input$date_range[1], input$date_range[2]) &
                           countryName == input$country)
   )
  })

  tbl_all = reactive ({
    corona_1() %>%
    dplyr::group_by(countryName) %>%
    slice(n()) %>%
    ungroup() %>%
    #select(-countryCode) %>%
    arrange(desc(confirmed)) %>%
    mutate(totalActivePer = Active/confirmed) %>%
    mutate(totalRecoveredPer = recovered/confirmed) %>%
    mutate(totalDeathPer = death/confirmed) %>%
    select(Pais = countryName, Confirmado = confirmed, Activos = Active,Recuperados = recovered,Mortes = death,"Activos (%)" = totalActivePer,"Recuperados (%)" = totalRecoveredPer,"Mortes (%)" = totalDeathPer)
  })
  # 
  # 
  # 
  corona_11 <- reactive({
    corona_1() %>%
      select_all() %>%
      group_by(date = lubridate::floor_date(date, unit = "day")) %>%
      summarise_at(vars(suspeitos:NewActive),sum)
  })
  # 
  corona_2 <- reactive ({
    switch (
      input$prd,
      "Diario" = corona_1() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "day")) %>%
        summarise_at(vars(suspeitos:Novas_mortes),sum),
      "Semanal" =  corona_1() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "week")) %>%
        summarise_at(vars(suspeitos:Novas_mortes), sum),
      "Mensal" =  corona_1() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "month")) %>%
       summarise_at(vars(suspeitos:Novas_mortes),sum)
    )
  })

  # 
  # 
  confirmed_tbl <- reactive({
    corona_1()[,c(1,5)]
    # confirmed_tbl$type <- "confirmed"
    # colnames(confirmed_tbl)<- c("date","cases")
  })
  # 
  active_tbl <- reactive({
    corona_1()[,c(1,8)]
  # active_tbl$type <- "active"
  # colnames(active_tbl)<- c("date","cases")
  })
  # 
  recovered_tbl <- reactive({
    corona_1()[,c(1,7)]
   # recovered_tbl$type <- "recovered"
   # colnames(recovered_tbl)<- c("date","cases","type")
  })
  # 
  death_tbl <- reactive({
    corona_1()[,c(1,6)]
  #  death_tbl$type <- "death"
  # colnames(death_tbl)<- c("date","cases","type")
  })
 
  
  output$sus = renderInfoBox(
    
    infoBox(
      title = tags$p("SUSPEITOS", style="font-size:90%;"),
      value = tags$p(scales::comma(sum(corona_2()$suspeitos)),style="font-size:150%;"),
      subtitle = tags$p(paste(scales::comma(corona_2()$suspeitos[nrow(corona_2())]),ifelse(input$prd=="Diario",
                                                                                           " casos nas ultimas 24h",ifelse(input$prd=="Semanal","casos na ultima semana",
                                                                                                                           "casos no ultimo mês"))), style="position:relative; bottom:5px;") ,      icon = icon("user-md"),
      color = "aqua"
    )
  )
  output$tes = renderInfoBox(
    
    infoBox(
      title = tags$p("TESTADOS", style="font-size:90%;"),
      value = tags$p(scales::comma(sum(corona_2()$testados)),style="font-size:150%;"),
      subtitle = tags$p(paste(scales::comma(corona_2()$testados[nrow(corona_2())]),ifelse(input$prd=="Diario",
                                                                                          " casos nas ultimas 24h",ifelse(input$prd=="Semanal","casos na ultima semana",
                                                                                                                          "casos no ultimo mês"))), style="position:relative; bottom:5px;"),
      icon = icon("microscope"),
      color = "orange"
    )
  )
  output$conf = renderInfoBox(
    
    infoBox(
      title = tags$p("CONFIRMADOS", style="font-size:90%;"),
      value = tags$p(scales::comma(sum(corona_2()$confirmed)),style="font-size:150%;"),
      subtitle =  tags$p(paste(scales::comma(corona_2()$confirmed[nrow(corona_2())]),ifelse(input$prd=="Diario",
                                                                                            " casos nas ultimas 24h",ifelse(input$prd=="Semanal","casos na ultima semana",
                                                                                                                            "casos no ultimo mês"))), style="position:relative; bottom:5px;"),
      icon = icon("users"),
      color = "blue"
    )
  )
  output$act = renderInfoBox(
    
    infoBox(
      title = tags$p("ACTIVOS", style="font-size:90%;"),
      value = tags$p(scales::comma(sum(corona_2()$Active)),style="font-size:150%;"),
      subtitle =  tags$p(paste(scales::comma(corona_2()$Active[nrow(corona_2())]),ifelse(input$prd=="Diario",
                                                                                         " casos nas ultimas 24h",ifelse(input$prd=="Semanal","casos na ultima semana",
                                                                                                                         "casos no ultimo mês"))), style="position:relative; bottom:5px;"),
      

      icon = icon("hospital"),
      color = "purple"
    )
  )
  output$rec = renderInfoBox(
    
    infoBox(
      title = tags$p("RECUPERADOS", style="font-size:90%;"),
      value = tags$p(scales::comma(sum(corona_2()$recovered)),style="font-size:150%;"),
      subtitle =  tags$p(paste(scales::comma(corona_2()$recovered[nrow(corona_2())]),ifelse(input$prd=="Diario",
                                                                                            " casos nas ultimas 24h",ifelse(input$prd=="Semanal","casos na ultima semana",
                                                                                                                            "casos no ultimo mês"))), style="position:relative; bottom:5px;"),
      icon = icon("smile"),
      color = "green"
    )
  )
  output$obt = renderInfoBox(
    
    infoBox(
      title = tags$p("OBITOS", style="font-size:90%;"),
      value = tags$p(scales::comma(sum(corona_2()$death)),style="font-size:150%;"),
      subtitle =  tags$p(paste(scales::comma(corona_2()$death[nrow(corona_2())]),ifelse(input$prd=="Diario",
                                                                                        " casos nas ultimas 24h",ifelse(input$prd=="Semanal","casos na ultima semana",
                                                                                                                        "casos no ultimo mês"))), style="position:relative; bottom:5px;"),
      icon = icon("heartbeat"),
      color = "red"
    )
  )
  

  output$txt <- renderText(
   paste("Ultima atualizacao:",format(max(aux$date),format="%d/%m/%Y"))
  )
  
  output$timeseries <- renderHighchart({
    highchart() %>%
      
   #  hc_chart(type = "areaspline") %>%
      
      hc_title(text = ifelse(input$country=="São Tomé e Principe","Evolução do virus no tempo",
                             paste("Evolução do virus no tempo -",input$country,sep = " "))
               ,   style = list(color = "#2b908f", useHTML = TRUE))%>%

      hc_xAxis(
        categories = format(corona_2()$date, format="%d-%m"))       %>%
     
      
      
      hc_add_series(
        name = "Suspeitos",
        data = cumsum(corona_2()$suspeitos),
        dataLabels = list(enabled = F)
      ) %>%
      
      
      hc_add_series(
        name = "Testados",
        data = cumsum(corona_2()$testados),
        dataLabels = list(enabled = F)
      ) %>%
      
        hc_add_series(
        name = "Confirmados",
        #data = corona_2()$NewConfirmed,
        data = cumsum(corona_2()$confirmed),
        dataLabels = list(enabled = F)
      ) %>%
      
      hc_add_series(
        name = "Activos",
        data = cumsum(corona_2()$Active),
        dataLabels = list(enabled = F)
      ) %>%
      
      hc_add_series(
        name = "Recuperados",
        data = cumsum(corona_2()$recovered),
        dataLabels = list(enabled = F)
      ) %>%
      
  
      hc_add_series(
        name = "Obitos",
        data = cumsum(corona_2()$death),
        dataLabels = list(enabled = FALSE)
      ) %>%
      hc_colors(c("#000000","#696969","#4B0082","#FF7F50","#2E8B57","#FF0000")) %>%
      
      hc_tooltip(
        crosshairs = F,
        backgroundColor = "#FCFFC5",
        shared = T
       # borderWidth = 3
      ) %>%
      hc_exporting(
        enabled = TRUE,
        menuItemDefinitions = list(
          label = list(
            onclick = JS('function() {
          this.renderer.label("You just clicked a custom menu item.", 100, 100)
          .attr({fill: "#a4edba", r: 5, padding: 10, zIndex: 10})
          .css({fontSize: "1.5em"})
          .add();
        }'),
            text = 'Show label'
          )
        ),
        buttons = list(
          contextButton = list(
            menuItems = list('downloadPNG', 'downloadPDF','downloadCSV', 'downloadXLS')
          ))
      )
    
  })
  

  
  
  a <- reactive({
        corona_2()%>%
      select(Data=date,Suspeitos=suspeitos,Testados=testados,Confirmados=confirmed,Recuperados=recovered,Mortes=death,Activos=active)
    
  })
  output$tabela_timeseries <- renderDataTable({

    
    datatable(a(),
              extensions = 'Buttons',
              rownames = FALSE,
              # filter = 'top',
              options = list(
                searchHighlight = TRUE,
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons =
                  list(
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'pdf'),
                      text = 'Download'
                    )
                  )
                
              )
              
    ) 
  })
  
  output$tabela <- renderDataTable({
    datatable(tbl_all(),
              extensions = 'Buttons',
              rownames = FALSE,
              # filter = 'top',
              options = list(
                searchHighlight = TRUE,
                pageLength = 25,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons =
                  list(
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'pdf'),
                      text = 'Download'
                    )
                  )
                
              )
              
    ) %>%
      formatPercentage('Activos (%)',2) %>%
      formatPercentage('Recuperados (%)',2) %>%
      formatPercentage('Mortes (%)',2) %>%
      formatStyle(
        'Activos (%)',
        background = styleColorBar(tbl_all()$'Activos (%)', '#31bed4'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Recuperados (%)',
        background = styleColorBar(tbl_all()$'Recuperados (%)', '#8bd431'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Mortes (%)',
        background = styleColorBar(tbl_all()$'Mortes (%)', '#ff5757'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
  }
    
  )
 
  
  base <- reactive({
    tbl_all() %>%
      filter(Pais%in%input$countrySel)
    
  })
  
  output$comp <- renderHighchart({
   
           highchart() %>%
      
      
      hc_title(
        text = "Cumulativos",
        style = list(color = "#2b908f", useHTML = TRUE)
      ) %>%
      
      hc_chart(type = "column") %>%
      
       hc_xAxis(
              categories =base()$Pais,
            title = list(text = "Paises")) %>%
           
     
      # hc_add_theme(hc_theme_google()) %>%
      
      hc_plotOptions(series = list(showInLegend = TRUE)) %>%
      
      hc_add_series(data =base()$Confirmado ,
                    name = "Confirmados"
                    ,  dataLabels = list(enabled = T)) %>%
      hc_add_series(data =base()$Activos,
                    name = "Activos  "
                    , dataLabels = list(enabled = T)) %>%
      hc_add_series(data = base()$Recuperados,
                    name = "Recuperados"
                    , dataLabels = list(enabled = T))%>%
      hc_add_series(data = base()$Mortes ,
                    name = "Obitos  "
                    , dataLabels = list(enabled = T)) %>%
      
      hc_colors(c("#4B0082","#FF7F50","#2E8B57","#FF0000")) %>%
      
      hc_tooltip(
        crosshairs = TRUE,
        borderWidth = 5,
        sort = TRUE,
        shared = TRUE,
        table = F
        
      )
  })
  

  
  output$cards <- renderUI({
    
    activos_total <- (sum(tbl_all()$Activos)/sum(tbl_all()$Confirmado))*100
    recovered_total <- (sum(tbl_all()$Recuperados)/sum(tbl_all()$Confirmado))*100
    death_total <- (sum(tbl_all()$Mortes)/sum(tbl_all()$Confirmado))*100
    activos_total <- (sum(tbl_all()$Activos)/sum(tbl_all()$Confirmado))*100
    

    
    argonRow(
      
      argonColumn(width = 4, argonBadge(paste(paste("Taxa de Activos ",scales::comma(activos_total),"%")))),
      argonColumn(width = 4, argonBadge(paste(paste("Taxa de Recuperados ",scales::comma(recovered_total),"%")))),
      argonColumn(width = 4, argonBadge(paste(paste("Taxa de Mortes ",scales::comma(death_total),"%"))))
      
    )
   
  })
  
  output$rates <- renderHighchart({
    
    highchart() %>%
      
      
      hc_title(
        text = ifelse(input$country=="São Tomé e Principe","Evolução dos principais indicadores no tempo",
                      paste("Evolução dos principais indicadores no tempo-",input$country)),
        style = list(color = "#2b908f", useHTML = T)
      ) %>%
      
      hc_chart(type = "spline") %>%
      
      hc_xAxis(
        categories = format(corona_2()$date, format="%d-%m")) %>%
      
      
      # hc_add_theme(hc_theme_google()) %>%
      
      hc_plotOptions(series = list(showInLegend = TRUE)) %>%
      
      hc_add_series(data =round((cumsum(corona_2()$Active)/cumsum(corona_2()$confirmed))*100,2) ,
                    name = "Taxa de Activos(%)"
                    ,  dataLabels = list(enabled = F)) %>%
      hc_add_series(data =round((cumsum(corona_2()$recovered)/cumsum(corona_2()$confirmed))*100,2),
                    name = "Taxa de Recuperados(%)"
                    , dataLabels = list(enabled = F)) %>%
      hc_add_series(data = round((cumsum(corona_2()$death)/cumsum(corona_2()$confirmed))*100,2),
                    name = "Taxa de Obitos(%)"
                    , dataLabels = list(enabled = F))%>%
      
       hc_colors(c("#FF7F50","#2E8B57","#A52A2A")) %>%
      
    hc_tooltip(
      crosshairs = F,
      backgroundColor = "#FCFFC5",
      shared = T
      # borderWidth = 3
    ) %>%
      hc_exporting(
        enabled = TRUE,
        menuItemDefinitions = list(
          label = list(
            onclick = JS('function() {
                         this.renderer.label("You just clicked a custom menu item.", 100, 100)
                         .attr({fill: "#a4edba", r: 5, padding: 10, zIndex: 10})
                         .css({fontSize: "1.5em"})
                         .add();
  }'),
            text = 'Show label'
            )
            ),
        buttons = list(
          contextButton = list(
            menuItems = list('downloadPNG', 'downloadPDF','downloadCSV', 'downloadXLS')
          ))
            )
    
  })
  

  output$tabela_rates <- renderDataTable({
    
    data_rates <- reactive({

      corona_1()%>%
        select(Data=date,`Taxa de activos`=Taxa_activos,`Taxa de recuperados`=Taxa_recuperados,`Taxa de Mortes`=Taxa_mortes)
      
    })
    
    datatable(data_rates(),
              extensions = 'Buttons',
              rownames = FALSE,
              # filter = 'top',
              options = list(
                searchHighlight = TRUE,
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons =
                  list(
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'pdf'),
                      text = 'Download'
                    )
                  )
                
              )
              
    ) 

    
  }
  
  )
  
  output$ratesNew <- renderHighchart({
    
    highchart() %>%
      
      hc_title(
        text = ifelse(input$country=="São Tomé e Principe","Evolução dos indicadores de casos novos no tempo",
                      paste("Evolução dos indicadores de casos novos no tempo-",input$country)),
        style = list(color = "#2b908f", useHTML = TRUE)
      ) %>%
      
      hc_chart(type = "column") %>%
      
      hc_xAxis(
        categories = format(corona_2()$date,format="%d-%m")) %>%
      
      hc_plotOptions(series = list(showInLegend = TRUE)) %>%
      
      hc_add_series(data =round(corona_2()$confirmed,2) ,
                    name = "Casos confirmados"
                    ,  dataLabels = list(enabled = F)) %>%
      hc_add_series(data =round(corona_2()$recovered,2),
                    name = "Casos recuperados"
                    , dataLabels = list(enabled = F)) %>%
      hc_add_series(data = round(corona_2()$death,2),
                    name = "Casos Mortes"
                    , dataLabels = list(enabled = F))%>%
      
      hc_colors(c("#4B0082","#2E8B57","#FF0000")) %>%

    hc_tooltip(
      crosshairs = F,
      backgroundColor = "#FCFFC5",
      shared = T
      # borderWidth = 3
    ) %>%
      
      hc_exporting(
        enabled = TRUE,
        menuItemDefinitions = list(
          label = list(
            onclick = JS('function() {
                         this.renderer.label("You just clicked a custom menu item.", 100, 100)
                         .attr({fill: "#a4edba", r: 5, padding: 10, zIndex: 10})
                         .css({fontSize: "1.5em"})
                         .add();
  }'),
            text = 'Show label'
          )
        ),
        buttons = list(
          contextButton = list(
            menuItems = list('downloadPNG', 'downloadPDF','downloadCSV', 'downloadXLS')
          ))
      )
    
  })
  
  output$tabela_Nrates <- renderDataTable({
    
    data_rates <- reactive({
      
      corona_1()%>%
        select(Data=date,`Novos confirmados`=NewConfirmed,
               `Novos recuperados`=NewRecovered,
               `Novas mortes`=NewDeaths)
      
    })
    
    datatable(data_rates(),
              extensions = 'Buttons',
              rownames = FALSE,
              # filter = 'top',
              options = list(
                searchHighlight = TRUE,
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons =
                  list(
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'pdf'),
                      text = 'Download'
                    )
                  )
                
              )
              
    )
  }
  
  )
  
  output$casesNew <- renderHighchart({
    
    highchart() %>%
      
      hc_title(
        text = ifelse(input$country=="São Tomé e Principe","Evolução de casos novos no tempo",
                      paste("Evolução de casos novos no tempo-",input$country)),
        style = list(color = "#2b908f", useHTML = TRUE)
      ) %>%
      
      hc_chart(type = "column") %>%
      
      hc_xAxis(
        categories = format(corona_2()$date, format="%d-%m")) %>%
      
      hc_plotOptions(series = list(showInLegend = TRUE)) %>%
      
      hc_add_series(data =corona_2()$NewConfirmed,
                    name = "Novos confirmados"
                    ,  dataLabels = list(enabled = F)) %>%
      # hc_add_series(data =corona_2()$NewActive ,
      #               name = "Novos activos"
      #               ,  dataLabels = list(enabled = F)) %>%
      hc_add_series(data =corona_2()$NewRecovered,
                    name = "Novos recuperados"
                    , dataLabels = list(enabled = F)) %>%
      hc_add_series(data = corona_2()$NewDeaths,
                    name = "Novas Mortes"
                    , dataLabels = list(enabled = F))%>%
      
      hc_colors(c("#4B0082","#2E8B57","#FF0000")) %>%
      
      hc_tooltip(
        crosshairs = F,
        backgroundColor = "#FCFFC5",
        shared = T
        # borderWidth = 3
      ) %>%
      
      hc_exporting(enabled = TRUE)
    

  })
  
  output$tabela_Newrates <- renderDataTable({
    
    data_rates <- reactive({
      
      corona_1()%>%
        select(Data=date,`Novos confirmados`=Novos_confirmados,
               `Novos recuperados`=Novos_recuperados,
               `Novas mortes`=Novas_mortes)
      
    })
    
    datatable(data_rates(),
              extensions = 'Buttons',
              rownames = FALSE,
              # filter = 'top',
              options = list(
                searchHighlight = TRUE,
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons =
                  list(
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'pdf'),
                      text = 'Download'
                    )
                  )
                
              )
              
    ) 

  })
  
  }
  
  
  
 