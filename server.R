

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
  confirmed_tbl <- reactive({
    corona_1()[,c(1,5)]
  })
  # 
  active_tbl <- reactive({
    corona_1()[,c(1,8)]

  })
  # 
  recovered_tbl <- reactive({
    corona_1()[,c(1,7)]

  })
  # 
  death_tbl <- reactive({
    corona_1()[,c(1,6)]
  })
 
  # 
  # output$sus = renderInfoBox(
  #   
  #   infoBox(
  #     title = tags$p("SUSPEITOS", style="font-size:85%;"),
  #     value = tags$p(sum(corona_2()$suspeitos),style="font-size:150%;"),
  #     subtitle = tags$p(paste(corona_2()$suspeitos[nrow(corona_2())]),ifelse(input$prd=="Diario",
  #                                                                                          " casos nas últimas 24h",ifelse(input$prd=="Semanal","casos na última semana",
  #                                                                                                                          "casos no último mês")), style="position:absolute; bottom:5px;") ,      icon = icon("user-md"),
  #     color = "aqua"
  #   )
  # )
  output$conf = renderInfoBox(
    
    infoBox(
      title = tags$p("CONFIRMADOS", style="font-size:85%;"),
      value = tags$p(sum(corona_2()$confirmed),style="font-size:150%;"),
      subtitle =  tags$p(paste(corona_2()$confirmed[nrow(corona_2())]),ifelse(input$prd=="Diario",
                                                                                            " últimas 24h",ifelse(input$prd=="Semanal","casos na última semana",
                                                                                                                            "casos no último mês")), style="position:absolute; bottom:1px;"),
      icon = icon("users"),
      color = "blue"
    )
  )
  output$act = renderInfoBox(
    
    infoBox(
      title = tags$p("ACTIVOS", style="font-size:85%;"),
      value = tags$p(sum(corona_2()$Active),style="font-size:150%;"),
      subtitle =  tags$p(paste(corona_2()$Active[nrow(corona_2())]),ifelse(input$prd=="Diario",
                                                                                         " últimas 24h",ifelse(input$prd=="Semanal","casos na última semana",
                                                                                                                         "casos no último mês")), style="position:absolute; bottom:1px;"),
      

      icon = icon("hospital"),
      color = "purple"
    )
  )
  output$rec = renderInfoBox(
    
    infoBox(
      title = tags$p("RECUPERADOS", style="font-size:85%;"),
      value = tags$p(sum(corona_2()$recovered),style="font-size:140%;"),
      subtitle =  tags$p(paste(corona_2()$recovered[nrow(corona_2())]),ifelse(input$prd=="Diario",
                                                                                            " últimas 24h",ifelse(input$prd=="Semanal","casos na última semana",
                                                                                                                            "casos no último mês")), style="position:absolute; bottom:1px;"),
      icon = icon("smile"),
      color = "green"
    )
  )
  output$obt = renderInfoBox(
    
    infoBox(
      
      title = tags$p("ÓBITOS", style="font-size:85%;"),
      value = tags$p(sum(corona_2()$death),style="font-size:150%;"),
      subtitle =  tags$p(paste(corona_2()$death[nrow(corona_2())]),ifelse(input$prd=="Diario",
                                                                                        " nas últimas 24h",ifelse(input$prd=="Semanal","casos na última semana",
                                                                                                                        "casos no último mês")), style="position:absolute; bottom:1px;"),
      icon = icon("heartbeat"),
      color = "red"
    )
  )
  

  output$txt <- renderText(
   paste("última atualização:",format(max(aux$date),format="%d/%m/%Y"))
  )
  
  output$mapa1 <- renderLeaflet({

    pal_sus <- colorNumeric(glasbey()[c(3,2)], domain = c(0,max(dados$suspeitos)))
    # 
    # pal_pos <- colorBin(palette=c("red","orange","green","blue"), dados$confirmed, bins=c(0,25,50,100,1000))
    # 
    # pal_mor <- colorBin(palette=c("red","orange","green","blue"), dados$death, bins=c(0,25,50,100,1000))
    # 
    # pal_rec <- colorBin(palette=c("red","orange","green","blue"), dados$recovered, bins=c(0,25,50,100,1000))
    # 
    # pal_act <- colorBin(palette=c("red","orange","green","blue"), dados$active, bins=c(0,25,50,100,1000))
    
    leaflet(stp_shp,options = leafletOptions(minZoom =8))%>%
      addTiles()%>%
      
      addPolygons(
        fillColor = ~pal_sus(dados$confirmed),
        color = "black",
        dashArray = 3,
        fillOpacity = 0.5,
        weight = 1,
        highlightOptions = highlightOptions(color = "black",
                                            weight = 2,
                                            bringToFront = T),
        label = ~htmlEscape(paste("nº de casos:",dados$confirmed)),
        group = "Confirmados"
      )%>%
      
      addPolygons(
        fillColor = ~pal_sus(dados$death),
        color = "black",
        dashArray = 3,
        fillOpacity = 0.5,
        weight = 1,
        highlightOptions = highlightOptions(color = "black",
                                            weight = 2,
                                            bringToFront = T),
        label = ~htmlEscape(paste("nº de casos:",dados$death)),
        group = "Óbitos"
      )%>%
      addPolygons(
        fillColor = ~pal_sus(dados$recovered),
        color = "black",
        dashArray = 3,
        fillOpacity = 0.5,
        weight = 1,
        highlightOptions = highlightOptions(color = "black",
                                            weight = 2,
                                            bringToFront = T),
        label = ~htmlEscape(paste("nº de casos:",dados$recovered)),
        group = "Recuperados"
      )%>%
      addPolygons(
        fillColor = ~pal_sus(dados$active),
        color = "black",
        dashArray = 3,
        fillOpacity = 0.5,
        weight = 1,
        highlightOptions = highlightOptions(color = "black",
                                            weight = 2,
                                            bringToFront = T),
        label = ~htmlEscape(paste("nº de casos:",dados$active)),
        group = "Ativos"
      )%>%
      
        addLayersControl(
        baseGroups = c("Confirmados","Ativos","Óbitos","Recuperados"),
        options = layersControlOptions(collapsed = T)
      )
  })
  

  output$mapa <- renderLeaflet({
    
    stp_shp@data <- dados[match(stp_shp@data$ADM1_CODE,dados$code),]
    pal <- colorBin("Reds",stp_shp$positive, bins = c(0,100,200,500,1000,2000))
    
    leaflet(stp_shp,options = leafletOptions(minZoom =8))%>%
      addTiles()%>%
      # addProviderTiles(providers$Stamen.TonerLite,
      #                  options = providerTileOptions(noWrap = T))%>%
      addPolygons(
        fillColor = ~pal(stp_shp$positive),
        color = "black",
        dashArray = 3,
        fillOpacity = 0.5,
        weight = 1,
        highlightOptions = highlightOptions(color = "black",
                                            weight = 2,
                                            bringToFront = T),
        label = ~htmlEscape(paste("nº de casos:",stp_shp$positive))
      )%>%
      addLegend(
        position = "bottomright",
        pal=pal,
        values = ~stp_shp$positive,
        title = "Casos confirmados"
        
      )
    
  })
  
  observe({

     if(input$country==levels(aux$countryName)[1])(
      leafletProxy("mapa1") %>%
    setView(lng =  7.4400, lat = 1.6130, zoom = 12)
    )
    else if(input$country==levels(aux$countryName)[2])(
      leafletProxy("mapa1") %>%
        setView(lng =  6.7308, lat = 0.1970, zoom = 10)
      )    
    else(
      leafletProxy("mapa1") %>%
        setView(lng =  7.6000, lat = 0.6352, zoom = 8)
    )  
  })
  
  
 
  
  
  output$timeseries <- renderHighchart({
    highchart() %>%
      
   #  hc_chart(type = "areaspline") %>%
      
      hc_title(text = ifelse(input$country=="São Tomé e Principe","Evolução no tempo",
                             paste("Evolução no tempo -",input$country,sep = " "))
               ,   style = list(color = "#2b858f", useHTML = TRUE))%>%

      hc_xAxis(
        categories = format(corona_2()$date, format="%d-%m"))       %>%
      
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
        name = "Óbitos",
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
      select(Data=date,Suspeitos=suspeitos,Confirmados=confirmed,Recuperados=recovered,Mortes=death,Activos=active)
    
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
        backgroundSize = '100% 85%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Recuperados (%)',
        background = styleColorBar(tbl_all()$'Recuperados (%)', '#8bd431'),
        backgroundSize = '100% 85%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Mortes (%)',
        background = styleColorBar(tbl_all()$'Mortes (%)', '#ff5757'),
        backgroundSize = '100% 85%',
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
        style = list(color = "#2b858f", useHTML = TRUE)
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
                    name = "Óbitos  "
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
        style = list(color = "#2b858f", useHTML = T)
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
                    name = "Taxa de Óbitos(%)"
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
        text = ifelse(input$country=="São Tomé e Principe","Evolução de casos novos no tempo",
                      paste("Evolução de casos novos no tempo-",input$country)),
        style = list(color = "#2b858f", useHTML = TRUE)
      ) %>%
      
      hc_chart(type = "column") %>%
      
      hc_xAxis(
        categories = format(corona_2()$date,format="%d-%m")) %>%
      
      hc_plotOptions(series = list(showInLegend = TRUE)) %>%
      
      hc_add_series(data =round(corona_2()$confirmed,2) ,
                    name = "Confirmados"
                    ,  dataLabels = list(enabled = F)) %>%
      hc_add_series(data =round(corona_2()$recovered,2),
                    name = "Recuperados"
                    , dataLabels = list(enabled = F)) %>%
      hc_add_series(data = round(corona_2()$death,2),
                    name = "Óbitos"
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
        style = list(color = "#2b858f", useHTML = TRUE)
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
                    name = "Novos Óbitos"
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
  
  
  
 