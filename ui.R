
tags$style(type="text/css", "
/*    Move everything below the header */
    .content-wrapper {
        margin-top: 50px;
    }
    .content {
        padding-top: 60px;
    }
/*    Format the title/subtitle text */
    .title-box {
        position: absolute;
        text-align: center;
        top: 50%;
        left: 50%;
        transform:translate(-50%, -50%);
    }
    @media (max-width: 590px) {
        .title-box {
            position: absolute;
            text-align: center;
            top: 10%;
            left: 10%;
            transform:translate(-5%, -5%);
        }
    }
    @media (max-width: 767px) {
        .primary-title {
            font-size: 1.1em;
        }
        .primary-subtitle {
            font-size: 1em;
        }
    }
/*    Make the image taller */
    .main-header .logo {
        height: 125px;
    }
/*    Override the default media-specific settings */
    @media (max-width: 5000px) {
        .main-header {
            padding: 0 0;
            position: relative;
        }
        .main-header .logo,
        .main-header .navbar {
            width: 100%;
            float: none;
        }
        .main-header .navbar {
            margin: 0;
        }
        .main-header .navbar-custom-menu {
            float: right;
        }
    }
/*    Move the sidebar down */
    .main-sidebar {
        position: absolute;
    }
    .left-side, .main-sidebar {
        padding-top: 175px;
    }"
)

source("packageLoad.R")
source("dataR/loadData1.R")


ui <- dashboardPage(
  
  dashboardHeader(title = "COVID19 | Portal"),
    
  dashboardSidebar(
 
    tags$img(src = "stp.jpg", width="230"),
    
    sidebarMenu(

      pickerInput(
        inputId = "country",
        label = strong("Ilhas"), 
        choices = c("São Tomé e Principe",levels(aux$countryName)),
        selected = "São Tomé e Principe",
        options = list(`live-search`=T,
                       title = "Selecione a Provincia")
      ),
      
      dateRangeInput(
        "date_range",
        "Selecione o intervalo de tempo",
        start = "2019-01-01",
        end = Sys.Date(),
        separator = "até",
        language = "pt"
      ),
      
      radioGroupButtons(
        inputId = "prd",
        label = "Periodo",
        choices = c("Diário"="Diario", "Semanal", "Mensal"),
        status = "primary",
        selected = "Diario",
        direction = "horizontal",
        size = "sm"
      )
      
      ,
      fluidRow(align = "center",
               submitButton(
                 "Atualizar!",
                 icon = icon("fas fa-sync-alt"),
                 width = "60%"
               )),

      tags$br(),
      fluidRow(
        column(offset=1,
          width=8,
          textOutput("txt")
                  )
       
        
      ),
      
      tags$br(),
      tags$br(),
      fluidRow(
        column(offset=1,
               width = 10,
      tags$h5("Fonte de dados:"))),
      fluidRow(
        column(offset=1,
               width = 10,
               tags$a(href="https://sisd.org.mz/covidstp",
                      HTML("<u>Sistema de Informação Nacional <br/> de Vigilância Epidemiológica </u>")))
      )
      ,
      tags$br(),
      tags$br(),
      tags$br(),
      tags$h5("Parceiros:", style="margin-left:15px;"),
      fluidRow(
        
        column(offset=1, width = 5,
               tags$img(src = "sdgts.png", width="90"))
       , 
       column(offset=1,width = 5,
              tags$img(src = "oslo.png", width="68")
       ),
       column(offset=1,width = 5,
              tags$img(src = "PNUD.png", width="60", style="margin-left:15px; margin-top:10px;")
       )
        
      )
      
    )
  
  ),
  
  dashboardBody(
    tags$head(includeHTML(("GA.html"))),

    setShadow(class = "dropdown-menu"),
    
    fluidRow(

       infoBoxOutput("conf", width = 3),
       infoBoxOutput("act", width = 3),
       infoBoxOutput("obt", width = 3),
       infoBoxOutput("rec", width = 3)
       

    ),
    fluidRow(
      box(
        width = 12,
        status = "primary",
        leafletOutput("mapa1")
      )
    ),
    
    
    fluidRow(
      box(
        width = 12,
        status = "primary",
        highchartOutput("timeseries")
      )
    ),
        fluidRow(
          box(
            width = 12,
            status = "primary",
            highchartOutput("rates")
            
          )),
    fluidRow(
      box(
        width = 12,
        status = "primary",
        highchartOutput("ratesNew")  
      )
      
    )
          
        )
      )
    

