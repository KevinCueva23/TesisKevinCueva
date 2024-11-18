#PROYECTO DE INVESTIGACIÓN
#Por: Kevin Cueva
#Tutor: Ing. Fidel Vallejo Gallardo, Ph.D.

#Cargar librerías
library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(openair)
library(leaflet)
library(gridExtra)
library(corrplot)
library(shinymanager)

# Definir usuarios para la autenticación
credentials <- data.frame(
  user = c("admin", "user"),
  password = c("admin", "user123"),
  stringsAsFactors = FALSE
)

# Cargar los datos
file_path <- "Data_Quito_Zonas.xlsx"
sheet_names <- excel_sheets(file_path)

data_list <- lapply(sheet_names, function(sheet) {
  df <- read_excel(file_path, sheet = sheet) %>% 
    filter_all(any_vars(!is.na(.))) # Eliminar nulos
  df$date <- as.POSIXct(df$Fecha, format="%Y-%m-%d %H:%M:%S")
  df$year <- year(df$date)
  df$parroquia <- sheet
  return(df)
})

names(data_list) <- sheet_names

# Lista de coordenadas
coords <- data.frame(
  parroquia = sheet_names,
  lat = c(-0.21018967079232298, -0.0941631913233585, -0.21832583248105075, -0.28562187961221214, -0.10540754721213547, -0.12362342615905705, -0.2498508781175223, -0.3276388278339068, -0.15889892779898834, -0.2665140121012801, -0.29186521267391957, -0.21088541467023406, -0.28024998690802544),
  lon = c(-78.4428128135124, -78.4508210751852, -78.51334865638042, -78.56433896894426, -78.51307533949115, -78.50452321914054, -78.51951562015202, -78.56759102092217, -78.46782686636986, -78.48334895215864, -78.5668932410136, -78.39665643866752, -78.54093890391562)
)

# Estándares de la OMS
classification_oms <- data.frame(
  variable = c("CO", "NO2", "O3", "SO2", "PM2.5", "PM10"),
  good = c(1, 20, 50, 20, 10, 20),
  moderate = c(2, 40, 100, 50, 25, 50),
  bad = c(10, 200, 240, 500, 75, 150),
  dangerous = c(Inf, Inf, Inf, Inf, Inf, Inf),
  category = c("Buena", "Moderada", "Mala", "Peligrosa", "Buena", "Moderada")
)

# Rangos de Clasificación Meteorológica
classification_meteorology <- data.frame(
  variable = c("HUM", "IUV", "LLU", "PRE", "RS", "TMP", "VEL"),
  low = c(0, 0, 0, -Inf, 0, -Inf, 0),
  moderate = c(30, 2, 10, 980, 200, -10, 1.5),
  high = c(60, 5, 20, 1030, 600, 0, 3.3),
  very_high = c(Inf, 7, 50, Inf, 1000, 20, 10.8),
  extreme = c(Inf, 11, Inf, Inf, Inf, 30, Inf),
  category = c("Baja", "Moderada", "Alta", "Normal", "Moderada", "Alta", "Baja")
)

# Interfaz de usuario
ui <- secure_app(
    dashboardPage(
  dashboardHeader(title = "CMDAQ (Cuadro de Mando de Datos Ambientales de Quito)"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "home", icon = icon("home")),
      menuItem("Datos", tabName = "data", icon = icon("database")),
      menuItem("Análisis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Mapa", tabName = "map", icon = icon("map")),
      menuItem("Resumen", tabName = "summary", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper {
          background-color: #f4f6f9;
        }
        .box-header {
          background-color: #3c8dbc;
          color: white;
        }
        .box {
          border-radius: 10px;
          margin-bottom: 20px;
        }
        .box-body {
          padding: 15px;
        }
        .form-control {
          border-radius: 5px;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(title = "Bienvenida", status = "primary", solidHeader = TRUE, 
                    p("Estimado usuario,"),
                    p("Le damos la bienvenida al Cuadro de Mando de Datos Ambientales de Quito (CMDAQ), una herramienta integral diseñada para proporcionar una visión detallada y completa de las condiciones meteorológicas y de calidad del aire en nuestra ciudad. Este dashboard interactivo le permitirá explorar datos históricos y actuales, facilitando el análisis multidimensional de las variables ambientales que afectan nuestra calidad de vida."),
                    p("En este panel, podrá acceder a información crucial sobre:"),
                    tags$ul(
                      tags$li("Monóxido de Carbono (CO)"),
                      tags$li("Dióxido de Nitrógeno (NO2)"),
                      tags$li("Ozono (O3)"),
                      tags$li("Partículas PM2.5 y PM10"),
                      tags$li("Dióxido de Azufre (SO2)"),
                      tags$li("Dirección del Viento (DIR)"),
                      tags$li("Humedad Relativa (HUM)"),
                      tags$li("Radiación Ultravioleta (IUV)"),
                      tags$li("Precipitación (LLU)"),
                      tags$li("Presión Barométrica (PRE)"),
                      tags$li("Radiación Solar (RS)"),
                      tags$li("Temperatura Media (TMP)"),
                      tags$li("Velocidad del Viento (VEL)")
                    ),
                    p("Este cuadro de mando ha sido desarrollado para apoyar la toma de decisiones informadas y para proporcionar una comprensión profunda de las condiciones ambientales en distintas zonas de Quito. Navegue a través de las distintas secciones y aproveche las funcionalidades interactivas para obtener análisis detallados y gráficos informativos."),
                    p("Esperamos que esta herramienta le resulte útil y le proporcione información valiosa para sus investigaciones y toma de decisiones."),
                    p("¡Gracias por utilizar nuestro panel!")
                )
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Selección de Datos", status = "primary", solidHeader = TRUE,
                    selectInput("parroquia", "Seleccionar Parroquia", choices = sheet_names),
                    dateRangeInput("dateRange", "Seleccionar Rango de Fechas:",
                                   start = min(data_list[[1]]$date), end = max(data_list[[1]]$date)),
                    uiOutput("variableSelector"),
                    actionButton("plotButton", "Generar Gráficas")
                )
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                tabBox(title = "Análisis", id = "tabset1", height = "600px",
                       tabPanel("Series Temporales", plotOutput("timeSeriesPlot")),
                       tabPanel("Distribución", plotOutput("distributionPlot")),
                       tabPanel("Calendar Plot", plotOutput("calendarPlot")),
                       tabPanel("Time Plot", plotOutput("timePlot")),
                       tabPanel("Time Variation", plotOutput("timeVariation")),
                       tabPanel("Comparación OMS", plotOutput("omsComparisonPlot")),
                       tabPanel("Correlaciones", plotOutput("correlationPlot")),
                       tabPanel("BoxPlot", plotOutput("boxPlot")),
                       tabPanel("Summary Plot", plotOutput("summaryPlot")),
                       tabPanel("Smooth Trend", plotOutput("smoothTrendPlot")),
                       tabPanel("Theil-Sen Trends", plotOutput("theilSenPlot")),
                       tabPanel("Wind Roses", plotOutput("windRosePlot"))
                )
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Mapa Espacial", status = "primary", solidHeader = TRUE,
                    leafletOutput("mapPlot")
                )
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Resumen de Datos", status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("summaryOutput")
                )
              )
      )
    )
  )
))

# Servidor
server <- function(input, output, session) {
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  observe({
    if (!is.null(input$parroquia) && !is.null(data_list[[input$parroquia]])) {
      df <- data_list[[input$parroquia]]
      if (nrow(df) > 0) {
        start_date <- min(df$date)
        end_date <- max(df$date)
        updateDateRangeInput(session, "dateRange", start = start_date, end = end_date)
      }
    }
  })
  
  output$variableSelector <- renderUI({
    variables <- names(data_list[[input$parroquia]])
    variables <- variables[!(variables %in% c("date", "year", "parroquia", "Fecha"))]  # Excluir columnas no deseadas
    selectInput("variables", "Seleccionar Variables", choices = variables, multiple = TRUE)
  })
  
  selected_data <- reactive({
    data_list[[input$parroquia]] %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  })
  
  output$timeSeriesPlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      p <- list()
      for (pollutant in pollutants) {
        p[[pollutant]] <- ggplot(data, aes_string(x = "date", y = pollutant)) +
          geom_line() +
          labs(title = paste("Serie Temporal de", pollutant, "en", input$parroquia), x = "Fecha", y = pollutant)
      }
      gridExtra::grid.arrange(grobs = p)
    })
  })
  
  output$distributionPlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      p <- list()
      for (pollutant in pollutants) {
        p[[pollutant]] <- ggplot(data, aes_string(x = pollutant)) +
          geom_histogram(binwidth = 1, fill = "blue", color = "black") +
          labs(title = paste("Distribución de", pollutant, "en", input$parroquia), x = pollutant, y = "Frecuencia")
      }
      gridExtra::grid.arrange(grobs = p)
    })
  })
  
  output$calendarPlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      
      if (nrow(data) == 0) {
        return(NULL)
      }
      
      for (pollutant in pollutants) {
        if (pollutant %in% c("NO2", "SO2", "O3", "CO", "PM2.5", "PM10")) {
          tryCatch({
            classification <- classification_oms %>% filter(variable == pollutant)
            data$category <- cut(
              data[[pollutant]],
              breaks = c(-Inf, classification$good, classification$moderate, classification$bad, classification$dangerous),
              labels = c("Buena", "Moderada", "Mala", "Peligrosa")
            )
            
            openair::calendarPlot(data, pollutant = pollutant, 
                                  main = paste("Calendar Plot de", pollutant),
                                  color = data$category)
          }, error = function(e) {
            print(paste("Error en calendarPlot para", pollutant, ":", e$message))
          })
        }
      }
    })
  })
  
  output$timePlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      openair::timePlot(data, pollutant = pollutants, avg.time = "month")
    })
  })
  
  output$timeVariation <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      
      if (length(pollutants) == 0) {
        plot.new()
        text(0.5, 0.5, "Selecciona al menos un contaminante para la variación temporal", cex = 1.5)
        return(NULL)
      }
      
      openair::timeVariation(data, pollutant = pollutants, period = "monthly")
    })
  })
  
  output$omsComparisonPlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      
      if (nrow(data) == 0) {
        return(NULL)
      }
      
      p <- list()
      for (pollutant in pollutants) {
        if (pollutant %in% c("NO2", "SO2", "O3", "CO", "PM2.5", "PM10")) {
          classification <- classification_oms %>% filter(variable == pollutant)
          data$category <- cut(
            data[[pollutant]],
            breaks = c(-Inf, classification$good, classification$moderate, classification$bad, classification$dangerous),
            labels = c("Buena", "Moderada", "Mala", "Peligrosa")
          )
          
          p[[pollutant]] <- ggplot(data, aes_string(x = "date", y = pollutant, color = "category")) +
            geom_line() +
            labs(title = paste("Comparación OMS de", pollutant, "en", input$parroquia), x = "Fecha", y = pollutant) +
            scale_color_manual(values = c("Buena" = "green", "Moderada" = "yellow", "Mala" = "orange", "Peligrosa" = "red"))
        }
      }
      gridExtra::grid.arrange(grobs = p)
    })
  })
  
  output$correlationPlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      
      if (length(pollutants) < 2) {
        plot.new()
        text(0.5, 0.5, "Selecciona al menos dos variables para calcular la correlación", cex = 1.5)
        return(NULL)
      }
      
      corr_data <- data %>%
        dplyr::select(all_of(pollutants)) %>%
        cor(use = "complete.obs")
      
      corrplot::corrplot(corr_data, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
    })
  })
  
  output$boxPlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      
      if (length(pollutants) == 0) {
        plot.new()
        text(0.5, 0.5, "Selecciona al menos una variable para el BoxPlot", cex = 1.5)
        return(NULL)
      }
      
      p <- list()
      for (pollutant in pollutants) {
        p[[pollutant]] <- ggplot(data, aes_string(x = pollutant)) +
          geom_boxplot() +
          labs(title = paste("BoxPlot de", pollutant, "en", input$parroquia), x = pollutant, y = "Valor")
      }
      gridExtra::grid.arrange(grobs = p)
    })
  })
  
  output$summaryPlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      p <- list()
      for (pollutant in pollutants) {
        p[[pollutant]] <- ggplot(data, aes_string(x = "date", y = pollutant)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "loess") +
          labs(title = paste("Summary Plot de", pollutant, "en", input$parroquia), x = "Fecha", y = pollutant)
      }
      gridExtra::grid.arrange(grobs = p)
    })
  })
  
  output$smoothTrendPlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      
      if (length(pollutants) == 0) {
        plot.new()
        text(0.5, 0.5, "Selecciona al menos una variable para el Smooth Trend", cex = 1.5)
        return(NULL)
      }
      
      p <- list()
      for (pollutant in pollutants) {
        p[[pollutant]] <- ggplot(data, aes_string(x = "date", y = pollutant)) +
          geom_smooth() +
          labs(title = paste("Tendencia Suavizada de", pollutant, "en", input$parroquia), x = "Fecha", y = pollutant)
      }
      gridExtra::grid.arrange(grobs = p)
    })
  })
  
  output$theilSenPlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      pollutants <- input$variables
      
      if (length(pollutants) == 0) {
        plot.new()
        text(0.5, 0.5, "Selecciona al menos una variable para Theil-Sen Trends", cex = 1.5)
        return(NULL)
      }
      
      # Usar la primera variable seleccionada para la tendencia
      pollutant <- pollutants[1]
      
      openair::TheilSen(
        data,
        pollutant = pollutant,
        ylab = paste(pollutant, "(ppb)"),
        deseason = TRUE,
        date.format = "%Y"
      )
    })
  })
  
  output$windRosePlot <- renderPlot({
    input$plotButton
    isolate({
      data <- selected_data()
      
      if (nrow(data) == 0) {
        plot.new()
        text(0.5, 0.5, "No hay datos para graficar", cex = 1.5)
        return(NULL)
      }
      
      if (!all(c("VEL", "DIR") %in% names(data))) {
        plot.new()
        text(0.5, 0.5, "Faltan variables 'VEL' o 'DIR' para el Wind Rose", cex = 1.5)
        return(NULL)
      }
      
      openair::windRose(data, ws = "VEL", wd = "DIR", type = "year", layout = c(4, 2))
    })
  })
  
  output$mapPlot <- renderLeaflet({
    leaflet(data = coords) %>%
      addTiles() %>%
      addMarkers(lng = ~lon, lat = ~lat, popup = ~parroquia)
  })
  
  output$summaryOutput <- renderPrint({
    data <- selected_data()
    summary(data)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
