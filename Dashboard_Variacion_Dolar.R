library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)
library(shinyWidgets)
library(plotly)
library(lubridate)
library(tidyr)

# Carga de datos
ruta <- "definir ruta de carpeta donde estan todas las bases de datos, dashboard.r y reporte.rmd"
#ruta<-"C:/Users/angel/Desktop/Base de datos Análisis de la Relación entre Inflación, Desempleo, Variación del Dólar y Crecimiento Económico en Chile/"
#ruta<-"C:/Users/pipen/Documents/4to IES/Bussiness Inteligence/Unidad 2/Dashboard"
setwd(ruta)

Agregado_Monetario_2010_2024_Mensual <- read_delim("Agregado_Monetario_2010_2024_Mensual.csv", 
                                                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                                   trim_ws = TRUE)
Expectativa_IPC_2010_2024_Mensual <- read_csv("Expectativa_IPC_2010_2024_Mensual.csv")
IMACEC_2010_2024_Mensual <- read_csv("IMACEC_2010_2024_Mensual.csv")
Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual <- read_csv("Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual.csv")
PIB_2010_2024_Anual <- read_csv("PIB_2010_2024_Anual.csv")
Poblacion_Total_Estimaciones_2010_2024_Anual <- read_csv("Población_Total_Estimaciones_2010_2024_Anual.csv")
Real_IPC_2010_2024_Mensual <- read_delim("Real_IPC_2010_2024_Mensual.csv", 
                                         delim = ";", escape_double = FALSE, 
                                         locale = locale(decimal_mark = ","), trim_ws = TRUE)
Resumen_Residencias_Definitivas_2010_2024 <- read_csv("Resumen_Residencias_Definitivas_2010_2024.csv")
Sueldo_Minimo_Nominal_2010_2024_Anual <- read_csv("Sueldo_Minimo_Nominal_2010_2024_Anual.csv")
Tasa_Desempleo_2010_2024_Mensual <- read_csv("Tasa_Desempleo_2010_2024_Mensual.csv")
Tasa_Interes_2010_2024_Mensual <- read_csv("Tasa_Interes_2010_2024_Mensual.csv")
USD_CLP_2010_2024_Mensual <- read_delim("USD_CLP_2010_2024_Mensual.csv", 
                                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                        trim_ws = TRUE)


Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual <- Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual %>%
  mutate(Anho = lubridate::year(as.Date(Anho)))
Resumen_Residencias_Definitivas_2010_2024 <- Resumen_Residencias_Definitivas_2010_2024 %>%
  rename(Anho = AÑO)

Agregado_Monetario_2010_2024_Mensual <- Agregado_Monetario_2010_2024_Mensual %>%
  rename(Anho = Año)
Poblacion_Total_Estimaciones_2010_2024_Anual <- Poblacion_Total_Estimaciones_2010_2024_Anual %>%
  mutate(Anho = lubridate::year(as.Date(Anho)))
PIB_2010_2024_Anual <- PIB_2010_2024_Anual %>%
  mutate(Anho = lubridate::year(as.Date(Anho)))


# UI

ui <- dashboardPage(
  dashboardHeader(
    title = HTML("<span style='font-size: 23px;'>Análisis Exploratorio<br>Económico en <br> Chile</span>"),
    tags$li(class = "dropdown",
            sliderTextInput(
              inputId = "anho_rango",
              label = NULL,  # No label para ahorrar espacio en el topbar
              choices = 2010:2024,
              selected = c(2010, 2024),
              grid = TRUE,
              animate = TRUE,
              width = "250px"
            )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("chart-line")),
      menuItem("Indicadores Macroeconómicos", tabName = "indicadores_macroeconomicos", icon = icon("tachometer-alt")),
      menuItem("Finanzas Públicas", tabName = "finanzas_publicas", icon = icon("landmark")),
      menuItem("Mercado Laboral y Demografía", tabName = "mercado_laboral_demografia", icon = icon("users")),
      menuItem("Sector Externo", tabName = "sector_externo", icon = icon("globe")),
      tags$div(
        style = "text-align: center; padding: 10px;",
        downloadButton("descargar_reporte", "Descargar Informe HTML", class = "btn btn-primary")
      ),
      
      # Botón de información de contacto justo debajo
      tags$div(
        style = "padding: 10px;",
        dropdownButton(
          label = "Información de Contacto",
          icon = icon("info-circle"),
          status = "info",
          circle = FALSE,
          width = "300px",
          tooltip = tooltipOptions(title = "Haz clic para información de contacto"),
          right = FALSE,
          
          # Contenido desplegable
          tags$div(
            style = "color: black;",
            tags$strong("👥 Integrantes del Proyecto"),
            tags$hr(),
            tags$p("👤 Angel T. Llanos Herrera"),
            tags$p("🎓 Estudiante de Ing. en Estadística"),
            tags$p("📧 angel.llanos@alu.ucm.cl"),
            tags$br(),
            tags$p("👤 Felipe S. Neira Rojas"),
            tags$p("🎓 Estudiante de Ing. en Estadística"),
            tags$p("📧 felipe.neira@alu.ucm.cl"),
            tags$hr(),
            tags$strong("📚 Curso: Business Intelligence [IES-414]"),
            tags$p("👨‍🏫 Docente: José M. Zuñiga Núñez")
          )
        ),
        tags$div(
          style = "text-align: center; padding-top: 5px; padding-bottom: 5px;", # Ajusta el padding
          actionButton(
            inputId = "infoButton", # ID para referenciarlo en el servidor
            label = "", # Sin texto, solo el icono
            icon = icon("info-circle"), # Icono de información
            class = "btn-default btn-xs", # Botón extra pequeño
            tooltip = tooltipOptions(title = "Haz clic para más información sobre el proyecto y los datos") # Tooltip al pasar el mouse
          )
        )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      /* Ajuste de altura del logo (título) */
      .main-header .logo {
        height: 80px !important;
        line-height: 20px !important;
        padding: 10px 0px 5px 15px;
        white-space: normal !important;
        font-size: 16px;
      }

      /* Ajuste de altura del navbar */
      .main-header .navbar {
        height: 80px !important;
        min-height: 80px !important;
      }

      /* Evita que el sidebar se suba detrás del header */
      .main-sidebar {
        margin-top: 80px !important;
      }
    "))
    ),
    tabItems(
      tabItem(
        tabName = "resumen",
        fluidRow(
          valueBoxOutput("box_m1"),
          valueBoxOutput("box_tpm"),
          valueBoxOutput("box_sueldo")
        ),
        fluidRow(
          valueBoxOutput("box_ipc"),
          valueBoxOutput("box_desempleo"),
          valueBoxOutput("box_dolar")
        ),
        fluidRow(
          box(
            title = "Comparación Mensual: Variación % del Dólar vs. IPC vs. TPM",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotlyOutput("grafico_variacion_dolar_ipc", height = "400px")
          )
        ),
        fluidRow(
          box(
            
            title= "Relación entre la Actividad y Estudios de Migrantes y la Tasa de desempleo",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            selectInput(
              inputId = "filtro_categoria",
              label = "Agrupar por:",
              choices = c("Estudios", "Actividad"),
              selected = "Estudios"
            ),
            
            
            plotlyOutput("grafico_apilado")
          )
        )
        
        
      ),
      
      tabItem(tabName = "indicadores_macroeconomicos",
              fluidPage(
                fluidRow(
                  box(
                    title = "Heatmap de Correlaciones para Indicadores Anuales",
                    width = 6, status = "info", solidHeader = TRUE, collapsible = TRUE,
                    plotlyOutput("heatmap_correlacion", height = "500px")
                  ),
                  box(
                    title = "Heatmap de Índices de Significancia para Coef. de Correlación",
                    width = 6, collapsible = TRUE,
                    status = "warning",
                    solidHeader = TRUE,
                    plotlyOutput("heatmap_pvalores", height = "500px")
                  )
                ),
                fluidRow(
                  box(
                    title = "Selecciona Variables para Comparar",
                    status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                    column(6,
                           selectInput(
                             inputId  = "x_var",
                             label    = "Eje X:",
                             choices  = c(
                               "IMACEC"             = "IMACEC",
                               "PIB"                = "PIB",
                               "TPM"                = "TPM",
                               "Expectativa IPC"    = "Expectativa_IPC",
                               "Variación IPC real" = "Real_IPC",
                               "M0"                 = "M0",
                               "M1"                 = "M1",
                               "M2"                 = "M2"
                             ),
                             selected = "IMACEC"
                           )
                    ),
                    column(6,
                           selectInput(
                             inputId  = "y_var",
                             label    = "Eje Y:",
                             choices  = c(
                               "IMACEC"             = "IMACEC",
                               "PIB"                = "PIB",
                               "TPM"                = "TPM",
                               "Expectativa IPC"    = "Expectativa_IPC",
                               "Variación IPC real" = "Real_IPC",
                               "M0"                 = "M0",
                               "M1"                 = "M1",
                               "M2"                 = "M2"
                             ),
                             selected = "PIB"
                           )
                    )
                  ),
                  box(
                    title = "Serie de Tiempo Comparativa para Índices Macroeconómicos",
                    status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                    plotlyOutput("dual_axis_time_series", height = "500px")
                  )
                )
              )
      ),
      tabItem(tabName = "finanzas_publicas",
              fluidRow(
                box(
                  
                  title= "Préstamo o Endeudamiento Neto del Gobierno por Año",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  plotlyOutput("plot_endeudamiento", height = "500px")
                ),
                
                box(
                  title = "Tipo de Visualización",
                  status= "primary",
                  width = 12,
                  collapsible = TRUE,
                  radioButtons(
                    inputId = "tipo_visual_tpm",
                    label = "Selecciona el tipo de gráfico:",
                    choices = c("Boxplot", "Serie de Tiempo"),
                    selected = "Boxplot",
                    inline = TRUE
                  )
                ),
                
                box(
                  title = "Análisis Mensual y Anual del TPM",
                  width = 12,
                  collapsible = TRUE,
                  status="primary",
                  plotlyOutput("plot_tpm_dinamico", height = "500px")
                )
                
                
                
                
                
              )
      ),
      tabItem(tabName = "mercado_laboral_demografia",
              fluidPage(
                # Control principal visible siempre
                fluidRow(
                  box(
                    title = "Seleccionar Tipo de Información",
                    width = 12,
                    collapsible = TRUE,
                    radioButtons(
                      inputId = "tipo_info",
                      label = NULL,
                      choices = c("Población", "Migración"),
                      selected = "Población",
                      inline = TRUE
                    )
                  )
                ),
                
                # Gráfico Tasa de Desempleo (siempre visible)
                fluidRow(
                  box(
                    title = "Tasa de Desempleo Anual",
                    width = 12,
                    collapsible = TRUE,
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("plot_desempleo")
                  )
                ),
                
                # Sección solo visible si se elige "Migración"
                conditionalPanel(
                  condition = "input.tipo_info == 'Migración'",
                  fluidRow(
                    column(
                      width = 4,
                      box(
                        title = "Año Seleccionado + Filtro de Visualización",
                        width = 12,
                        COLLAPSIBLE=TRUE,
                        verbatimTextOutput("anho_seleccionado_text"),
                        br(),
                        radioButtons(
                          inputId = "ver_por",
                          label = "Visualizar Por:",
                          choices = c("Total", "Sexo", "Estudios", "Actividad"),
                          selected = "Total"
                        )
                      )
                    ),
                    column(
                      width = 8,
                      box(
                        title = "Distribución de Residencias Definitivas",
                        width = 12,
                        collapsible = TRUE,
                        status = "info",
                        solidHeader = TRUE,
                        plotlyOutput("plot_torta_inmigracion", height = "500px")
                      )
                    )
                  )
                ),
                
                
                
                # Sección Población (condicional)
                conditionalPanel(
                  condition = "input.tipo_info == 'Población'",
                  fluidRow(
                    column(
                      width = 4,
                      box(
                        title = "Visualizar Por Sexo",
                        width = 12,
                        collapsible = TRUE,
                        radioButtons(
                          inputId = "ver_poblacion",
                          label = "Seleccione Visualización:",
                          choices = c("Total", "Sexo"),
                          selected = "Total"
                        )
                      )
                    ),
                    column(
                      width = 8,
                      box(
                        title = "Distribución de Población Total",
                        width = 12,
                        collapsible = TRUE,
                        status = "info",
                        solidHeader = TRUE,
                        plotlyOutput("plot_poblacion", height = "500px")
                      )
                    )
                  )
                )
                
                
                
              )
              
              
      ),
      tabItem(tabName = "sector_externo",
              fluidRow(
                box(
                  title = "Exportaciones y Tipo de Cambio",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = TRUE,
                  
                  # Menú para seleccionar uno o ambos gráficos
                  checkboxGroupInput(
                    inputId = "graficos_sector_externo",
                    label = "Selecciona los gráficos a visualizar:",
                    choices = c("Exportación Cobre Bruto" = "cobre", 
                                "Valor Promedio Anual del Dólar" = "dolar"),
                    selected = c("cobre", "dolar"),
                    inline = TRUE
                  ),
                  
                  # Panel de gráficos renderizados por separado
                  conditionalPanel(
                    condition = "input.graficos_sector_externo.includes('cobre')",
                    plotlyOutput("plot_cobre_bruto", height = "400px")
                  ),
                  conditionalPanel(
                    condition = "input.graficos_sector_externo.includes('dolar')",
                    plotlyOutput("plot_dolar_promedio", height = "400px")
                  )
                )
              )
              
      )
    )
  )
)



# Server
server <- function(input, output) {
  
  
  
  ##################### Resumen ################################
  # Filtra y calcula la variación porcentual del dólar
  Reactive_USD_CLP_2010_2024_Mensual <- reactive({
    USD_CLP_2010_2024_Mensual %>%
      filter(Anho >= input$anho_rango[1],
             Anho <= input$anho_rango[2]) %>%
      arrange(Anho, Mes) %>%
      mutate(Variacion_Dolar = (`Dólar` / lag(`Dólar`) - 1))
  })
  
  # Filtra la variación del IPC (ya está calculada)
  Reactive_Real_IPC_2010_2024_Mensual <- reactive({
    Real_IPC_2010_2024_Mensual %>%
      filter(Anho >= input$anho_rango[1],
             Anho <= input$anho_rango[2]) %>%
      arrange(Anho, Mes)
  })
  
  Reactive_Tasa_Interes_2010_2024_Mensual <- reactive({
    Tasa_Interes_2010_2024_Mensual %>%
      filter(Anho >= input$anho_rango[1],
             Anho <= input$anho_rango[2]) %>%
      arrange(Anho, Mes) %>%
      rename(TPM = `1. Tasa de política monetaria (TPM) (porcentaje)`)
  })
  
  
  
  meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  output$grafico_variacion_dolar_ipc <- renderPlotly({
    df_dolar <- Reactive_USD_CLP_2010_2024_Mensual() %>%
      select(Anho, Mes, Variacion_Dolar)
    
    df_ipc <- Reactive_Real_IPC_2010_2024_Mensual() %>%
      select(Anho, Mes, `Variación IPC`)
    
    df_tpm <- Reactive_Tasa_Interes_2010_2024_Mensual() %>%
      select(Anho, Mes, TPM)
    
    # Unimos las tres bases por Anho y Mes
    df_combined <- df_dolar %>%
      inner_join(df_ipc, by = c("Anho", "Mes")) %>%
      inner_join(df_tpm, by = c("Anho", "Mes")) %>%
      mutate(Fecha = as.Date(paste0(Anho, "-", match(Mes, meses_es), "-01")))
    
    # Gráfico con tres líneas
    plot_ly(df_combined, x = ~Fecha) %>%
      add_lines(y = ~Variacion_Dolar * 100, name = "Variación % Dólar", line = list(color = 'blue')) %>%
      add_lines(y = ~`Variación IPC`, name = "Variación % IPC", line = list(color = 'red')) %>%
      add_lines(y = ~TPM, name = "TPM (%)", line = list(color = 'green')) %>%
      layout(
        title = "Variación Porcentual Mensual: Dólar vs IPC vs TPM",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Porcentaje (%)"),
        legend = list(x = 0.05, y = 0.95)
      )
  })
  
  
  Reactive_Resumen_Residencias_Definitivas_2010_2024 <- reactive({
    Resumen_Residencias_Definitivas_2010_2024
    
  })
  Reactive_Tasa_Desempleo_2010_2024_Mensual <- reactive({
    Tasa_Desempleo_2010_2024_Mensual %>%
      mutate(Anho = as.numeric(Anho)) %>%
      group_by(Anho) %>%
      summarise(Tasa_Desempleo_Prom = mean(`1. Nacional`, na.rm = TRUE)) %>%
      filter(Anho >= input$anho_rango[1], Anho <= input$anho_rango[2])
  })
  
  
  datos_apilados <- reactive({
    df <- Reactive_Resumen_Residencias_Definitivas_2010_2024() %>%
      select(Anho,
             starts_with("ESTUDIOS_"),
             starts_with("ACTIVIDAD_")) %>%
      group_by(Anho) %>%
      summarise(across(everything(), sum, na.rm = TRUE)) %>%
      ungroup()
    
    data_long <- df %>%
      pivot_longer(
        cols = -Anho,
        names_to = "categoria",
        values_to = "personas"
      )
    
    # Filtro según input
    if (input$filtro_categoria == "Estudios") {
      categorias_filtradas <- grep("^ESTUDIOS_", data_long$categoria, value = TRUE)
    } else {
      categorias_filtradas <- grep("^ACTIVIDAD_", data_long$categoria, value = TRUE)
    }
    
    # Unir con la tasa de desempleo anual
    # Asegúrate de que 'Anho' sea numérico en ambos data.frames para la unión
    tasa_desempleo_anual <- Reactive_Tasa_Desempleo_2010_2024_Mensual()
    
    data_long_filtered <- data_long %>%
      filter(categoria %in% categorias_filtradas)
    
    # Unir los datos apilados con la tasa de desempleo anual
    data_long_filtered %>%
      left_join(tasa_desempleo_anual, by = "Anho")
  })
  
  output$grafico_apilado <- renderPlotly({
    data_plot <- datos_apilados()
    
    # Calcular el total de personas por año para posicionar las etiquetas
    data_totals <- data_plot %>%
      group_by(Anho) %>%
      summarise(Total_Personas = sum(personas, na.rm = TRUE),
                Tasa_Desempleo_Prom = first(Tasa_Desempleo_Prom)) %>% # Tomar la tasa de desempleo (es la misma para todas las filas del mismo año)
      ungroup()
    
    p <- plot_ly(data_plot,
                 x = ~Anho,
                 y = ~personas,
                 color = ~categoria,
                 type = 'bar',
                 # Para el hover, puedes mostrar la tasa de desempleo también
                 text = ~paste0("Año: ", Anho, "<br>",
                                "Categoría: ", categoria, "<br>",
                                "Personas: ", personas, "<br>",
                                "Tasa Desempleo: ", round(Tasa_Desempleo_Prom, 2), "%"),
                 hoverinfo = "text") %>%
      layout(
        barmode = "stack",
        title = list(text = paste("Distribución por", input$filtro_categoria), y = 0.95), # Ajusta el título para mejor visibilidad
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1), # Asegura que los años se muestren como enteros si es necesario
        yaxis = list(title = "Total de Personas"),
        legend = list(orientation = "h", x = 0.1, y = -0.3, # Ajusta la posición de la leyenda para evitar superposición
                      font = list(size = 10)), # Ajusta el tamaño de la fuente de la leyenda
        margin = list(b = 100) # Aumenta el margen inferior para la leyenda y posibles etiquetas de texto
      )
    
    # Agregar las anotaciones de la tasa de desempleo
    p %>%
      add_annotations(
        data = data_totals,
        x = ~Anho,
        y = ~Total_Personas, # Posiciona la etiqueta en la parte superior de la pila
        text = ~paste0(round(Tasa_Desempleo_Prom, 1), "%"), # Formatea el texto de la tasa
        xanchor = 'center',
        yanchor = 'bottom', # Asegura que el texto esté justo encima de la barra
        showarrow = FALSE, # No muestra flechas
        textangle = 0, # Ángulo del texto
        font = list(size = 10, color = 'black'), # Tamaño y color de la fuente para las etiquetas
        yshift = 10 # Desplazamiento vertical para que no toque la barra
      )
  })
  
  # Normaliza los meses con mayúscula inicial
  meses_esp <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  
  # M1
  calc_variacion_m1 <- reactive({
    df <- Agregado_Monetario_2010_2024_Mensual
    v2010 <- df %>% filter(Anho == 2010, Mes == "Enero") %>% pull(M1)
    v2024 <- df %>% filter(Anho == 2024, Mes == "Diciembre") %>% pull(M1)
    round((v2024 - v2010) / v2010 * 100, 2)
  })
  
  # TPM
  calc_variacion_tpm <- reactive({
    df <- Tasa_Interes_2010_2024_Mensual
    var <- "1. Tasa de política monetaria (TPM) (porcentaje)"
    v2010 <- df %>% filter(Anho == 2010, Mes == "Enero") %>% pull(var)
    v2024 <- df %>% filter(Anho == 2024, Mes == "Diciembre") %>% pull(var)
    round((v2024 - v2010), 2)
  })
  
  # Desempleo
  calc_variacion_desempleo <- reactive({
    df <- Tasa_Desempleo_2010_2024_Mensual
    var <- "1. Nacional"
    v2010 <- df %>% filter(Anho == 2010, Mes == "Marzo") %>% pull(var)
    v2024 <- df %>% filter(Anho == 2024, Mes == "Diciembre") %>% pull(var)
    round((v2024 - v2010), 2)
  })
  
  # IPC
  calc_ipc_compuesta <- reactive({
    df <- Real_IPC_2010_2024_Mensual %>%
      filter(Anho >= 2010 & Anho <= 2024)
    
    # Convertimos el IPC mensual en multiplicador: 1 + (variación / 100)
    multiplicadores <- 1 + (df$`Variación IPC` / 100)
    
    # Inflación compuesta: producto de todos los multiplicadores
    inflacion_total <- prod(multiplicadores, na.rm = TRUE)
    
    # Variación porcentual compuesta
    variacion_compuesta <- (inflacion_total - 1) * 100
    
    round(variacion_compuesta, 2)
  })
  
  
  output$box_m1 <- renderValueBox({
    valueBox(
      paste0(calc_variacion_m1(), "%"),
      subtitle = "Variación M1",
      icon = icon("money-bill-wave"),
      color = "blue"
    )
  })
  
  output$box_tpm <- renderValueBox({
    valueBox(
      paste0(calc_variacion_tpm(), "%"),
      subtitle = "Variación Tasa de Interés (TPM)",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$box_ipc <- renderValueBox({
    valueBox(
      paste0(calc_ipc_compuesta(), "%"),
      subtitle = "Variación IPC Compuesto",
      icon = icon("percent"),
      color = "green"
    )
  })
  
  output$box_desempleo <- renderValueBox({
    valueBox(
      paste0(calc_variacion_desempleo(), "%"),
      subtitle = "Variación Tasa de Desempleo",
      icon = icon("user-slash"),
      color = "red"
    )
  })
  
  # Variación Sueldo Mínimo Nominal
  Variacion_Sueldo_Minimo <- reactive({
    df <- Sueldo_Minimo_Nominal_2010_2024_Anual
    valor_2010 <- df %>% filter(Anho == 2010) %>% pull(`Monto nominal en pesos`)
    valor_2024 <- df %>% filter(Anho == 2024) %>% pull(`Monto nominal en pesos`)
    variacion <- ((valor_2024 - valor_2010) / valor_2010) * 100
    return(round(variacion, 2))
  })
  
  # Variación del Dólar
  Variacion_Dolar <- reactive({
    df <- USD_CLP_2010_2024_Mensual
    valor_2010 <- df %>% filter(Anho == 2010) %>% arrange(desc(Mes)) %>% slice(1) %>% pull(Dólar)
    valor_2024 <- df %>% filter(Anho == 2024) %>% arrange(desc(Mes)) %>% slice(1) %>% pull(Dólar)
    variacion <- ((valor_2024 - valor_2010) / valor_2010) * 100
    return(round(variacion, 2))
  })
  
  output$box_sueldo <- renderValueBox({
    valueBox(
      paste0(Variacion_Sueldo_Minimo(), "%"),
      "Variación Sueldo Mínimo",
      icon = icon("money-bill-wave"),
      color = "olive"
    )
  })
  
  output$box_dolar <- renderValueBox({
    valueBox(
      paste0(Variacion_Dolar(), "%"),
      "Variación Dólar (USD/CLP)",
      icon = icon("dollar-sign"),
      color = "teal"
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #######################Indicadores macroeconomicos#################################
  Reactive_Macro_Datos <- reactive({
    # 1) IMACEC → variación interanual a partir de promedio anual
    imacec <- IMACEC_2010_2024_Mensual %>%
      group_by(Anho) %>%
      summarise(IMACEC = mean(`1. Imacec`, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Fecha = as.Date(sprintf("%d-01-01", Anho)),
        IMACEC = (IMACEC / lag(IMACEC) - 1) * 100
      ) %>%
      filter(!is.na(IMACEC)) %>%
      select(Fecha, IMACEC)
    
    # 2) PIB anual → variación % interanual
    pib <- PIB_2010_2024_Anual %>%
      filter(Anho >= input$anho_rango[1], Anho <= input$anho_rango[2]) %>%
      mutate(
        Fecha = as.Date(sprintf("%d-01-01", Anho)),
        PIB   = (`1. PIB a precios corrientes` / lag(`1. PIB a precios corrientes`) - 1) * 100
      ) %>%
      select(Fecha, PIB) %>%
      filter(!is.na(PIB))
    
    # 3) TPM → variación interanual a partir del promedio anual
    tpm <- Tasa_Interes_2010_2024_Mensual %>%
      group_by(Anho) %>%
      summarise(TPM = mean(`1. Tasa de política monetaria (TPM) (porcentaje)`, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Fecha = as.Date(sprintf("%d-01-01", Anho)),
        TPM = (TPM / lag(TPM) - 1) * 100
      ) %>%
      filter(!is.na(TPM)) %>%
      select(Fecha, TPM)
    
    # 4) Expectativa IPC → variación interanual del promedio
    expect_ipc <- Expectativa_IPC_2010_2024_Mensual %>%
      group_by(Anho) %>%
      summarise(Expectativa_IPC = mean(Expectativa, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Fecha = as.Date(sprintf("%d-01-01", Anho)),
        Expectativa_IPC = (Expectativa_IPC / lag(Expectativa_IPC) - 1) * 100
      ) %>%
      filter(!is.na(Expectativa_IPC)) %>%
      select(Fecha, Expectativa_IPC)
    
    # 5) Real IPC → ya es variación mensual, pero lo promediamos anual
    real_ipc <- Real_IPC_2010_2024_Mensual %>%
      group_by(Anho) %>%
      summarise(Real_IPC = mean(`Variación IPC`, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Fecha = as.Date(sprintf("%d-01-01", Anho)),
        Real_IPC = (Real_IPC / lag(Real_IPC) - 1) * 100
      ) %>%
      filter(!is.na(Real_IPC)) %>%
      select(Fecha, Real_IPC)
    
    # 6) M0
    M0 <- Agregado_Monetario_2010_2024_Mensual %>%
      group_by(Anho) %>%
      summarise(M0 = mean(M0, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Fecha = as.Date(sprintf("%d-01-01", Anho)),
        M0 = (M0 / lag(M0) - 1) * 100
      ) %>%
      filter(!is.na(M0)) %>%
      select(Fecha, M0)
    
    # 7) M1
    M1 <- Agregado_Monetario_2010_2024_Mensual %>%
      group_by(Anho) %>%
      summarise(M1 = mean(M1, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Fecha = as.Date(sprintf("%d-01-01", Anho)),
        M1 = (M1 / lag(M1) - 1) * 100
      ) %>%
      filter(!is.na(M1)) %>%
      select(Fecha, M1)
    
    # 8) M2
    M2 <- Agregado_Monetario_2010_2024_Mensual %>%
      group_by(Anho) %>%
      summarise(M2 = mean(M2, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Fecha = as.Date(sprintf("%d-01-01", Anho)),
        M2 = (M2 / lag(M2) - 1) * 100
      ) %>%
      filter(!is.na(M2)) %>%
      select(Fecha, M2)
    
    # Unión de todos los dataframes por Fecha
    df <- imacec %>%
      inner_join(pib,         by = "Fecha") %>%
      inner_join(tpm,         by = "Fecha") %>%
      inner_join(expect_ipc,  by = "Fecha") %>%
      inner_join(real_ipc,    by = "Fecha") %>%
      inner_join(M0,          by = "Fecha") %>%
      inner_join(M1,          by = "Fecha") %>%
      inner_join(M2,          by = "Fecha")
    
    df
  })
  
  
  
  
  
  Reactive_Macro_Datos_Normalizado <- reactive({
    df <- Reactive_Macro_Datos()
    
    df %>%
      mutate(across(
        .cols = -Fecha,
        .fns  = ~ {
          min_val <- min(., na.rm = TRUE)
          max_val <- max(., na.rm = TRUE)
          if (max_val == min_val) {
            rep(0, length(.))  # Evita división por cero si todos los valores son iguales
          } else {
            2 * ((. - min_val) / (max_val - min_val)) - 1  # Normalización a [-1, 1]
          }
        }
      ))
  })
  
  
  
  
  
  cor_pmat <- function(df) {
    mat <- matrix(NA, ncol = ncol(df), nrow = ncol(df))
    colnames(mat) <- rownames(mat) <- colnames(df)
    
    for (i in 1:ncol(df)) {
      for (j in 1:ncol(df)) {
        x <- df[[i]]
        y <- df[[j]]
        test <- try(cor.test(x, y, method = "pearson"), silent = TRUE)
        mat[i, j] <- if (inherits(test, "try-error")) NA else test$p.value
      }
    }
    
    return(mat)
  }
  
  
  output$heatmap_correlacion <- renderPlotly({
    df <- Reactive_Macro_Datos_Normalizado() %>%
      select(-Fecha)
    
    # 1. Filtrar columnas que tengan varianza nula o muchos NA
    df <- df[, sapply(df, function(x) var(x, na.rm = TRUE) > 0 && sum(!is.na(x)) > 1)]
    
    # 2. Calcular matriz de correlación
    cm <- cor(df, use = "pairwise.complete.obs")
    
    # 3. Reemplazar NAs por 0 o eliminarlos si es necesario
    cm[is.na(cm)] <- 0  # alternativa: cm <- na.omit(cm)
    
    # 4. Graficar si la matriz no está vacía
    if (nrow(cm) > 1) {
      plot_ly(
        x = colnames(cm),
        y = rownames(cm),
        z = cm,
        type = "heatmap",
        zmin = -1,
        zmax = 1,
        colorscale = list(
          c(0, "#FF0000"),
          c(0.5, "#FFFFFF"),
          c(1, "#0000FF")
        ),
        colorbar = list(title = "correlación")
      ) %>%
        layout(
          xaxis = list(tickangle = -45),
          yaxis = list(autorange = "reversed")
        )
    } else {
      plot_ly() %>%
        layout(title = "No hay suficientes datos válidos para calcular la correlación.")
    }
  })
  
  
  
  output$heatmap_pvalores <- renderPlotly({
    df <- Reactive_Macro_Datos_Normalizado() %>% select(-Fecha)
    
    pmat <- cor_pmat(df)
    
    plot_ly(
      x = colnames(pmat),
      y = rownames(pmat),
      z = pmat,
      type = "heatmap",
      zmin = 0,
      zmax = 1,
      colorscale = "Viridis",
      colorbar = list(title = "p-valor")
    ) %>%
      layout(
        xaxis = list(tickangle = -45),
        yaxis = list(autorange = "reversed")
      )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$dual_axis_time_series <- renderPlotly({
    df <- Reactive_Macro_Datos_Normalizado()  # Datos normalizados [-1, 1]
    
    label_map <- list(
      IMACEC            = "IMACEC",
      PIB               = "PIB",
      TPM               = "TPM",
      Expectativa_IPC   = "Variación IPC Expectativa",
      Real_IPC          = "Variación IPC real",
      M0 = "Agregado monetario M0",
      M1 = "Agregado monetario M1",
      M2 = "Agregado monetario M2"
    )
    
    p <- plot_ly()
    
    # Línea 1 (variable en eje X)
    p <- p %>%
      add_lines(
        data = df,
        x    = ~Fecha,
        y    = as.formula(paste0("~", input$x_var)),
        name = label_map[[input$x_var]],
        line = list(width = 2, color = "gray")
      )
    
    # Línea 2 (variable en eje Y)
    p <- p %>%
      add_lines(
        data = df,
        x    = ~Fecha,
        y    = as.formula(paste0("~", input$y_var)),
        name = label_map[[input$y_var]],
        line = list(width = 2, dash = "dash", color = "black")
      )
    
    p %>%
      layout(
        title      = paste0(label_map[[input$y_var]], " vs. ", label_map[[input$x_var]]),
        xaxis      = list(title = "Fecha", tickangle = -45),
        yaxis      = list(
          title     = "Valor normalizado [-1, 1]",
          zeroline  = FALSE,
          range     = c(-1, 1),
          standoff  = 20
        ),
        showlegend = TRUE,
        margin     = list(l = 80, r = 80, t = 80, b = 80),
        hovermode  = "x unified"
      )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################### Finanzas publicas #############################
  
  Reactive_Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual <- reactive({
    Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual %>%
      filter(Anho >= input$anho_rango[1],
             Anho <= input$anho_rango[2])
  })
  
  output$plot_endeudamiento <- renderPlotly({
    df <- Reactive_Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual() %>%
      select(Anho, `7. Préstamo o endeudamiento neto`) %>%
      rename(Endeudamiento = `7. Préstamo o endeudamiento neto`)
    
    plot_ly(
      data = df,
      x = ~Anho,
      y = ~Endeudamiento,
      type = 'bar',
      marker = list(color = ~ifelse(Endeudamiento >= 0, 'green', 'red'))
    ) %>%
      layout(
        title = "Gráfico de Barras para la Tasa de Variación Anual del Préstamo o Endeudamiento",
        xaxis = list(title = "Año"),
        yaxis = list(title = "Monto en Millones (CLP)"),
        bargap = 0.3
      )
  })
  
  
  
  meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  Reactive_Tasa_Interes_2010_2024_Mensual <- reactive({
    req(input$anho_rango)
    
    Tasa_Interes_2010_2024_Mensual %>%
      rename(
        TPM = `1. Tasa de política monetaria (TPM) (porcentaje)`
      ) %>%
      filter(
        Anho >= input$anho_rango[1],
        Anho <= input$anho_rango[2]
      ) %>%
      mutate(
        Fecha = as.Date(paste0(Anho, "-", match(Mes, meses_es), "-01"))
      )
  })
  
  output$plot_tpm_dinamico <- renderPlotly({
    req(input$tipo_visual_tpm)
    df <- Reactive_Tasa_Interes_2010_2024_Mensual()
    
    if (input$tipo_visual_tpm == "Boxplot") {
      plot_ly(
        data = df,
        x = ~as.factor(Anho),
        y = ~TPM,
        type = "box",
        boxpoints = "outliers",
        name = "TPM por Año",
        marker = list(color = 'rgba(7, 164, 181, 0.6)'),
        line = list(color = 'rgba(7, 164, 181, 1)')
      ) %>%
        layout(
          title = "Boxplot para el TPM (%) Anual",
          xaxis = list(title = "Año"),
          yaxis = list(title = "TPM (%)")
        )
    } else {
      plot_ly(
        data = df,
        x = ~Fecha,
        y = ~TPM,
        type = "scatter",
        mode = "lines+markers",
        name = "TPM mensual",
        line = list(color = 'rgba(200, 30, 30, 0.8)', width = 2),
        marker = list(size = 4)
      ) %>%
        layout(
          title = "Serie de Tiempo Mensual de la TPM",
          xaxis = list(title = "Año"),
          yaxis = list(title = "TPM (%)")
        )
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$plot_box_tasa_interes <- renderPlotly({
    df <- Reactive_Tasa_Interes_2010_2024_Mensual() %>%
      select(Anho, Mes, TPM = `1. Tasa de política monetaria (TPM) (porcentaje)`)
    
    plot_ly(
      data = df,
      x = ~as.factor(Anho),
      y = ~TPM,
      type = "box",
      boxpoints = "all",     # Muestra todos los puntos individuales
      jitter = 0.3,          # Separación horizontal de los puntos
      pointpos = -1.8,       # Posición de los puntos en relación al box
      marker = list(color = 'rgba(7, 164, 181, 0.6)')
    ) %>%
      layout(
        title = "Distribución Mensual de la TPM por Año",
        xaxis = list(title = "Año"),
        yaxis = list(title = "TPM (%)"),
        boxmode = "group"
      )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############ Mercado laboral y demografia #############################
  
  
  
  
  
  # Reactive para sueldo mínimo promedio anual
  Reactive_Sueldo_Minimo_Nominal_2010_2024_Anual <- reactive({
    Sueldo_Minimo_Nominal_2010_2024_Anual %>%
      mutate(Anho = as.numeric(Anho)) %>%
      group_by(Anho) %>%
      summarise(Sueldo_Promedio = mean(`Monto nominal en pesos`, na.rm = TRUE)) %>%
      filter(Anho >= input$anho_rango[1], Anho <= input$anho_rango[2])
  })
  
  # Output para gráfico plotly
  output$plot_sueldo_minimo <- renderPlotly({
    plot_ly(
      data = Reactive_Sueldo_Minimo_Nominal_2010_2024_Anual(),
      x = ~Anho,
      y = ~Sueldo_Promedio,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'green'),
      marker = list(size = 8)
    ) %>%
      layout(
        title = "Sueldo Mínimo Promedio Anual (Nominal)",
        xaxis = list(title = "Año"),
        yaxis = list(title = "Monto en Pesos"),
        hovermode = "x unified"
      )
  })
  
  
  
  
  
  output$plot_inmigracion <- renderPlotly({
    req(input$ver_por)
    
    df <- Reactive_Resumen_Residencias_Definitivas_2010_2024() %>%
      select(Anho, total_personas,
             hombres, mujeres,
             starts_with("ESTUDIOS_"),
             starts_with("ACTIVIDAD_")) %>%
      group_by(Anho) %>%
      summarise(across(everything(), sum, na.rm = TRUE)) %>%
      ungroup()
    
    data_long <- df %>%
      pivot_longer(
        cols = -c(Anho),
        names_to = "categoria",
        values_to = "personas"
      )
    
    # Filtrar columnas según selección
    filtros <- c()
    if ("Total" %in% input$ver_por) {
      filtros <- c(filtros, "total_personas")
    }
    if ("Sexo" %in% input$ver_por) {
      filtros <- c(filtros, "hombres", "mujeres")
    }
    if ("Estudios" %in% input$ver_por) {
      filtros <- c(filtros, grep("^ESTUDIOS_", names(df), value = TRUE))
    }
    if ("Actividad" %in% input$ver_por) {
      filtros <- c(filtros, grep("^ACTIVIDAD_", names(df), value = TRUE))
    }
    
    data_plot <- data_long %>%
      filter(categoria %in% filtros)
    
    plot_ly(data_plot, x = ~Anho, y = ~personas, color = ~categoria,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Serie de Tiempo por Categoría",
             xaxis = list(title = "Año"),
             yaxis = list(title = "Total de Personas"),
             legend = list(x = 0.05, y = 0.95))
  })
  
  Reactive_Poblacion_Total_Estimaciones_2010_2024_Anual <- reactive({
    Poblacion_Total_Estimaciones_2010_2024_Anual %>%
      filter(Anho >= input$anho_rango[1],
             Anho <= input$anho_rango[2])
  })
  
  
  output$plot_poblacion <- renderPlotly({
    req(input$ver_poblacion)
    
    df <- Reactive_Poblacion_Total_Estimaciones_2010_2024_Anual() %>%
      select(Anho, `1. Total`, `1.1. Hombres`, `1.2. Mujeres`) %>%
      rename(
        Total = `1. Total`,
        Hombres = `1.1. Hombres`,
        Mujeres = `1.2. Mujeres`
      ) %>%
      group_by(Anho) %>%
      summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop") %>%
      pivot_longer(-Anho, names_to = "categoria", values_to = "personas")
    
    # Filtro por selección
    categorias <- c()
    if ("Total" %in% input$ver_poblacion) categorias <- c(categorias, "Total")
    if ("Sexo" %in% input$ver_poblacion) categorias <- c(categorias, "Hombres", "Mujeres")
    
    df_plot <- df %>%
      filter(categoria %in% categorias)
    
    plot_ly(df_plot, x = ~Anho, y = ~personas, color = ~categoria,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Serie de Tiempo del Total de la Población según Sexo",
             xaxis = list(title = "Año"),
             yaxis = list(title = "Número de Personas (en Millones)"),
             legend = list(x = 0.05, y = 0.95))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Reactive para Tasa de Desempleo Promedio Anual
  Reactive_Tasa_Desempleo_2010_2024_Mensual <- reactive({
    Tasa_Desempleo_2010_2024_Mensual %>%
      mutate(Anho = as.numeric(Anho)) %>%
      group_by(Anho) %>%
      summarise(Tasa_Desempleo_Prom = mean(`1. Nacional`, na.rm = TRUE)) %>%
      filter(Anho >= input$anho_rango[1], Anho <= input$anho_rango[2])
  })
  
  # Reactive para migración
  Reactive_Resumen_Residencias_Definitivas_2010_2024 <- reactive({
    Resumen_Residencias_Definitivas_2010_2024 %>%
      filter(Anho >= input$anho_rango[1],
             Anho <= input$anho_rango[2])
  })
  
  # Año seleccionado desde gráfico de desempleo
  selected_year <- reactiveVal(2024)
  
  observeEvent(event_data("plotly_click", source = "desempleo_plot"), {
    punto <- event_data("plotly_click", source = "desempleo_plot")
    if (!is.null(punto)) {
      selected_year(punto$x)
    }
  })
  
  # Texto para mostrar año seleccionado
  output$anho_seleccionado_text <- renderText({
    req(selected_year())
    paste("Año seleccionado:", selected_year())
  })
  
  # Gráfico de Tasa de Desempleo con source para click tracking
  output$plot_desempleo <- renderPlotly({
    plot_ly(
      data = Reactive_Tasa_Desempleo_2010_2024_Mensual(),
      x = ~Anho,
      y = ~Tasa_Desempleo_Prom,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'red'),
      marker = list(size = 8),
      source = "desempleo_plot"
    ) %>%
      layout(
        title = "Serie de Tiempo para la Tasa de Desempleo Promedio Anual",
        xaxis = list(title = "Año"),
        yaxis = list(title = "Tasa de Desempleo (%)"),
        hovermode = "x unified"
      )
  })
  
  # Gráfico de torta reactivo por inmigración
  output$plot_torta_inmigracion <- renderPlotly({
    req(selected_year())
    req(input$ver_por)
    
    df <- Reactive_Resumen_Residencias_Definitivas_2010_2024() %>%
      filter(Anho == selected_year()) %>%
      select(Anho, total_personas, hombres, mujeres, starts_with("ESTUDIOS_"), starts_with("ACTIVIDAD_")) %>%
      group_by(Anho) %>%
      summarise(across(everything(), sum, na.rm = TRUE)) %>%
      ungroup()
    
    filtros <- switch(input$ver_por,
                      "Total" = c("total_personas"),
                      "Sexo" = c("hombres", "mujeres"),
                      "Estudios" = grep("^ESTUDIOS_", names(df), value = TRUE),
                      "Actividad" = grep("^ACTIVIDAD_", names(df), value = TRUE)
    )
    
    df_long <- df %>%
      pivot_longer(
        cols = all_of(filtros),
        names_to = "categoria",
        values_to = "valor"
      )
    
    plot_ly(
      data = df_long,
      labels = ~categoria,
      values = ~valor,
      type = "pie",
      insidetextorientation = "radial",
      marker = list(line = list(color = "#FFFFFF", width = 1))
    ) %>%
      layout(
        title = paste("Distribución:", input$ver_por, "en", selected_year()),
        showlegend = TRUE # <-- Esto asegura que la leyenda se mantenga visible
      )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############# SECTOR EXTERNO #####################
  
  
  
  
  Reactive_Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual <- reactive({
    Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual %>%
      filter(Anho >= input$anho_rango[1],
             Anho <= input$anho_rango[2])
  })
  
  Reactive_USD_CLP_2010_2024_Anual <- reactive({
    USD_CLP_2010_2024_Mensual %>%
      filter(Anho >= input$anho_rango[1],
             Anho <= input$anho_rango[2]) %>%
      group_by(Anho) %>%
      summarise(Dolar_Promedio = mean(`Dólar`, na.rm = TRUE))
  })
  
  
  
  
  output$plot_dolar_promedio <- renderPlotly({
    plot_ly(
      data = Reactive_USD_CLP_2010_2024_Anual(),
      x = ~Anho,
      y = ~Dolar_Promedio,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'blue'),
      marker = list(size = 6),
      name = "Dólar Promedio"
    ) %>%
      layout(
        title = "Serie de Tiempo para el Promedio Anual del Dólar",
        xaxis = list(title = "Año"),
        yaxis = list(title = "CLP", tickformat = ".2f"),
        hovermode = "x unified"
      )
  })
  
  output$plot_cobre_bruto <- renderPlotly({
    plot_ly(
      data = Reactive_Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual(),
      x = ~Anho,
      y = ~`1.2. Cobre bruto`,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'orange'),
      marker = list(size = 6),
      name = "Cobre bruto"
    ) %>%
      layout(
        title = "Serie de Tiempo para las Exportaciones de Cobre Bruto",
        xaxis = list(title = "Año"),
        yaxis = list(title = "Miles de millones (CLP)", tickformat = ".2f"),
        hovermode = "x unified"
      )
  })
  
  output$descargar_reporte <- downloadHandler(
    filename = function() {
      paste0("Reporte_Análisis_Exploratorio_Economía_Chile_", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "reporte_dashboard.Rmd")
      file.copy("reporte_dashboard.Rmd", tempReport, overwrite = TRUE)
      
      # Prepare parameters to pass to Rmd
      params <- list(
        anho_rango_shiny = input$anho_rango,
        filtro_categoria_shiny = input$filtro_categoria,
        x_var_shiny = input$x_var,
        y_var_shiny = input$y_var,
        tipo_visual_tpm_shiny = input$tipo_visual_tpm,
        tipo_info_shiny = input$tipo_info,
        ver_por_shiny = input$ver_por,
        ver_poblacion_shiny = input$ver_poblacion,
        graficos_sector_externo_shiny = input$graficos_sector_externo,
        selected_year_shiny = selected_year()
      )
      
      # Render the Rmd report, passing the parameters
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  
  observeEvent(input$infoButton, {
    showModal(modalDialog(
      title = "Acerca de este Dashboard y los Datos",
      HTML("
        <p>Los datos recopilados se extrajeron de la base de datos que provee el Banco Central de Chile, recuperado en Mayo, 2025. Puedes acceder a la fuente original aquí: 'https://si3.bcentral.cl/siete' </a></p>
    "),
      footer = modalButton("Cerrar")
    ))
  })
  
  
}



# Run app
shinyApp(ui = ui, server = server)