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


##############
# Carga de datos
# Asume que esta ruta existe o ajusta según tu entorno
ruta<-"C:/Users/angel/Desktop/UNIVERSIDAD/Base de datos Análisis de la Relación entre Inflación, Desempleo, Variación del Dólar y Crecimiento Económico en Chile/"
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
##############

##############
library(dplyr)
library(stringr)

Agregado_Monetario_2010_2024_Mensual <- Agregado_Monetario_2010_2024_Mensual %>%
  mutate(Mes = str_to_title(Mes))

Expectativa_IPC_2010_2024_Mensual <- Expectativa_IPC_2010_2024_Mensual %>%
  mutate(Mes = str_to_title(Mes))

IMACEC_2010_2024_Mensual <- IMACEC_2010_2024_Mensual %>%
  mutate(Mes = str_to_title(Mes))


Real_IPC_2010_2024_Mensual <- Real_IPC_2010_2024_Mensual %>%
  mutate(Mes = str_to_title(Mes))


Tasa_Desempleo_2010_2024_Mensual <- Tasa_Desempleo_2010_2024_Mensual %>%
  mutate(Mes = str_to_title(Mes))

Tasa_Interes_2010_2024_Mensual <- Tasa_Interes_2010_2024_Mensual %>%
  mutate(Mes = str_to_title(Mes))

USD_CLP_2010_2024_Mensual <- USD_CLP_2010_2024_Mensual %>%
  mutate(Mes = str_to_title(Mes))
##############

##############
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

##############


##############
# ---- DATA PREP (global) -------------------------------------------------
# Desestacionalizados: IPC, Desempleo, IMACEC, M0–M3
# ── ahora con la serie de desempleo iniciando en marzo-2010 ──

library(dplyr)
library(seasonal)

Mes_orden <- c(
  "Enero","Febrero","Marzo","Abril","Mayo","Junio",
  "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"
)

# ── Base calendario completa (180 meses) ─────────────────────────────────
df_base <- expand.grid(
  Anho = 2010:2024,
  Mes  = factor(Mes_orden, levels = Mes_orden, ordered = TRUE)
) |>
  arrange(Anho, Mes)

# ── IPC ───────────────────────────────────────────────────────────────────
df_ipc <- Real_IPC_2010_2024_Mensual |> 
  mutate(Mes = factor(Mes, levels = Mes_orden, ordered = TRUE)) |> 
  arrange(Anho, Mes)

ts_ipc <- ts(df_ipc$`Variación IPC`, start = c(2010, 1), frequency = 12)
sa_ipc <- seasonal::final(seasonal::seas(ts_ipc)) |> as.numeric()

# ── Desempleo ─────────────────────────────────────────────────────────────
df_desem <- Tasa_Desempleo_2010_2024_Mensual |> 
  mutate(Mes = factor(Mes, levels = Mes_orden, ordered = TRUE)) |> 
  arrange(Anho, Mes)

ts_desem <- ts(df_desem$`1. Nacional`, start = c(2010, 3), frequency = 12)  # ← marzo-2010
sa_desem <- seasonal::final(seasonal::seas(ts_desem)) |> as.numeric()

df_desem_sa <- df_desem |> 
  mutate(Desempleo = sa_desem) |> 
  select(Anho, Mes, Desempleo)

# ── IMACEC ────────────────────────────────────────────────────────────────
df_imacec <- IMACEC_2010_2024_Mensual |> 
  mutate(Mes = factor(Mes, levels = Mes_orden, ordered = TRUE)) |> 
  arrange(Anho, Mes)

ts_imacec <- ts(df_imacec$`1. Imacec`, start = c(2010, 1), frequency = 12)
sa_imacec <- seasonal::final(seasonal::seas(ts_imacec)) |> as.numeric()

# ── Agregados Monetarios (M0-M3) ─────────────────────────────────────────
df_agreg <- Agregado_Monetario_2010_2024_Mensual |> 
  mutate(Mes = factor(Mes, levels = Mes_orden, ordered = TRUE)) |> 
  arrange(Anho, Mes)

ts_m0 <- ts(df_agreg$M0, start = c(2010, 1), frequency = 12)
ts_m1 <- ts(df_agreg$M1, start = c(2010, 1), frequency = 12)
ts_m2 <- ts(df_agreg$M2, start = c(2010, 1), frequency = 12)
ts_m3 <- ts(df_agreg$M3, start = c(2010, 1), frequency = 12)

sa_m0 <- seasonal::final(seasonal::seas(ts_m0)) |> as.numeric()
sa_m1 <- seasonal::final(seasonal::seas(ts_m1)) |> as.numeric()
sa_m2 <- seasonal::final(seasonal::seas(ts_m2)) |> as.numeric()
sa_m3 <- seasonal::final(seasonal::seas(ts_m3)) |> as.numeric()

# ── Consolidado ──────────────────────────────────────────────────────────
Desestacionalizados <- df_base |>
  mutate(
    IPC       = sa_ipc,
    IMACEC    = sa_imacec,
    M0        = sa_m0,
    M1        = sa_m1,
    M2        = sa_m2,
    M3        = sa_m3
  ) |>
  left_join(df_desem_sa, by = c("Anho", "Mes"))  # agrega Desempleo (NA en ene-feb 2010)


##############


# UI

ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "position: relative; left: -8px;",   # ajusta los px si quieres más/menos desplazamiento
      HTML("<span style='font-size: 23px;'>Análisis Exploratorio<br>Económico en <br>Chile</span>")
    ),
    tags$li(
      class = "dropdown",
      tags$div(
        style = "position: relative; left: -45px; top: 8px; width: 300px;",  # ← 5 px hacia abajo
        sliderTextInput(
          inputId  = "anho_rango",
          label    = NULL,
          choices  = 2010:2024,
          selected = c(2010, 2024),
          grid     = TRUE,
          animate  = TRUE,
          width    = "300px"
        )
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("chart-bar")),
      menuItem("Indicadores Macroeconómicos", tabName = "indicadores_macroeconomicos", icon = icon("tachometer-alt")),
      menuItem("Finanzas Públicas", tabName = "finanzas_publicas", icon = icon("landmark")),
      menuItem("Mercado Laboral y Demografía", tabName = "mercado_laboral_demografia", icon = icon("users")),
      menuItem("Sector Externo", tabName = "sector_externo", icon = icon("globe")),
      menuItem("Avanzado (desestacionalizar)", tabName = "panel_avanzado", icon = icon("chart-line")),
      
      tags$div(
        style = "text-align: center; padding: 10px; top: 200px",
        downloadButton("descargar_reporte", "Descargar Informe HTML", class = "btn btn-primary")
      ),
      
      # Botón de información de contacto justo debajo
      tags$div(
        style = "padding:10px 0; display:inline-block; vertical-align:middle;",
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
            title = "Comparación Mensual: Variación % del Dólar vs. Variación % del IPC vs. TPM",
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
        ),
        fluidRow(
          column(
            width = 4,
            ## Selector (1 o múltiple).  “Agregado monetario” → M0–M3
            ## y “Ingreso y gasto de gobierno” → ambas columnas.
            selectizeInput(
              inputId  = "vars_tabla",
              label    = "Variables a resumir:",
              multiple = TRUE,
              choices  = c(
                "Agregado monetario",          # M0–M3
                "Ingreso y gasto de gobierno", # Total ingresos + Total gastos
                "Expectativa IPC",
                "IMACEC",
                "PIB corriente",
                "Variación IPC",
                "Residencias definitivas",
                "Sueldo mínimo",
                "Tasa de desempleo",
                "TPM",
                "USD/CLP"
              )
            )
          ),
          column(
            width = 8,
            DT::dataTableOutput("tabla_resumen")
          )
        )
        
        
      ),
      
      tabItem(tabName = "indicadores_macroeconomicos",
              fluidPage(
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
                ),
                fluidRow(
                  column(
                    width = 8, offset = 2,   # 8 de ancho + 2 de offset = 12 columnas
                    box(
                      title = "Heatmap de Correlaciones para Indicadores Anuales",
                      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
                      plotlyOutput("heatmap_correlacion", height = "600px")
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "finanzas_publicas",
              fluidRow(
                box(
                  
                  title= "Ingreso y Gasto Total del Gobierno por año",
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
                box(
                  title = "Tasa de Desempleo Promedio Anual",
                  width = 12, status = "primary", solidHeader = TRUE,
                  # Filtro para superponer máximo migración
                  fluidRow(
                    column(
                      width = 4,
                      radioButtons(
                        inputId = "ver_migracion",
                        label = "Mostrar Máximo de (residencias definitivas a extranjero):",
                        choices = c("Ninguno","Total","Sexo","Estudios","Actividad"),
                        selected = "Ninguno",
                        inline = TRUE
                      )
                    )
                  )
                  ,
                  plotlyOutput("plot_desempleo", height = "400px")
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
              
      ),
      tabItem(tabName = "panel_avanzado",
              
              fluidRow(
                column(
                  width = 4,
                  # ¿Qué mostrar? (una o ambas)
                  checkboxGroupInput(
                    inputId   = "tipo_serie",
                    label     = "Mostrar serie:",
                    choices   = c("Serie original" = "orig",
                                  "Serie desestacionalizada" = "sa"),
                    selected  = "sa",                       # al menos una por defecto
                    inline    = TRUE
                  ),
                  # ¿Cuál serie? (selección única)
                  radioButtons(
                    inputId  = "serie_elegida",
                    label    = "Serie:",
                    choices  = c("IPC", "Tasa de desempleo",
                                 "IMACEC", "M0", "M1", "M2", "M3"),
                    selected = "IPC",
                    inline   = FALSE
                  )
                ),
                column(
                  width = 8,
                  tabsetPanel(
                    id   = "tab_avanzado",
                    type = "pills",
                    
                    tabPanel("Serie", plotlyOutput("plot_sa_vs_orig", height = "420px")),
                    tabPanel("STL",   plotlyOutput("plot_stl",        height = "420px"))
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  actionButton(
                    inputId = "btn_recomendaciones",
                    label   = "Ver recomendaciones",
                    icon    = icon("lightbulb"),
                    style   = "margin-bottom: 15px;"  # un poco de espacio abajo
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
        title = "",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Porcentaje (%)"),
        legend = list(x = 0.05, y = 0.95)
      )
  })
  
  
  Reactive_Resumen_Residencias_Definitivas_2010_2024 <- reactive({
    Resumen_Residencias_Definitivas_2010_2024 %>%
      filter(Anho >= input$anho_rango[1],
             Anho <= input$anho_rango[2])
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
      ) %>%
      # Aquí limpiamos los prefijos
      mutate(categoria = sub("^(ESTUDIOS_|ACTIVIDAD_)", "", categoria))
    
    # Filtro según input
    if (input$filtro_categoria == "Estudios") {
      categorias_filtradas <- grep("^ESTUDIOS_", names(df), value = TRUE) %>%
        sub("^ESTUDIOS_", "", .)
    } else {
      categorias_filtradas <- grep("^ACTIVIDAD_", names(df), value = TRUE) %>%
        sub("^ACTIVIDAD_", "", .)
    }
    
    # Unir con la tasa de desempleo anual
    tasa_desempleo_anual <- Reactive_Tasa_Desempleo_2010_2024_Mensual()
    
    data_long %>%
      filter(categoria %in% categorias_filtradas) %>%
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
      subtitle = "Variación M1 (2010-2024)",
      icon = icon("money-bill-wave"),
      color = "blue"
    )
  })
  
  output$box_tpm <- renderValueBox({
    valueBox(
      paste0(calc_variacion_tpm(), "%"),
      subtitle = "Variación Tasa de Interés (TPM) (2010-2024)",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$box_ipc <- renderValueBox({
    valueBox(
      paste0(calc_ipc_compuesta(), "%"),
      subtitle = "Variación IPC Compuesto (2010-2024)",
      icon = icon("percent"),
      color = "green"
    )
  })
  
  output$box_desempleo <- renderValueBox({
    valueBox(
      paste0(calc_variacion_desempleo(), "%"),
      subtitle = "Variación Tasa de Desempleo (2010-2024)",
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
      "Variación Sueldo Mínimo (2010-2024)",
      icon = icon("money-bill-wave"),
      color = "olive"
    )
  })
  
  output$box_dolar <- renderValueBox({
    valueBox(
      paste0(Variacion_Dolar(), "%"),
      "Variación Dólar (USD/CLP) (2010-2024)",
      icon = icon("dollar-sign"),
      color = "teal"
    )
  })
  
  
  
  library(dplyr)
  library(DT)
  
  Mes_orden <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                 "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  
  # ---- mapa variable → dataset / columna ---------------------------------
  mapa <- list(
    "M0"                 = list(df = Agregado_Monetario_2010_2024_Mensual, col = "M0"),
    "M1"                 = list(df = Agregado_Monetario_2010_2024_Mensual, col = "M1"),
    "M2"                 = list(df = Agregado_Monetario_2010_2024_Mensual, col = "M2"),
    "M3"                 = list(df = Agregado_Monetario_2010_2024_Mensual, col = "M3"),
    "Ingresos públicos"  = list(df = Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual, col = "5. Total ingresos"),
    "Gastos públicos"    = list(df = Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual, col = "6. Total gastos"),
    "Expectativa IPC"    = list(df = Expectativa_IPC_2010_2024_Mensual,       col = "Expectativa"),
    "IMACEC"             = list(df = IMACEC_2010_2024_Mensual,                col = "1. Imacec"),
    "PIB corriente"      = list(df = PIB_2010_2024_Anual,                     col = "1. PIB a precios corrientes"),
    "Variación IPC"      = list(df = Real_IPC_2010_2024_Mensual,              col = "Variación IPC"),
    "Residencias definitivas" = list(df = Resumen_Residencias_Definitivas_2010_2024, col = "total_personas"),
    "Sueldo mínimo"      = list(df = Sueldo_Minimo_Nominal_2010_2024_Anual,   col = "Monto nominal en pesos"),
    "Tasa de desempleo"  = list(df = Tasa_Desempleo_2010_2024_Mensual,        col = "1. Nacional"),
    "TPM"                = list(df = Tasa_Interes_2010_2024_Mensual,          col = "1. Tasa de política monetaria (TPM) (porcentaje)"),
    "USD/CLP"            = list(df = USD_CLP_2010_2024_Mensual,               col = "Dólar")
  )
  
  # ---- tabla resumen reactiva --------------------------------------------
  Reactive_Tabla <- reactive({
    req(input$vars_tabla)
    
    # Expansiones automáticas
    vars <- input$vars_tabla
    if ("Agregado monetario" %in% vars) {
      vars <- setdiff(vars, "Agregado monetario")
      vars <- c(vars, "M0", "M1", "M2", "M3")
    }
    if ("Ingreso y gasto de gobierno" %in% vars) {
      vars <- setdiff(vars, "Ingreso y gasto de gobierno")
      vars <- c(vars, "Ingresos públicos", "Gastos públicos")
    }
    vars <- unique(vars)
    
    anho_min <- input$anho_rango[1]
    anho_max <- input$anho_rango[2]
    
    resumen_list <- lapply(vars, function(v) {
      info <- mapa[[v]]
      df_v <- info$df %>%
        filter(Anho >= anho_min, Anho <= anho_max) %>%
        pull(!!sym(info$col))
      
      data.frame(
        Variable = v,
        Min      = round(min(df_v, na.rm = TRUE),3),
        Q1       = round(quantile(df_v, 0.25, na.rm = TRUE),3),
        Median   = round(median(df_v, na.rm = TRUE),3),
        Mean     = round(mean(df_v, na.rm = TRUE),3),
        Q3       = round(quantile(df_v, 0.75, na.rm = TRUE),3),
        Max      = round(max(df_v, na.rm = TRUE),3),
        SD       = round(sd(df_v,  na.rm = TRUE),3),
        N_obs    = sum(!is.na(df_v)),
        check.names = FALSE
      )
    })
    
    bind_rows(resumen_list)
  })
  
  # ---- render en DataTable -----------------------------------------------
  output$tabla_resumen <- DT::renderDataTable({
    DT::datatable(
      Reactive_Tabla(),
      options = list(pageLength = 15, searching = FALSE, scrollX = TRUE),
      rownames = FALSE
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
    
    # Filtrar columnas con varianza no nula y suficientes datos
    df <- df[, sapply(df, function(x) var(x, na.rm = TRUE) > 0 && sum(!is.na(x)) > 1)]
    
    # Matriz de correlación
    cm <- cor(df, use = "pairwise.complete.obs")
    cm[is.na(cm)] <- 0
    
    # Calcular matriz de p-values
    pmat <- cor_pmat(df)
    pmat[is.na(pmat)] <- 1
    
    # Generar símbolos de significancia
    sig_mat <- matrix(
      "", nrow = nrow(pmat), ncol = ncol(pmat), dimnames = dimnames(pmat)
    )
    sig_mat[pmat < 0.01] <- "•"
    sig_mat[pmat >= 0.01 & pmat < 0.05] <- "*"
    sig_mat[pmat >= 0.05 & pmat < 0.1] <- "."
    
    # Preparar etiquetas con valor y símbolo solo si p < 0.2
    text_mat <- matrix(
      ifelse(
        pmat < 0.10001,
        paste0(format(round(cm, 2), nsmall = 1), sig_mat),
        ""
      ),
      nrow = nrow(cm), ncol = ncol(cm), dimnames = dimnames(cm)
    )
    
    # Eliminar textos en la diagonal
    diag(text_mat) <- ""
    
    if (nrow(cm) > 1) {
      plot_ly(
        x = colnames(cm),
        y = rownames(cm),
        z = cm,
        type = "heatmap",
        colorscale = list(
          c(0, "#FF0000"),
          c(0.5, "#FFFFFF"),
          c(1, "#0000FF")
        ),
        zmin = -1,
        zmax = 1,
        showscale = TRUE,
        text = text_mat,
        texttemplate = "%{text}",
        hoverinfo = "none"
      ) %>%
        layout(
          xaxis = list(tickangle = -45),
          yaxis = list(autorange = "reversed"),
          margin = list(l = 100, b = 100),
          # Leyenda de significancia
          annotations = list(
            list(xref = 'paper', yref = 'paper', x = 1.01, y = 0.2,
                 text = '<b>Significancia:</b>', showarrow = FALSE, xanchor = 'left'),
            list(xref = 'paper', yref = 'paper', x = 1.01, y = 0.15,
                 text = '"•" p < 0.01', showarrow = FALSE, xanchor = 'left'),
            list(xref = 'paper', yref = 'paper', x = 1.01, y = 0.1,
                 text = '"*" p < 0.05', showarrow = FALSE, xanchor = 'left'),
            list(xref = 'paper', yref = 'paper', x = 1.01, y = 0.05,
                 text = '"." p < 0.10', showarrow = FALSE, xanchor = 'left')
          )
        )
    } else {
      plot_ly() %>%
        layout(title = "No hay suficientes datos válidos para calcular la correlación.")
    }
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################### Finanzas publicas ##############################
  
  
  
  
  # Reactive para datos de Ingreso, Gasto y Endeudamiento
  Reactive_Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual <- reactive({
    Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual %>%
      filter(
        Anho >= input$anho_rango[1],
        Anho <= input$anho_rango[2]
      )
  })
  
  output$plot_endeudamiento <- renderPlotly({
    # Preparar datos y calcular posiciones de texto
    df2 <- Reactive_Ingreso_Gasto_Gobierno_Exportacion_Cobre_2010_2024_Anual() %>%
      mutate(
        Ingresos = `5. Total ingresos`,
        Gastos = -`6. Total gastos`,
        Endeudamiento_R = round(`7. Préstamo o endeudamiento neto`, 0),
        # Calcular posición Y del texto con offset del 20%
        y_text = if_else(
          Endeudamiento_R >= 0,
          Ingresos + abs(Ingresos) * 0.20,
          Gastos - abs(Gastos) * 0.20
        ),
        text_color = if_else(Endeudamiento_R >= 0, 'green', 'red')
      )
    
    # Barras de ingreso y gasto
    p <- plot_ly(df2, x = ~Anho) %>%
      add_bars(
        y = ~Ingresos,
        name = "Ingresos",
        marker = list(color = 'green')
      ) %>%
      add_bars(
        y = ~Gastos,
        name = "Gastos",
        marker = list(color = 'red')
      )
    
    # Etiquetas de endeudamiento sin puntos superpuestos
    p %>% add_trace(
      data = df2,
      x = ~Anho,
      y = ~y_text,
      type = 'scatter',
      mode = 'text',
      text = ~Endeudamiento_R,
      textposition = 'top center',
      textfont = list(color = ~text_color),
      showlegend = FALSE
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Año"),
        yaxis = list(title = "Monto en Millones (CLP)"),
        barmode = 'relative',
        bargap = 0.2,
        legend = list(x = 0.1, y = 0.9)
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
  
  # Reactive: tasa anual promedio
  Reactive_Tasa_Desempleo_Anual <- reactive({
    Tasa_Desempleo_2010_2024_Mensual %>%
      filter(Anho >= input$anho_rango[1], Anho <= input$anho_rango[2]) %>%
      group_by(Anho) %>%
      summarise(Tasa_Prom_Anual = mean(`1. Nacional`, na.rm = TRUE), .groups = 'drop')
  })
  
  # Reactive: resumen migración anual
  Reactive_Resumen_Migracion <- reactive({
    Resumen_Residencias_Definitivas_2010_2024 %>%
      filter(Anho >= input$anho_rango[1], Anho <= input$anho_rango[2]) %>%
      group_by(Anho) %>%
      summarise(
        total_personas = sum(total_personas, na.rm = TRUE),
        hombres = sum(hombres, na.rm = TRUE),
        mujeres = sum(mujeres, na.rm = TRUE),
        across(starts_with("ESTUDIOS_"), sum, na.rm = TRUE),
        across(starts_with("ACTIVIDAD_"), sum, na.rm = TRUE),
        .groups = 'drop'
      )
  })
  
  # Render: tasa con opción de máximo migración
  output$plot_desempleo <- renderPlotly({
    df_des <- Reactive_Tasa_Desempleo_Anual()
    p <- plot_ly(
      df_des, x = ~Anho, y = ~Tasa_Prom_Anual,
      type = 'scatter', mode = 'lines+markers',
      line = list(color = 'blue'), marker = list(size = 8),
      name = 'Tasa Anual'
    )
    
    # Sólo anotaciones sobre cada punto
    if (input$ver_migracion == "Total") {
      df_ann <- Reactive_Resumen_Migracion() %>%
        select(Anho, label = total_personas) %>%
        left_join(df_des, by = 'Anho')
      p <- p %>% add_text(
        data = df_ann,
        x = ~Anho, y = ~Tasa_Prom_Anual,
        text = ~label,
        textposition = 'top center',
        textfont = list(color = 'darkorange', size = 12),
        textoffset = 10,
        showlegend = FALSE
      )
    } else if (input$ver_migracion %in% c("Sexo", "Estudios", "Actividad")) {
      df_mig0 <- Reactive_Resumen_Migracion()
      sel_cols <- switch(
        input$ver_migracion,
        "Sexo" = c("hombres", "mujeres"),
        "Estudios" = grep("^ESTUDIOS_", names(df_mig0), value = TRUE),
        "Actividad" = grep("^ACTIVIDAD_", names(df_mig0), value = TRUE)
      )
      df_ann <- df_mig0 %>%
        pivot_longer(cols = all_of(sel_cols), names_to = 'categoria', values_to = 'valor') %>%
        mutate(
          pct = valor / total_personas * 100,
          categoria = case_when(
            str_starts(categoria, "ESTUDIOS_") ~ str_remove(categoria, "^ESTUDIOS_"),
            str_starts(categoria, "ACTIVIDAD_") ~ str_remove(categoria, "^ACTIVIDAD_"),
            TRUE ~ categoria
          )
        ) %>%
        group_by(Anho) %>%
        slice_max(pct, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        left_join(df_des, by = 'Anho')
      p <- p %>% add_text(
        data = df_ann,
        x = ~Anho, y = ~Tasa_Prom_Anual,
        text = ~paste0("", round(pct,1), "%
  ", categoria, "
                       "),
        textposition = 'top center',
        textfont = list(color = 'darkgreen', size = 12),
        textoffset = 10,
        showlegend = FALSE
      )
    }
    
    p %>% layout(
      title = '',
      xaxis = list(title = 'Año'),
      yaxis = list(title = 'Tasa (%)', range = c(5.5, 12)),
      hovermode = 'x unified',
      legend = list(x = 0.1, y = 0.9)
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
        yaxis = list(title = "Monto en Millones (CLP)", tickformat = ".2f"),
        hovermode = "x unified"
      )
  })
  
  
  ##############PANEL AVANZADO
  
  observeEvent(input$btn_recomendaciones, {
    showModal(
      modalDialog(
        title       = "Recomendaciones",
        HTML(paste0(
          "<h5>Recomendaciones a corto plazo (1-3 meses)</h5>",
          "<ul style='text-align:left;'>",
          "<li>Incorporar mensualmente los nuevos datos publicados por el Banco Central de Chile.</li>",
          "<li>Validación de series desestacionalizadas una vez se integren nuevos datos.</li>",
          "</ul>",
          "<h5>Recomendaciones a mediano plazo (3-12 meses)</h5>",
          "<ul style='text-align:left;'>",
          "<li>Conectar el dashboard con el Banco Central de Chile para actualizaciones automáticas y en tiempo real.</li>",
          "<li>Implementar medidas para operar en un rango de inflación anual -1% a 3%, y dólar &lt; 1 000 pesos chilenos.</li>",
          "</ul>",
          "<h5>Recomendaciones general</h5>",
          "<ul style='text-align:left;'>",
          "<li>Usar el IMACEC como indicador adelantado al PIB, aprovechando su alta correlación comprobada para anticipar desaceleraciones o repuntes.</li>",
          "<li>Las series desestacionalizadas permiten detectar anticipadamente cambios tendenciales para prevenir y aplicar medidas para contrarrestar mayores efectos negativos para el desempeño económico de Chile.</li>",
          "<li>Vigilar la relación entre TPM baja y expansión monetaria M2 para evitar tensiones inflacionarias.</li>",
          "<li>Relacionar movimientos del dólar con cambios en exportaciones de cobre para calibrar proyecciones de modelos bancarios.</li>",
          "</ul>",
          "<h5>Limitaciones y consideraciones</h5>",
          "<ul style='text-align:left;'>",
          "<li>Incluir variables de crisis (pandemia, estallido social, flujos migratorios) como controles en futuros modelos predictivos.</li>",
          "<li>Programar revisiones semestrales de la arquitectura del dashboard y de los algoritmos de ajuste estacional para evitar obsolescencias.</li>",
          "</ul>"
        )),
        size      = "l",
        easyClose  = TRUE,
        footer     = modalButton("Cerrar")
      )
    )
  })
  
  
  # Carga de librerías necesarias
  library(dplyr)
  library(plotly)
  library(forecast)  # autoplot.STL
  library(ggplot2)
  
  # ── Constantes y ayudas ─────────────────────────────────────────────────
  Mes_orden <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                 "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  
  nombre_largo <- function(x){
    switch(x,
           "IPC"               = "Variación mensual IPC (%)",
           "Tasa de desempleo" = "Tasa de desempleo (%)",
           "IMACEC"            = "Índice IMACEC",
           "M0"                = "Agregado monetario M0",
           "M1"                = "Agregado monetario M1",
           "M2"                = "Agregado monetario M2",
           "M3"                = "Agregado monetario M3")
  }
  
  mapa_dataset <- list(
    "IPC"               = list(df = Real_IPC_2010_2024_Mensual,       col = "Variación IPC"),
    "Tasa de desempleo" = list(df = Tasa_Desempleo_2010_2024_Mensual, col = "1. Nacional"),
    "IMACEC"            = list(df = IMACEC_2010_2024_Mensual,         col = "1. Imacec"),
    "M0"                = list(df = Agregado_Monetario_2010_2024_Mensual, col = "M0"),
    "M1"                = list(df = Agregado_Monetario_2010_2024_Mensual, col = "M1"),
    "M2"                = list(df = Agregado_Monetario_2010_2024_Mensual, col = "M2"),
    "M3"                = list(df = Agregado_Monetario_2010_2024_Mensual, col = "M3")
  )
  
  # ── Serie reactiva (original y/o desestacionalizada) ────────────────────
  Reactive_Serie <- reactive({
    
    serie <- input$serie_elegida
    req(serie)
    
    anho_min <- input$anho_rango[1]
    anho_max <- input$anho_rango[2]
    
    lista <- list()
    
    # --- Serie original ----------------------------------------------------
    if ("orig" %in% input$tipo_serie){
      info <- mapa_dataset[[serie]]
      df_o <- info$df %>%
        mutate(Mes = factor(Mes, levels = Mes_orden, ordered = TRUE)) %>%
        arrange(Anho, Mes) %>%
        filter(Anho >= anho_min, Anho <= anho_max) %>%
        transmute(
          Anho, Mes,
          Valor = .data[[info$col]],
          Tipo  = "Original"
        )
      lista[[length(lista)+1]] <- df_o
    }
    
    # --- Serie desestacionalizada -----------------------------------------
    if ("sa" %in% input$tipo_serie){
      col_sa <- switch(serie,
                       "IPC"               = "IPC",
                       "Tasa de desempleo" = "Desempleo",
                       "IMACEC"            = "IMACEC",
                       "M0"                = "M0",
                       "M1"                = "M1",
                       "M2"                = "M2",
                       "M3"                = "M3")
      df_sa <- Desestacionalizados %>%
        mutate(Mes = factor(Mes, levels = Mes_orden, ordered = TRUE)) %>%
        filter(Anho >= anho_min, Anho <= anho_max) %>%
        transmute(
          Anho, Mes,
          Valor = .data[[col_sa]],
          Tipo  = "Desestacionalizada"
        )
      lista[[length(lista)+1]] <- df_sa
    }
    
    validate(need(length(lista) > 0, "Selecciona al menos una versión de la serie."))
    bind_rows(lista)
  })
  
  # ── Gráfico Serie vs Desestacionalizada ─────────────────────────────────
  output$plot_sa_vs_orig <- renderPlotly({
    datos <- Reactive_Serie()
    
    datos <- datos %>%
      arrange(Anho, Mes) %>%
      mutate(Time = interaction(Anho, Mes, lex.order = TRUE))
    
    plot_ly(data = datos,
            x     = ~Time,
            y     = ~Valor,
            color = ~Tipo,
            colors = c("Original" = "#4E79A7", "Desestacionalizada" = "#E15759"),
            type  = "scatter",
            mode  = "lines") |>
      layout(
        title  = paste("Serie:", input$serie_elegida),
        xaxis  = list(title = "Año-Mes",
                      tickangle = -45,
                      tickmode  = "array",
                      tickvals  = datos$Time[seq(1, nrow(datos), 12)],
                      ticktext  = datos$Anho[seq(1, nrow(datos), 12)]),
        yaxis  = list(title = nombre_largo(input$serie_elegida)),
        legend = list(orientation = "h", x = 0.1, y = -0.25)
      ) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Descomposición STL (solo si “Serie original” está marcada) ──────────
  Reactive_STL <- reactive({
    req("orig" %in% input$tipo_serie)   # fuerza que la original esté activa
    
    serie <- input$serie_elegida
    info  <- mapa_dataset[[serie]]
    
    df_o <- info$df %>%
      mutate(Mes = factor(Mes, levels = Mes_orden, ordered = TRUE)) %>%
      arrange(Anho, Mes) %>%
      filter(
        Anho >= input$anho_rango[1],
        Anho <= input$anho_rango[2]
      )
    
    validate(need(nrow(df_o) >= 24,
                  "Se requieren al menos 24 observaciones para STL."))
    
    ts_orig <- ts(
      df_o[[info$col]],
      start     = c(df_o$Anho[1],
                    match(as.character(df_o$Mes[1]), Mes_orden)),
      frequency = 12
    )
    
    stl(ts_orig, s.window = "periodic")
  })
  
  output$plot_stl <- renderPlotly({
    validate(
      need("orig" %in% input$tipo_serie,
           "Activa “Serie original” para ver la descomposición STL.")
    )
    
    descomp <- Reactive_STL()
    
    p <- forecast::autoplot(descomp) +
      labs(title = paste("Descomposición STL –", input$serie_elegida),
           x = "Tiempo", y = "")
    
    ggplotly(p) |> config(displayModeBar = FALSE)
  })
  
  
  
  
  
  
  
  
  output$descargar_reporte <- downloadHandler(
    filename = function() {
      paste0("Reporte_Análisis_Exploratorio_Economía_Chile_", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "reporte_dashboard.Rmd")
      # Asegúrate de que el archivo reporte_dashboard.Rmd esté en el mismo directorio que app.R
      # o proporciona la ruta completa si está en otro lugar.
      file.copy("reporte_dashboard.Rmd", tempReport, overwrite = TRUE)
      
      # Prepara los parámetros para pasar a Rmd
      params <- list(
        anho_rango_shiny = input$anho_rango,
        filtro_categoria_shiny = input$filtro_categoria,
        x_var_shiny = input$x_var,
        y_var_shiny = input$y_var,
        tipo_visual_tpm_shiny = input$tipo_visual_tpm,
        ver_migracion_shiny = input$ver_migracion, # Añadido
        graficos_sector_externo_shiny = input$graficos_sector_externo, # Añadido
        tipo_serie_shiny = input$tipo_serie, # Añadido
        serie_elegida_shiny = input$serie_elegida # Añadido
      )
      
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



