# Aplicación Shiny para Análisis Estadístico
# Esta aplicación permite:
# 1. Cargar archivos Excel y Word
# 2. Mostrar estadísticas descriptivas
# 3. Realizar análisis ANOVA y pruebas t

# Instalar paquetes necesarios (descomenta si es necesario)
# install.packages(c("shiny", "shinydashboard", "readxl", "officer", "ggplot2", "dplyr", 
#                    "DT", "tidyr", "car", "openxlsx"))

# Cargar librerías
library(shiny)
library(shinydashboard)
library(readxl)
library(officer)      # Para archivos Word
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(car)          # Para análisis estadísticos
library(openxlsx)     # Para exportar resultados

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Análisis Estadístico"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cargar Datos", tabName = "cargar", icon = icon("upload")),
      menuItem("Estadísticas Descriptivas", tabName = "descriptivas", icon = icon("chart-simple")),
      menuItem("Pruebas Estadísticas", tabName = "pruebas", icon = icon("calculator")),
      menuItem("Visualización", tabName = "visualizacion", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Pestaña para cargar datos
      tabItem(tabName = "cargar",
              fluidRow(
                box(
                  title = "Cargar Archivos",
                  width = 12,
                  fileInput("archivo_excel", "Seleccionar archivo Excel",
                            accept = c(".xlsx", ".xls")),
                  fileInput("archivo_word", "Seleccionar archivo Word (opcional)",
                            accept = c(".docx")),
                  actionButton("cargar_btn", "Cargar y Procesar", 
                               icon = icon("play"), 
                               class = "btn-primary")
                )
              ),
              fluidRow(
                box(
                  title = "Vista previa de los datos",
                  width = 12,
                  DTOutput("tabla_datos")
                )
              )
      ),
      
      # Pestaña de estadísticas descriptivas
      tabItem(tabName = "descriptivas",
              fluidRow(
                box(
                  title = "Selección de Variables",
                  width = 3,
                  uiOutput("selector_variables_num"),
                  uiOutput("selector_factor"),
                  actionButton("calcular_btn", "Calcular Estadísticas", 
                               icon = icon("calculator"),
                               class = "btn-success")
                ),
                box(
                  title = "Estadísticas Descriptivas",
                  width = 9,
                  DTOutput("tabla_estadisticas"),
                  downloadButton("descargar_est", "Descargar Estadísticas")
                )
              )
      ),
      
      # Pestaña de pruebas estadísticas
      tabItem(tabName = "pruebas",
              fluidRow(
                box(
                  title = "Configuración de Pruebas",
                  width = 3,
                  selectInput("tipo_prueba", "Tipo de Prueba:",
                              choices = c("Prueba t" = "t_test", 
                                          "ANOVA" = "anova")),
                  uiOutput("variables_prueba"),
                  actionButton("ejecutar_prueba", "Ejecutar Prueba", 
                               icon = icon("play"),
                               class = "btn-primary")
                ),
                box(
                  title = "Resultados de la Prueba",
                  width = 9,
                  verbatimTextOutput("resultado_prueba"),
                  downloadButton("descargar_prueba", "Descargar Resultados")
                )
              )
      ),
      
      # Pestaña de visualización
      tabItem(tabName = "visualizacion",
              fluidRow(
                box(
                  title = "Configuración de Gráfico",
                  width = 3,
                  selectInput("tipo_grafico", "Tipo de Gráfico:",
                              choices = c("Diagrama de Cajas" = "boxplot", 
                                          "Histograma" = "histogram",
                                          "Gráfico de Barras" = "barplot",
                                          "Gráfico de Dispersión" = "scatter")),
                  uiOutput("vars_grafico"),
                  actionButton("generar_grafico", "Generar Gráfico", 
                               icon = icon("chart-bar"),
                               class = "btn-info")
                ),
                box(
                  title = "Gráfico",
                  width = 9,
                  plotOutput("grafico_output", height = "500px"),
                  downloadButton("descargar_grafico", "Descargar Gráfico")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Almacenamiento reactivo para los datos
  datos <- reactiveVal(NULL)
  datos_word <- reactiveVal(NULL)
  
  # Función para cargar los datos
  observeEvent(input$cargar_btn, {
    # Verificar si se cargó un archivo Excel
    req(input$archivo_excel)
    
    # Leer archivo Excel
    archivo <- input$archivo_excel
    ext <- tools::file_ext(archivo$name)
    
    if(ext %in% c("xlsx", "xls")) {
      df <- readxl::read_excel(archivo$datapath)
      datos(df)
      showNotification("Archivo Excel cargado correctamente", type = "message")
    } else {
      showNotification("Formato de archivo no válido. Por favor, carga un archivo Excel.", 
                       type = "error")
    }
    
    # Leer archivo Word si está presente
    if(!is.null(input$archivo_word)) {
      archivo_word <- input$archivo_word
      ext_word <- tools::file_ext(archivo_word$name)
      
      if(ext_word == "docx") {
        # Extraer texto del archivo Word
        doc <- read_docx(archivo_word$datapath)
        contenido <- docx_summary(doc)
        datos_word(contenido)
        showNotification("Archivo Word cargado correctamente", type = "message")
      }
    }
  })
  
  # Vista previa de los datos
  output$tabla_datos <- renderDT({
    req(datos())
    datatable(datos(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # UI dinámico para selección de variables
  output$selector_variables_num <- renderUI({
    req(datos())
    vars_num <- names(datos())[sapply(datos(), is.numeric)]
    selectInput("vars_num", "Variables Numéricas (múltiples):",
                choices = vars_num, multiple = TRUE,
                selected = vars_num[1:min(3, length(vars_num))])
  })
  
  output$selector_factor <- renderUI({
    req(datos())
    vars_factor <- names(datos())[sapply(datos(), function(x) is.character(x) || is.factor(x))]
    if(length(vars_factor) > 0) {
      selectInput("var_factor", "Variable de Agrupación (opcional):",
                  choices = c("Ninguna" = "", vars_factor),
                  selected = "")
    }
  })
  
  # Calcular estadísticas descriptivas
  observeEvent(input$calcular_btn, {
    req(datos(), input$vars_num)
    
    df <- datos()
    vars <- input$vars_num
    
    if(input$var_factor != "") {
      # Estadísticas por grupo
      tabla_stats <- df %>%
        group_by(!!sym(input$var_factor)) %>%
        summarise(across(all_of(vars), 
                         list(
                           n = ~sum(!is.na(.)),
                           media = ~mean(., na.rm = TRUE),
                           mediana = ~median(., na.rm = TRUE),
                           desvest = ~sd(., na.rm = TRUE),
                           min = ~min(., na.rm = TRUE),
                           max = ~max(., na.rm = TRUE),
                           q1 = ~quantile(., 0.25, na.rm = TRUE),
                           q3 = ~quantile(., 0.75, na.rm = TRUE)
                         ), .names = "{.col}_{.fn}"))
    } else {
      # Estadísticas generales
      tabla_stats <- df %>%
        summarise(across(all_of(vars), 
                         list(
                           n = ~sum(!is.na(.)),
                           media = ~mean(., na.rm = TRUE),
                           mediana = ~median(., na.rm = TRUE),
                           desvest = ~sd(., na.rm = TRUE),
                           min = ~min(., na.rm = TRUE),
                           max = ~max(., na.rm = TRUE),
                           q1 = ~quantile(., 0.25, na.rm = TRUE),
                           q3 = ~quantile(., 0.75, na.rm = TRUE)
                         ), .names = "{.col}_{.fn}"))
    }
    
    # Reordenar para mejor visualización
    tabla_stats_ordenada <- tabla_stats %>%
      pivot_longer(cols = -any_of(input$var_factor),
                   names_to = c("variable", "estadistica"),
                   names_pattern = "(.)_(.)") %>%
      pivot_wider(names_from = "estadistica", 
                  values_from = "value") %>%
      select(any_of(c(input$var_factor, "variable", "n", "media", "mediana", 
                      "desvest", "min", "max", "q1", "q3")))
    
    output$tabla_estadisticas <- renderDT({
      datatable(tabla_stats_ordenada, 
                caption = "Estadísticas Descriptivas",
                options = list(scrollX = TRUE))
    })
  })
  
  # Descargar estadísticas
  output$descargar_est <- downloadHandler(
    filename = function() {
      paste("estadisticas_descriptivas_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      tabla_est <- datatableProxy("tabla_estadisticas")[["data"]]
      write.xlsx(tabla_est, file)
    }
  )
  
  # UI dinámico para pruebas estadísticas
  output$variables_prueba <- renderUI({
    req(datos())
    
    vars_num <- names(datos())[sapply(datos(), is.numeric)]
    vars_factor <- names(datos())[sapply(datos(), function(x) is.character(x) || is.factor(x))]
    
    if(input$tipo_prueba == "t_test") {
      tagList(
        selectInput("var_t_num", "Variable Numérica:",
                    choices = vars_num, selected = vars_num[1]),
        selectInput("var_t_grupo", "Variable de Agrupación (2 grupos):",
                    choices = vars_factor, selected = vars_factor[1])
      )
    } else if(input$tipo_prueba == "anova") {
      tagList(
        selectInput("var_anova_num", "Variable Numérica:", 
                    choices = vars_num, selected = vars_num[1]),
        selectInput("var_anova_grupo", "Variable de Agrupación:",
                    choices = vars_factor, selected = vars_factor[1])
      )
    }
  })
  
  # Ejecutar pruebas estadísticas
  observeEvent(input$ejecutar_prueba, {
    req(datos())
    
    df <- datos()
    
    if(input$tipo_prueba == "t_test") {
      req(input$var_t_num, input$var_t_grupo)
      
      # Verificar que hay exactamente dos grupos
      grupos <- unique(na.omit(df[[input$var_t_grupo]]))
      if(length(grupos) != 2) {
        output$resultado_prueba <- renderText({
          "Error: La prueba t requiere exactamente 2 grupos. La variable seleccionada tiene un número diferente de grupos."
        })
        return()
      }
      
      # Realizar prueba t
      formula <- as.formula(paste(input$var_t_num, "~", input$var_t_grupo))
      test_result <- t.test(formula, data = df)
      
      output$resultado_prueba <- renderPrint({
        test_result
      })
      
    } else if(input$tipo_prueba == "anova") {
      req(input$var_anova_num, input$var_anova_grupo)
      
      # Realizar ANOVA
      formula <- as.formula(paste(input$var_anova_num, "~", input$var_anova_grupo))
      modelo_anova <- aov(formula, data = df)
      
      output$resultado_prueba <- renderPrint({
        cat("Resultados del ANOVA:\n\n")
        print(summary(modelo_anova))
        
        cat("\n\nPrueba de Tukey (comparaciones múltiples):\n")
        print(TukeyHSD(modelo_anova))
        
        cat("\n\nPrueba de homogeneidad de varianzas (Levene):\n")
        print(leveneTest(formula, data = df))
      })
    }
  })
  
  # Descargar resultados de prueba
  output$descargar_prueba <- downloadHandler(
    filename = function() {
      paste("resultados_", input$tipo_prueba, "_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      resultado <- capture.output(eval(parse(text = paste("output$resultado_prueba$result"))))
      writeLines(resultado, file)
    }
  )
  
  # UI dinámico para gráficos
  output$vars_grafico <- renderUI({
    req(datos())
    
    vars_num <- names(datos())[sapply(datos(), is.numeric)]
    vars_factor <- names(datos())[sapply(datos(), function(x) is.character(x) || is.factor(x))]
    
    if(input$tipo_grafico == "boxplot") {
      tagList(
        selectInput("var_box_y", "Variable Numérica:", 
                    choices = vars_num, selected = vars_num[1]),
        selectInput("var_box_x", "Variable de Agrupación:",
                    choices = vars_factor, selected = vars_factor[1])
      )
    } else if(input$tipo_grafico == "histogram") {
      tagList(
        selectInput("var_hist", "Variable Numérica:", 
                    choices = vars_num, selected = vars_num[1]),
        sliderInput("bins", "Número de Bins:", min = 5, max = 50, value = 20),
        checkboxInput("add_density", "Añadir curva de densidad", FALSE)
      )
    } else if(input$tipo_grafico == "barplot") {
      tagList(
        selectInput("var_bar_x", "Variable Categórica:", 
                    choices = vars_factor, selected = vars_factor[1]),
        selectInput("var_bar_fill", "Variable de Color (opcional):",
                    choices = c("Ninguna" = "", vars_factor), selected = "")
      )
    } else if(input$tipo_grafico == "scatter") {
      tagList(
        selectInput("var_scat_x", "Variable X:", 
                    choices = vars_num, selected = vars_num[1]),
        selectInput("var_scat_y", "Variable Y:",
                    choices = vars_num, selected = vars_num[min(2, length(vars_num))]),
        selectInput("var_scat_color", "Variable de Color (opcional):",
                    choices = c("Ninguna" = "", c(vars_num, vars_factor)), selected = "")
      )
    }
  })
  
  # Generar gráficos
  observeEvent(input$generar_grafico, {
    req(datos())
    
    df <- datos()
    
    if(input$tipo_grafico == "boxplot") {
      req(input$var_box_y, input$var_box_x)
      
      output$grafico_output <- renderPlot({
        ggplot(df, aes_string(x = input$var_box_x, y = input$var_box_y)) +
          geom_boxplot(fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Diagrama de Cajas de", input$var_box_y, "por", input$var_box_x),
               x = input$var_box_x,
               y = input$var_box_y) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
    } else if(input$tipo_grafico == "histogram") {
      req(input$var_hist)
      
      output$grafico_output <- renderPlot({
        p <- ggplot(df, aes_string(x = input$var_hist)) +
          geom_histogram(bins = input$bins, fill = "steelblue", color = "black", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Histograma de", input$var_hist),
               x = input$var_hist,
               y = "Frecuencia")
        
        if(input$add_density) {
          p <- p + geom_density(alpha = 0.2, fill = "red")
        }
        
        p
      })
      
    } else if(input$tipo_grafico == "barplot") {
      req(input$var_bar_x)
      
      output$grafico_output <- renderPlot({
        if(input$var_bar_fill != "") {
          ggplot(df, aes_string(x = input$var_bar_x, fill = input$var_bar_fill)) +
            geom_bar(position = "dodge") +
            theme_minimal() +
            labs(title = paste("Gráfico de Barras de", input$var_bar_x, "por", input$var_bar_fill),
                 x = input$var_bar_x,
                 y = "Conteo") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
          ggplot(df, aes_string(x = input$var_bar_x)) +
            geom_bar(fill = "steelblue") +
            theme_minimal() +
            labs(title = paste("Gráfico de Barras de", input$var_bar_x),
                 x = input$var_bar_x,
                 y = "Conteo") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
      })
      
    } else if(input$tipo_grafico == "scatter") {
      req(input$var_scat_x, input$var_scat_y)
      
      output$grafico_output <- renderPlot({
        if(input$var_scat_color != "") {
          ggplot(df, aes_string(x = input$var_scat_x, y = input$var_scat_y, 
                                color = input$var_scat_color)) +
            geom_point(size = 3, alpha = 0.7) +
            theme_minimal() +
            labs(title = paste("Gráfico de Dispersión de", input$var_scat_x, "vs", input$var_scat_y),
                 x = input$var_scat_x,
                 y = input$var_scat_y)
        } else {
          ggplot(df, aes_string(x = input$var_scat_x, y = input$var_scat_y)) +
            geom_point(size = 3, alpha = 0.7, color = "steelblue") +
            theme_minimal() +
            labs(title = paste("Gráfico de Dispersión de", input$var_scat_x, "vs", input$var_scat_y),
                 x = input$var_scat_x,
                 y = input$var_scat_y)
        }
      })
    }
  })
  
  # Descargar gráfico
  output$descargar_grafico <- downloadHandler(
    filename = function() {
      paste("grafico_", input$tipo_grafico, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 8, units = "in")
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)