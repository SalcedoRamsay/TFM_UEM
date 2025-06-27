# ========================================================
# POST MATCH REPORT - SHINY APP
# ========================================================
# Aplicación Shiny para generar reportes post-partido
# con selección de competición, temporada y partido
# ========================================================

library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(magick)
library(lubridate)
library(png)
library(ggrepel)
library(shinycssloaders)

# Cargar funciones modulares
source("report_plot_functions.R")
source("generate_full_post_match_report.R")

# ========================================================
# INTERFAZ DE USUARIO (UI)
# ========================================================

ui <- fluidPage(
  
  # Habilitar shinyjs
  useShinyjs(),
  
  # Título de la aplicación
  titlePanel(
    div(
      style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #1A78CF, #64B5F6); color: white; border-radius: 10px; margin-bottom: 20px;",
      h1("POST MATCH REPORT", style = "margin: 0; font-weight: bold;"),
      p("Comprehensive football match analysis", style = "margin: 5px 0 0 0; font-size: 16px; opacity: 0.9;")
    )
  ),
  
  # Layout principal con sidebar
  sidebarLayout(
    
    # Panel izquierdo - Controles
    sidebarPanel(
      width = 3,
      style = "background-color: #f8f9fa; border-radius: 10px; padding: 20px;",
      
      h3("Match Selection", style = "color: #333; margin-bottom: 20px;"),
      
      # Selector de competición
      selectInput(
        "competition",
        label = "Competition:",
        choices = NULL,
        selected = NULL
      ),
      
      # Selector de temporada
      selectInput(
        "season",
        label = "Season:",
        choices = NULL,
        selected = NULL
      ),
      
      # Selector de partido
      selectInput(
        "match",
        label = "Match:",
        choices = NULL,
        selected = NULL
      ),
      
      # Información del partido seleccionado (bloque informativo, no botón)
      div(
        style = "margin-top: 30px; padding: 15px; background-color: #f4f8fb; border-radius: 8px; border-left: 4px solid #1A78CF; box-shadow: 0 2px 8px rgba(0,0,0,0.03);",
        h4("Match Information", style = "color: #1A78CF; margin-bottom: 10px; font-weight: bold;"),
        uiOutput("match_info", style = "font-size: 14px; color: #333;")
      ),
      
      # Botón para generar el reporte
      div(
        style = "margin-top: 30px; text-align: center;",
        actionButton(
          "generate_report",
          label = "Generate Report",
          icon = icon("play"),
          style = "background-color: #007bff; color: white; border: none; padding: 12px 24px; border-radius: 6px; font-weight: bold; width: 100%; margin-bottom: 10px;"
        )
      ),
      
      # Botón de descarga (deshabilitado hasta que se genere el reporte)
      div(
        style = "margin-top: 10px; text-align: center;",
        uiOutput("download_report_ui")
      )
    ),
    
    # Panel principal - Visualización
    mainPanel(
      width = 9,
      uiOutput("panel_grid")
    )
  ),
  
  # Estilos CSS personalizados
  tags$head(
    tags$style(HTML("
      .shiny-download-link {
        background-color: #28a745 !important;
        color: white !important;
        border: none !important;
        padding: 12px 24px !important;
        border-radius: 6px !important;
        font-weight: bold !important;
        text-decoration: none !important;
        display: inline-block !important;
        width: 100% !important;
        text-align: center !important;
      }
      .shiny-download-link:hover {
        background-color: #218838 !important;
        color: white !important;
        text-decoration: none !important;
      }
    ")),
    tags$style(HTML('
      .panel-card {
        background: #fff;
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        margin-bottom: 18px;
        padding: 12px 8px 8px 8px;
        text-align: center;
      }
      .panel-title {
        color: #1A78CF;
        font-size: 18px;
        font-weight: bold;
        margin-bottom: 8px;
      }
    '))
  )
)

# ========================================================
# LÓGICA DEL SERVIDOR (SERVER)
# ========================================================

server <- function(input, output, session) {
  
  # Rutas de datos
  data_path <- "/Users/sebastiansalcedo/Documents/UEM/TFM/base_datos_completa_epl.csv"
  
  # Cargar datos
  data <- reactive({
    req(file.exists(data_path))
    read.csv(data_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  })
  
  # Actualizar selector de competiciones
  observe({
    req(data())
    competitions <- unique(data()$competition_known_name)
    competitions <- competitions[!is.na(competitions)]
    updateSelectInput(session, "competition", choices = c("Select competition" = "", competitions))
  })
  
  # Actualizar selector de temporadas
  observe({
    req(input$competition, input$competition != "")
    
    # Mostrar indicador de carga
    withProgress(message = 'Loading seasons...', value = 0, {
      incProgress(0.5)
      
      filtered_data <- data() %>% filter(competition_known_name == input$competition)
      seasons <- unique(filtered_data$season)
      seasons <- seasons[!is.na(seasons)]
      
      incProgress(0.5)
      
      seasons <- sort(seasons, decreasing = TRUE)
      updateSelectInput(session, "season", choices = c("Select season" = "", seasons))
    })
  })
  
  # Actualizar selector de partidos
  observe({
    req(input$competition, input$season, input$competition != "", input$season != "")
    
    withProgress(message = 'Loading matches...', value = 0, {
      incProgress(0.5)
      
      filtered_data <- data() %>% 
        filter(competition_known_name == input$competition, season == input$season)
      
      incProgress(0.5)
      
      matches <- filtered_data %>%
        group_by(match_id, local_date, local_time, team_home, team_away) %>%
        summarise(
          home_goals = sum(event == "Goal" & team_position == "home", na.rm = TRUE),
          away_goals = sum(event == "Goal" & team_position == "away", na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(
          match_label = paste0(
            team_home, " vs ", team_away, " (",
            home_goals, "-", away_goals, ") - ",
            format(as.Date(local_date), "%d/%m/%Y")
          )
        )
      
      match_choices <- setNames(matches$match_id, matches$match_label)
      updateSelectInput(session, "match", choices = c("Select match" = "", match_choices))
    })
  })
  
  # Actualizar información del partido
  observe({
    req(input$match, input$match != "")
    
    match_data <- data() %>% filter(match_id == input$match) %>% slice(1)
    
    if (nrow(match_data) > 0) {
      match_info_html <- paste0(
        "<strong>Home:</strong> ", match_data$team_home, "<br>",
        "<strong>Away:</strong> ", match_data$team_away, "<br>",
        "<strong>Date:</strong> ", format(as.Date(match_data$local_date), "%d/%m/%Y"), "<br>",
        "<strong>Stadium:</strong> ", match_data$venue_long_name, "<br>",
        "<strong>Competition:</strong> ", match_data$competition_known_name
      )
      
      output$match_info <- renderUI({
        HTML(match_info_html)
      })
    }
  })
  
  # Estado reactivo para el reporte generado
  generated_report <- reactiveVal(NULL)
  
  # Controla si el usuario ya intentó generar un reporte
  generacion_intentada <- reactiveVal(FALSE)
  
  # Estado reactivo para todos los paneles
  goalkick_home_panel <- reactiveVal(NULL)
  goalkick_away_panel <- reactiveVal(NULL)
  shotmap_panel <- reactiveVal(NULL)
  crosses_home_panel <- reactiveVal(NULL)
  crosses_away_panel <- reactiveVal(NULL)
  goalmouth_panel <- reactiveVal(NULL)
  zone14_home_panel <- reactiveVal(NULL)
  zone14_away_panel <- reactiveVal(NULL)
  defensive_panel <- reactiveVal(NULL)
  
  # Estado reactivo para controlar el estado de la descarga
  descarga_en_progreso <- reactiveVal(FALSE)
  descarga_lista <- reactiveVal(FALSE)
  
  observeEvent(input$generate_report, {
    descarga_en_progreso(TRUE)
    descarga_lista(FALSE)
    tryCatch({
      match_id <- input$match
      datos <- read.csv(data_path, stringsAsFactors = FALSE, encoding = "UTF-8")
      datos <- datos[datos$match_id == match_id, ]
      home_team <- unique(datos[datos$team_position == "home", "team_name"])[1]
      away_team <- unique(datos[datos$team_position == "away", "team_name"])[1]
      goalkick_home_panel(generate_goalkick_panel(match_id = match_id, is_home = TRUE, data_path = data_path, title = "Goalkick & GK Pass Home"))
      goalkick_away_panel(generate_goalkick_panel(match_id = match_id, is_home = FALSE, data_path = data_path, title = "Goalkick & GK Pass Away"))
      shotmap_panel(generate_shotmap_panel(match_id = match_id, data_path = data_path, title = "Shotmap"))
      crosses_home_panel(generate_crosses_buildup_panel(match_id = match_id, is_home = TRUE, team_name = home_team, data_path = data_path, title = "Crosses Home"))
      crosses_away_panel(generate_crosses_buildup_panel(match_id = match_id, is_home = FALSE, team_name = away_team, data_path = data_path, title = "Crosses Away"))
      goalmouth_panel(generate_goalmouth_panel(match_id = match_id, data_path = data_path, title = "Goalmouth"))
      zone14_home_panel(generate_zone14_panel(match_id = match_id, is_home = TRUE, team_name = home_team, data_path = data_path, title = "Pass Zone 14"))
      zone14_away_panel(generate_zone14_panel(match_id = match_id, is_home = FALSE, team_name = away_team, data_path = data_path, title = "Pass Zone 14"))
      defensive_panel(generate_convexhull_defensive_panel(match_id = match_id, data_path = data_path, title = "Defensive Actions"))
      # Generar el reporte completo para descarga
      report <- generate_full_post_match_report(match_id = match_id, data_path = data_path)
      generated_report(report)
      descarga_en_progreso(FALSE)
      descarga_lista(TRUE)
      showNotification("Report ready for download!", type = "message")
    }, error = function(e) {
      descarga_en_progreso(FALSE)
      descarga_lista(FALSE)
      showNotification(paste("Error generating report for download:", e$message), type = "error")
    })
  })
  
  # Renderizar cada panel en su pestaña
  tab_names <- c(
    "Goalkick & GK Pass Home", "Shotmap", "Goalkick & GK Pass Away",
    "Crosses Home", "Goalmouth", "Crosses Away",
    "Pass Zone 14 (Home)", "Defensive Actions", "Pass Zone 14 (Away)"
  )
  
  output$panel_grid <- renderUI({
    fluidRow(
      column(4,
        div(class = "panel-card", h4(tab_names[1], class = "panel-title"),
          if (is.null(goalkick_home_panel())) {
            div("(Panel vacío)", style = "color: #bbb; padding: 60px 0;")
          } else {
            withSpinner(plotOutput("goalkick_home_plot", height = "300px"), type = 6, color = "#1A78CF")
          }
        ),
        div(class = "panel-card", h4(tab_names[4], class = "panel-title"),
          if (is.null(crosses_home_panel())) {
            div("(Panel vacío)", style = "color: #bbb; padding: 60px 0;")
          } else {
            withSpinner(plotOutput("crosses_home_plot", height = "300px"), type = 6, color = "#1A78CF")
          }
        ),
        div(class = "panel-card", h4(tab_names[7], class = "panel-title"),
          if (is.null(zone14_home_panel())) {
            div("(Panel vacío)", style = "color: #bbb; padding: 60px 0;")
          } else {
            withSpinner(plotOutput("zone14_home_plot", height = "300px"), type = 6, color = "#1A78CF")
          }
        )
      ),
      column(4,
        div(class = "panel-card", h4(tab_names[2], class = "panel-title"),
          if (is.null(shotmap_panel())) {
            div("(Panel vacío)", style = "color: #bbb; padding: 60px 0;")
          } else {
            withSpinner(plotOutput("shotmap_plot", height = "300px"), type = 6, color = "#1A78CF")
          }
        ),
        div(class = "panel-card", h4(tab_names[5], class = "panel-title"),
          if (is.null(goalmouth_panel())) {
            div("(Panel vacío)", style = "color: #bbb; padding: 60px 0;")
          } else {
            withSpinner(plotOutput("goalmouth_plot", height = "300px"), type = 6, color = "#1A78CF")
          }
        ),
        div(class = "panel-card", h4(tab_names[8], class = "panel-title"),
          if (is.null(defensive_panel())) {
            div("(Panel vacío)", style = "color: #bbb; padding: 60px 0;")
          } else {
            withSpinner(plotOutput("defensive_plot", height = "300px"), type = 6, color = "#1A78CF")
          }
        )
      ),
      column(4,
        div(class = "panel-card", h4(tab_names[3], class = "panel-title"),
          if (is.null(goalkick_away_panel())) {
            div("(Panel vacío)", style = "color: #bbb; padding: 60px 0;")
          } else {
            withSpinner(plotOutput("goalkick_away_plot", height = "300px"), type = 6, color = "#1A78CF")
          }
        ),
        div(class = "panel-card", h4(tab_names[6], class = "panel-title"),
          if (is.null(crosses_away_panel())) {
            div("(Panel vacío)", style = "color: #bbb; padding: 60px 0;")
          } else {
            withSpinner(plotOutput("crosses_away_plot", height = "300px"), type = 6, color = "#1A78CF")
          }
        ),
        div(class = "panel-card", h4(tab_names[9], class = "panel-title"),
          if (is.null(zone14_away_panel())) {
            div("(Panel vacío)", style = "color: #bbb; padding: 60px 0;")
          } else {
            withSpinner(plotOutput("zone14_away_plot", height = "300px"), type = 6, color = "#1A78CF")
          }
        )
      )
    )
  })
  
  output$goalkick_home_plot <- renderPlot({ req(goalkick_home_panel()); grid::grid.newpage(); grid::grid.draw(goalkick_home_panel()) })
  output$goalkick_away_plot <- renderPlot({ req(goalkick_away_panel()); grid::grid.newpage(); grid::grid.draw(goalkick_away_panel()) })
  output$shotmap_plot <- renderPlot({ req(shotmap_panel()); grid::grid.newpage(); grid::grid.draw(shotmap_panel()) })
  output$crosses_home_plot <- renderPlot({ req(crosses_home_panel()); grid::grid.newpage(); grid::grid.draw(crosses_home_panel()) })
  output$crosses_away_plot <- renderPlot({ req(crosses_away_panel()); grid::grid.newpage(); grid::grid.draw(crosses_away_panel()) })
  output$goalmouth_plot <- renderPlot({ req(goalmouth_panel()); grid::grid.newpage(); grid::grid.draw(goalmouth_panel()) })
  output$zone14_home_plot <- renderPlot({ req(zone14_home_panel()); grid::grid.newpage(); grid::grid.draw(zone14_home_panel()) })
  output$zone14_away_plot <- renderPlot({ req(zone14_away_panel()); grid::grid.newpage(); grid::grid.draw(zone14_away_panel()) })
  output$defensive_plot <- renderPlot({ req(defensive_panel()); grid::grid.newpage(); grid::grid.draw(defensive_panel()) })
  
  # Botón de descarga solo habilitado si el reporte está listo
  observe({
    shinyjs::toggleState("download_report", condition = descarga_lista())
  })
  
  # Spinner o mensaje en el botón de descarga
  output$download_report_ui <- renderUI({
    if (descarga_en_progreso()) {
      tags$button(class = "btn btn-success shiny-download-link", disabled = NA,
        span(class = "spinner-border spinner-border-sm", role = "status", ""),
        " Generating file..."
      )
    } else {
      downloadButton(
        "download_report",
        label = if (descarga_lista()) "Download Report" else "Download Report",
        style = "background-color: #28a745; color: white; border: none; padding: 12px 24px; border-radius: 6px; font-weight: bold; width: 100%;"
      )
    }
  })
  
  # Descargar reporte
  output$download_report <- downloadHandler(
    filename = function() {
      req(input$match, input$match != "")
      match_data <- data() %>% filter(match_id == input$match) %>% slice(1)
      clean_date <- format(as.Date(match_data$local_date), "%Y%m%d")
      paste0("post_match_report_", 
             gsub(" ", "_", match_data$team_home), "_vs_", 
             gsub(" ", "_", match_data$team_away), "_",
             clean_date, ".png")
    },
    content = function(file) {
      req(generated_report())
      ggsave(file, generated_report(), width = 16, height = 20, dpi = 300, bg = "#F5F5F5")
    }
  )
}

# ========================================================
# EJECUTAR APLICACIÓN
# ========================================================

shinyApp(ui = ui, server = server) 