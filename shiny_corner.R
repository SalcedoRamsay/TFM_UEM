library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(ggplot2)
library(ggforce)
library(htmltools)
library(shinyWidgets)

# ===== CONFIGURACIÓN INICIAL =====
# Cargar datos
data <- read.csv('/Users/sebastiansalcedo/Documents/UEM/TFM/base_datos_completa_epl.csv', 
                 stringsAsFactors = FALSE, 
                 encoding = "UTF-8")

# Transformar datos
data <- data %>%
  mutate(
    Team = team_name,        # Para filtrar equipos (nombres largos)
    MatchID = match_id,      # Para identificar partidos
    MatchDay = matchday,     # Para mostrar matchday
    HomeTeam = team_home,    # Para descripción de partidos
    AwayTeam = team_away     # Para descripción de partidos
  )

# ===== PALETA DE COLORES =====
colors <- list(
  background = "#F5F5F5",
  background_light = "#FFFFFF",
  text_primary = "#333333",
  corner_point = "#FF9800",        # Naranja para corners
  heatmap_start = "#E3F2FD",       # Azul claro para heatmap
  heatmap_end = "#1976D2",         # Azul para heatmap
  left_corner = "#4CAF50",         # Verde para corner izquierdo
  right_corner = "#F44336",        # Rojo para corner derecho
  lines = "#000000"
)

# ===== FUNCIONES AUXILIARES =====
# Función para dibujar campo de fútbol
draw_soccer_field <- function(fill_color = colors$background_light, line_color = colors$lines) {
  ggplot() +
    # Campo principal
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), 
              fill = fill_color, color = line_color, linewidth = 1) +
    # Línea central
    geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), 
                 color = line_color, linewidth = 0.5) +
    # Círculo central
    geom_circle(aes(x0 = 50, y0 = 50, r = 9.15), 
                color = line_color, fill = NA, linewidth = 0.5) +
    # Punto central
    geom_point(aes(x = 50, y = 50), size = 0.8, color = line_color) +
    # Áreas grandes
    geom_rect(aes(xmin = 0, xmax = 16.5, ymin = 21.1, ymax = 78.9), 
              fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_rect(aes(xmin = 83.5, xmax = 100, ymin = 21.1, ymax = 78.9), 
              fill = fill_color, color = line_color, linewidth = 0.5) +
    # Áreas pequeñas
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = 36.8, ymax = 63.2), 
              fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_rect(aes(xmin = 94.5, xmax = 100, ymin = 36.8, ymax = 63.2), 
              fill = fill_color, color = line_color, linewidth = 0.5) +
    # Puntos de penalty
    geom_point(aes(x = 11, y = 50), size = 0.8, color = line_color) +
    geom_point(aes(x = 89, y = 50), size = 0.8, color = line_color) +
    # Semicírculos de áreas
    geom_arc(aes(x0 = 11, y0 = 50, r = 9.15, start = 37*pi/180, end = 143*pi/180), 
             color = line_color, linewidth = 0.5) +
    geom_arc(aes(x0 = 89, y0 = 50, r = 9.15, start = -37*pi/180, end = -143*pi/180), 
             color = line_color, linewidth = 0.5) +
    # Líneas de tercios (punteadas)
    geom_vline(xintercept = c(33.33, 66.66), 
               color = line_color, linetype = "dotted", alpha = 0.5) +
    # Marcar esquinas con pequeños arcos
    geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = 0, end = pi/2), 
             color = line_color, linewidth = 0.3) +
    geom_arc(aes(x0 = 100, y0 = 0, r = 1, start = pi/2, end = pi), 
             color = line_color, linewidth = 0.3) +
    geom_arc(aes(x0 = 0, y0 = 100, r = 1, start = -pi/2, end = 0), 
             color = line_color, linewidth = 0.3) +
    geom_arc(aes(x0 = 100, y0 = 100, r = 1, start = pi, end = 3*pi/2), 
             color = line_color, linewidth = 0.3) +
    coord_fixed() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = colors$background_light, color = NA),
      panel.background = element_rect(fill = colors$background_light, color = NA)
    )
}

# Función para crear zonas del campo (heatmap)
create_field_zones <- function() {
  bin_width <- 100 / 10   # Más zonas para corners
  bin_height <- 100 / 6   # Más zonas para corners
  
  zones <- expand.grid(bin_x = 0:9, bin_y = 0:5) %>%
    mutate(
      xmin = bin_x * bin_width,
      xmax = (bin_x + 1) * bin_width,
      ymin = bin_y * bin_height,
      ymax = (bin_y + 1) * bin_height,
      center_x = (xmin + xmax) / 2,
      center_y = (ymin + ymax) / 2,
      id = bin_x + bin_y * 10
    )
  
  return(zones)
}

# Función para obtener tiros después de corners
get_shots_after_corners <- function(match_data, team_name_filter, matchdays) {
  # Eventos de tiro CORREGIDOS
  shot_events <- c("Miss", "Goal", "Post", "Attempt Saved")
  
  # Obtener corners del equipo
  corners <- get_corners(match_data, team_name_filter, matchdays)
  
  if(nrow(corners) == 0) return(data.frame())
  
  shots_after_corners <- list()
  
  # Procesar cada partido por separado
  for(current_matchday in matchdays) {
    
    # Datos ordenados por tiempo SOLO del partido actual
    ordered_data <- match_data %>%
      filter(matchday_num == current_matchday) %>%
      arrange(period_id, time_min, time_sec) %>%
      mutate(row_number = row_number())
    
    # Corners SOLO del partido actual
    match_corners <- corners %>%
      filter(matchday_num == current_matchday)
    
    if(nrow(match_corners) == 0) next
    
    for(i in 1:nrow(match_corners)) {
      corner <- match_corners[i,]
      
      # Encontrar la posición del corner en los datos ordenados del partido
      corner_row <- which(ordered_data$general_id == corner$general_id)
      
      if(length(corner_row) == 0) next
      
      corner_time <- corner$time_min * 60 + corner$time_sec
      max_time_limit <- 3  # 3 segundos después del corner
      
      # Buscar tiros del PROPIO EQUIPO después del corner
      for(j in (corner_row + 1):min(corner_row + 50, nrow(ordered_data))) {
        if(j > nrow(ordered_data)) break
        
        next_event <- ordered_data[j,]
        event_time <- next_event$time_min * 60 + next_event$time_sec
        time_elapsed <- event_time - corner_time
        
        # Si pasa el tiempo límite, parar
        if(time_elapsed > max_time_limit) break
        
        # Si cambia el período, parar
        if(next_event$period_id != corner$period_id) break
        
        # Si encontramos un tiro del PROPIO EQUIPO
        if(next_event$Team == team_name_filter && next_event$event %in% shot_events) {
          
          # Verificar que el tiro tenga coordenadas válidas
          shot_x <- as.numeric(next_event$x)
          shot_y <- as.numeric(next_event$y)
          
          if(!is.na(shot_x) && !is.na(shot_y) && shot_x >= 0 && shot_x <= 100 && shot_y >= 0 && shot_y <= 100) {
            
            shot_info <- next_event %>%
              mutate(
                corner_id = corner$general_id,
                corner_player = corner$player_name,
                time_from_corner = time_elapsed,
                shot_x = shot_x,
                shot_y = shot_y,
                corner_side = corner$corner_simple,
                corner_end_zone = corner$end_area
              )
            
            shots_after_corners[[length(shots_after_corners) + 1]] <- shot_info
          }
          
          break  # Solo tomar el primer tiro después del corner
        }
      }
    }
  }
  
  if(length(shots_after_corners) > 0) {
    return(bind_rows(shots_after_corners))
  } else {
    return(data.frame())
  }
}
get_corners <- function(match_data, team_name_filter, matchdays) {
  corner_events <- match_data %>%
    filter(
      Team == team_name_filter,     # Equipo seleccionado
      event == "Pass",              # Evento es Pass
      Corner.taken == "1",          # Corner.taken es 1
      matchday_num %in% matchdays
    ) %>%
    mutate(
      x_start = as.numeric(x),
      y_start = as.numeric(y),
      x_end = as.numeric(Pass.End.X),
      y_end = as.numeric(Pass.End.Y),
      # Determinar lado del corner basado en la posición inicial
      corner_side = case_when(
        x_start >= 90 & y_start <= 15 ~ "Right Side Corner",    # Banda derecha del campo
        x_start >= 90 & y_start >= 85 ~ "Left Side Corner",     # Banda izquierda del campo
        x_start <= 10 & y_start <= 15 ~ "Own Right Corner",     # Esquina propia derecha (no debería haber)
        x_start <= 10 & y_start >= 85 ~ "Own Left Corner",      # Esquina propia izquierda (no debería haber)
        TRUE ~ paste0("Unknown Corner (x:", round(x_start, 1), ", y:", round(y_start, 1), ")")
      ),
      # Determinar lado simplificado para análisis
      corner_simple = case_when(
        x_start >= 90 & y_start <= 15 ~ "Right Side",    # Banda derecha
        x_start >= 90 & y_start >= 85 ~ "Left Side",     # Banda izquierda
        x_start <= 10 & y_start <= 15 ~ "Own Right",     # Propio derecho
        x_start <= 10 & y_start >= 85 ~ "Own Left",      # Propio izquierdo
        TRUE ~ "Unknown"
      ),
      # Determinar área del campo donde termina el corner
      end_zone = case_when(
        x_end <= 33.33 ~ "Defensive Third",
        x_end <= 66.66 ~ "Middle Third", 
        x_end > 66.66 ~ "Attacking Third",
        TRUE ~ "Unknown"
      ),
      # Determinar área específica donde termina - NUEVA DIVISIÓN DETALLADA
      end_area = case_when(
        # Usar las coordenadas EXACTAS para las estadísticas (igual que el heatmap)
        x_end >= 83 & x_end <= 100 & y_end >= 78.9 & y_end <= 100 ~ "Out of Box Deep Left",
        x_end >= 94.2 & x_end <= 100 & y_end >= 63.2 & y_end <= 78.9 ~ "Box Deep Left", 
        x_end >= 83 & x_end < 94.2 & y_end >= 63.2 & y_end <= 78.9 ~ "Box Left",
        x_end >= 94.2 & x_end <= 100 & y_end >= 54.4 & y_end <= 63.2 ~ "Small Box Left",
        x_end >= 94.2 & x_end <= 100 & y_end >= 45.6 & y_end <= 54.4 ~ "Small Box",
        x_end >= 94.2 & x_end <= 100 & y_end >= 36.8 & y_end <= 45.6 ~ "Small Box Right",
        x_end >= 83 & x_end < 94.2 & y_end >= 36.8 & y_end <= 63.2 ~ "Box Center",
        x_end >= 83 & x_end < 94.2 & y_end >= 21.1 & y_end <= 36.8 ~ "Box Right",
        x_end >= 94.2 & x_end <= 100 & y_end >= 21.1 & y_end <= 36.8 ~ "Box Deep Right",
        x_end >= 83 & x_end <= 100 & y_end >= 0 & y_end <= 21.1 ~ "Out of Box Deep Right",
        x_end >= 66.66 & x_end < 83 & y_end >= 78.9 & y_end <= 100 ~ "Out of Box Left",
        x_end >= 66.66 & x_end < 83 & y_end >= 21.1 & y_end <= 78.9 ~ "Out of Box Center",
        x_end >= 66.66 & x_end < 83 & y_end >= 0 & y_end <= 21.1 ~ "Out of Box Right",
        
        # Otras áreas del campo
        x_end >= 33.33 & x_end < 66.66 ~ "Middle Third",
        x_end >= 0 & x_end < 33.33 ~ "Defensive Third",
        
        TRUE ~ "Unknown Area"
      )
    ) %>%
    filter(
      !is.na(x_start) & !is.na(y_start) & 
        !is.na(x_end) & !is.na(y_end) &
        x_start >= 0 & x_start <= 100 & 
        y_start >= 0 & y_start <= 100 &
        x_end >= 0 & x_end <= 100 & 
        y_end >= 0 & y_end <= 100
    )
  
  return(corner_events)
}

# ===== UI =====
ui <- dashboardPage(
  dashboardHeader(title = "Set Pieces"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Corner Analysis", tabName = "analysis", icon = icon("chart-area"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
        }
        .select2-container {
          width: 100% !important;
        }
      "))
    ),
    
    tabItems(
      # Tab de análisis principal
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "Team Selection", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("selected_team", "Select Team:",
                              choices = NULL,
                              width = "100%"
                  )
                ),
                box(
                  title = "Season Selection", status = "info", solidHeader = TRUE, width = 6,
                  selectInput("selected_season", "Select Season:",
                              choices = NULL,
                              width = "100%"
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Match Selection", status = "warning", solidHeader = TRUE, width = 12,
                  column(12,
                         h4("Select Matches to Analyze:"),
                         DT::dataTableOutput("match_selector", height = "300px")
                  ),
                  hr(),
                  column(6,
                         actionButton("select_all", "Select All Matches", 
                                      class = "btn-info", style = "margin-right: 10px;"),
                         actionButton("clear_all", "Clear Selection", 
                                      class = "btn-secondary")
                  ),
                  column(6,
                         actionButton("update_analysis", "Update Analysis", 
                                      class = "btn-warning", style = "float: right;")
                  ),
                  br(), br(),
                  div(
                    style = "background-color: #f9f9f9; padding: 10px; border-radius: 5px;",
                    h5("Instructions:"),
                    p("• Select a team and season"),
                    p("• Check the matches you want to include in the analysis"),
                    p("• Click 'Update Analysis' to see corner heat map"),
                    p("• Use filters to analyze corners by side and area"),
                    p("• Heat map shows where corners end up most frequently")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Display Options", status = "success", solidHeader = TRUE, width = 12,
                  column(4,
                         selectInput("corner_side_filter", "Filter by Corner Side:",
                                     choices = c("All Corners" = "all",
                                                 "Right Side (Bottom Band)" = "Right Side", 
                                                 "Left Side (Top Band)" = "Left Side"),
                                     selected = "all")
                  ),
                  column(4,
                         materialSwitch("show_shots_after_corners", "Show Shots After Corners", 
                                        value = FALSE, status = "danger")
                  ),
                  br(),
                  div(
                    style = "background-color: #e8f5e8; padding: 10px; border-radius: 5px; border-left: 4px solid #4CAF50;",
                    h5("Corner Analysis Legend:"),
                    p("🔵 Heat Map: Shows where corner kicks end up (final ball position)"),
                    p("🟠 Corner Start Points: Shown automatically when filtering by specific side"),
                    p("🔴 Shot Points: Shows shots taken immediately after corners (max 3 seconds)"),
                    p("📊 Side Analysis: Right Side (bottom band y≤15) vs Left Side (top band y≥85)"),
                    p("🎯 Detailed Box Analysis: Small Box (94.2-100), Box areas (83.5-94.2), Out of Box (83.5-100)"),
                    p("📍 Zones: Center (45.6-54.4), Left (54.4-78.9), Right (21.1-45.6), Deep sides (outer areas)"),
                    p("Note: All teams attack left→right, so corners are from right side of field only")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Corner Heat Map Analysis", status = "success", solidHeader = TRUE, width = 12,
                  div(id = "wrap_corner_plot_panel",
                      plotlyOutput("corner_plot", height = "600px"),
                      br(),
                      actionButton("download_corner_plot_panel", "\U1F4BE Download Field Panel as PNG", class = "btn-success download-panel-btn", style = "margin-top: 10px; width:100%")
                  )
                )
              ),
              
              fluidRow(
                column(2,
                       box(
                         title = "Corner Statistics", status = "primary", solidHeader = TRUE, width = NULL,
                         tableOutput("corner_stats")
                       )
                ),
                column(2,
                       box(
                         title = "Side Analysis", status = "info", solidHeader = TRUE, width = NULL,
                         tableOutput("side_stats")
                       )
                ),
                column(2,
                       box(
                         title = "Target Areas", status = "warning", solidHeader = TRUE, width = NULL,
                         tableOutput("area_stats")
                       )
                ),
                column(3,
                       box(
                         title = "Corner Takers", status = "success", solidHeader = TRUE, width = NULL,
                         div(id = "wrap_corner_takers_panel",
                             DT::dataTableOutput("corner_takers_stats"),
                             br(),
                             actionButton("download_corner_takers_panel", "\U1F4BE Download Corner Takers as PNG", class = "btn-success download-panel-btn", style = "margin-top: 10px; width:100%")
                         )
                       )
                ),
                column(3,
                       box(
                         title = "Shot Takers After Corners", status = "danger", solidHeader = TRUE, width = NULL,
                         div(id = "wrap_shot_takers_panel",
                             DT::dataTableOutput("shot_takers_stats"),
                             br(),
                             actionButton("download_shot_takers_panel", "\U1F4BE Download Shot Takers as PNG", class = "btn-success download-panel-btn", style = "margin-top: 10px; width:100%")
                         )
                       )
                )
              )
      )
    )
  )
)

# ===== SERVER =====
server <- function(input, output, session) {
  
  # Preparar lista de equipos únicos
  teams_list <- data %>%
    select(Team) %>%
    distinct() %>%
    arrange(Team) %>%
    pull(Team)
  
  # Preparar lista de temporadas
  seasons_list <- data %>%
    select(season) %>%
    distinct() %>%
    arrange(season) %>%
    pull(season)
  
  # Actualizar opciones al iniciar
  observe({
    updateSelectInput(session, "selected_team",
                      choices = teams_list,
                      selected = if("Chelsea" %in% teams_list) "Chelsea" else teams_list[1]
    )
    
    updateSelectInput(session, "selected_season",
                      choices = seasons_list,
                      selected = seasons_list[1]
    )
  })
  
  # Datos reactivos filtrados por equipo y temporada
  team_season_data <- reactive({
    req(input$selected_team, input$selected_season)
    
    filtered_data <- data %>%
      filter(
        Team == input$selected_team,
        season == input$selected_season
      )
    
    return(filtered_data)
  })
  
  # Crear tabla de selección de partidos
  output$match_selector <- DT::renderDataTable({
    req(team_season_data())
    
    match_info <- team_season_data() %>%
      select(matchday_num, local_date, MatchDay, HomeTeam, AwayTeam) %>%
      distinct() %>%
      arrange(matchday_num) %>%
      mutate(
        MatchDescription = paste0(MatchDay, ": ", HomeTeam, " vs ", AwayTeam)
      ) %>%
      select(matchday_num, local_date, MatchDescription)
    
    DT::datatable(
      match_info,
      selection = list(mode = 'multiple', selected = 1:min(5, nrow(match_info))),
      options = list(
        pageLength = 10,
        scrollY = "250px",
        scrollCollapse = TRUE,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        paging = FALSE
      ),
      colnames = c("Matchday", "Date", "Match"),
      rownames = FALSE
    )
  })
  
  # Botones de selección
  observeEvent(input$select_all, {
    req(input$match_selector_rows_all)
    DT::dataTableProxy('match_selector') %>%
      DT::selectRows(input$match_selector_rows_all)
  })
  
  observeEvent(input$clear_all, {
    DT::dataTableProxy('match_selector') %>%
      DT::selectRows(NULL)
  })
  
  # Obtener matchdays seleccionados
  selected_matchdays <- reactive({
    req(team_season_data(), input$match_selector_rows_selected)
    
    match_info <- team_season_data() %>%
      select(matchday_num, local_date, description, team_home, team_away) %>%
      distinct() %>%
      arrange(matchday_num)
    
    selected_rows <- input$match_selector_rows_selected
    if(length(selected_rows) > 0) {
      return(match_info$matchday_num[selected_rows])
    } else {
      return(integer(0))
    }
  })
  
  # Datos reactivos de tiros después de corners
  shots_after_corners_data <- eventReactive(input$update_analysis, {
    req(team_season_data(), selected_matchdays())
    
    if(length(selected_matchdays()) == 0) {
      return(data.frame())
    }
    
    shots <- get_shots_after_corners(team_season_data(), input$selected_team, selected_matchdays())
    return(shots)
  }, ignoreNULL = FALSE)
  corner_data <- eventReactive(input$update_analysis, {
    req(team_season_data(), selected_matchdays())
    
    if(length(selected_matchdays()) == 0) {
      return(data.frame())
    }
    
    corners <- get_corners(team_season_data(), input$selected_team, selected_matchdays())
    return(corners)
  }, ignoreNULL = FALSE)
  
  # Crear gráfico de corners
  output$corner_plot <- renderPlotly({
    req(corner_data())
    
    corners <- corner_data()
    shots_data <- shots_after_corners_data()
    
    # Definir total_shots AQUÍ para evitar problemas de scope
    total_shots <- ifelse(is.data.frame(shots_data), nrow(shots_data), 0)
    
    if(nrow(corners) == 0) {
      p <- draw_soccer_field() +
        labs(title = paste("Corner Analysis -", input$selected_team),
             subtitle = "No corners found for selected matches")
      return(ggplotly(p))
    }
    
    # Filtrar por posición si está seleccionado
    if(input$corner_side_filter != "all") {
      corners <- corners %>%
        filter(corner_simple == input$corner_side_filter)
    }
    
    # Filtrar por posición si está seleccionado
    if(input$corner_side_filter != "all") {
      corners <- corners %>%
        filter(corner_simple == input$corner_side_filter)
    }
    
    if(nrow(corners) == 0) {
      p <- draw_soccer_field() +
        labs(title = paste("Corner Analysis -", input$selected_team),
             subtitle = paste("No corners found for", input$corner_side_filter))
      return(ggplotly(p))
    }
    
    # Ahora usar directamente end_area en lugar de recalcular
    corner_counts <- corners %>%
      count(end_area) %>%
      filter(!end_area %in% c("Unknown Area", "Middle Third", "Defensive Third"))
    
    # Definir rectángulos para dibujar - usando TUS coordenadas exactas
    zone_rectangles <- data.frame(
      end_area = c("Out of Box Deep Left", "Box Deep Left", "Box Left", "Small Box Left", 
                   "Small Box", "Small Box Right", "Box Center", "Box Right", "Box Deep Right", 
                   "Out of Box Deep Right", "Out of Box Left", "Out of Box Center", "Out of Box Right"),
      xmin = c(83, 94.2, 83, 94.2, 94.2, 94.2, 83, 83, 94.2, 83, 66.66, 66.66, 66.66),
      xmax = c(100, 100, 94.2, 100, 100, 100, 94.2, 94.2, 100, 100, 83, 83, 83),
      ymin = c(78.9, 63.2, 63.2, 54.4, 45.6, 36.8, 36.8, 21.1, 21.1, 0, 78.9, 21.1, 0),
      ymax = c(100, 78.9, 78.9, 63.2, 54.4, 45.6, 63.2, 36.8, 36.8, 21.1, 100, 78.9, 21.1),
      stringsAsFactors = FALSE
    ) %>%
      left_join(corner_counts, by = "end_area") %>%
      mutate(
        n = ifelse(is.na(n), 0, n),
        center_x = (xmin + xmax) / 2,
        center_y = (ymin + ymax) / 2
      )
    
    # Colores del heatmap
    heatmap_colors <- colorRampPalette(c(colors$heatmap_start, colors$heatmap_end))(20)
    
    # Crear gráfico base
    p <- draw_soccer_field()
    
    # Heatmap con TUS zonas específicas
    p <- p +
      geom_rect(
        data = zone_rectangles %>% filter(n > 0),
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n,
            text = paste0(end_area, "\nCorners: ", n)),
        color = "white", linetype = "solid", size = 0.3, alpha = 0.8
      ) +
      scale_fill_gradientn(colors = heatmap_colors, 
                           name = "Corners",
                           guide = guide_colorbar(title.position = "top")) +
      geom_text(
        data = zone_rectangles %>% filter(n > 0),
        aes(x = center_x, y = center_y, label = n),
        color = "white", fontface = "bold", size = 3
      )
    
    # Añadir puntos de inicio de corner si se seleccionó un lado específico
    if(input$corner_side_filter %in% c("Right Side", "Left Side")) {
      p <- p +
        geom_point(
          data = corners,
          aes(x = x_start, y = y_start,
              text = paste("🟠 Corner Start<br>",
                           "Player:", player_name, "<br>",
                           "Time:", time_min, ":", sprintf("%02d", time_sec), "<br>",
                           "Side:", corner_side, "<br>",
                           "End Zone:", end_area)),
          color = colors$corner_point, size = 2, shape = 17,
          alpha = 0.8, stroke = 1
        )
    }
    
    # Añadir tiros después de corners si está activado
    if(input$show_shots_after_corners && nrow(shots_data) > 0) {
      
      # Filtrar shots por lado si está seleccionado
      filtered_shots <- shots_data
      if(input$corner_side_filter != "all") {
        filtered_shots <- shots_data %>%
          filter(corner_side == input$corner_side_filter)
      }
      
      if(nrow(filtered_shots) > 0) {
        p <- p +
          geom_point(
            data = filtered_shots,
            aes(x = shot_x, y = shot_y,
                text = paste("🔴 Shot after Corner<br>",
                             "Shooter:", player_name, "<br>",
                             "Shot Type:", event, "<br>",
                             "Time:", time_min, ":", sprintf("%02d", time_sec), "<br>",
                             "Seconds from corner:", round(time_from_corner, 1), "<br>",
                             "Corner by:", corner_player)),
            color = "#DC3545", size = 1.5, shape = 16,
            alpha = 0.9, stroke = 0.5
          )
      }
    }
    total_corners <- nrow(corners)
    matches_analyzed <- length(selected_matchdays())
    right_side_corners <- sum(corners$corner_simple == "Right Side", na.rm = TRUE)  # Banda derecha
    left_side_corners <- sum(corners$corner_simple == "Left Side", na.rm = TRUE)    # Banda izquierda
    # Calcular corners a penalty area (todas las zonas dentro del área grande)
    penalty_zones <- c("Small Box", "Small Box Left", "Small Box Right", 
                       "Box Deep Left", "Box Deep Right", "Box Left", "Box Center", "Box Right")
    
    small_box_corners <- sum(corners$end_area %in% c("Small Box", "Small Box Left", "Small Box Right"), na.rm = TRUE)
    penalty_area_corners <- sum(corners$end_area %in% penalty_zones, na.rm = TRUE)
    
    # Flecha de dirección de ataque
    arrow_triangle <- data.frame(
      x = c(80, 77, 77, 80),
      y = c(95, 97, 93, 95)
    )
    
    p <- p +
      # Línea de la flecha
      geom_segment(
        aes(x = 20, y = 95, xend = 77, yend = 95),
        linewidth = 1.5, color = colors$corner_point
      ) +
      # Punta triangular de la flecha
      geom_polygon(data = arrow_triangle, aes(x = x, y = y), 
                   fill = colors$corner_point, color = colors$corner_point) +
      # Texto
      annotate("text", x = 50, y = 88, 
               label = "Your team's direction of attack",
               color = colors$corner_point,
               fontface = "bold", size = 4)
    
    # Títulos
    side_text <- ifelse(input$corner_side_filter == "all", "All Corners", input$corner_side_filter)
    
    subtitle_text <- paste(
      "Season:", input$selected_season, "| Matches:", matches_analyzed, "| Filter:", side_text, "\n",
      "Total Corners:", total_corners, 
      "| Right Side (Bottom Band):", right_side_corners, "| Left Side (Top Band):", left_side_corners, "\n",
      "To Penalty Area:", penalty_area_corners, "| To Small Box:", small_box_corners, 
      "| Shots after Corners:", total_shots, "| Avg per Match:", round(total_corners/matches_analyzed, 1)
    )
    
    p <- p +
      labs(
        title = paste("Corner Analysis - Heat Map of Final Ball Position -", input$selected_team),
        subtitle = subtitle_text
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.margin = margin(t = 10),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
      )
    
    ggplotly(p) %>%
      layout(
        showlegend = TRUE,
        margin = list(l = 50, r = 50, t = 100, b = 100)
      )
  })
  
  # Estadísticas de corners
  output$corner_stats <- renderTable({
    req(corner_data())
    
    corners <- corner_data()
    
    if(nrow(corners) == 0) {
      return(data.frame(Metric = "No corners found", Value = "0"))
    }
    
    # Filtrar por lado si está seleccionado
    if(input$corner_side_filter != "all") {
      corners <- corners %>%
        filter(corner_simple == input$corner_side_filter)
    }
    
    if(nrow(corners) == 0) {
      return(data.frame(Metric = paste("No corners found for", input$corner_side_filter), Value = "0"))
    }
    
    stats <- data.frame(
      Metric = c("Total Corners", "Matches Analyzed", "Avg per Match", 
                 "To Attacking Third", "To Middle Third", "To Own Half"),
      Value = c(
        nrow(corners),
        length(selected_matchdays()),
        round(nrow(corners) / length(selected_matchdays()), 1),
        sum(corners$end_zone == "Attacking Third", na.rm = TRUE),
        sum(corners$end_zone == "Middle Third", na.rm = TRUE),
        sum(corners$end_zone == "Defensive Third", na.rm = TRUE)
      )
    )
    
    stats
  }, striped = TRUE, hover = TRUE)
  
  # Estadísticas por lado
  output$side_stats <- renderTable({
    req(corner_data())
    
    corners <- corner_data()
    
    if(nrow(corners) == 0) {
      return(data.frame(Metric = "No corners found", Value = "0"))
    }
    
    right_side <- corners %>% filter(corner_simple == "Right Side")  # Banda derecha
    left_side <- corners %>% filter(corner_simple == "Left Side")    # Banda izquierda
    
    # Calcular corners a penalty area (todas las zonas dentro del área grande)
    penalty_zones <- c("Small Box", "Small Box Left", "Small Box Right", 
                       "Box Deep Left", "Box Deep Right", "Box Left", "Box Center", "Box Right")
    
    right_to_penalty <- sum(right_side$end_area %in% penalty_zones, na.rm = TRUE)
    left_to_penalty <- sum(left_side$end_area %in% penalty_zones, na.rm = TRUE)
    
    stats <- data.frame(
      Metric = c("Right Side Total", "Left Side Total", "Right Side to PA", "Left Side to PA"),
      Value = c(
        nrow(right_side),
        nrow(left_side),
        right_to_penalty,
        left_to_penalty
      )
    )
    
    stats
  }, striped = TRUE, hover = TRUE)
  
  # Estadísticas por área objetivo
  output$area_stats <- renderTable({
    req(corner_data())
    
    corners <- corner_data()
    
    if(nrow(corners) == 0) {
      return(data.frame(Metric = "No corners found", Value = "0"))
    }
    
    # Filtrar por lado si está seleccionado
    if(input$corner_side_filter != "all") {
      corners <- corners %>%
        filter(corner_simple == input$corner_side_filter)
    }
    
    if(nrow(corners) == 0) {
      return(data.frame(Metric = paste("No corners found for", input$corner_side_filter), Value = "0"))
    }
    
    stats <- data.frame(
      Metric = c("Small Box", "Small Box Left", "Small Box Right", "Box Deep Left", "Box Deep Right", 
                 "Box Left", "Box Center", "Box Right", "Out Box Deep Left", "Out Box Deep Right",
                 "Out Box Left", "Out Box Center", "Out Box Right"),
      Value = c(
        sum(corners$end_area == "Small Box", na.rm = TRUE),
        sum(corners$end_area == "Small Box Left", na.rm = TRUE),
        sum(corners$end_area == "Small Box Right", na.rm = TRUE),
        sum(corners$end_area == "Box Deep Left", na.rm = TRUE),
        sum(corners$end_area == "Box Deep Right", na.rm = TRUE),
        sum(corners$end_area == "Box Left", na.rm = TRUE),
        sum(corners$end_area == "Box Center", na.rm = TRUE),
        sum(corners$end_area == "Box Right", na.rm = TRUE),
        sum(corners$end_area == "Out of Box Deep Left", na.rm = TRUE),
        sum(corners$end_area == "Out of Box Deep Right", na.rm = TRUE),
        sum(corners$end_area == "Out of Box Left", na.rm = TRUE),
        sum(corners$end_area == "Out of Box Center", na.rm = TRUE),
        sum(corners$end_area == "Out of Box Right", na.rm = TRUE)
      )
    )
    
    stats
  }, striped = TRUE, hover = TRUE)
  
  # Estadísticas de cobradores de corners
  output$corner_takers_stats <- DT::renderDataTable({
    req(corner_data())
    corners <- corner_data()
    if(nrow(corners) == 0) {
      return(DT::datatable(
        data.frame(Player = "No corners found", Corners = "0"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    if(input$corner_side_filter != "all") {
      corners <- corners %>% filter(corner_simple == input$corner_side_filter)
    }
    if(nrow(corners) == 0) {
      return(DT::datatable(
        data.frame(Player = paste("No corners for", input$corner_side_filter), Corners = "0"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    player_photo_map <- read.csv('www/player_photo_map.csv', stringsAsFactors = FALSE, sep = ';', encoding = 'UTF-8')
    get_player_photo <- function(player_name) {
      if (!exists("player_photo_map") || is.null(player_photo_map) || nrow(player_photo_map) == 0) {
        return("Jugadores/placeholder.png")
      }
      if (is.null(player_name) || is.na(player_name) || player_name == "" || length(player_name) == 0) {
        return("Jugadores/placeholder.png")
      }
      tryCatch({
        row <- player_photo_map[player_photo_map$player_name == player_name, ]
        if (nrow(row) == 0) {
          return("Jugadores/placeholder.png")
        }
        photo_file <- row$photo_file[1]
        if (!is.na(photo_file) && photo_file != "" && photo_file != "placeholder.png") {
          return(file.path("Jugadores", photo_file))
        } else {
          return("Jugadores/placeholder.png")
        }
      }, error = function(e) {
        return("Jugadores/placeholder.png")
      })
    }
    corner_takers <- corners %>%
      count(player_name, sort = TRUE) %>%
      rename(Player = player_name, Corners = n) %>%
      head(10)
    corner_takers <- corner_takers %>%
      mutate(
        Photo = sapply(Player, function(name) {
          path <- get_player_photo(name)
          sprintf('<img src="%s" height="40" style="border-radius:50%%;margin-right:8px;">', path)
        }),
        Player = paste0(Photo, Player)
      )
    DT::datatable(
      corner_takers[, c("Player", "Corners")],
      options = list(
        pageLength = 10,
        dom = 't',
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        paging = FALSE
      ),
      rownames = FALSE,
      colnames = c("Player", "Corners"),
      escape = FALSE
    )
  })
  
  # Estadísticas de rematadores después de corners
  output$shot_takers_stats <- DT::renderDataTable({
    req(shots_after_corners_data())
    shots_data <- shots_after_corners_data()
    if(nrow(shots_data) == 0) {
      return(DT::datatable(
        data.frame(Player = "No shots after corners", Shots = "0", Goals = "0"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    if(input$corner_side_filter != "all") {
      shots_data <- shots_data %>% filter(corner_side == input$corner_side_filter)
    }
    if(nrow(shots_data) == 0) {
      return(DT::datatable(
        data.frame(Player = paste("No shots for", input$corner_side_filter), Shots = "0", Goals = "0"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    player_photo_map <- read.csv('www/player_photo_map.csv', stringsAsFactors = FALSE, sep = ';', encoding = 'UTF-8')
    get_player_photo <- function(player_name) {
      if (!exists("player_photo_map") || is.null(player_photo_map) || nrow(player_photo_map) == 0) {
        return("Jugadores/placeholder.png")
      }
      if (is.null(player_name) || is.na(player_name) || player_name == "" || length(player_name) == 0) {
        return("Jugadores/placeholder.png")
      }
      tryCatch({
        row <- player_photo_map[player_photo_map$player_name == player_name, ]
        if (nrow(row) == 0) {
          return("Jugadores/placeholder.png")
        }
        photo_file <- row$photo_file[1]
        if (!is.na(photo_file) && photo_file != "" && photo_file != "placeholder.png") {
          return(file.path("Jugadores", photo_file))
        } else {
          return("Jugadores/placeholder.png")
        }
      }, error = function(e) {
        return("Jugadores/placeholder.png")
      })
    }
    shot_takers <- shots_data %>%
      group_by(player_name) %>%
      summarise(
        total_shots = n(),
        goals = sum(event == "Goal", na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(total_shots)) %>%
      rename(Player = player_name, Shots = total_shots, Goals = goals) %>%
      head(10)
    shot_takers <- shot_takers %>%
      mutate(
        Photo = sapply(Player, function(name) {
          path <- get_player_photo(name)
          sprintf('<img src="%s" height="40" style="border-radius:50%%;margin-right:8px;">', path)
        }),
        Player = paste0(Photo, Player)
      )
    DT::datatable(
      shot_takers[, c("Player", "Shots", "Goals")],
      options = list(
        pageLength = 10,
        dom = 't',
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        paging = FALSE
      ),
      rownames = FALSE,
      colnames = c("Player", "Shots", "Goals"),
      escape = FALSE
    )
  })
  
  observeEvent(input$download_corner_plot_panel, {
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_corner_plot_panel",
      filename = paste0("Corner_Field_Panel_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    ))
  })
  
  observeEvent(input$download_corner_takers_panel, {
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_corner_takers_panel",
      filename = paste0("Corner_Takers_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    ))
  })
  
  observeEvent(input$download_shot_takers_panel, {
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_shot_takers_panel",
      filename = paste0("Shot_Takers_After_Corners_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    ))
  })
}

# ===== EJECUTAR APP =====
shinyApp(ui = ui, server = server)