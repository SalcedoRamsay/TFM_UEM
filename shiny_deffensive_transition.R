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

# Transformar datos siguiendo la lógica EXACTA del código Won Duels
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
  loss_point = "#FF5722",         # Naranja para ball loss
  heatmap_start = "#FFF3E0",      # Naranja claro para heatmap
  heatmap_end = "#FF5722",        # Naranja para heatmap
  lines = "#000000",
  recovery_point = "#4CAF50",     # Verde para ball recovery
  carry_color = "#9C27B0",
  pass_color = "#2196F3",
  defensive_sequence = "#1976D2"   # Azul para secuencias defensivas
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
    # Círculo central - usando geom_circle de ggforce
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
    # Semicírculos de áreas - usando geom_arc de ggforce
    geom_arc(aes(x0 = 11, y0 = 50, r = 9.15, start = 37*pi/180, end = 143*pi/180), 
             color = line_color, linewidth = 0.5) +
    geom_arc(aes(x0 = 89, y0 = 50, r = 9.15, start = -37*pi/180, end = -143*pi/180), 
             color = line_color, linewidth = 0.5) +
    # Líneas de tercios (punteadas)
    geom_vline(xintercept = c(33.33, 66.66), 
               color = line_color, linetype = "dotted", alpha = 0.5) +
    coord_fixed() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = colors$background_light, color = NA),
      panel.background = element_rect(fill = colors$background_light, color = NA)
    )
}

# Función para crear zonas del campo (heatmap)
create_field_zones <- function() {
  bin_width <- 100 / 6
  bin_height <- 100 / 3
  
  zones <- expand.grid(bin_x = 0:5, bin_y = 0:2) %>%
    mutate(
      xmin = bin_x * bin_width,
      xmax = (bin_x + 1) * bin_width,
      ymin = bin_y * bin_height,
      ymax = (bin_y + 1) * bin_height,
      center_x = (xmin + xmax) / 2,
      center_y = (ymin + ymax) / 2,
      id = bin_x + bin_y * 6
    )
  
  return(zones)
}

# Función para obtener ball losses acumulados (Dispossessed)
get_accumulated_ball_losses <- function(match_data, team_name_filter, matchdays) {
  ball_loss_events <- match_data %>%
    filter(
      Team == team_name_filter,  # Usar Team (team_name)
      event == "Dispossessed",   # Evento de pérdida de balón
      matchday_num %in% matchdays
    ) %>%
    mutate(
      x = as.numeric(x),
      y = as.numeric(y),
      field_zone = case_when(
        x <= 33.33 ~ "Defensive Third",
        x <= 66.66 ~ "Middle Third", 
        x > 66.66 ~ "Attacking Third",
        TRUE ~ "Unknown"
      ),
      lane = case_when(
        y <= 33.33 ~ "Right Lane",
        y <= 66.66 ~ "Central Lane",
        y > 66.66 ~ "Left Lane",
        TRUE ~ "Unknown"
      )
    ) %>%
    filter(
      !is.na(x) & !is.na(y) & 
        x >= 0 & x <= 100 & 
        y >= 0 & y <= 100
    )
  
  return(ball_loss_events)
}

# Función para encontrar secuencias de ball loss a shot del rival (ORIGINAL)
find_ball_loss_to_shot_sequences <- function(match_data, team_name_filter, matchdays) {
  # Eventos de disparo identificados
  shot_events <- c("Miss", "Goal", "Saved Shot", "Post", "Save")
  
  # Obtener ball losses del equipo
  ball_losses <- get_accumulated_ball_losses(match_data, team_name_filter, matchdays)
  
  if(nrow(ball_losses) == 0) return(list())
  
  sequences <- list()
  
  # PROCESAR CADA PARTIDO POR SEPARADO para evitar cruces entre partidos
  for(current_matchday in matchdays) {
    
    cat("DEBUG SHOT - Processing matchday:", current_matchday, "\n")
    
    # Datos ordenados por tiempo SOLO del partido actual
    ordered_data <- match_data %>%
      filter(matchday_num == current_matchday) %>%  # TODOS los equipos del partido
      arrange(period_id, time_min, time_sec) %>%
      mutate(row_number = row_number())
    
    # Ball losses SOLO del partido actual
    match_losses <- ball_losses %>%
      filter(matchday_num == current_matchday)
    
    cat("  Ball losses in this match:", nrow(match_losses), "\n")
    
    if(nrow(match_losses) == 0) next
    
    for(i in 1:nrow(match_losses)) {
      ball_loss <- match_losses[i,]
      
      # Encontrar la posición del ball loss en los datos ordenados del partido
      loss_row <- which(ordered_data$general_id == ball_loss$general_id)
      
      if(length(loss_row) == 0) next
      
      loss_time <- ball_loss$time_min * 60 + ball_loss$time_sec
      max_time_limit <- 15  # 15 segundos
      
      # Buscar shot events del EQUIPO RIVAL después del ball loss
      for(j in (loss_row + 1):min(loss_row + 150, nrow(ordered_data))) {
        if(j > nrow(ordered_data)) break
        
        next_event <- ordered_data[j,]
        event_time <- next_event$time_min * 60 + next_event$time_sec
        time_elapsed <- event_time - loss_time
        
        # Si pasa el tiempo límite, parar
        if(time_elapsed > max_time_limit) break
        
        # Si cambia el período, parar
        if(next_event$period_id != ball_loss$period_id) break
        
        # Si nuestro equipo recupera el balón, parar
        if(next_event$Team == team_name_filter && 
           next_event$event %in% c("Ball recovery", "Pass", "Interception", "Tackle")) {
          break
        }
        
        # VERIFICAR SI ENCONTRAMOS UN SHOT EVENT DEL EQUIPO RIVAL
        if(next_event$Team != team_name_filter && next_event$event %in% shot_events) {
          
          # VERIFICAR QUE EL SHOT TENGA COORDENADAS VÁLIDAS
          shot_x <- as.numeric(next_event$x)
          shot_y <- as.numeric(next_event$y)
          
          if(is.na(shot_x) || is.na(shot_y) || shot_x < 0 || shot_x > 100 || shot_y < 0 || shot_y > 100) {
            break  # Si el shot no tiene coordenadas válidas, salir
          }
          
          # VALIDACIÓN: El shot rival debe ser hacia NUESTRO arco (izquierda)
          if(shot_x > 16.5) {
            cat("DEBUG SHOT - Rival shot rejected: x =", shot_x, "(too far from our goal)\n")
            break  # Shot rival hacia su propio arco, no válido
          }
          
          # Crear la secuencia con ball loss y shot
          shot_sequence <- list(
            ball_loss = ball_loss %>% mutate(
              field_zone = case_when(
                x <= 33.33 ~ "Defensive Third",
                x <= 66.66 ~ "Middle Third", 
                x > 66.66 ~ "Attacking Third",
                TRUE ~ "Unknown"
              ),
              lane = case_when(
                y <= 33.33 ~ "Right Lane",
                y <= 66.66 ~ "Central Lane",
                y > 66.66 ~ "Left Lane",
                TRUE ~ "Unknown"
              )
            ),
            shot = next_event %>% mutate(
              time_from_loss = time_elapsed,
              field_zone = case_when(
                shot_x <= 33.33 ~ "Defensive Third",
                shot_x <= 66.66 ~ "Middle Third", 
                shot_x > 66.66 ~ "Attacking Third",
                TRUE ~ "Unknown"
              ),
              lane = case_when(
                as.numeric(y) <= 33.33 ~ "Right Lane",
                as.numeric(y) <= 66.66 ~ "Central Lane",
                as.numeric(y) > 66.66 ~ "Left Lane",
                TRUE ~ "Unknown"
              )
            )
          )
          
          sequences[[length(sequences) + 1]] <- shot_sequence
          cat("  ✅ Shot sequence added for matchday", current_matchday, "\n")
          break  # Solo tomar el primer shot después del ball loss
        }
      }
    }
  }
  
  cat("DEBUG SHOT - Total shot sequences found:", length(sequences), "\n")
  return(sequences)
}

# Función MODIFICADA: encontrar secuencias de ball loss a ball recovery del propio equipo
find_ball_loss_to_recovery_sequences <- function(match_data, team_name_filter, matchdays) {
  # Eventos de recuperación del propio equipo - SOLO Ball recovery
  recovery_events <- c("Ball recovery")
  
  # Obtener ball losses del equipo
  ball_losses <- get_accumulated_ball_losses(match_data, team_name_filter, matchdays)
  
  if(nrow(ball_losses) == 0) return(list())
  
  sequences <- list()
  
  # PROCESAR CADA PARTIDO POR SEPARADO para evitar cruces entre partidos
  for(current_matchday in matchdays) {
    
    cat("DEBUG - Processing matchday:", current_matchday, "\n")
    
    # Datos ordenados por tiempo SOLO del partido actual
    ordered_data <- match_data %>%
      filter(matchday_num == current_matchday) %>%  # TODOS los equipos del partido
      arrange(period_id, time_min, time_sec) %>%
      mutate(row_number = row_number())
    
    # Ball losses SOLO del partido actual
    match_losses <- ball_losses %>%
      filter(matchday_num == current_matchday)
    
    cat("  Ball losses in this match:", nrow(match_losses), "\n")
    
    if(nrow(match_losses) == 0) next
    
    for(i in 1:nrow(match_losses)) {
      ball_loss <- match_losses[i,]
      
      # Encontrar la posición del ball loss en los datos ordenados del partido
      loss_row <- which(ordered_data$general_id == ball_loss$general_id)
      
      if(length(loss_row) == 0) next
      
      loss_time <- ball_loss$time_min * 60 + ball_loss$time_sec
      max_time_limit <- 15  # 15 segundos
      
      # Buscar eventos de RECUPERACIÓN del PROPIO EQUIPO después del ball loss
      for(j in (loss_row + 1):min(loss_row + 150, nrow(ordered_data))) {
        if(j > nrow(ordered_data)) break
        
        next_event <- ordered_data[j,]
        event_time <- next_event$time_min * 60 + next_event$time_sec
        time_elapsed <- event_time - loss_time
        
        # Si pasa el tiempo límite, parar
        if(time_elapsed > max_time_limit) break
        
        # Si cambia el período, parar
        if(next_event$period_id != ball_loss$period_id) break
        
        # VERIFICAR SI ENCONTRAMOS UN EVENTO DE RECUPERACIÓN DEL PROPIO EQUIPO
        if(next_event$Team == team_name_filter && next_event$event %in% recovery_events) {
          
          # VERIFICAR QUE LA RECUPERACIÓN TENGA COORDENADAS VÁLIDAS
          recovery_x <- as.numeric(next_event$x)
          recovery_y <- as.numeric(next_event$y)
          
          if(is.na(recovery_x) || is.na(recovery_y) || recovery_x < 0 || recovery_x > 100 || recovery_y < 0 || recovery_y > 100) {
            break  # Si la recuperación no tiene coordenadas válidas, salir
          }
          
          # Crear la secuencia COMPLETA desde ball loss hasta ball recovery
          # Incluir todos los eventos del rival entre el ball loss y la recuperación
          sequence_events <- ordered_data[(loss_row):(j),] %>%
            mutate(
              sequence_id = paste0("recovery_seq_", current_matchday, "_", ball_loss$general_id),
              time_from_loss = (time_min * 60 + time_sec) - loss_time,
              is_recovery = general_id == next_event$general_id,
              recovery_type = ifelse(general_id == next_event$general_id, next_event$event, NA),
              field_zone = case_when(
                as.numeric(x) <= 33.33 ~ "Defensive Third",
                as.numeric(x) <= 66.66 ~ "Middle Third", 
                as.numeric(x) > 66.66 ~ "Attacking Third",
                TRUE ~ "Unknown"
              ),
              lane = case_when(
                as.numeric(y) <= 33.33 ~ "Right Lane",
                as.numeric(y) <= 66.66 ~ "Central Lane",
                as.numeric(y) > 66.66 ~ "Left Lane",
                TRUE ~ "Unknown"
              )
            )
          
          # Marcar el ball loss inicial
          sequence_events[1, "is_loss"] <- TRUE
          
          # Filtrar eventos relevantes: ball loss, eventos del rival, y recuperación
          relevant_events <- sequence_events %>%
            filter(
              (row_number() == 1) |  # Ball loss inicial
                (Team != team_name_filter & event %in% c("Pass", "Dribble", "Carry")) |  # Eventos del rival
                (Team == team_name_filter & event %in% recovery_events)  # Recuperación propia
            ) %>%
            arrange(time_from_loss)
          
          if(nrow(relevant_events) < 2) next  # Debe haber al menos loss + recovery
          
          # Solo añadir la secuencia si termina en recuperación
          if(any(relevant_events$is_recovery, na.rm = TRUE)) {
            final_sequence <- list(
              events = relevant_events
            )
            
            sequences[[length(sequences) + 1]] <- final_sequence
            cat("  ✅ Defensive recovery sequence added for matchday", current_matchday, "\n")
          }
          
          break  # Solo tomar la primera recuperación después del ball loss
        }
      }
    }
  }
  
  cat("DEBUG - Total defensive recovery sequences found:", length(sequences), "\n")
  return(sequences)
}

# ===== CARGA DE FOTOS DE JUGADORES =====
player_photo_map <- read.csv('www/player_photo_map.csv', sep = ';', stringsAsFactors = FALSE)

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

# ===== UI =====
ui <- dashboardPage(
  dashboardHeader(title = "Defensive Transition Analysis - Ball Loss to Recovery"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Team Analysis", tabName = "analysis", icon = icon("chart-area"))
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
                    p("• Click 'Update Analysis' to see accumulated ball losses"),
                    p("• Use display options to overlay different sequence types"),
                    p("• Heatmap shows where your team loses the ball most frequently")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Display Options", status = "success", solidHeader = TRUE, width = 12,
                  column(6,
                         materialSwitch("show_defensive_sequences", "Show Ball Recovery Points", 
                                        value = FALSE, status = "success")
                  ),
                  column(6,
                         materialSwitch("show_shot_sequences", "Show Ball-Loss-to-Shot Sequences", 
                                        value = FALSE, status = "danger")
                  ),
                  br(),
                  div(
                    style = "background-color: #d4edda; padding: 10px; border-radius: 5px; border-left: 4px solid #28a745;",
                    h5("Defensive Analysis Legend:"),
                    p("🟢 Ball Recovery Points: Where your team recovers the ball after losing it"),
                    p("🟠 Ball Loss → 🔴 Rival Shot: Sequences from your loss to rival shots"),
                    p("Shows defensive effectiveness and transition vulnerabilities (max 15 seconds)")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Accumulated Ball Losses Heatmap with Defensive Recovery Sequences", status = "success", solidHeader = TRUE, width = 12,
                  plotlyOutput("field_plot", height = "700px")
                )
              ),
              
              fluidRow(
                column(3,
                       box(
                         title = "Ball Loss Statistics", status = "primary", solidHeader = TRUE, width = NULL,
                         tableOutput("loss_stats")
                       )
                ),
                column(3,
                       box(
                         title = "Selected Matches", status = "primary", solidHeader = TRUE, width = NULL,
                         tableOutput("selected_matches_summary")
                       )
                ),
                column(3,
                       box(
                         title = "Recovery Sequences", status = "success", solidHeader = TRUE, width = NULL,
                         tableOutput("defensive_sequences_stats")
                       )
                ),
                column(3,
                       box(
                         title = "Shot Sequences", status = "danger", solidHeader = TRUE, width = NULL,
                         tableOutput("shot_sequences_stats")
                       )
                )
              ),

              # NUEVOS PANELES: TOP 5 PÉRDIDAS Y TOP 3 RECUPERADORES
              fluidRow(
                column(6,
                       box(
                         title = "Top 5 Ball Losers (Dispossessed)", status = "danger", solidHeader = TRUE, width = NULL,
                         DT::dataTableOutput("top5_ball_losers")
                       )
                ),
                column(6,
                       box(
                         title = "Top 3 Ball Recovery Points", status = "success", solidHeader = TRUE, width = NULL,
                         DT::dataTableOutput("top3_ball_recovery_points")
                       )
                )
              )
      )
    )
  )
)

# ===== SERVER =====
server <- function(input, output, session) {
  
  # Preparar lista de equipos únicos (nombres largos para filtrar)
  teams_list <- data %>%
    select(Team) %>%  # Usar Team (team_name)
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
        Team == input$selected_team,  # Filtrar por Team (team_name)
        season == input$selected_season
      )
    
    return(filtered_data)
  })
  
  # Crear tabla de selección de partidos
  output$match_selector <- DT::renderDataTable({
    req(team_season_data())
    
    # LÓGICA EXACTA DEL WON DUELS: Solo mostrar descripción completa del partido
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
  
  # Datos reactivos de ball losses acumulados
  accumulated_ball_losses <- eventReactive(input$update_analysis, {
    req(team_season_data(), selected_matchdays())
    
    if(length(selected_matchdays()) == 0) {
      return(data.frame())
    }
    
    # Obtener ball losses del equipo
    losses <- get_accumulated_ball_losses(team_season_data(), input$selected_team, selected_matchdays())
    return(losses)
  }, ignoreNULL = FALSE)
  
  # Datos reactivos de secuencias de recuperación defensiva
  defensive_recovery_sequences <- eventReactive(input$update_analysis, {
    req(team_season_data(), selected_matchdays())
    
    if(length(selected_matchdays()) == 0) {
      return(list())
    }
    
    # Necesitamos datos de TODO el partido para ver las secuencias completas
    all_match_data <- data %>%
      filter(
        season == input$selected_season,
        matchday_num %in% selected_matchdays()
      )
    
    sequences <- find_ball_loss_to_recovery_sequences(all_match_data, input$selected_team, selected_matchdays())
    return(sequences)
  }, ignoreNULL = FALSE)
  
  # Datos reactivos de secuencias de shot defensivas
  defensive_shot_sequences <- eventReactive(input$update_analysis, {
    req(team_season_data(), selected_matchdays())
    
    if(length(selected_matchdays()) == 0) {
      return(list())
    }
    
    # Necesitamos datos de TODO el partido para ver las secuencias completas
    all_match_data <- data %>%
      filter(
        season == input$selected_season,
        matchday_num %in% selected_matchdays()
      )
    
    sequences <- find_ball_loss_to_shot_sequences(all_match_data, input$selected_team, selected_matchdays())
    return(sequences)
  }, ignoreNULL = FALSE)
  
  # Crear gráfico del campo con heatmap
  output$field_plot <- renderPlotly({
    req(accumulated_ball_losses())
    
    ball_losses <- accumulated_ball_losses()
    recovery_seqs <- defensive_recovery_sequences()
    shot_seqs <- defensive_shot_sequences()
    
    if(nrow(ball_losses) == 0) {
      p <- draw_soccer_field() +
        labs(title = paste("Defensive Transition Analysis -", input$selected_team),
             subtitle = "No ball losses found for selected matches")
      return(ggplotly(p))
    }
    
    # Crear zonas y contar ball losses por zona
    zones <- create_field_zones()
    
    loss_heatmap <- ball_losses %>%
      mutate(
        bin_x = pmin(pmax(floor(x / (100/6)), 0), 5),
        bin_y = pmin(pmax(floor(y / (100/3)), 0), 2)
      ) %>%
      count(bin_x, bin_y) %>%
      mutate(id = bin_x + bin_y * 6) %>%
      right_join(zones, by = "id") %>%
      mutate(
        n = ifelse(is.na(n), 0, n),
        zone_label = paste0("Zone (", bin_x.x, ",", bin_y.x, ")"),
        tooltip_text = paste0("Zone: (", bin_x.x, ",", bin_y.x, ")\nBall Losses: ", n)
      )
    
    # Colores del heatmap (naranja para ball losses)
    heatmap_colors <- colorRampPalette(c(colors$heatmap_start, colors$heatmap_end))(20)
    
    # Crear gráfico base
    p <- draw_soccer_field()
    
    # Añadir heatmap
    p <- p +
      geom_rect(
        data = loss_heatmap,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
        color = "white", linetype = "dashed", size = 0.3, alpha = 0.8
      ) +
      scale_fill_gradientn(colors = heatmap_colors, 
                           name = "Ball Losses",
                           guide = guide_colorbar(title.position = "top")) +
      geom_text(
        data = loss_heatmap %>% filter(n > 0),
        aes(x = center_x, y = center_y, label = n),
        color = "white", fontface = "bold", size = 4
      )
    
    # Estadísticas para el subtítulo
    total_losses <- nrow(ball_losses)
    matches_analyzed <- length(selected_matchdays())
    def_losses <- sum(ball_losses$field_zone == "Defensive Third", na.rm = TRUE)
    mid_losses <- sum(ball_losses$field_zone == "Middle Third", na.rm = TRUE)
    att_losses <- sum(ball_losses$field_zone == "Attacking Third", na.rm = TRUE)
    
    # Contar secuencias de recuperación y shot defensiva
    total_recovery_sequences <- length(recovery_seqs)
    total_shot_sequences <- length(shot_seqs)
    
    # Flecha de dirección de ataque con punta triangular
    arrow_triangle <- data.frame(
      x = c(80, 77, 77, 80),
      y = c(95, 97, 93, 95)
    )
    
    p <- p +
      # Línea de la flecha
      geom_segment(
        aes(x = 20, y = 95, xend = 77, yend = 95),
        linewidth = 1.5, color = colors$loss_point
      ) +
      # Punta triangular de la flecha
      geom_polygon(data = arrow_triangle, aes(x = x, y = y), 
                   fill = colors$loss_point, color = colors$loss_point) +
      # Texto
      annotate("text", x = 50, y = 88, 
               label = "Your team's direction of attack",
               color = colors$loss_point,
               fontface = "bold", size = 4)
    
    # Añadir secuencias de recuperación defensiva si está activado
    if(input$show_defensive_sequences && total_recovery_sequences > 0) {
      
      cat("DEBUG - Processing", total_recovery_sequences, "defensive recovery sequences\n")
      
      # Extraer eventos de todas las secuencias
      all_events <- list()
      
      for(i in 1:length(recovery_seqs)) {
        if(!is.null(recovery_seqs[[i]]$events)) {
          all_events[[i]] <- recovery_seqs[[i]]$events
        }
      }
      
      if(length(all_events) > 0) {
        sequence_events <- bind_rows(all_events) %>%
          mutate(
            x = as.numeric(x),
            y = as.numeric(y)
          ) %>%
          filter(!is.na(x) & !is.na(y))
        
        # SOLO mostrar ball recoveries (NO los ball losses)
        valid_recoveries <- sequence_events %>% 
          filter(
            is_recovery == TRUE &
              !is.na(x) & !is.na(y) & 
              x >= 0 & x <= 100 &
              y >= 0 & y <= 100 &
              event %in% c("Ball recovery")
          )
        
        cat("DEBUG - Valid ball recoveries found:", nrow(valid_recoveries), "\n")
        
        if(nrow(valid_recoveries) > 0) {
          # Añadir recuperaciones (CÍRCULOS VERDES)
          p <- p +
            geom_point(
              data = valid_recoveries,
              aes(x = x, y = y,
                  text = paste("🟢 BALL RECOVERY<br>",
                               "Player:", player_name, "<br>",
                               "Time:", time_min, ":", sprintf("%02d", time_sec), "<br>",
                               "Zone:", field_zone, "<br>",
                               "Lane:", lane, "<br>",
                               "Seconds from ball loss:", round(time_from_loss, 1))),
              color = colors$recovery_point, size = 3, shape = 16,
              alpha = 0.9, stroke = 1.5
            )
          
          cat("DEBUG - Ball recovery points successfully added to plot\n")
        }
      }
    }
    
    # Añadir secuencias de shot si está activado
    if(input$show_shot_sequences && total_shot_sequences > 0) {
      
      cat("DEBUG SHOT - Processing", total_shot_sequences, "shot sequences\n")
      
      # Extraer ball losses y shots de las secuencias
      shot_ball_losses <- list()
      rival_shots <- list()
      
      for(i in 1:length(shot_seqs)) {
        if(!is.null(shot_seqs[[i]]$ball_loss)) {
          shot_ball_losses[[i]] <- shot_seqs[[i]]$ball_loss
        }
        if(!is.null(shot_seqs[[i]]$shot)) {
          rival_shots[[i]] <- shot_seqs[[i]]$shot
        }
      }
      
      if(length(shot_ball_losses) > 0 && length(rival_shots) > 0) {
        ball_losses_df <- bind_rows(shot_ball_losses) %>%
          mutate(
            x = as.numeric(x),
            y = as.numeric(y)
          ) %>%
          filter(!is.na(x) & !is.na(y))
        
        rival_shots_df <- bind_rows(rival_shots) %>%
          mutate(
            x = as.numeric(x),
            y = as.numeric(y)
          ) %>%
          filter(!is.na(x) & !is.na(y))
        
        # Añadir ball losses que terminan en shot (puntos naranjas)
        if(nrow(ball_losses_df) > 0) {
          p <- p +
            geom_point(
              data = ball_losses_df,
              aes(x = x, y = y,
                  text = paste("🟠 Ball Loss → Rival Shot<br>",
                               "Player:", player_name, "<br>",
                               "Time:", time_min, ":", sprintf("%02d", time_sec), "<br>",
                               "Zone:", field_zone, "<br>",
                               "Lane:", lane)),
              color = colors$loss_point, size = 2, shape = 18,
              alpha = 0.8, stroke = 1
            )
        }
        
        # Añadir shots rivales (círculos rojos)
        if(nrow(rival_shots_df) > 0) {
          p <- p +
            geom_point(
              data = rival_shots_df,
              aes(x = x, y = y,
                  text = paste("🔴 RIVAL SHOT:", event, "<br>",
                               "Player:", player_name, "<br>",
                               "Time:", time_min, ":", sprintf("%02d", time_sec), "<br>",
                               "Zone:", field_zone, "<br>",
                               "Lane:", lane, "<br>",
                               "Seconds from ball loss:", round(time_from_loss, 1))),
              color = "#DC3545", size = 3, shape = 16,
              alpha = 0.9, stroke = 1.5
            )
        }
        
        cat("DEBUG SHOT - Shot sequences successfully added to plot\n")
      }
    }
    
    # Títulos actualizados
    subtitle_text <- paste(
      "Season:", input$selected_season, "| Matches analyzed:", matches_analyzed, "\n",
      "Total Ball Losses:", total_losses, 
      "| Zones: Def(", def_losses, ") Mid(", mid_losses, ") Att(", att_losses, ")\n",
      "Recovery Sequences:", total_recovery_sequences,
      if(input$show_defensive_sequences) " (shown)" else " (hidden)",
      "| Shot Sequences:", total_shot_sequences,
      if(input$show_shot_sequences) " (shown)" else " (hidden)",
      "| Max time: 15 seconds"
    )
    
    p <- p +
      labs(
        title = paste("Defensive Transition Analysis - Ball Loss to Recovery -", input$selected_team),
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
  
  # Estadísticas de ball loss
  output$loss_stats <- renderTable({
    req(accumulated_ball_losses())
    
    ball_losses <- accumulated_ball_losses()
    
    if(nrow(ball_losses) == 0) {
      return(data.frame(Metric = "No ball losses found", Value = "0"))
    }
    
    stats <- data.frame(
      Metric = c("Total Ball Losses", "Defensive Third", "Middle Third", "Attacking Third", 
                 "Right Lane", "Central Lane", "Left Lane", "Matches Analyzed", "Avg per Match"),
      Value = c(
        nrow(ball_losses),
        sum(ball_losses$field_zone == "Defensive Third", na.rm = TRUE),
        sum(ball_losses$field_zone == "Middle Third", na.rm = TRUE),
        sum(ball_losses$field_zone == "Attacking Third", na.rm = TRUE),
        sum(ball_losses$lane == "Right Lane", na.rm = TRUE),
        sum(ball_losses$lane == "Central Lane", na.rm = TRUE),
        sum(ball_losses$lane == "Left Lane", na.rm = TRUE),
        length(selected_matchdays()),
        round(nrow(ball_losses) / length(selected_matchdays()), 1)
      )
    )
    
    stats
  }, striped = TRUE, hover = TRUE)
  
  # Resumen de partidos seleccionados
  output$selected_matches_summary <- renderTable({
    req(team_season_data(), selected_matchdays())
    
    if(length(selected_matchdays()) == 0) {
      return(data.frame(Message = "No matches selected"))
    }
    
    # LÓGICA EXACTA DEL WON DUELS: Solo mostrar descripción completa
    match_summary <- team_season_data() %>%
      filter(matchday_num %in% selected_matchdays()) %>%
      select(matchday_num, local_date, MatchDay, HomeTeam, AwayTeam) %>%
      distinct() %>%
      arrange(matchday_num) %>%
      mutate(
        MatchDescription = paste0(MatchDay, ": ", HomeTeam, " vs ", AwayTeam)
      ) %>%
      select(matchday_num, local_date, MatchDescription) %>%
      rename(Matchday = matchday_num, Date = local_date, Match = MatchDescription)
    
    match_summary
  }, striped = TRUE, hover = TRUE)
  
  # Estadísticas de secuencias de recuperación defensiva
  output$defensive_sequences_stats <- renderTable({
    req(defensive_recovery_sequences())
    
    recovery_seqs <- defensive_recovery_sequences()
    
    if(length(recovery_seqs) == 0) {
      return(data.frame(Metric = "No defensive recovery sequences found", Value = "0"))
    }
    
    # Analizar las secuencias
    all_events <- list()
    for(i in 1:length(recovery_seqs)) {
      if(!is.null(recovery_seqs[[i]]$events)) {
        all_events[[i]] <- recovery_seqs[[i]]$events
      }
    }
    
    if(length(all_events) > 0) {
      all_sequences <- bind_rows(all_events)
      recovery_events <- all_sequences %>% filter(is_recovery == TRUE)
      
      # Contar tipos de recuperaciones
      recovery_types <- table(recovery_events$event)
      ball_recoveries <- ifelse("Ball recovery" %in% names(recovery_types), recovery_types[["Ball recovery"]], 0)
      
      # Tiempo promedio desde ball loss hasta recuperación
      avg_time_to_recovery <- round(mean(recovery_events$time_from_loss, na.rm = TRUE), 1)
      
      # Estadísticas de efectividad defensiva
      total_ball_losses <- length(recovery_seqs)
      recovery_rate <- round((length(recovery_seqs) / nrow(accumulated_ball_losses())) * 100, 1)
      
      # Estadísticas por zona de recuperación
      def_recoveries <- sum(recovery_events$field_zone == "Defensive Third", na.rm = TRUE)
      mid_recoveries <- sum(recovery_events$field_zone == "Middle Third", na.rm = TRUE)
      att_recoveries <- sum(recovery_events$field_zone == "Attacking Third", na.rm = TRUE)
      
      stats <- data.frame(
        Metric = c("Total Recovery Sequences", "Ball Recoveries", "Avg Time to Recovery", 
                   "Recovery Rate %", "Def Third Recoveries", "Mid Third Recoveries", "Att Third Recoveries"),
        Value = c(
          length(recovery_seqs),
          ball_recoveries,
          paste0(avg_time_to_recovery, "s"),
          paste0(recovery_rate, "%"),
          def_recoveries,
          mid_recoveries,
          att_recoveries
        )
      )
    } else {
      stats <- data.frame(
        Metric = "No valid defensive recovery sequences found",
        Value = "0"
      )
    }
    
    stats
  }, striped = TRUE, hover = TRUE)
  
  # Estadísticas de secuencias de shot
  output$shot_sequences_stats <- renderTable({
    req(defensive_shot_sequences())
    
    shot_seqs <- defensive_shot_sequences()
    
    if(length(shot_seqs) == 0) {
      return(data.frame(Metric = "No shot sequences found", Value = "0"))
    }
    
    # Analizar las secuencias de shot
    ball_losses_list <- list()
    shots_list <- list()
    
    for(i in 1:length(shot_seqs)) {
      if(!is.null(shot_seqs[[i]]$ball_loss)) {
        ball_losses_list[[i]] <- shot_seqs[[i]]$ball_loss
      }
      if(!is.null(shot_seqs[[i]]$shot)) {
        shots_list[[i]] <- shot_seqs[[i]]$shot
      }
    }
    
    if(length(shots_list) > 0) {
      all_shots <- bind_rows(shots_list)
      all_ball_losses <- bind_rows(ball_losses_list)
      
      # Contar tipos de shots rivales
      shot_types <- table(all_shots$event)
      goals_conceded <- ifelse("Goal" %in% names(shot_types), shot_types[["Goal"]], 0)
      saves <- ifelse("Save" %in% names(shot_types), shot_types[["Save"]], 0) + 
        ifelse("Saved Shot" %in% names(shot_types), shot_types[["Saved Shot"]], 0)
      misses <- ifelse("Miss" %in% names(shot_types), shot_types[["Miss"]], 0)
      posts <- ifelse("Post" %in% names(shot_types), shot_types[["Post"]], 0)
      
      # Tiempo promedio desde ball loss hasta shot rival
      avg_time_to_shot <- round(mean(all_shots$time_from_loss, na.rm = TRUE), 1)
      
      # Estadísticas por zona de pérdida
      def_losses <- sum(all_ball_losses$field_zone == "Defensive Third", na.rm = TRUE)
      mid_losses <- sum(all_ball_losses$field_zone == "Middle Third", na.rm = TRUE)
      att_losses <- sum(all_ball_losses$field_zone == "Attacking Third", na.rm = TRUE)
      
      stats <- data.frame(
        Metric = c("Total Shot Sequences", "Goals Conceded", "Saves", "Rival Misses", "Posts",
                   "Avg Time to Shot", "From Def Third", "From Mid Third", "From Att Third"),
        Value = c(
          length(shot_seqs),
          goals_conceded,
          saves,
          misses,
          posts,
          paste0(avg_time_to_shot, "s"),
          def_losses,
          mid_losses,
          att_losses
        )
      )
    } else {
      stats <- data.frame(
        Metric = "No valid shot sequences found",
        Value = "0"
      )
    }
    
    stats
  }, striped = TRUE, hover = TRUE)

  # Top 5 jugadores que más pierden el balón
  output$top5_ball_losers <- DT::renderDataTable({
    req(accumulated_ball_losses())
    losses <- accumulated_ball_losses()
    if(nrow(losses) == 0) {
      return(DT::datatable(
        data.frame(Player = "No ball losses found", Losses = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    top5 <- losses %>%
      group_by(player_name) %>%
      summarise(losses = n()) %>%
      arrange(desc(losses)) %>%
      slice_head(n = 5) %>%
      mutate(
        Photo = sapply(player_name, function(name) {
          path <- get_player_photo(name)
          sprintf('<img src="%s" height="40" style="border-radius:50%%;margin-right:8px;">', path)
        }),
        Player = paste0(Photo, player_name)
      )
    DT::datatable(
      top5[, c("Player", "losses")],
      options = list(pageLength = 5, dom = 't'),
      rownames = FALSE,
      colnames = c("Player", "Losses"),
      escape = FALSE
    )
  })

  # Top 3 jugadores con más Ball Recovery Points
  output$top3_ball_recovery_points <- DT::renderDataTable({
    req(defensive_recovery_sequences())
    recovery_seqs <- defensive_recovery_sequences()
    if(length(recovery_seqs) == 0) {
      return(DT::datatable(
        data.frame(Player = "No recovery points found", Recoveries = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    all_events <- list()
    for(i in 1:length(recovery_seqs)) {
      if(!is.null(recovery_seqs[[i]]$events)) {
        all_events[[i]] <- recovery_seqs[[i]]$events
      }
    }
    if(length(all_events) > 0) {
      all_sequences <- bind_rows(all_events)
      recovery_events <- all_sequences %>% filter(is_recovery == TRUE)
      top3 <- recovery_events %>%
        group_by(player_name) %>%
        summarise(recoveries = n()) %>%
        arrange(desc(recoveries)) %>%
        slice_head(n = 3) %>%
        mutate(
          Photo = sapply(player_name, function(name) {
            path <- get_player_photo(name)
            sprintf('<img src="%s" height="40" style="border-radius:50%%;margin-right:8px;">', path)
          }),
          Player = paste0(Photo, player_name)
        )
      DT::datatable(
        top3[, c("Player", "recoveries")],
        options = list(pageLength = 3, dom = 't'),
        rownames = FALSE,
        colnames = c("Player", "Recoveries"),
        escape = FALSE
      )
    } else {
      return(DT::datatable(
        data.frame(Player = "No recovery points found", Recoveries = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
  })
}

# ===== EJECUTAR APP =====
shinyApp(ui = ui, server = server)