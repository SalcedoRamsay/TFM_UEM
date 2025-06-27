library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(ggplot2)
library(ggforce)
library(htmltools)
library(shinyWidgets)
library(stringi) # Para normalizar nombres
library(webshot) # Para capturar elementos HTML
library(htmlwidgets) # Para exportar widgets

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
  recovery_point = "#D32F2F",
  heatmap_start = "#FFF5F5",
  heatmap_end = "#D32F2F",
  lines = "#000000",
  shot_sequence = "#FF6B35",
  shot_point = "#4CAF50",  # Verde para el shot
  carry_color = "#9C27B0",
  pass_color = "#2196F3"
)

# ===== FUNCIONES AUXILIARES =====
# Función auxiliar para crear puntos de un círculo
circle_points <- function(center = c(0,0), r = 1, npoints = 100, start = 0, end = 2*pi) {
  theta <- seq(start, end, length.out = npoints)
  data.frame(
    x = center[1] + r * cos(theta),
    y = center[2] + r * sin(theta)
  )
}

# Función para dibujar campo de fútbol
draw_soccer_field_base <- function(fill_color = colors$background_light, line_color = colors$lines) {
  # Círculo central (más suave)
  circle_central <- circle_points(center = c(50, 50), r = 9.15, npoints = 200)
  # Semicírculos eliminados

  ggplot() +
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), fill = fill_color, color = line_color, linewidth = 1.2) +
    geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color = line_color, linewidth = 0.7) +
    geom_rect(aes(xmin = 0, xmax = 16.5, ymin = 21.1, ymax = 78.9), fill = fill_color, color = line_color, linewidth = 0.7) +
    geom_rect(aes(xmin = 83.5, xmax = 100, ymin = 21.1, ymax = 78.9), fill = fill_color, color = line_color, linewidth = 0.7) +
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = 36.8, ymax = 63.2), fill = fill_color, color = line_color, linewidth = 0.7) +
    geom_rect(aes(xmin = 94.5, xmax = 100, ymin = 36.8, ymax = 63.2), fill = fill_color, color = line_color, linewidth = 0.7) +
    geom_point(aes(x = 11, y = 50), size = 1.2, color = line_color) +
    geom_point(aes(x = 89, y = 50), size = 1.2, color = line_color) +
    geom_segment(aes(x = 0, xend = 100, y = 33.33, yend = 33.33), color = line_color, linewidth = 0.3, linetype = "dashed", alpha = 0.5) +
    geom_segment(aes(x = 0, xend = 100, y = 66.66, yend = 66.66), color = line_color, linewidth = 0.3, linetype = "dashed", alpha = 0.5) +
    geom_path(data = circle_central, aes(x = x, y = y), color = line_color, linewidth = 0.7) +
    coord_fixed() +
    theme_void()
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

# Función para obtener ball recoveries acumulados
get_accumulated_recoveries <- function(match_data, team_name_filter, matchdays) {
  ball_recovery_events <- match_data %>%
    filter(
      Team == team_name_filter,  # Usar Team (team_name) - igual que Won Duels
      event == "Ball recovery",
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
      )
    ) %>%
    filter(
      !is.na(x) & !is.na(y) & 
        x >= 0 & x <= 100 & 
        y >= 0 & y <= 100
    )
  
  return(ball_recovery_events)
}

# Nueva función mejorada para encontrar secuencias completas de ball recovery a shot
find_recovery_to_shot_sequences <- function(match_data, team_name_filter, matchdays) {
  # Eventos de disparo identificados
  shot_events <- c("Miss", "Goal", "Saved Shot", "Post", "Save")
  
  # Obtener recoveries del heatmap base (solo los que están en los partidos seleccionados)
  recoveries <- get_accumulated_recoveries(match_data, team_name_filter, matchdays)
  
  if(nrow(recoveries) == 0) return(list())
  
  sequences <- list()
  
  # PROCESAR CADA PARTIDO POR SEPARADO para evitar cruces entre partidos
  for(current_matchday in matchdays) {
    
    cat("DEBUG - Processing matchday:", current_matchday, "\n")
    
    # Datos ordenados por tiempo SOLO del partido actual
    ordered_data <- match_data %>%
      filter(
        Team == team_name_filter,
        matchday_num == current_matchday  # SOLO este partido
      ) %>%
      arrange(period_id, time_min, time_sec) %>%
      mutate(row_number = row_number())
    
    # Recoveries SOLO del partido actual
    match_recoveries <- recoveries %>%
      filter(matchday_num == current_matchday)
    
    cat("  Recoveries in this match:", nrow(match_recoveries), "\n")
    
    if(nrow(match_recoveries) == 0) next
    
    for(i in 1:nrow(match_recoveries)) {
      recovery <- match_recoveries[i,]
      
      # Encontrar la posición del recovery en los datos ordenados del partido
      recovery_row <- which(ordered_data$general_id == recovery$general_id)
      
      if(length(recovery_row) == 0) next
      
      recovery_time <- recovery$time_min * 60 + recovery$time_sec
      max_time_limit <- 15  # 15 segundos
      
      # Buscar shot events después del recovery EN EL MISMO PARTIDO
      for(j in (recovery_row + 1):min(recovery_row + 100, nrow(ordered_data))) {
        if(j > nrow(ordered_data)) break
        
        next_event <- ordered_data[j,]
        event_time <- next_event$time_min * 60 + next_event$time_sec
        time_elapsed <- event_time - recovery_time
        
        # Si pasa el tiempo límite, parar
        if(time_elapsed > max_time_limit) break
        
        # Si cambia el período, parar
        if(next_event$period_id != recovery$period_id) break
        
        # Si hay un evento del equipo contrario (pérdida de posesión), parar
        if(next_event$Team != team_name_filter && 
           next_event$event %in% c("Pass", "Ball recovery", "Interception", "Tackle")) {
          break
        }
        
        # VERIFICAR SI ENCONTRAMOS UN SHOT EVENT DEL MISMO EQUIPO
        if(next_event$Team == team_name_filter && next_event$event %in% shot_events) {
          
          # VERIFICAR QUE EL SHOT TENGA COORDENADAS VÁLIDAS
          shot_x <- as.numeric(next_event$x)
          shot_y <- as.numeric(next_event$y)
          
          if(is.na(shot_x) || is.na(shot_y) || shot_x < 0 || shot_x > 100 || shot_y < 0 || shot_y > 100) {
            break  # Si el shot no tiene coordenadas válidas, salir
          }
          
          # NUEVA VALIDACIÓN: El shot debe ser hacia el arco rival (derecha)
          # Como todos atacan de izquierda a derecha, los shots válidos deben estar en x > 83.5 (área rival)
          if(shot_x < 83.5) {
            cat("DEBUG - Shot rejected: x =", shot_x, "(too far from rival goal)\n")
            break  # Shot hacia arco propio o medio campo, no válido
          }
          
          # Crear la secuencia COMPLETA desde recovery hasta shot DENTRO DEL MISMO PARTIDO
          sequence_events <- ordered_data[(recovery_row):(j),] %>%
            filter(Team == team_name_filter) %>%
            mutate(
              sequence_id = paste0("seq_", current_matchday, "_", recovery$general_id),  # Incluir matchday en ID
              time_from_recovery = (time_min * 60 + time_sec) - recovery_time,
              is_recovery = general_id == recovery$general_id,
              is_shot = general_id == next_event$general_id,
              shot_type = ifelse(general_id == next_event$general_id, next_event$event, NA),
              recovery_zone = recovery$field_zone
            )
          
          # NUEVA LÓGICA: Encontrar el ÚLTIMO recovery antes del shot
          ball_recoveries_in_sequence <- sequence_events %>%
            filter(event == "Ball recovery") %>%
            arrange(time_from_recovery)
          
          # Si hay múltiples recoveries, usar solo el ÚLTIMO
          if(nrow(ball_recoveries_in_sequence) > 1) {
            last_recovery <- ball_recoveries_in_sequence[nrow(ball_recoveries_in_sequence),]
            
            # Recalcular desde el último recovery
            last_recovery_time <- last_recovery$time_min * 60 + last_recovery$time_sec
            
            # Filtrar eventos solo desde el último recovery hasta el shot
            sequence_events <- sequence_events %>%
              filter((time_min * 60 + time_sec) >= last_recovery_time) %>%
              mutate(
                # Recalcular tiempo desde el último recovery
                time_from_recovery = (time_min * 60 + time_sec) - last_recovery_time,
                # Solo marcar como recovery el último
                is_recovery = general_id == last_recovery$general_id,
                recovery_zone = last_recovery$field_zone
              )
          }
          
          # NUEVA LÓGICA: Crear conexiones secuenciales explícitas
          sequence_connections <- list()
          
          # Filtrar solo eventos relevantes y ordenarlos
          relevant_events <- sequence_events %>%
            filter(event %in% c("Ball recovery", "Pass", shot_events) | is_recovery | is_shot) %>%
            arrange(time_from_recovery)
          
          if(nrow(relevant_events) < 2) next  # Debe haber al menos recovery + shot
          
          # Variable para rastrear la posición actual del balón
          current_ball_x <- as.numeric(relevant_events[1,]$x)  # Empieza en el recovery
          current_ball_y <- as.numeric(relevant_events[1,]$y)
          
          # Crear conexiones entre eventos consecutivos
          for(k in 1:(nrow(relevant_events)-1)) {
            current_event <- relevant_events[k,]
            next_seq_event <- relevant_events[k+1,]
            
            if(next_seq_event$event == "Pass") {
              # CASO 1: El próximo evento es un PASE
              
              # Paso 1: CARRY desde posición actual del balón hasta donde inicia el pase
              pass_start_x <- as.numeric(next_seq_event$x)
              pass_start_y <- as.numeric(next_seq_event$y)
              
              if(!is.na(current_ball_x) && !is.na(current_ball_y) && !is.na(pass_start_x) && !is.na(pass_start_y)) {
                carry_distance <- sqrt((pass_start_x - current_ball_x)^2 + (pass_start_y - current_ball_y)^2)
                
                # Añadir CARRY si hay movimiento significativo
                if(carry_distance > 1) {
                  carry_connection <- data.frame(
                    sequence_id = current_event$sequence_id,
                    connection_type = "carry",
                    start_x = current_ball_x,
                    start_y = current_ball_y,
                    end_x = pass_start_x,
                    end_y = pass_start_y,
                    distance = carry_distance,
                    from_event = current_event$event,
                    to_event = "carry_to_pass",
                    from_player = current_event$player_name,
                    to_player = next_seq_event$player_name,
                    time_from_recovery = current_event$time_from_recovery,
                    stringsAsFactors = FALSE
                  )
                  sequence_connections[[length(sequence_connections) + 1]] <- carry_connection
                }
              }
              
              # Paso 2: PASS desde inicio del pase hasta donde llega
              if(!is.na(next_seq_event$Pass.End.X) && !is.na(next_seq_event$Pass.End.Y)) {
                pass_end_x <- as.numeric(next_seq_event$Pass.End.X)
                pass_end_y <- as.numeric(next_seq_event$Pass.End.Y)
                
                pass_distance <- sqrt((pass_end_x - pass_start_x)^2 + (pass_end_y - pass_start_y)^2)
                
                if(pass_distance > 1) {
                  pass_connection <- data.frame(
                    sequence_id = current_event$sequence_id,
                    connection_type = "pass",
                    start_x = pass_start_x,
                    start_y = pass_start_y,
                    end_x = pass_end_x,
                    end_y = pass_end_y,
                    distance = pass_distance,
                    from_event = "pass_start",
                    to_event = "pass_end",
                    from_player = next_seq_event$player_name,
                    to_player = "receiver",
                    time_from_recovery = next_seq_event$time_from_recovery,
                    stringsAsFactors = FALSE
                  )
                  sequence_connections[[length(sequence_connections) + 1]] <- pass_connection
                  
                  # ACTUALIZAR la posición actual del balón al final del pase
                  current_ball_x <- pass_end_x
                  current_ball_y <- pass_end_y
                }
              }
              
            } else {
              # CASO 2: Para cualquier otro evento (shot, etc.) - solo CARRY desde posición actual del balón
              end_x <- as.numeric(next_seq_event$x)
              end_y <- as.numeric(next_seq_event$y)
              
              if(!is.na(current_ball_x) && !is.na(current_ball_y) && !is.na(end_x) && !is.na(end_y)) {
                distance <- sqrt((end_x - current_ball_x)^2 + (end_y - current_ball_y)^2)
                
                if(distance > 1) {
                  connection <- data.frame(
                    sequence_id = current_event$sequence_id,
                    connection_type = "carry",
                    start_x = current_ball_x,
                    start_y = current_ball_y,
                    end_x = end_x,
                    end_y = end_y,
                    distance = distance,
                    from_event = current_event$event,
                    to_event = next_seq_event$event,
                    from_player = current_event$player_name,
                    to_player = next_seq_event$player_name,
                    time_from_recovery = current_event$time_from_recovery,
                    stringsAsFactors = FALSE
                  )
                  sequence_connections[[length(sequence_connections) + 1]] <- connection
                  
                  # ACTUALIZAR la posición actual del balón
                  current_ball_x <- end_x
                  current_ball_y <- end_y
                }
              }
            }
          }
          
          # Solo añadir la secuencia si tiene conexiones válidas Y termina en shot
          if(length(sequence_connections) > 0 && any(relevant_events$is_shot, na.rm = TRUE)) {
            # Combinar eventos y conexiones
            connections_df <- bind_rows(sequence_connections)
            
            final_sequence <- list(
              events = relevant_events,
              connections = connections_df
            )
            
            sequences[[length(sequences) + 1]] <- final_sequence
            cat("  ✅ Sequence added for matchday", current_matchday, "\n")
          }
          
          break  # Solo tomar el primer shot después del recovery
        }
      }
    }
  }
  
  cat("DEBUG - Total sequences found:", length(sequences), "\n")
  return(sequences)
}

# Cargar el mapping de fotos de jugadores
player_photo_map <- read.csv('www/player_photo_map.csv', 
                            stringsAsFactors = FALSE, 
                            sep = ";")

# Debug: verificar que se cargó correctamente
cat("DEBUG - player_photo_map cargado:", nrow(player_photo_map), "filas\n")
cat("DEBUG - Columnas:", paste(names(player_photo_map), collapse = ", "), "\n")
if(nrow(player_photo_map) > 0) {
  cat("DEBUG - Primeras 3 filas:\n")
  print(head(player_photo_map, 3))
}

get_player_photo <- function(player_name) {
  # Verificar que player_photo_map existe y tiene datos
  if (!exists("player_photo_map") || is.null(player_photo_map) || nrow(player_photo_map) == 0) {
    return("Jugadores/placeholder.png")
  }
  
  # Si el nombre es NA, vacío o NULL, devuelve el placeholder
  if (is.null(player_name) || is.na(player_name) || player_name == "" || length(player_name) == 0) {
    return("Jugadores/placeholder.png")
  }
  
  # Busca el nombre en el mapping
  tryCatch({
    row <- player_photo_map[player_photo_map$player_name == player_name, ]
    
    # Si no hay coincidencia, devuelve el placeholder
    if (nrow(row) == 0) {
      return("Jugadores/placeholder.png")
    }
    
    # Si hay coincidencia, devuelve la ruta de la foto
    photo_file <- row$photo_file[1]
    
    # Si el archivo existe y no es placeholder, devuelve la ruta completa
    if (!is.na(photo_file) && photo_file != "" && photo_file != "placeholder.png") {
      return(file.path("Jugadores", photo_file))
    } else {
      return("Jugadores/placeholder.png")
    }
  }, error = function(e) {
    # Si hay cualquier error, devuelve el placeholder
    return("Jugadores/placeholder.png")
  })
}

# ===== UI =====
ui <- dashboardPage(
  dashboardHeader(title = "Ball Recovery Analysis with Shot Sequences"),
  
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
        
        /* Scroll interno para el panel de Selected Matches */
        .selected-matches-scroll {
          max-height: 200px;
          overflow-y: auto;
          overflow-x: hidden;
          border: 1px solid #ddd;
          border-radius: 4px;
          background-color: white;
        }
        
        .selected-matches-scroll table {
          margin-bottom: 0;
        }
        
        .selected-matches-scroll::-webkit-scrollbar {
          width: 8px;
        }
        
        .selected-matches-scroll::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 4px;
        }
        
        .selected-matches-scroll::-webkit-scrollbar-thumb {
          background: #c1c1c1;
          border-radius: 4px;
        }
        
        .selected-matches-scroll::-webkit-scrollbar-thumb:hover {
          background: #a8a8a8;
        }
      ")),
      
      # JavaScript para descargas
      tags$script(HTML("
        Shiny.addCustomMessageHandler('downloadPanel', function(message) {
          // Ocultar todos los botones de descarga
          var buttons = document.querySelectorAll('.download-panel-btn');
          buttons.forEach(function(btn) { btn.style.visibility = 'hidden'; });

          setTimeout(function() {
            var element = document.querySelector(message.selector);
            if (element) {
              html2canvas(element, {
                scale: 2,
                useCORS: true,
                allowTaint: true,
                backgroundColor: '#ffffff'
              }).then(function(canvas) {
                // Volver a mostrar los botones
                buttons.forEach(function(btn) { btn.style.visibility = 'visible'; });
                var link = document.createElement('a');
                link.download = message.filename;
                link.href = canvas.toDataURL('image/png');
                link.click();
              });
            } else {
              // Volver a mostrar los botones si hay error
              buttons.forEach(function(btn) { btn.style.visibility = 'visible'; });
              console.log('Element not found:', message.selector);
            }
          }, 500);
        });
      ")),
      
      # Incluir html2canvas
      tags$script(src = "https://html2canvas.hertzen.com/dist/html2canvas.min.js")
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
                    p("• Click 'Update Analysis' to see accumulated recoveries"),
                    p("• Use 'Show Shot Sequences' to overlay complete recovery-to-shot sequences"),
                    p("• Heatmap shows intensity of recoveries across selected matches")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Display Options", status = "success", solidHeader = TRUE, width = 12,
                  materialSwitch("show_shot_sequences", "Show Recovery-to-Shot Sequences", 
                                 value = FALSE, status = "danger"),
                  br(),
                  div(
                    style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border-left: 4px solid #ffc107;",
                    h5("Complete Sequence Legend:"),
                    p("🔴 Ball Recovery Point | 🟢 Shot Event (Goal/Miss/Save/Post)"),
                    p("🟣 Purple dashed lines: Ball carries | 🔵 Blue arrows: Passes"),
                    p("Shows complete connected path from recovery to shot (max 15 seconds)")
                  )
                )
              ),

              fluidRow(
                box(
                  title = "Accumulated Ball Recoveries Heatmap with Shot Sequences", status = "success", solidHeader = TRUE, width = 12,
                  plotlyOutput("field_plot", height = "700px"),
                  br(),
                  downloadButton("download_main_plot", "Download Heatmap", class = "btn-primary", style = "margin: 5px;")
                )
              ),

              fluidRow(
                box(
                  title = "Export Options", status = "info", solidHeader = TRUE, width = 12,
                  column(12,
                         h5("Download Options:"),
                         p("Use the download buttons within each panel to export individual visualizations as PNG images.")
                  )
                )
              ),
              
              fluidRow(
                column(3,
                       div(id = "wrap_recovery_stats",
                         box(
                           title = "Recovery Statistics", status = "primary", solidHeader = TRUE, width = NULL,
                           tableOutput("recovery_stats"),
                           br(),
                           actionButton("download_recovery_stats", "Download", class = "btn-primary btn-sm download-panel-btn", style = "width: 100%;")
                         )
                       )
                ),
                column(3,
                       box(
                         title = "Selected Matches", status = "primary", solidHeader = TRUE, width = NULL,
                         div(
                           class = "selected-matches-scroll",
                           tableOutput("selected_matches_summary")
                         )
                       )
                ),
                column(3,
                       div(id = "wrap_shot_sequences_stats",
                         box(
                           title = "Shot Sequences", status = "danger", solidHeader = TRUE, width = NULL,
                           tableOutput("shot_sequences_stats"),
                           br(),
                           actionButton("download_shot_sequences_stats", "Download", class = "btn-danger btn-sm download-panel-btn", style = "width: 100%;")
                         )
                       )
                ),
                column(3,
                       div(id = "wrap_top3_recoverers",
                         box(
                           title = "Top 3 Ball Recovery Leaders",
                           width = NULL,
                           status = "primary",
                           solidHeader = TRUE,
                           DT::dataTableOutput("top3_recoverers"),
                           br(),
                           actionButton("download_top3_recoverers", "Download", class = "btn-primary btn-sm download-panel-btn", style = "width: 100%;")
                         )
                       )
                )
              ),

              fluidRow(
                column(3,
                       div(id = "wrap_top3_sequence_initiators",
                         box(
                           title = "Top 3 Sequence Initiators (Recoveries that led to shots)",
                           width = NULL,
                           status = "danger",
                           solidHeader = TRUE,
                           DT::dataTableOutput("top3_sequence_initiators"),
                           br(),
                           actionButton("download_sequence_initiators", "Download", class = "btn-danger btn-sm download-panel-btn", style = "width: 100%;")
                         )
                       )
                ),
                column(3,
                       div(id = "wrap_top3_pass_masters",
                         box(
                           title = "Top 3 Pass Masters in Sequences",
                           width = NULL,
                           status = "info",
                           solidHeader = TRUE,
                           DT::dataTableOutput("top3_pass_masters"),
                           br(),
                           actionButton("download_pass_masters", "Download", class = "btn-info btn-sm download-panel-btn", style = "width: 100%;")
                         )
                       )
                ),
                column(3,
                       div(id = "wrap_top3_ball_carriers",
                         box(
                           title = "Top 3 Ball Carriers in Sequences",
                           width = NULL,
                           status = "warning",
                           solidHeader = TRUE,
                           DT::dataTableOutput("top3_ball_carriers"),
                           br(),
                           actionButton("download_ball_carriers", "Download", class = "btn-warning btn-sm download-panel-btn", style = "width: 100%;")
                         )
                       )
                ),
                column(3,
                       div(id = "wrap_top3_shot_takers",
                         box(
                           title = "Top 3 Shot Takers",
                           width = NULL,
                           status = "success",
                           solidHeader = TRUE,
                           DT::dataTableOutput("top3_shot_takers"),
                           br(),
                           actionButton("download_shot_takers", "Download", class = "btn-success btn-sm download-panel-btn", style = "width: 100%;")
                         )
                       )
                )
              ),

              fluidRow(
                column(4,
                       div(id = "wrap_top3_1player_sequences",
                         box(
                           title = "Top 3 Sequences - 1 Player (Recovery → Shot)",
                           width = NULL,
                           status = "danger",
                           solidHeader = TRUE,
                           DT::dataTableOutput("top3_1player_sequences"),
                           br(),
                           actionButton("download_1player_sequences", "Download", class = "btn-danger btn-sm download-panel-btn", style = "width: 100%;")
                         )
                       )
                ),
                column(4,
                       div(id = "wrap_top3_2player_sequences",
                         box(
                           title = "Top 3 Sequences - 2 Players",
                           width = NULL,
                           status = "warning",
                           solidHeader = TRUE,
                           DT::dataTableOutput("top3_2player_sequences"),
                           br(),
                           actionButton("download_2player_sequences", "Download", class = "btn-warning btn-sm download-panel-btn", style = "width: 100%;")
                         )
                       )
                ),
                column(4,
                       div(id = "wrap_top3_3plus_sequences",
                         box(
                           title = "All Sequences - 3+ Players",
                           width = NULL,
                           status = "info",
                           solidHeader = TRUE,
                           DT::dataTableOutput("top3_3plus_sequences"),
                           br(),
                           actionButton("download_3plus_sequences", "Download", class = "btn-info btn-sm download-panel-btn", style = "width: 100%;")
                         )
                       )
                )
              ),

              fluidRow(
                column(12,
                       div(id = "wrap_top3_effective_sequences",
                         box(
                           title = "Sequence Effective (All Sequences that ended in Goal)",
                           width = NULL,
                           status = "success",
                           solidHeader = TRUE,
                           DT::dataTableOutput("top3_effective_sequences"),
                           br(),
                           actionButton("download_goal_sequences", "Download", class = "btn-success btn-sm download-panel-btn", style = "width: 200px;")
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
  
  # Datos reactivos de recuperaciones acumuladas
  accumulated_recoveries <- eventReactive(input$update_analysis, {
    req(team_season_data(), selected_matchdays())
    
    if(length(selected_matchdays()) == 0) {
      return(data.frame())
    }
    
    # Usar Team (team_name) para filtrar recuperaciones - igual que Won Duels
    recoveries <- get_accumulated_recoveries(team_season_data(), input$selected_team, selected_matchdays())
    return(recoveries)
  }, ignoreNULL = FALSE)
  
  # Datos reactivos de secuencias recovery-to-shot
  shot_sequences <- eventReactive(input$update_analysis, {
    req(team_season_data(), selected_matchdays())
    
    if(length(selected_matchdays()) == 0) {
      return(list())
    }
    
    sequences <- find_recovery_to_shot_sequences(team_season_data(), input$selected_team, selected_matchdays())
    return(sequences)
  }, ignoreNULL = FALSE)
  
  # Crear gráfico del campo con heatmap
  output$field_plot <- renderPlotly({
    req(accumulated_recoveries())
    recoveries <- accumulated_recoveries()
    shot_seqs <- shot_sequences()
    if(nrow(recoveries) == 0) {
      p <- draw_soccer_field_base() +
        labs(title = paste("Ball Recovery Analysis -", input$selected_team),
             subtitle = "No ball recoveries found for selected matches")
      return(ggplotly(p))
    }
    zones <- create_field_zones()
    recovery_heatmap <- recoveries %>%
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
        tooltip_text = paste0("Zone: (", bin_x.x, ",", bin_y.x, ")\nRecoveries: ", n)
      )
    heatmap_colors <- colorRampPalette(c(colors$heatmap_start, colors$heatmap_end))(20)
    # Campo base
    p <- draw_soccer_field_base()
    # Añadir heatmap con alpha bajo
    p <- p +
      geom_rect(
        data = recovery_heatmap,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
        color = "white", linetype = "dashed", size = 0.3, alpha = 0.8
      ) +
      scale_fill_gradientn(colors = heatmap_colors, 
                           name = "Recoveries",
                           guide = guide_colorbar(title.position = "top")) +
      geom_text(
        data = recovery_heatmap %>% filter(n > 0),
        aes(x = center_x, y = center_y, label = n),
        color = "#222", fontface = "bold", size = 4
      )
    
    # Estadísticas para el subtítulo
    total_recoveries <- nrow(recoveries)
    matches_analyzed <- length(selected_matchdays())
    def_recoveries <- sum(recoveries$field_zone == "Defensive Third", na.rm = TRUE)
    mid_recoveries <- sum(recoveries$field_zone == "Middle Third", na.rm = TRUE)
    att_recoveries <- sum(recoveries$field_zone == "Attacking Third", na.rm = TRUE)
    
    # Contar secuencias de shot
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
        linewidth = 1.5, color = colors$recovery_point
      ) +
      # Punta triangular de la flecha
      geom_polygon(data = arrow_triangle, aes(x = x, y = y), 
                   fill = colors$recovery_point, color = colors$recovery_point) +
      # Texto
      annotate("text", x = 50, y = 88, 
               label = "Direction of attack",
               color = colors$recovery_point,
               fontface = "bold", size = 4)
    
    # Añadir secuencias completas de shot si está activado
    if(input$show_shot_sequences && total_shot_sequences > 0) {
      
      cat("DEBUG - Processing", total_shot_sequences, "shot sequences\n")
      
      # Extraer eventos y conexiones de todas las secuencias
      all_events <- list()
      all_connections <- list()
      
      for(i in 1:length(shot_seqs)) {
        if(!is.null(shot_seqs[[i]]$events)) {
          all_events[[i]] <- shot_seqs[[i]]$events
        }
        if(!is.null(shot_seqs[[i]]$connections)) {
          all_connections[[i]] <- shot_seqs[[i]]$connections
        }
      }
      
      if(length(all_events) > 0) {
        sequence_events <- bind_rows(all_events) %>%
          mutate(
            x = as.numeric(x),
            y = as.numeric(y)
          ) %>%
          filter(!is.na(x) & !is.na(y))
        
        # VERIFICAR QUE HAY SHOTS VÁLIDOS
        valid_shots <- sequence_events %>% 
          filter(
            is_shot == TRUE &
              !is.na(x) & !is.na(y) & 
              x >= 83.5 & x <= 100 &  # Solo shots hacia arco rival
              y >= 0 & y <= 100 &
              event %in% c("Miss", "Goal", "Saved Shot", "Post", "Save")
          )
        
        cat("DEBUG - Valid shots found:", nrow(valid_shots), "\n")
        
        if(nrow(valid_shots) > 0) {
          
          # SOLO mostrar recoveries que tengan shots válidos en su secuencia
          valid_sequence_ids <- unique(valid_shots$sequence_id)
          
          # Filtrar recoveries solo de secuencias que tienen shots válidos
          sequence_recoveries <- sequence_events %>% 
            filter(is_recovery == TRUE & sequence_id %in% valid_sequence_ids)
          
          cat("DEBUG - Recoveries with valid shots:", nrow(sequence_recoveries), 
              "| Valid shots:", nrow(valid_shots), "\n")
          
          # VERIFICACIÓN: Debe haber el mismo número de recoveries que shots
          if(nrow(sequence_recoveries) != nrow(valid_shots)) {
            cat("WARNING - Mismatch: recoveries =", nrow(sequence_recoveries), 
                "shots =", nrow(valid_shots), "\n")
          }
          
          # Procesar conexiones si existen - SOLO de secuencias válidas
          if(length(all_connections) > 0) {
            connections_df <- bind_rows(all_connections)
            
            # Filtrar conexiones solo de secuencias que tienen shots válidos
            if(nrow(connections_df) > 0) {
              valid_connections <- connections_df %>%
                filter(sequence_id %in% valid_sequence_ids)
              
              # Solo carries y passes
              carries <- valid_connections %>% filter(connection_type == "carry")
              passes <- valid_connections %>% filter(connection_type == "pass")
              
              cat("DEBUG - Connections: carries =", nrow(carries), "| passes =", nrow(passes), "\n")
              
              # Añadir carries (líneas punteadas moradas)
              if(nrow(carries) > 0) {
                p <- p +
                  geom_segment(
                    data = carries,
                    aes(x = start_x, y = start_y, xend = end_x, yend = end_y,
                        text = paste("🟣 Ball Carry<br>",
                                     "Player:", from_player, "<br>",
                                     "Distance:", round(distance, 1), "m<br>",
                                     "Time:", round(time_from_recovery, 1), "s")),
                    color = colors$carry_color, 
                    linewidth = 0.8, alpha = 0.8,
                    linetype = "dashed"
                  )
              }
              
              # Añadir pases (flechas azules)
              if(nrow(passes) > 0) {
                p <- p +
                  geom_segment(
                    data = passes,
                    aes(x = start_x, y = start_y, xend = end_x, yend = end_y,
                        text = paste("🔵 Pass<br>",
                                     "Player:", from_player, "<br>",
                                     "Distance:", round(distance, 1), "m<br>",
                                     "Time:", round(time_from_recovery, 1), "s")),
                    color = colors$pass_color, 
                    linewidth = 0.6, alpha = 0.9,
                    arrow = arrow(length = unit(0.15, "cm"))
                  )
              }
            }
          }
          
          # Añadir recoveries (puntos rojos)
          if(nrow(sequence_recoveries) > 0) {
            p <- p +
              geom_point(
                data = sequence_recoveries,
                aes(x = x, y = y,
                    text = paste("🔴 Recovery → Shot<br>",
                                 "Player:", player_name, "<br>",
                                 "Time:", time_min, ":", sprintf("%02d", time_sec), "<br>",
                                 "Zone:", recovery_zone)),
                color = colors$recovery_point, size = 2, shape = 18,
                alpha = 0.8, stroke = 1
              )
          }
          
          # Añadir shots (CÍRCULOS VERDES)
          p <- p +
            geom_point(
              data = valid_shots,
              aes(x = x, y = y,
                  text = paste("🟢 SHOT:", event, "<br>",
                               "Player:", player_name, "<br>",
                               "Time:", time_min, ":", sprintf("%02d", time_sec), "<br>",
                               "Seconds from recovery:", round(time_from_recovery, 1))),
              color = colors$shot_point, size = 3, shape = 16,
              alpha = 0.9, stroke = 1.5
            )
          
          cat("DEBUG - Sequences successfully added to plot\n")
          
        } else {
          cat("DEBUG - NO valid shots found - not showing any sequences\n")
        }
      } else {
        cat("DEBUG - No events found in sequences\n")
      }
    } else {
      if(!input$show_shot_sequences) {
        cat("DEBUG - Shot sequences switch is OFF\n")
      } else {
        cat("DEBUG - No shot sequences found (total_shot_sequences = 0)\n")
      }
    }
    
    # Títulos actualizados
    subtitle_text <- paste(
      "Season:", input$selected_season, "| Matches analyzed:", matches_analyzed, "\n",
      "Total Recoveries:", total_recoveries, 
      "| Zones: Def(", def_recoveries, ") Mid(", mid_recoveries, ") Att(", att_recoveries, ")\n",
      "Shot Sequences:", total_shot_sequences,
      if(input$show_shot_sequences) " (shown - complete paths)" else " (hidden)",
      "| Max time: 8 seconds | Direction: Left to Right"
    )
    
    p <- p +
      labs(
        title = paste("Ball Recovery Analysis with Complete Shot Sequences -", input$selected_team),
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
  
  # Estadísticas de recovery
  output$recovery_stats <- renderTable({
    req(accumulated_recoveries())
    
    recoveries <- accumulated_recoveries()
    
    if(nrow(recoveries) == 0) {
      return(data.frame(Metric = "No recoveries found", Value = "0"))
    }
    
    stats <- data.frame(
      Metric = c("Total Recoveries", "Defensive Third", "Middle Third", "Attacking Third", 
                 "Matches Analyzed", "Avg per Match"),
      Value = c(
        nrow(recoveries),
        sum(recoveries$field_zone == "Defensive Third", na.rm = TRUE),
        sum(recoveries$field_zone == "Middle Third", na.rm = TRUE),
        sum(recoveries$field_zone == "Attacking Third", na.rm = TRUE),
        length(selected_matchdays()),
        round(nrow(recoveries) / length(selected_matchdays()), 1)
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
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # Estadísticas de secuencias de shot
  output$shot_sequences_stats <- renderTable({
    req(shot_sequences())
    
    shot_seqs <- shot_sequences()
    
    if(length(shot_seqs) == 0) {
      return(data.frame(Metric = "No shot sequences found", Value = "0"))
    }
    
    # Analizar las secuencias
    all_events <- list()
    for(i in 1:length(shot_seqs)) {
      if(!is.null(shot_seqs[[i]]$events)) {
        all_events[[i]] <- shot_seqs[[i]]$events
      }
    }
    
    if(length(all_events) > 0) {
      all_sequences <- bind_rows(all_events)
      shot_events <- all_sequences %>% filter(is_shot == TRUE)
      
      all_connections <- list()
      for(i in 1:length(shot_seqs)) {
        if(!is.null(shot_seqs[[i]]$connections)) {
          all_connections[[i]] <- shot_seqs[[i]]$connections
        }
      }
      
      if(length(all_connections) > 0) {
        connections_df <- bind_rows(all_connections)
        carries <- connections_df %>% filter(connection_type == "carry")
        passes <- connections_df %>% filter(connection_type == "pass")
      } else {
        carries <- data.frame()
        passes <- data.frame()
      }
      
      # Contar tipos de shots
      shot_types <- table(shot_events$event)
      goals <- ifelse("Goal" %in% names(shot_types), shot_types[["Goal"]], 0)
      saves <- ifelse("Save" %in% names(shot_types), shot_types[["Save"]], 0) + 
        ifelse("Saved Shot" %in% names(shot_types), shot_types[["Saved Shot"]], 0)
      misses <- ifelse("Miss" %in% names(shot_types), shot_types[["Miss"]], 0)
      posts <- ifelse("Post" %in% names(shot_types), shot_types[["Post"]], 0)
      
      # Tiempo promedio desde recovery hasta shot
      avg_time_to_shot <- round(mean(shot_events$time_from_recovery, na.rm = TRUE), 1)
      
      # Estadísticas de carries y pases
      total_carries <- nrow(carries)
      total_passes <- nrow(passes)
      avg_carry_distance <- ifelse(nrow(carries) > 0, round(mean(carries$distance, na.rm = TRUE), 1), 0)
      
      stats <- data.frame(
        Metric = c("Total Sequences", "Goals", "Saves", "Misses", "Posts", 
                   "Avg Time to Shot", "Total Carries", "Avg Carry Distance", "Total Passes"),
        Value = c(
          length(shot_seqs),
          goals,
          saves,
          misses,
          posts,
          paste0(avg_time_to_shot, "s"),
          total_carries,
          paste0(avg_carry_distance, "m"),
          total_passes
        )
      )
    } else {
      stats <- data.frame(
        Metric = "No valid sequences found",
        Value = "0"
      )
    }
    
    stats
  }, striped = TRUE, hover = TRUE)
  
  # Top 3 recuperadores dinámico
  output$top3_recoverers <- DT::renderDataTable({
    req(accumulated_recoveries())
    recoveries <- accumulated_recoveries()
    if(nrow(recoveries) == 0) {
      return(DT::datatable(
        data.frame(Player = "No recoveries found", Recoveries = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    top3_recoverers <- recoveries %>%
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
      top3_recoverers[, c("Player", "recoveries")],
      options = list(pageLength = 3, dom = 't'),
      rownames = FALSE,
      colnames = c("Player", "Recoveries"),
      escape = FALSE
    )
  })
  
  # Top 3 secuencias iniciadoras
  output$top3_sequence_initiators <- DT::renderDataTable({
    req(shot_sequences())
    
    shot_seqs <- shot_sequences()
    
    if(length(shot_seqs) == 0) {
      return(DT::datatable(
        data.frame(Player = "No sequences found", Sequences = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Obtener las secuencias completas
    all_sequences <- list()
    for(i in 1:length(shot_seqs)) {
      if(!is.null(shot_seqs[[i]]$events)) {
        all_sequences[[i]] <- shot_seqs[[i]]$events
      }
    }
    
    if(length(all_sequences) > 0) {
      all_sequences <- bind_rows(all_sequences)
      
      # Filtrar solo los eventos de recuperación inicial (is_recovery = TRUE)
      recovery_initiators <- all_sequences %>% 
        filter(is_recovery == TRUE)
      
      if(nrow(recovery_initiators) > 0) {
        top3_sequence_initiators <- recovery_initiators %>%
          group_by(player_name) %>%
          summarise(sequences = n()) %>%
          arrange(desc(sequences)) %>%
          slice_head(n = 3) %>%
          mutate(
            Photo = sapply(player_name, function(name) {
              path <- get_player_photo(name)
              sprintf('<img src="%s" height="40" style="border-radius:50%%;margin-right:8px;">', path)
            }),
            Player = paste0(Photo, player_name)
          )
        
        DT::datatable(
          top3_sequence_initiators[, c("Player", "sequences")],
          options = list(pageLength = 3, dom = 't'),
          rownames = FALSE,
          colnames = c("Player", "Sequences"),
          escape = FALSE
        )
      } else {
        return(DT::datatable(
          data.frame(Player = "No recovery initiators found", Sequences = NA),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
    } else {
      return(DT::datatable(
        data.frame(Player = "No sequences found", Sequences = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
  })
  
  # Top 3 Pass Masters en secuencias
  output$top3_pass_masters <- DT::renderDataTable({
    req(shot_sequences())
    
    shot_seqs <- shot_sequences()
    
    if(length(shot_seqs) == 0) {
      return(DT::datatable(
        data.frame(Player = "No sequences found", Passes = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Obtener todas las conexiones de pases
    all_connections <- list()
    for(i in 1:length(shot_seqs)) {
      if(!is.null(shot_seqs[[i]]$connections)) {
        all_connections[[i]] <- shot_seqs[[i]]$connections
      }
    }
    
    if(length(all_connections) > 0) {
      connections_df <- bind_rows(all_connections)
      passes <- connections_df %>% filter(connection_type == "pass")
      
      if(nrow(passes) > 0) {
        top3_pass_masters <- passes %>%
          group_by(from_player) %>%
          summarise(passes = n()) %>%
          arrange(desc(passes)) %>%
          slice_head(n = 3) %>%
          mutate(
            Photo = sapply(from_player, function(name) {
              path <- get_player_photo(name)
              sprintf('<img src="%s" height="40" style="border-radius:50%%;margin-right:8px;">', path)
            }),
            Player = paste0(Photo, from_player)
          )
        
        DT::datatable(
          top3_pass_masters[, c("Player", "passes")],
          options = list(pageLength = 3, dom = 't'),
          rownames = FALSE,
          colnames = c("Player", "Passes"),
          escape = FALSE
        )
      } else {
        return(DT::datatable(
          data.frame(Player = "No passes found", Passes = NA),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
    } else {
      return(DT::datatable(
        data.frame(Player = "No sequences found", Passes = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
  })
  
  # Top 3 Ball Carriers en secuencias
  output$top3_ball_carriers <- DT::renderDataTable({
    req(shot_sequences())
    
    shot_seqs <- shot_sequences()
    
    if(length(shot_seqs) == 0) {
      return(DT::datatable(
        data.frame(Player = "No sequences found", Carries = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Obtener todas las conexiones de carries
    all_connections <- list()
    for(i in 1:length(shot_seqs)) {
      if(!is.null(shot_seqs[[i]]$connections)) {
        all_connections[[i]] <- shot_seqs[[i]]$connections
      }
    }
    
    if(length(all_connections) > 0) {
      connections_df <- bind_rows(all_connections)
      carries <- connections_df %>% filter(connection_type == "carry")
      
      if(nrow(carries) > 0) {
        top3_ball_carriers <- carries %>%
          group_by(from_player) %>%
          summarise(carries = n()) %>%
          arrange(desc(carries)) %>%
          slice_head(n = 3) %>%
          mutate(
            Photo = sapply(from_player, function(name) {
              path <- get_player_photo(name)
              sprintf('<img src="%s" height="40" style="border-radius:50%%;margin-right:8px;">', path)
            }),
            Player = paste0(Photo, from_player)
          )
        
        DT::datatable(
          top3_ball_carriers[, c("Player", "carries")],
          options = list(pageLength = 3, dom = 't'),
          rownames = FALSE,
          colnames = c("Player", "Carries"),
          escape = FALSE
        )
      } else {
        return(DT::datatable(
          data.frame(Player = "No carries found", Carries = NA),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
    } else {
      return(DT::datatable(
        data.frame(Player = "No sequences found", Carries = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
  })
  
  # Top 3 Secuencias más efectivas (por tiempo)
  output$top3_effective_sequences <- DT::renderDataTable({
    req(shot_sequences())
    
    shot_seqs <- shot_sequences()
    
    if(length(shot_seqs) == 0) {
      return(DT::datatable(
        data.frame(Sequence = "No sequences found", Time = NA, Player = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Analizar secuencias que terminaron en gol
    goal_sequences <- list()
    
    for(i in 1:length(shot_seqs)) {
      if(!is.null(shot_seqs[[i]]$events) && !is.null(shot_seqs[[i]]$connections)) {
        events <- shot_seqs[[i]]$events
        connections <- shot_seqs[[i]]$connections
        
        # Verificar si la secuencia terminó en gol
        shot_event <- events %>% filter(is_shot == TRUE) %>% first()
        
        if(!is.null(shot_event) && shot_event$event == "Goal") {
          # Es una secuencia que terminó en gol
          recovery_player <- events %>% filter(is_recovery == TRUE) %>% pull(player_name) %>% first()
          goal_player <- shot_event$player_name
          
          # Obtener todos los jugadores involucrados
          all_players <- c(recovery_player, goal_player)
          
          # Agregar jugadores de conexiones
          if(nrow(connections) > 0) {
            pass_players <- connections %>% filter(connection_type == "pass") %>% pull(from_player) %>% unique()
            carry_players <- connections %>% filter(connection_type == "carry") %>% pull(from_player) %>% unique()
            all_players <- c(all_players, pass_players, carry_players) %>% unique()
          }
          
          # Crear secuencia visual completa
          sequence_visual <- ""
          for(j in 1:length(all_players)) {
            player <- all_players[j]
            
            # Agregar foto del jugador
            photo_path <- get_player_photo(player)
            photo_html <- sprintf('<img src="%s" height="25" style="border-radius:50%%;margin:0 3px;">', photo_path)
            
            # Agregar nombre del jugador
            player_html <- sprintf('<span style="font-weight:bold;color:#333;">%s</span>', player)
            
            # Agregar icono según el rol
            if(j == 1) {
              role_text <- "Recovery"
            } else if(j == length(all_players)) {
              role_text <- "GOAL"
            } else {
              role_text <- "Pass/Carry"
            }
            
            # Construir elemento del jugador
            player_element <- sprintf('%s %s <span style="color:#666;font-size:10px;">(%s)</span>', 
                                    photo_html, player_html, role_text)
            
            sequence_visual <- paste0(sequence_visual, player_element)
            
            # Agregar flecha si no es el último
            if(j < length(all_players)) {
              sequence_visual <- paste0(sequence_visual, ' <span style="color:#999;font-size:14px;">→</span> ')
            }
          }
          
          goal_sequences[[length(goal_sequences) + 1]] <- data.frame(
            sequence = sequence_visual,
            time_from_recovery = shot_event$time_from_recovery,
            player_count = length(all_players),
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if(length(goal_sequences) > 0) {
      sequences_df <- bind_rows(goal_sequences)
      
      # Ordenar por tiempo (más rápidas primero)
      effective_sequences <- sequences_df %>%
        arrange(time_from_recovery) %>%
        mutate(
          Time = paste0(round(time_from_recovery, 1), "s"),
          Players = paste0(player_count, " players")
        )
      
      DT::datatable(
        effective_sequences[, c("sequence", "Time", "Players")],
        options = list(
          pageLength = 10,
          dom = 'ft',
          searching = TRUE,
          ordering = TRUE,
          info = TRUE,
          paging = TRUE
        ),
        rownames = FALSE,
        colnames = c("Complete Goal Sequence", "Time", "Players"),
        escape = FALSE
      )
    } else {
      return(DT::datatable(
        data.frame(Sequence = "No goal sequences found", Time = NA, Players = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
  })
  
  # Top 3 Shot Takers (disparadores)
  output$top3_shot_takers <- DT::renderDataTable({
    req(shot_sequences())
    
    shot_seqs <- shot_sequences()
    
    if(length(shot_seqs) == 0) {
      return(DT::datatable(
        data.frame(Player = "No sequences found", Shots = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Obtener las secuencias completas
    all_sequences <- list()
    for(i in 1:length(shot_seqs)) {
      if(!is.null(shot_seqs[[i]]$events)) {
        all_sequences[[i]] <- shot_seqs[[i]]$events
      }
    }
    
    if(length(all_sequences) > 0) {
      all_sequences <- bind_rows(all_sequences)
      
      # Filtrar solo los eventos de disparo
      shot_events <- all_sequences %>% 
        filter(is_shot == TRUE)
      
      if(nrow(shot_events) > 0) {
        top3_shot_takers <- shot_events %>%
          group_by(player_name) %>%
          summarise(shots = n()) %>%
          arrange(desc(shots)) %>%
          slice_head(n = 3) %>%
          mutate(
            Photo = sapply(player_name, function(name) {
              path <- get_player_photo(name)
              sprintf('<img src="%s" height="40" style="border-radius:50%%;margin-right:8px;">', path)
            }),
            Player = paste0(Photo, player_name)
          )
        
        DT::datatable(
          top3_shot_takers[, c("Player", "shots")],
          options = list(pageLength = 3, dom = 't'),
          rownames = FALSE,
          colnames = c("Player", "Shots"),
          escape = FALSE
        )
      } else {
        return(DT::datatable(
          data.frame(Player = "No shot events found", Shots = NA),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
    } else {
      return(DT::datatable(
        data.frame(Player = "No sequences found", Shots = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
  })
  
  # Top 3 Secuencias de 1 Jugador (Recovery → Shot directo)
  output$top3_1player_sequences <- DT::renderDataTable({
    req(shot_sequences())
    
    shot_seqs <- shot_sequences()
    
    if(length(shot_seqs) == 0) {
      return(DT::datatable(
        data.frame(Player = "No sequences found", Count = NA, Time = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Analizar secuencias de 1 jugador
    one_player_sequences <- list()
    
    for(i in 1:length(shot_seqs)) {
      if(!is.null(shot_seqs[[i]]$events)) {
        events <- shot_seqs[[i]]$events
        
        # Verificar si es secuencia de 1 jugador (recovery y shot del mismo jugador)
        recovery_player <- events %>% filter(is_recovery == TRUE) %>% pull(player_name) %>% first()
        shot_player <- events %>% filter(is_shot == TRUE) %>% pull(player_name) %>% first()
        
        if(!is.na(recovery_player) && !is.na(shot_player) && recovery_player == shot_player) {
          # Es secuencia de 1 jugador
          shot_event <- events %>% filter(is_shot == TRUE) %>% first()
          
          one_player_sequences[[length(one_player_sequences) + 1]] <- data.frame(
            player_name = recovery_player,
            time_from_recovery = shot_event$time_from_recovery,
            event = shot_event$event,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if(length(one_player_sequences) > 0) {
      sequences_df <- bind_rows(one_player_sequences)
      
      top3_1player <- sequences_df %>%
        group_by(player_name) %>%
        summarise(
          count = n(),
          avg_time = round(mean(time_from_recovery, na.rm = TRUE), 1)
        ) %>%
        arrange(desc(count)) %>%
        slice_head(n = 3) %>%
        mutate(
          Photo = sapply(player_name, function(name) {
            path <- get_player_photo(name)
            sprintf('<img src="%s" height="30" style="border-radius:50%%;margin-right:8px;">', path)
          }),
          Player = paste0(Photo, player_name)
        )
      
      DT::datatable(
        top3_1player[, c("Player", "count", "avg_time")],
        options = list(pageLength = 3, dom = 't'),
        rownames = FALSE,
        colnames = c("Player", "Sequences", "Avg Time (s)"),
        escape = FALSE
      )
    } else {
      return(DT::datatable(
        data.frame(Player = "No 1-player sequences found", Count = NA, Time = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
  })
  
  # Top 3 Secuencias de 2 Jugadores
  output$top3_2player_sequences <- DT::renderDataTable({
    req(shot_sequences())
    
    shot_seqs <- shot_sequences()
    
    if(length(shot_seqs) == 0) {
      return(DT::datatable(
        data.frame(Sequence = "No sequences found", Count = NA, Time = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Analizar secuencias de 2 jugadores
    two_player_sequences <- list()
    
    for(i in 1:length(shot_seqs)) {
      if(!is.null(shot_seqs[[i]]$events) && !is.null(shot_seqs[[i]]$connections)) {
        events <- shot_seqs[[i]]$events
        connections <- shot_seqs[[i]]$connections
        
        recovery_player <- events %>% filter(is_recovery == TRUE) %>% pull(player_name) %>% first()
        shot_player <- events %>% filter(is_shot == TRUE) %>% pull(player_name) %>% first()
        
        # Obtener jugadores únicos involucrados
        all_players <- c(recovery_player, shot_player)
        
        # Agregar jugadores de conexiones
        if(nrow(connections) > 0) {
          pass_players <- connections %>% filter(connection_type == "pass") %>% pull(from_player) %>% unique()
          carry_players <- connections %>% filter(connection_type == "carry") %>% pull(from_player) %>% unique()
          all_players <- c(all_players, pass_players, carry_players) %>% unique()
        }
        
        # Verificar si es secuencia de exactamente 2 jugadores
        if(length(all_players) == 2 && recovery_player != shot_player) {
          shot_event <- events %>% filter(is_shot == TRUE) %>% first()
          
          # Crear secuencia visual
          photo1 <- get_player_photo(all_players[1])
          photo2 <- get_player_photo(all_players[2])
          
          sequence_visual <- sprintf(
            '<img src="%s" height="25" style="border-radius:50%%;margin:0 3px;">%s → <img src="%s" height="25" style="border-radius:50%%;margin:0 3px;">%s',
            photo1, all_players[1], photo2, all_players[2]
          )
          
          two_player_sequences[[length(two_player_sequences) + 1]] <- data.frame(
            sequence = sequence_visual,
            player1 = all_players[1],
            player2 = all_players[2],
            time_from_recovery = shot_event$time_from_recovery,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if(length(two_player_sequences) > 0) {
      sequences_df <- bind_rows(two_player_sequences)
      
      top3_2player <- sequences_df %>%
        group_by(player1, player2) %>%
        summarise(
          count = n(),
          avg_time = round(mean(time_from_recovery, na.rm = TRUE), 1)
        ) %>%
        arrange(desc(count)) %>%
        slice_head(n = 3) %>%
        mutate(
          photo1 = sapply(player1, get_player_photo),
          photo2 = sapply(player2, get_player_photo),
          Sequence = sprintf(
            '<img src="%s" height="25" style="border-radius:50%%;margin:0 3px;">%s → <img src="%s" height="25" style="border-radius:50%%;margin:0 3px;">%s',
            photo1, player1, photo2, player2
          )
        )
      
      DT::datatable(
        top3_2player[, c("Sequence", "count", "avg_time")],
        options = list(pageLength = 3, dom = 't'),
        rownames = FALSE,
        colnames = c("Sequence", "Count", "Avg Time (s)"),
        escape = FALSE
      )
    } else {
      return(DT::datatable(
        data.frame(Sequence = "No 2-player sequences found", Count = NA, Time = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
  })
  
  # Top 3 Secuencias de 3+ Jugadores
  output$top3_3plus_sequences <- DT::renderDataTable({
    req(shot_sequences())
    
    shot_seqs <- shot_sequences()
    
    if(length(shot_seqs) == 0) {
      return(DT::datatable(
        data.frame(Pattern = "No sequences found", Count = NA, Players = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Analizar secuencias de 3+ jugadores
    three_plus_sequences <- list()
    
    for(i in 1:length(shot_seqs)) {
      if(!is.null(shot_seqs[[i]]$events) && !is.null(shot_seqs[[i]]$connections)) {
        events <- shot_seqs[[i]]$events
        connections <- shot_seqs[[i]]$connections
        
        recovery_player <- events %>% filter(is_recovery == TRUE) %>% pull(player_name) %>% first()
        shot_player <- events %>% filter(is_shot == TRUE) %>% pull(player_name) %>% first()
        
        # Obtener jugadores únicos involucrados
        all_players <- c(recovery_player, shot_player)
        
        # Agregar jugadores de conexiones
        if(nrow(connections) > 0) {
          pass_players <- connections %>% filter(connection_type == "pass") %>% pull(from_player) %>% unique()
          carry_players <- connections %>% filter(connection_type == "carry") %>% pull(from_player) %>% unique()
          all_players <- c(all_players, pass_players, carry_players) %>% unique()
        }
        
        # Verificar si es secuencia de 3+ jugadores
        if(length(all_players) >= 3) {
          shot_event <- events %>% filter(is_shot == TRUE) %>% first()
          
          # Crear secuencia visual completa
          sequence_visual <- ""
          for(j in 1:length(all_players)) {
            player <- all_players[j]
            
            # Agregar foto del jugador
            photo_path <- get_player_photo(player)
            photo_html <- sprintf('<img src="%s" height="25" style="border-radius:50%%;margin:0 3px;">', photo_path)
            
            # Agregar nombre del jugador
            player_html <- sprintf('<span style="font-weight:bold;color:#333;">%s</span>', player)
            
            # Agregar icono según el rol
            if(j == 1) {
              role_text <- "Recovery"
            } else if(j == length(all_players)) {
              role_text <- "Shot"
            } else {
              role_text <- "Pass/Carry"
            }
            
            # Construir elemento del jugador
            player_element <- sprintf('%s %s <span style="color:#666;font-size:10px;">(%s)</span>', 
                                    photo_html, player_html, role_text)
            
            sequence_visual <- paste0(sequence_visual, player_element)
            
            # Agregar flecha si no es el último
            if(j < length(all_players)) {
              sequence_visual <- paste0(sequence_visual, ' <span style="color:#999;font-size:14px;">→</span> ')
            }
          }
          
          three_plus_sequences[[length(three_plus_sequences) + 1]] <- data.frame(
            sequence = sequence_visual,
            player_count = length(all_players),
            time_from_recovery = shot_event$time_from_recovery,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if(length(three_plus_sequences) > 0) {
      sequences_df <- bind_rows(three_plus_sequences)
      
      # Top 3 secuencias más rápidas de 3+ jugadores
      top3_fastest_3plus <- sequences_df %>%
        arrange(time_from_recovery) %>%
        slice_head(n = 3) %>%
        mutate(
          Time = paste0(round(time_from_recovery, 1), "s"),
          Players = paste0(player_count, " players")
        )
      
      DT::datatable(
        top3_fastest_3plus[, c("sequence", "Time", "Players")],
        options = list(
          pageLength = 3,
          dom = 't',
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          paging = FALSE
        ),
        rownames = FALSE,
        colnames = c("Complete Sequence", "Time", "Players"),
        escape = FALSE
      )
    } else {
      return(DT::datatable(
        data.frame(Pattern = "No 3+ player sequences found", Count = NA, Time = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
  })
  
  # ===== FUNCIONES DE DESCARGA INDIVIDUAL =====
  
  # Descargar Goal Sequences
  observeEvent(input$download_goal_sequences, {
    # Crear nombre de archivo
    filename <- paste0("Goal_Sequences_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    
    # Capturar el panel específico
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_top3_effective_sequences",
      filename = filename
    ))
  })
  
  # Descargar Sequence Initiators
  observeEvent(input$download_sequence_initiators, {
    filename <- paste0("Sequence_Initiators_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_top3_sequence_initiators",
      filename = filename
    ))
  })
  
  # Descargar Pass Masters
  observeEvent(input$download_pass_masters, {
    filename <- paste0("Pass_Masters_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_top3_pass_masters",
      filename = filename
    ))
  })
  
  # Descargar Ball Carriers
  observeEvent(input$download_ball_carriers, {
    filename <- paste0("Ball_Carriers_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_top3_ball_carriers",
      filename = filename
    ))
  })
  
  # Descargar Shot Takers
  observeEvent(input$download_shot_takers, {
    filename <- paste0("Shot_Takers_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_top3_shot_takers",
      filename = filename
    ))
  })
  
  # Descargar 1-Player Sequences
  observeEvent(input$download_1player_sequences, {
    filename <- paste0("1Player_Sequences_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_top3_1player_sequences",
      filename = filename
    ))
  })
  
  # Descargar 2-Player Sequences
  observeEvent(input$download_2player_sequences, {
    filename <- paste0("2Player_Sequences_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_top3_2player_sequences",
      filename = filename
    ))
  })
  
  # Descargar 3+ Player Sequences
  observeEvent(input$download_3plus_sequences, {
    filename <- paste0("3Plus_Sequences_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_top3_3plus_sequences",
      filename = filename
    ))
  })
  
  # Descargar Main Heatmap
  output$download_main_plot <- downloadHandler(
    filename = function() {
      paste0("Main_Heatmap_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      req(accumulated_recoveries())
      recoveries <- accumulated_recoveries()
      shot_seqs <- shot_sequences()
      if(nrow(recoveries) == 0) {
        p <- draw_soccer_field_base() +
          labs(title = paste("Ball Recovery Analysis -", input$selected_team),
               subtitle = "No ball recoveries found for selected matches")
        ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
        return()
      }
      zones <- create_field_zones()
      recovery_heatmap <- recoveries %>%
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
          tooltip_text = paste0("Zone: (", bin_x.x, ",", bin_y.x, ")\nRecoveries: ", n)
        )
      heatmap_colors <- colorRampPalette(c(colors$heatmap_start, colors$heatmap_end))(20)
      p <- draw_soccer_field_base()
      p <- p +
        geom_rect(
          data = recovery_heatmap,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
          color = "white", linetype = "dashed", size = 0.3, alpha = 0.8
        ) +
        scale_fill_gradientn(colors = heatmap_colors, 
                             name = "Recoveries",
                             guide = guide_colorbar(title.position = "top")) +
        geom_text(
          data = recovery_heatmap %>% filter(n > 0),
          aes(x = center_x, y = center_y, label = n),
          color = "#222", fontface = "bold", size = 4
        )
      # Estadísticas para el subtítulo
      total_recoveries <- nrow(recoveries)
      matches_analyzed <- length(selected_matchdays())
      def_recoveries <- sum(recoveries$field_zone == "Defensive Third", na.rm = TRUE)
      mid_recoveries <- sum(recoveries$field_zone == "Middle Third", na.rm = TRUE)
      att_recoveries <- sum(recoveries$field_zone == "Attacking Third", na.rm = TRUE)
      total_shot_sequences <- length(shot_seqs)
      # Flecha de dirección de ataque
      arrow_triangle <- data.frame(
        x = c(80, 77, 77, 80),
        y = c(95, 97, 93, 95)
      )
      p <- p +
        geom_segment(
          aes(x = 20, y = 95, xend = 77, yend = 95),
          linewidth = 1.5, color = colors$recovery_point
        ) +
        geom_polygon(data = arrow_triangle, aes(x = x, y = y), 
                     fill = colors$recovery_point, color = colors$recovery_point) +
        annotate("text", x = 50, y = 88, 
                 label = "Direction of attack",
                 color = colors$recovery_point,
                 fontface = "bold", size = 4)
      # Añadir secuencias completas de shot si está activado
      if(input$show_shot_sequences && total_shot_sequences > 0) {
        all_events <- list()
        all_connections <- list()
        for(i in 1:length(shot_seqs)) {
          if(!is.null(shot_seqs[[i]]$events)) {
            all_events[[i]] <- shot_seqs[[i]]$events
          }
          if(!is.null(shot_seqs[[i]]$connections)) {
            all_connections[[i]] <- shot_seqs[[i]]$connections
          }
        }
        if(length(all_events) > 0) {
          sequence_events <- bind_rows(all_events) %>%
            mutate(x = as.numeric(x), y = as.numeric(y)) %>%
            filter(!is.na(x) & !is.na(y))
          valid_shots <- sequence_events %>% 
            filter(
              is_shot == TRUE &
                !is.na(x) & !is.na(y) & 
                x >= 83.5 & x <= 100 &
                y >= 0 & y <= 100 &
                event %in% c("Miss", "Goal", "Saved Shot", "Post", "Save")
            )
          if(nrow(valid_shots) > 0) {
            valid_sequence_ids <- unique(valid_shots$sequence_id)
            sequence_recoveries <- sequence_events %>% 
              filter(is_recovery == TRUE & sequence_id %in% valid_sequence_ids)
            if(length(all_connections) > 0) {
              connections_df <- bind_rows(all_connections)
              if(nrow(connections_df) > 0) {
                valid_connections <- connections_df %>%
                  filter(sequence_id %in% valid_sequence_ids)
                carries <- valid_connections %>% filter(connection_type == "carry")
                passes <- valid_connections %>% filter(connection_type == "pass")
                if(nrow(carries) > 0) {
                  p <- p +
                    geom_segment(
                      data = carries,
                      aes(x = start_x, y = start_y, xend = end_x, yend = end_y),
                      color = colors$carry_color, 
                      linewidth = 0.8, alpha = 0.8,
                      linetype = "dashed"
                    )
                }
                if(nrow(passes) > 0) {
                  p <- p +
                    geom_segment(
                      data = passes,
                      aes(x = start_x, y = start_y, xend = end_x, yend = end_y),
                      color = colors$pass_color, 
                      linewidth = 0.6, alpha = 0.9,
                      arrow = arrow(length = unit(0.15, "cm"))
                    )
                }
              }
            }
            if(nrow(sequence_recoveries) > 0) {
              p <- p +
                geom_point(
                  data = sequence_recoveries,
                  aes(x = x, y = y),
                  color = colors$recovery_point, size = 2, shape = 18,
                  alpha = 0.8, stroke = 1
                )
            }
            p <- p +
              geom_point(
                data = valid_shots,
                aes(x = x, y = y),
                color = colors$shot_point, size = 3, shape = 16,
                alpha = 0.9, stroke = 1.5
              )
          }
        }
      }
      subtitle_text <- paste(
        "Season:", input$selected_season, "| Matches analyzed:", matches_analyzed, "\n",
        "Total Recoveries:", total_recoveries, 
        "| Zones: Def(", def_recoveries, ") Mid(", mid_recoveries, ") Att(", att_recoveries, ")\n",
        "Shot Sequences:", total_shot_sequences,
        if(input$show_shot_sequences) " (shown - complete paths)" else " (hidden)",
        "| Max time: 8 seconds | Direction: Left to Right"
      )
      p <- p +
        labs(
          title = paste("Ball Recovery Analysis with Complete Shot Sequences -", input$selected_team),
          subtitle = subtitle_text
        ) +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.margin = margin(t = 10),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
          plot.background = element_rect(fill = "white", color = NA)
        )
      ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
    }
  )
  
  # Descargar Recovery Statistics
  observeEvent(input$download_recovery_stats, {
    filename <- paste0("Recovery_Statistics_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_recovery_stats",
      filename = filename
    ))
  })
  
  # Descargar Shot Sequences Stats
  observeEvent(input$download_shot_sequences_stats, {
    filename <- paste0("Shot_Sequences_Stats_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_shot_sequences_stats",
      filename = filename
    ))
  })
  
  # Descargar Top 3 Ball Recovery Leaders
  observeEvent(input$download_top3_recoverers, {
    filename <- paste0("Top3_Ball_Recovery_Leaders_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_top3_recoverers",
      filename = filename
    ))
  })
}

# ===== EJECUTAR APP =====
shinyApp(ui = ui, server = server)