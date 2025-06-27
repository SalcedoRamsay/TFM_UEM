library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(ggplot2)
library(ggforce)
library(shinyWidgets)
library(gridExtra)
library(grid)
library(magick)

# ===== CONFIGURACIÓN INICIAL =====
data <- read.csv('/Users/sebastiansalcedo/Documents/UEM/TFM/base_datos_completa_epl.csv', 
                 stringsAsFactors = FALSE, 
                 encoding = "UTF-8")

# ===== PALETA DE COLORES =====
colors <- list(
  background = "#F5F5F5",
  background_light = "#FFFFFF",
  text_primary = "#333333",
  accent = "#1A78CF",
  lines = "#777777",
  avg_line = "#FFA500",
  heatmap_start = "#FFF5F5",
  heatmap_end = "#D32F2F",
  home_team = "#007BFF",
  away_team = "#FF0000"
)

# ===== FUNCIONES AUXILIARES =====
draw_soccer_field <- function(fill_color = colors$background_light, line_color = colors$lines) {
  ggplot() +
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), fill = fill_color, color = line_color, linewidth = 1) +
    geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color = line_color, linewidth = 0.5) +
    geom_circle(aes(x0 = 50, y0 = 50, r = 9.15), color = line_color, fill = NA, linewidth = 0.5) +
    geom_point(aes(x = 50, y = 50), size = 0.8, color = line_color) +
    geom_rect(aes(xmin = 0, xmax = 16.5, ymin = 21.1, ymax = 78.9), fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_rect(aes(xmin = 83.5, xmax = 100, ymin = 21.1, ymax = 78.9), fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = 36.8, ymax = 63.2), fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_rect(aes(xmin = 94.5, xmax = 100, ymin = 36.8, ymax = 63.2), fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_point(aes(x = 11, y = 50), size = 0.8, color = line_color) +
    geom_point(aes(x = 89, y = 50), size = 0.8, color = line_color) +
    geom_arc(aes(x0 = 11, y0 = 50, r = 9.15, start = 37*pi/180, end = 143*pi/180), color = line_color, linewidth = 0.5) +
    geom_arc(aes(x0 = 89, y0 = 50, r = 9.15, start = -37*pi/180, end = -143*pi/180), color = line_color, linewidth = 0.5) +
    coord_fixed() +
    theme_void()
}

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

get_team_goal_kicks <- function(match_data, team_name_filter, matchdays) {
  team_gk_passes <- match_data %>%
    filter(
      team_name == team_name_filter,
      (Goal.Kick == 1 | position == "GK"),
      type_id == 1, # Pase
      matchday_num %in% matchdays
    ) %>%
    mutate(
      x = as.numeric(x),
      y = as.numeric(y),
      Pass.End.X = as.numeric(Pass.End.X),
      Pass.End.Y = as.numeric(Pass.End.Y),
      outcome = as.integer(outcome)
    ) %>%
    filter(
      !is.na(x) & !is.na(y) & !is.na(Pass.End.X) & !is.na(Pass.End.Y) &
        !is.na(outcome) & outcome %in% c(0, 1)
    )
  return(team_gk_passes)
}

# Nueva función para obtener pases de continuación después de distribuciones exitosas
get_buildup_after_successful_passes <- function(match_data, team_name_filter, matchdays) {
  # Obtener pases exitosos de distribución (Goal Kicks + GK Open Play)
  successful_distribution <- match_data %>%
    filter(
      team_name == team_name_filter,
      matchday_num %in% matchdays,
      type_id == 1, # Pases
      outcome == 1, # Solo exitosos
      (Goal.Kick == 1 | position == "GK"), # Goal Kicks (cualquier jugador) + GK Open Play
      !is.na(Pass.End.X) & !is.na(Pass.End.Y)
    ) %>%
    mutate(
      x = as.numeric(x),
      y = as.numeric(y),
      Pass.End.X = as.numeric(Pass.End.X),
      Pass.End.Y = as.numeric(Pass.End.Y),
      time_total_sec = time_min * 60 + time_sec
    ) %>%
    arrange(match_id, period_id, time_total_sec)
  
  if(nrow(successful_distribution) == 0) {
    return(data.frame())
  }
  
  # Para cada pase exitoso, buscar el siguiente pase del mismo equipo
  continuation_passes <- data.frame()
  
  for(i in 1:nrow(successful_distribution)) {
    current_pass <- successful_distribution[i, ]
    
    # Buscar el siguiente pase del mismo equipo en los siguientes 8 segundos
    next_passes <- match_data %>%
      filter(
        match_id == current_pass$match_id,
        period_id == current_pass$period_id,
        team_name == team_name_filter,
        type_id == 1, # Pases
        time_min * 60 + time_sec > current_pass$time_total_sec,
        time_min * 60 + time_sec <= current_pass$time_total_sec + 8, # 8 segundos después
        !is.na(Pass.End.X) & !is.na(Pass.End.Y)
      ) %>%
      mutate(
        x = as.numeric(x),
        y = as.numeric(y),
        Pass.End.X = as.numeric(Pass.End.X),
        Pass.End.Y = as.numeric(Pass.End.Y),
        outcome = as.integer(outcome),
        time_total_sec = time_min * 60 + time_sec
      ) %>%
      arrange(time_total_sec) %>%
      slice(1) # Tomar solo el primer pase
    
    if(nrow(next_passes) > 0) {
      continuation_passes <- rbind(continuation_passes, next_passes)
    }
  }
  
  return(continuation_passes)
}

# Añadir función auxiliar para campo de crosses con carriles y etiquetas
crosses_draw_soccer_field <- function(fill_color = "#FFFFFF", line_color = "#777777", lane_stats = NULL, is_home = TRUE) {
  lower_label <- if(is_home) "Right Wing" else "Left Wing"
  upper_label <- if(is_home) "Left Wing" else "Right Wing"
  label_x <- if(is_home) 3 else 97
  label_hjust <- if(is_home) 0 else 1
  left_wing_stats <- lane_stats %>% dplyr::filter(lane == "Left Wing")
  right_wing_stats <- lane_stats %>% dplyr::filter(lane == "Right Wing")
  left_stats <- if(nrow(left_wing_stats) > 0) sprintf("%s\n(%d crosses - %s\n%d successful - %s)",
                       "Left Wing", left_wing_stats$Crosses, paste0(left_wing_stats$Percent, "%"),
                       left_wing_stats$Successful, paste0(left_wing_stats$SuccessRate, "%")) else "Left Wing\n(0 crosses)"
  right_stats <- if(nrow(right_wing_stats) > 0) sprintf("%s\n(%d crosses - %s\n%d successful - %s)",
                        "Right Wing", right_wing_stats$Crosses, paste0(right_wing_stats$Percent, "%"),
                        right_wing_stats$Successful, paste0(right_wing_stats$SuccessRate, "%")) else "Right Wing\n(0 crosses)"
  upper_stats <- if(is_home) left_stats else right_stats
  lower_stats <- if(is_home) right_stats else left_stats
  ggplot() +
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color = line_color, linewidth = 0.5) +
    geom_circle(aes(x0 = 50, y0 = 50, r = 9.15), color = line_color, fill = NA, linewidth = 0.5) +
    geom_point(aes(x = 50, y = 50), size = 0.8, color = line_color) +
    geom_rect(aes(xmin = 0, xmax = 16.5, ymin = 21.1, ymax = 78.9), fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_rect(aes(xmin = 83.5, xmax = 100, ymin = 21.1, ymax = 78.9), fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = 36.8, ymax = 63.2), fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_rect(aes(xmin = 94.5, xmax = 100, ymin = 36.8, ymax = 63.2), fill = fill_color, color = line_color, linewidth = 0.5) +
    geom_point(aes(x = 11, y = 50), size = 0.8, color = line_color) +
    geom_point(aes(x = 89, y = 50), size = 0.8, color = line_color) +
    geom_arc(aes(x0 = 11, y0 = 50, r = 9.15, start = 37*pi/180, end = 143*pi/180), color = line_color, linewidth = 0.5) +
    geom_arc(aes(x0 = 89, y0 = 50, r = 9.15, start = -37*pi/180, end = -143*pi/180), color = line_color, linewidth = 0.5) +
    geom_segment(aes(x = 0, xend = 100, y = 33.33, yend = 33.33), color = line_color, linewidth = 0.3, linetype = "dashed", alpha = 0.5) +
    geom_segment(aes(x = 0, xend = 100, y = 66.66, yend = 66.66), color = line_color, linewidth = 0.3, linetype = "dashed", alpha = 0.5) +
    annotate("text", x = label_x, y = 16.665, label = lower_stats, color = "#777777", size = 2.5, alpha = 0.9, hjust = label_hjust) +
    annotate("text", x = label_x, y = 83.33, label = upper_stats, color = "#777777", size = 2.5, alpha = 0.9, hjust = label_hjust) +
    coord_fixed() +
    theme_void()
}

# ===== UI =====
ui <- dashboardPage(
  dashboardHeader(title = "Offensive Phase Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Goal Kicks & GK Pass", tabName = "analysis", icon = icon("futbol")),
      menuItem("Crosses & Buildup", tabName = "crosses", icon = icon("arrows-alt-h"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis",
        fluidRow(
          box(
            title = "Team Selection", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("selected_team", "Select Team:", choices = NULL, width = "100%")
          ),
          box(
            title = "Season Selection", status = "info", solidHeader = TRUE, width = 6,
            selectInput("selected_season", "Select Season:", choices = NULL, width = "100%")
          )
        ),
        fluidRow(
          box(
            title = "Match Selection", status = "warning", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("match_selector", height = "300px"),
            hr(),
            actionButton("select_all", "Select All Matches", class = "btn-info", style = "margin-right: 10px;"),
            actionButton("clear_all", "Clear Selection", class = "btn-secondary"),
            actionButton("update_analysis", "Update Analysis", class = "btn-warning", style = "float: right;")
          )
        ),
        fluidRow(
          box(
            title = "Analysis Type", status = "success", solidHeader = TRUE, width = 12,
            radioButtons("analysis_type", "Select Analysis Type:", 
                        choices = c("Goalkeeper Distribution" = "distribution", 
                                  "Build-up After Successful Passes" = "buildup"),
                        selected = "distribution", inline = TRUE)
          )
        ),
        fluidRow(
          box(
            title = "Goal Kicks & GK Passes Heatmap", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("field_plot", height = "700px"),
            downloadButton("download_heatmap", "Download Heatmap as PNG", class = "btn-success", style = "margin-top: 10px;")
          )
        ),
        fluidRow(
          box(
            title = "GK Passes Statistics", status = "info", solidHeader = TRUE, width = 6,
            tableOutput("gk_passes_stats"),
            downloadButton("download_gk_stats", "Download GK Passes Statistics as PNG", class = "btn-success", style = "margin-top: 10px;")
          ),
          box(
            title = "Passes by Lane & Third", status = "info", solidHeader = TRUE, width = 6,
            tableOutput("lane_third_table"),
            plotOutput("lane_third_plot", height = "300px"),
            downloadButton("download_lane_third_plot", "Download Lane/Third Plot as PNG", class = "btn-success", style = "margin-top: 10px;")
          ),
          box(
            title = "Passes by Destination Lane & Destination Third", status = "info", solidHeader = TRUE, width = 6,
            tableOutput("dest_lane_table"),
            plotOutput("dest_lane_plot", height = "200px"),
            downloadButton("download_dest_lane_plot", "Download Dest Lane Plot as PNG", class = "btn-success", style = "margin-top: 10px;"),
            tableOutput("dest_third_table"),
            plotOutput("dest_third_plot", height = "200px"),
            downloadButton("download_dest_third_plot", "Download Dest Third Plot as PNG", class = "btn-success", style = "margin-top: 10px;")
          )
        )
      ),
      tabItem(tabName = "crosses",
        fluidRow(
          box(
            title = "Team Selection", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("crosses_selected_team", "Select Team:", choices = NULL, width = "100%")
          ),
          box(
            title = "Season Selection", status = "info", solidHeader = TRUE, width = 6,
            selectInput("crosses_selected_season", "Select Season:", choices = NULL, width = "100%")
          )
        ),
        fluidRow(
          box(
            title = "Match Selection", status = "warning", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("crosses_match_selector", height = "300px"),
            hr(),
            actionButton("crosses_select_all", "Select All Matches", class = "btn-info", style = "margin-right: 10px;"),
            actionButton("crosses_clear_all", "Clear Selection", class = "btn-secondary"),
            actionButton("crosses_update_analysis", "Update Analysis", class = "btn-warning", style = "float: right;")
          )
        ),
        fluidRow(
          box(
            title = "Crosses Filters", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(6,
                h5("Filter by Side:", style = "margin-top: 0;"),
                radioButtons("crosses_side_filter", NULL, 
                           choices = c("All Sides" = "all", "Left Wing" = "left", "Right Wing" = "right"),
                           selected = "all", inline = TRUE)
              ),
              column(6,
                h5("Filter by Outcome:", style = "margin-top: 0;"),
                radioButtons("crosses_outcome_filter", NULL, 
                           choices = c("All Crosses" = "all", "Successful" = "successful", "Unsuccessful" = "unsuccessful"),
                           selected = "all", inline = TRUE)
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Crosses & Build Field", status = "info", solidHeader = TRUE, width = 12,
            div(id = "wrap_crosses_buildup_field",
                plotOutput("crosses_buildup_field_plot", height = "400px"),
                br(),
                actionButton("download_crosses_buildup_field", "\U1F4BE Download Crosses & Build Field as PNG", class = "btn-success download-panel-btn", style = "margin-top: 10px; width:100%")
            )
          ),
          box(
            title = "Crosses by Lane Statistics", status = "info", solidHeader = TRUE, width = 6,
            tableOutput("crosses_lane_table"),
            div(id = "wrap_crosses_lane_panel",
                plotOutput("crosses_lane_plot", height = "300px"),
                br(),
                actionButton("download_crosses_lane_panel", "\U1F4BE Download Lane Panel as PNG", class = "btn-success download-panel-btn", style = "margin-top: 10px; width:100%")
            )
          )
        ),
        fluidRow(
          box(
            title = "Top Crossers", status = "info", solidHeader = TRUE, width = 6,
            div(id = "crosses_top_players_panel", style = "background: #fff; padding: 10px; border-radius: 8px;", 
                DT::dataTableOutput("crosses_top_players_plot", height = "220px")
            ),
            actionButton("download_crosses_top_players", "\U1F4BE Download Top Players as PNG", class = "btn-success download-panel-btn", style = "margin-top: 10px; width:100%")
          )
        )
      )
    )
  ),
  tags$head(
    tags$script(src = "https://html2canvas.hertzen.com/dist/html2canvas.min.js"),
    tags$script(HTML('
      Shiny.addCustomMessageHandler("downloadPanel", function(message) {
        var buttons = document.querySelectorAll(".download-panel-btn");
        buttons.forEach(function(btn) { btn.style.visibility = "hidden"; });
        setTimeout(function() {
          var element = document.querySelector(message.selector);
          if (element) {
            html2canvas(element, {
              scale: 2,
              useCORS: true,
              allowTaint: true,
              backgroundColor: "#ffffff"
            }).then(function(canvas) {
              buttons.forEach(function(btn) { btn.style.visibility = "visible"; });
              var link = document.createElement("a");
              link.download = message.filename;
              link.href = canvas.toDataURL("image/png");
              link.click();
            });
          } else {
            buttons.forEach(function(btn) { btn.style.visibility = "visible"; });
            console.log("Element not found:", message.selector);
          }
        }, 500);
      });
    '))
  )
)

# ===== SERVER =====
server <- function(input, output, session) {
  teams_list <- data %>% select(Team = team_name) %>% distinct() %>% arrange(Team) %>% pull(Team)
  seasons_list <- data %>% select(season) %>% distinct() %>% arrange(season) %>% pull(season)
  observe({
    updateSelectInput(session, "selected_team", choices = teams_list, selected = teams_list[1])
    updateSelectInput(session, "selected_season", choices = seasons_list, selected = seasons_list[1])
  })
  team_season_data <- reactive({
    req(input$selected_team, input$selected_season)
    data %>% filter(team_name == input$selected_team, season == input$selected_season)
  })
  output$match_selector <- DT::renderDataTable({
    req(team_season_data())
    match_info <- team_season_data() %>%
      select(matchday_num, local_date, matchday, team_home, team_away) %>%
      distinct() %>%
      arrange(matchday_num) %>%
      mutate(MatchDescription = paste0(matchday, ": ", team_home, " vs ", team_away)) %>%
      select(matchday_num, local_date, MatchDescription)
    DT::datatable(
      match_info,
      selection = list(mode = 'multiple', selected = 1:min(5, nrow(match_info))),
      options = list(pageLength = 10, scrollY = "250px", scrollCollapse = TRUE, searching = FALSE, ordering = FALSE, info = FALSE, paging = FALSE),
      colnames = c("Matchday", "Date", "Match"),
      rownames = FALSE
    )
  })
  observeEvent(input$select_all, {
    req(input$match_selector_rows_all)
    DT::dataTableProxy('match_selector') %>% DT::selectRows(input$match_selector_rows_all)
  })
  observeEvent(input$clear_all, {
    DT::dataTableProxy('match_selector') %>% DT::selectRows(NULL)
  })
  selected_matchdays <- reactive({
    req(team_season_data(), input$match_selector_rows_selected)
    match_info <- team_season_data() %>%
      select(matchday_num, local_date, matchday, team_home, team_away) %>%
      distinct() %>%
      arrange(matchday_num)
    selected_rows <- input$match_selector_rows_selected
    if(length(selected_rows) > 0) {
      return(match_info$matchday_num[selected_rows])
    } else {
      return(integer(0))
    }
  })
  
  # Datos reactivos según el tipo de análisis
  distribution_passes <- eventReactive(input$update_analysis, {
    req(team_season_data(), selected_matchdays())
    if(length(selected_matchdays()) == 0) return(data.frame())
    get_team_goal_kicks(team_season_data(), input$selected_team, selected_matchdays())
  }, ignoreNULL = FALSE)
  
  buildup_passes <- eventReactive(input$update_analysis, {
    req(team_season_data(), selected_matchdays())
    if(length(selected_matchdays()) == 0) return(data.frame())
    get_buildup_after_successful_passes(team_season_data(), input$selected_team, selected_matchdays())
  }, ignoreNULL = FALSE)
  
  output$field_plot <- renderPlot({
    req(input$analysis_type)
    
    if(input$analysis_type == "distribution") {
      passes <- distribution_passes()
      plot_title <- paste("Goal Kicks & GK Passes Distribution -", input$selected_team)
    } else {
      passes <- buildup_passes()
      plot_title <- paste("Build-up After Successful Distribution -", input$selected_team)
    }
    
    if(nrow(passes) == 0) {
      p <- draw_soccer_field() +
        labs(title = plot_title,
             subtitle = "No passes found for selected matches")
      return(p)
    }
    
    zones <- create_field_zones()
    
    if(input$analysis_type == "distribution") {
      # Normalización y clasificación de origen para distribución
      passes <- passes %>%
        mutate(
          Goal.Kick = case_when(
            is.na(Goal.Kick) ~ 0,
            Goal.Kick %in% c(1, "1", 1.0, TRUE, "TRUE") ~ 1,
            TRUE ~ 0
          ),
          position = toupper(trimws(as.character(position))),
          pass_origin = case_when(
            Goal.Kick == 1 ~ "Goal Kick",
            position == "GK" ~ "GK Open Play",
            TRUE ~ "Other"
          )
        )
      
      # Estadísticas para el subtítulo de distribución
      gk_total <- sum(passes$pass_origin == "Goal Kick", na.rm = TRUE)
      gk_success <- sum(passes$pass_origin == "Goal Kick" & passes$outcome == 1, na.rm = TRUE)
      gk_fail <- sum(passes$pass_origin == "Goal Kick" & passes$outcome == 0, na.rm = TRUE)
      open_total <- sum(passes$pass_origin == "GK Open Play", na.rm = TRUE)
      open_success <- sum(passes$pass_origin == "GK Open Play" & passes$outcome == 1, na.rm = TRUE)
      open_fail <- sum(passes$pass_origin == "GK Open Play" & passes$outcome == 0, na.rm = TRUE)
      subtitle_text <- paste0(
        "Season: ", input$selected_season, " | Matches analyzed: ", length(selected_matchdays()), " | Total Passes: ", nrow(passes), "\n",
        "Goal Kicks: ", gk_total, " (", gk_success, " successful, ", gk_fail, " unsuccessful)\n",
        "GK Open Play: ", open_total, " (", open_success, " successful, ", open_fail, " unsuccessful)"
      )
    } else {
      # Para build-up, usar los pases de continuación directamente
      subtitle_text <- paste0(
        "Season: ", input$selected_season, " | Matches analyzed: ", length(selected_matchdays()), " | Continuation Passes: ", nrow(passes), "\n",
        "Shows destinations of passes after successful Goal Kicks & GK Open Play (within 8 seconds)"
      )
    }
    
    # Crear heatmap
    passes_heatmap <- passes %>%
      mutate(
        bin_x = pmin(pmax(floor(Pass.End.X / (100/6)), 0), 5),
        bin_y = pmin(pmax(floor(Pass.End.Y / (100/3)), 0), 2)
      ) %>%
      count(bin_x, bin_y) %>%
      mutate(id = bin_x + bin_y * 6) %>%
      right_join(zones, by = "id") %>%
      mutate(n = ifelse(is.na(n), 0, n))
    
    heatmap_colors <- colorRampPalette(c(colors$heatmap_start, colors$heatmap_end))(20)
    
    p <- draw_soccer_field() +
      geom_rect(
        data = passes_heatmap,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
        color = "white", linetype = "dashed", size = 0.3, alpha = 0.8
      ) +
      scale_fill_gradientn(colors = heatmap_colors, name = "Passes", guide = guide_colorbar(title.position = "top")) +
      geom_text(
        data = passes_heatmap %>% filter(n > 0),
        aes(x = center_x, y = center_y, label = n),
        color = "#222", fontface = "bold", size = 4
      ) +
      # --- LÍNEA PUNTEADA AMARILLA DE DISTANCIA PROMEDIO ---
      {
        avg_distance <- mean(passes$Pass.End.X, na.rm = TRUE)
        list(
          geom_vline(xintercept = avg_distance, color = colors$avg_line, linetype = "dashed", linewidth = 0.8),
          annotate("text", x = avg_distance, y = 5,
                   label = paste("Avg. Distance:", round(avg_distance, 2)),
                   hjust = 0.5, vjust = 0, size = 4, fontface = "italic", color = colors$text_primary)
        )
      } +
      # --- FLECHA DE DIRECCIÓN DE ATAQUE ---
      {
        # Determinar si el equipo es local o visitante basándose en los partidos seleccionados
        team_season_matches <- team_season_data() %>%
          filter(matchday_num %in% selected_matchdays()) %>%
          select(matchday_num, team_home, team_away) %>%
          distinct()
        
        # Si el equipo aparece como local en más partidos, es local; si no, visitante
        home_count <- sum(team_season_matches$team_home == input$selected_team, na.rm = TRUE)
        away_count <- sum(team_season_matches$team_away == input$selected_team, na.rm = TRUE)
        is_home <- home_count >= away_count
        
        attack_arrow_x <- if(is_home) { c(20, 80) } else { c(80, 20) }
        
        list(
          geom_segment(
            aes(x = attack_arrow_x[1], y = 95, xend = attack_arrow_x[2], yend = 95),
            arrow = arrow(length = unit(0.5, "cm")), linewidth = 1.2,
            color = if(is_home) colors$home_team else colors$away_team
          ),
          annotate("text", x = 50, y = 90, label = "Direction of attack",
                   color = if(is_home) colors$home_team else colors$away_team,
                   fontface = "bold", size = 3.5)
        )
      } +
      labs(title = plot_title,
           subtitle = subtitle_text) +
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
    p
  })
  
  output$gk_passes_stats <- renderTable({
    req(input$analysis_type)
    
    if(input$analysis_type == "distribution") {
      passes <- distribution_passes()
      if(nrow(passes) == 0) {
        return(data.frame(Metric = "No passes found", Value = "0"))
      }
      # Normalización robusta y clasificación de origen
      passes <- passes %>%
        mutate(
          Goal.Kick = case_when(
            is.na(Goal.Kick) ~ 0,
            Goal.Kick %in% c(1, "1", 1.0, TRUE, "TRUE") ~ 1,
            TRUE ~ 0
          ),
          position = toupper(trimws(as.character(position))),
          pass_origin = case_when(
            Goal.Kick == 1 ~ "Goal Kick",
            position == "GK" ~ "GK Open Play",
            TRUE ~ "Other"
          )
        )
      stats <- data.frame(
        Metric = c("Total Passes", "Goal Kicks", "GK Open Play Passes", "Successful Passes", "Failed Passes", "Matches Analyzed"),
        Value = c(
          nrow(passes),
          sum(passes$pass_origin == "Goal Kick", na.rm = TRUE),
          sum(passes$pass_origin == "GK Open Play", na.rm = TRUE),
          sum(passes$outcome == 1, na.rm = TRUE),
          sum(passes$outcome == 0, na.rm = TRUE),
          length(selected_matchdays())
        )
      )
    } else {
      passes <- buildup_passes()
      if(nrow(passes) == 0) {
        return(data.frame(Metric = "No continuation passes found", Value = "0"))
      }
      stats <- data.frame(
        Metric = c("Continuation Passes", "Successful Continuation", "Failed Continuation", "Matches Analyzed"),
        Value = c(
          nrow(passes),
          sum(passes$outcome == 1, na.rm = TRUE),
          sum(passes$outcome == 0, na.rm = TRUE),
          length(selected_matchdays())
        )
      )
    }
    stats
  }, striped = TRUE, hover = TRUE)

  # Panel de análisis por carriles y tercios
  output$lane_third_table <- renderTable({
    req(input$analysis_type)
    passes <- if (input$analysis_type == "distribution") distribution_passes() else buildup_passes()
    if(nrow(passes) == 0) return(data.frame())
    # Usar destino del pase para el análisis
    passes <- passes %>%
      mutate(
        lane = case_when(
          Pass.End.Y < 33.33 ~ 'Right',
          Pass.End.Y >= 33.33 & Pass.End.Y < 66.66 ~ 'Center',
          Pass.End.Y >= 66.66 ~ 'Left'
        ),
        third = case_when(
          Pass.End.X < 33.33 ~ 'Defensive',
          Pass.End.X >= 33.33 & Pass.End.X < 66.66 ~ 'Middle',
          Pass.End.X >= 66.66 ~ 'Attacking'
        )
      )
    summary <- passes %>%
      group_by(lane, third) %>%
      summarise(Passes = n(), .groups = 'drop') %>%
      mutate(Percent = round(100 * Passes / sum(Passes), 1)) %>%
      arrange(desc(Passes))
    summary
  }, striped = TRUE, hover = TRUE)

  output$lane_third_plot <- renderPlot({
    req(input$analysis_type)
    passes <- if (input$analysis_type == "distribution") distribution_passes() else buildup_passes()
    if(nrow(passes) == 0) return(NULL)
    passes <- passes %>%
      mutate(
        lane = case_when(
          Pass.End.Y < 33.33 ~ 'Right',
          Pass.End.Y >= 33.33 & Pass.End.Y < 66.66 ~ 'Center',
          Pass.End.Y >= 66.66 ~ 'Left'
        ),
        third = case_when(
          Pass.End.X < 33.33 ~ 'Defensive',
          Pass.End.X >= 33.33 & Pass.End.X < 66.66 ~ 'Middle',
          Pass.End.X >= 66.66 ~ 'Attacking'
        )
      )
    summary <- passes %>%
      group_by(lane, third) %>%
      summarise(Passes = n(), .groups = 'drop')
    ggplot(summary, aes(x = third, y = Passes, fill = lane)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      labs(title = "Passes by Lane & Third (by destination)", x = "Third", y = "Number of Passes", fill = "Lane") +
      theme_minimal(base_size = 14)
  })

  # Panel de análisis por carril destino
  output$dest_lane_table <- renderTable({
    req(input$analysis_type)
    passes <- if (input$analysis_type == "distribution") distribution_passes() else buildup_passes()
    if(nrow(passes) == 0) return(data.frame())
    passes <- passes %>%
      mutate(
        lane = case_when(
          Pass.End.Y < 33.33 ~ 'Right',
          Pass.End.Y >= 33.33 & Pass.End.Y < 66.66 ~ 'Center',
          Pass.End.Y >= 66.66 ~ 'Left'
        )
      )
    summary <- passes %>%
      group_by(lane) %>%
      summarise(Passes = n(), .groups = 'drop') %>%
      mutate(Percent = round(100 * Passes / sum(Passes), 1)) %>%
      arrange(desc(Passes))
    summary
  }, striped = TRUE, hover = TRUE)

  output$dest_lane_plot <- renderPlot({
    req(input$analysis_type)
    passes <- if (input$analysis_type == "distribution") distribution_passes() else buildup_passes()
    if(nrow(passes) == 0) return(NULL)
    passes <- passes %>%
      mutate(
        lane = case_when(
          Pass.End.Y < 33.33 ~ 'Right',
          Pass.End.Y >= 33.33 & Pass.End.Y < 66.66 ~ 'Center',
          Pass.End.Y >= 66.66 ~ 'Left'
        )
      )
    summary <- passes %>%
      group_by(lane) %>%
      summarise(Passes = n(), .groups = 'drop')
    ggplot(summary, aes(x = lane, y = Passes, fill = lane)) +
      geom_bar(stat = "identity") +
      labs(title = "Passes by Destination Lane", x = "Lane (Destination)", y = "Number of Passes") +
      theme_minimal(base_size = 14) +
      scale_fill_brewer(palette = "Set1")
  })

  # Panel de análisis por tercio destino
  output$dest_third_table <- renderTable({
    req(input$analysis_type)
    passes <- if (input$analysis_type == "distribution") distribution_passes() else buildup_passes()
    if(nrow(passes) == 0) return(data.frame())
    passes <- passes %>%
      mutate(
        third = case_when(
          Pass.End.X < 33.33 ~ 'Defensive',
          Pass.End.X >= 33.33 & Pass.End.X < 66.66 ~ 'Middle',
          Pass.End.X >= 66.66 ~ 'Attacking'
        )
      )
    summary <- passes %>%
      group_by(third) %>%
      summarise(Passes = n(), .groups = 'drop') %>%
      mutate(Percent = round(100 * Passes / sum(Passes), 1)) %>%
      arrange(desc(Passes))
    summary
  }, striped = TRUE, hover = TRUE)

  output$dest_third_plot <- renderPlot({
    req(input$analysis_type)
    passes <- if (input$analysis_type == "distribution") distribution_passes() else buildup_passes()
    if(nrow(passes) == 0) return(NULL)
    passes <- passes %>%
      mutate(
        third = case_when(
          Pass.End.X < 33.33 ~ 'Defensive',
          Pass.End.X >= 33.33 & Pass.End.X < 66.66 ~ 'Middle',
          Pass.End.X >= 66.66 ~ 'Attacking'
        )
      )
    summary <- passes %>%
      group_by(third) %>%
      summarise(Passes = n(), .groups = 'drop')
    ggplot(summary, aes(x = third, y = Passes, fill = third)) +
      geom_bar(stat = "identity") +
      labs(title = "Passes by Destination Third", x = "Third (Destination)", y = "Number of Passes") +
      theme_minimal(base_size = 14) +
      scale_fill_brewer(palette = "Set2")
  })

  output$download_heatmap <- downloadHandler(
    filename = function() {
      type <- if (input$analysis_type == "distribution") "distribution" else "buildup"
      paste0("heatmap_", type, "_", gsub(" ", "_", input$selected_team), ".png")
    },
    content = function(file) {
      req(input$analysis_type)
      passes <- if (input$analysis_type == "distribution") distribution_passes() else buildup_passes()
      if(nrow(passes) == 0) return(NULL)
      zones <- create_field_zones()
      passes_heatmap <- passes %>%
        mutate(
          bin_x = pmin(pmax(floor(Pass.End.X / (100/6)), 0), 5),
          bin_y = pmin(pmax(floor(Pass.End.Y / (100/3)), 0), 2)
        ) %>%
        count(bin_x, bin_y) %>%
        mutate(id = bin_x + bin_y * 6) %>%
        right_join(zones, by = "id") %>%
        mutate(n = ifelse(is.na(n), 0, n))
      heatmap_colors <- colorRampPalette(c(colors$heatmap_start, colors$heatmap_end))(20)
      plot_title <- if (input$analysis_type == "distribution") {
        paste("Goal Kicks & GK Passes Distribution -", input$selected_team)
      } else {
        paste("Build-up After Successful Distribution -", input$selected_team)
      }
      subtitle_text <- if (input$analysis_type == "distribution") {
        gk_total <- sum(passes$Goal.Kick == 1, na.rm = TRUE)
        open_total <- sum(toupper(trimws(as.character(passes$position))) == "GK", na.rm = TRUE)
        paste0(
          "Season: ", input$selected_season, " | Matches analyzed: ", length(selected_matchdays()), " | Total Passes: ", nrow(passes), "\n",
          "Goal Kicks: ", gk_total, "\n",
          "GK Open Play: ", open_total
        )
      } else {
        paste0(
          "Season: ", input$selected_season, " | Matches analyzed: ", length(selected_matchdays()), " | Continuation Passes: ", nrow(passes), "\n",
          "Shows destinations of passes after successful Goal Kicks & GK Open Play (within 8 seconds)"
        )
      }
      avg_distance <- mean(passes$Pass.End.X, na.rm = TRUE)
      team_season_matches <- team_season_data() %>%
        filter(matchday_num %in% selected_matchdays()) %>%
        select(matchday_num, team_home, team_away) %>%
        distinct()
      home_count <- sum(team_season_matches$team_home == input$selected_team, na.rm = TRUE)
      away_count <- sum(team_season_matches$team_away == input$selected_team, na.rm = TRUE)
      is_home <- home_count >= away_count
      attack_arrow_x <- if(is_home) { c(20, 80) } else { c(80, 20) }
      p <- draw_soccer_field() +
        geom_rect(
          data = passes_heatmap,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
          color = "white", linetype = "dashed", size = 0.3, alpha = 0.8
        ) +
        scale_fill_gradientn(colors = heatmap_colors, name = "Passes", guide = guide_colorbar(title.position = "top")) +
        geom_text(
          data = passes_heatmap %>% filter(n > 0),
          aes(x = center_x, y = center_y, label = n),
          color = "#222", fontface = "bold", size = 4
        ) +
        geom_vline(xintercept = avg_distance, color = colors$avg_line, linetype = "dashed", linewidth = 0.8) +
        annotate("text", x = avg_distance, y = 5,
                 label = paste("Avg. Distance:", round(avg_distance, 2)),
                 hjust = 0.5, vjust = 0, size = 4, fontface = "italic", color = colors$text_primary) +
        geom_segment(
          aes(x = attack_arrow_x[1], y = 95, xend = attack_arrow_x[2], yend = 95),
          arrow = arrow(length = unit(0.5, "cm")), linewidth = 1.2,
          color = if(is_home) colors$home_team else colors$away_team
        ) +
        annotate("text", x = 50, y = 90, label = "Direction of attack",
                 color = if(is_home) colors$home_team else colors$away_team,
                 fontface = "bold", size = 3.5) +
        labs(title = plot_title, subtitle = subtitle_text) +
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
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300, bg = "white")
    }
  )

  # Download handler para Lane/Third Plot (tabla + gráfico en un PNG)
  output$download_lane_third_plot <- downloadHandler(
    filename = function() {
      type <- if (input$analysis_type == "distribution") "distribution" else "buildup"
      paste0("lane_third_panel_", type, "_", gsub(" ", "_", input$selected_team), ".png")
    },
    content = function(file) {
      passes <- if (input$analysis_type == "distribution") distribution_passes() else buildup_passes()
      if(nrow(passes) == 0) return(NULL)
      passes <- passes %>%
        mutate(
          lane = case_when(
            Pass.End.Y < 33.33 ~ 'Right',
            Pass.End.Y >= 33.33 & Pass.End.Y < 66.66 ~ 'Center',
            Pass.End.Y >= 66.66 ~ 'Left'
          ),
          third = case_when(
            Pass.End.X < 33.33 ~ 'Defensive',
            Pass.End.X >= 33.33 & Pass.End.X < 66.66 ~ 'Middle',
            Pass.End.X >= 66.66 ~ 'Attacking'
          )
        )
      summary <- passes %>%
        group_by(lane, third) %>%
        summarise(Passes = n(), .groups = 'drop') %>%
        mutate(Percent = round(100 * Passes / sum(Passes), 1))
      p <- ggplot(summary, aes(x = third, y = Passes, fill = lane)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
        labs(title = NULL, x = "Third", y = "Number of Passes", fill = "Lane") +
        theme_minimal(base_size = 14)
      tbl <- tableGrob(summary, rows = NULL)
      title <- textGrob("Passes by Lane & Third (by destination)", gp = gpar(fontsize = 18, fontface = "bold"))
      panel <- grid.arrange(title, tbl, p, ncol = 1, heights = c(0.12, 0.38, 0.5))
      ggsave(file, panel, width = 8, height = 8, dpi = 300, bg = "white")
    }
  )

  # Download handler para Dest Lane Plot (tabla + gráfico en un PNG)
  output$download_dest_lane_plot <- downloadHandler(
    filename = function() {
      type <- if (input$analysis_type == "distribution") "distribution" else "buildup"
      paste0("dest_lane_panel_", type, "_", gsub(" ", "_", input$selected_team), ".png")
    },
    content = function(file) {
      passes <- if (input$analysis_type == "distribution") distribution_passes() else buildup_passes()
      if(nrow(passes) == 0) return(NULL)
      passes <- passes %>%
        mutate(
          lane = case_when(
            Pass.End.Y < 33.33 ~ 'Right',
            Pass.End.Y >= 33.33 & Pass.End.Y < 66.66 ~ 'Center',
            Pass.End.Y >= 66.66 ~ 'Left'
          )
        )
      summary <- passes %>%
        group_by(lane) %>%
        summarise(Passes = n(), .groups = 'drop') %>%
        mutate(Percent = round(100 * Passes / sum(Passes), 1))
      p <- ggplot(summary, aes(x = lane, y = Passes, fill = lane)) +
        geom_bar(stat = "identity") +
        labs(title = NULL, x = "Lane (Destination)", y = "Number of Passes") +
        theme_minimal(base_size = 14) +
        scale_fill_brewer(palette = "Set1")
      tbl <- tableGrob(summary, rows = NULL)
      title <- textGrob("Passes by Destination Lane", gp = gpar(fontsize = 18, fontface = "bold"))
      panel <- grid.arrange(title, tbl, p, ncol = 1, heights = c(0.15, 0.35, 0.5))
      ggsave(file, panel, width = 7, height = 7, dpi = 300, bg = "white")
    }
  )

  # Download handler para Dest Third Plot (tabla + gráfico en un PNG)
  output$download_dest_third_plot <- downloadHandler(
    filename = function() {
      type <- if (input$analysis_type == "distribution") "distribution" else "buildup"
      paste0("dest_third_panel_", type, "_", gsub(" ", "_", input$selected_team), ".png")
    },
    content = function(file) {
      passes <- if (input$analysis_type == "distribution") distribution_passes() else buildup_passes()
      if(nrow(passes) == 0) return(NULL)
      passes <- passes %>%
        mutate(
          third = case_when(
            Pass.End.X < 33.33 ~ 'Defensive',
            Pass.End.X >= 33.33 & Pass.End.X < 66.66 ~ 'Middle',
            Pass.End.X >= 66.66 ~ 'Attacking'
          )
        )
      summary <- passes %>%
        group_by(third) %>%
        summarise(Passes = n(), .groups = 'drop') %>%
        mutate(Percent = round(100 * Passes / sum(Passes), 1))
      p <- ggplot(summary, aes(x = third, y = Passes, fill = third)) +
        geom_bar(stat = "identity") +
        labs(title = NULL, x = "Third (Destination)", y = "Number of Passes") +
        theme_minimal(base_size = 14) +
        scale_fill_brewer(palette = "Set2")
      tbl <- tableGrob(summary, rows = NULL)
      title <- textGrob("Passes by Destination Third", gp = gpar(fontsize = 18, fontface = "bold"))
      panel <- grid.arrange(title, tbl, p, ncol = 1, heights = c(0.15, 0.35, 0.5))
      ggsave(file, panel, width = 7, height = 7, dpi = 300, bg = "white")
    }
  )

  # Download handler para GK Passes Statistics (tabla como PNG)
  output$download_gk_stats <- downloadHandler(
    filename = function() {
      paste0("gk_passes_statistics_", gsub(" ", "_", input$selected_team), ".png")
    },
    content = function(file) {
      stats <- if (input$analysis_type == "distribution") {
        passes <- distribution_passes()
        if(nrow(passes) == 0) return(NULL)
        passes <- passes %>%
          mutate(
            Goal.Kick = case_when(
              is.na(Goal.Kick) ~ 0,
              Goal.Kick %in% c(1, "1", 1.0, TRUE, "TRUE") ~ 1,
              TRUE ~ 0
            ),
            position = toupper(trimws(as.character(position))),
            pass_origin = case_when(
              Goal.Kick == 1 ~ "Goal Kick",
              position == "GK" ~ "GK Open Play",
              TRUE ~ "Other"
            )
          )
        data.frame(
          Metric = c("Total Passes", "Goal Kicks", "GK Open Play Passes", "Successful Passes", "Failed Passes", "Matches Analyzed"),
          Value = c(
            nrow(passes),
            sum(passes$pass_origin == "Goal Kick", na.rm = TRUE),
            sum(passes$pass_origin == "GK Open Play", na.rm = TRUE),
            sum(passes$outcome == 1, na.rm = TRUE),
            sum(passes$outcome == 0, na.rm = TRUE),
            length(selected_matchdays())
          )
        )
      } else {
        passes <- buildup_passes()
        if(nrow(passes) == 0) return(NULL)
        data.frame(
          Metric = c("Continuation Passes", "Successful Continuation", "Failed Continuation", "Matches Analyzed"),
          Value = c(
            nrow(passes),
            sum(passes$outcome == 1, na.rm = TRUE),
            sum(passes$outcome == 0, na.rm = TRUE),
            length(selected_matchdays())
          )
        )
      }
      tbl <- tableGrob(stats, rows = NULL)
      title <- textGrob("GK Passes Statistics", gp = gpar(fontsize = 18, fontface = "bold"))
      panel <- grid.arrange(title, tbl, ncol = 1, heights = c(0.18, 0.82))
      ggsave(file, panel, width = 6, height = 4, dpi = 300, bg = "white")
    }
  )

  # === CROSSES & BUILDUP ===
  crosses_teams_list <- data %>% select(Team = team_name) %>% distinct() %>% arrange(Team) %>% pull(Team)
  crosses_seasons_list <- data %>% select(season) %>% distinct() %>% arrange(season) %>% pull(season)
  observe({
    updateSelectInput(session, "crosses_selected_team", choices = crosses_teams_list, selected = crosses_teams_list[1])
    updateSelectInput(session, "crosses_selected_season", choices = crosses_seasons_list, selected = crosses_seasons_list[1])
  })
  crosses_team_season_data <- reactive({
    req(input$crosses_selected_team, input$crosses_selected_season)
    data %>% filter(team_name == input$crosses_selected_team, season == input$crosses_selected_season)
  })
  output$crosses_match_selector <- DT::renderDataTable({
    req(crosses_team_season_data())
    match_info <- crosses_team_season_data() %>%
      select(matchday_num, local_date, matchday, team_home, team_away) %>%
      distinct() %>%
      arrange(matchday_num) %>%
      mutate(MatchDescription = paste0(matchday, ": ", team_home, " vs ", team_away)) %>%
      select(matchday_num, local_date, MatchDescription)
    DT::datatable(
      match_info,
      selection = list(mode = 'multiple', selected = 1:min(5, nrow(match_info))),
      options = list(pageLength = 10, scrollY = "250px", scrollCollapse = TRUE, searching = FALSE, ordering = FALSE, info = FALSE, paging = FALSE),
      colnames = c("Matchday", "Date", "Match"),
      rownames = FALSE
    )
  })
  observeEvent(input$crosses_select_all, {
    req(input$crosses_match_selector_rows_all)
    DT::dataTableProxy('crosses_match_selector') %>% DT::selectRows(input$crosses_match_selector_rows_all)
  })
  observeEvent(input$crosses_clear_all, {
    DT::dataTableProxy('crosses_match_selector') %>% DT::selectRows(NULL)
  })
  crosses_selected_matchdays <- reactive({
    req(crosses_team_season_data(), input$crosses_match_selector_rows_selected)
    match_info <- crosses_team_season_data() %>%
      select(matchday_num, local_date, matchday, team_home, team_away) %>%
      distinct() %>%
      arrange(matchday_num)
    selected_rows <- input$crosses_match_selector_rows_selected
    if(length(selected_rows) > 0) {
      return(match_info$matchday_num[selected_rows])
    } else {
      return(integer(0))
    }
  })
  crosses_data <- eventReactive(input$crosses_update_analysis, {
    req(crosses_team_season_data(), crosses_selected_matchdays())
    if(length(crosses_selected_matchdays()) == 0) return(data.frame())
    crosses_team_season_data() %>% filter(matchday_num %in% crosses_selected_matchdays())
  }, ignoreNULL = FALSE)
  crosses_filtered <- reactive({
    req(crosses_data())
    crosses <- crosses_data() %>%
      filter(type_id == 1,  # Evento de pase (no type_id == 2)
             !is.na(Pass.End.X) & !is.na(Pass.End.Y),
             # EXCLUIR CORNERS
             (is.na(Corner.taken) | Corner.taken != "1"),
             # Crosses que van hacia las áreas
             ((Pass.End.X >= 83.5 & Pass.End.Y >= 21.1 & Pass.End.Y <= 78.9) |
                (Pass.End.X <= 16.5 & Pass.End.Y >= 21.1 & Pass.End.Y <= 78.9)) &
               # Origen fuera de cualquier área
               !((x >= 83.5 & x <= 100 & y >= 21.1 & y <= 78.9) |
                   (x >= 0 & x <= 16.5 & y >= 21.1 & y <= 78.9))
      )
    
    # Filtrar por columna Cross si está disponible
    if(sum(!is.na(crosses$Cross) & crosses$Cross == "1") > 0) {
      crosses <- crosses %>% filter(Cross == "1")
    }
    
    crosses <- crosses %>%
      mutate(
        x = as.numeric(x),
        y = as.numeric(y),
        Pass.End.X = as.numeric(Pass.End.X),
        Pass.End.Y = as.numeric(Pass.End.Y),
        outcome = as.integer(outcome),
        cross_side = case_when(
          y < 33.33 ~ "Left",
          y > 66.66 ~ "Right",
          TRUE ~ "Center"
        )
      ) %>%
      filter(!is.na(x) & !is.na(y) & !is.na(outcome) & outcome %in% c(0, 1))
    
    # Aplicar filtros de lado
    if(input$crosses_side_filter == "left") {
      crosses <- crosses %>% filter(cross_side == "Left")
    } else if(input$crosses_side_filter == "right") {
      crosses <- crosses %>% filter(cross_side == "Right")
    }
    # Si es "all", no filtrar por lado
    
    # Aplicar filtros de resultado
    if(input$crosses_outcome_filter == "successful") {
      crosses <- crosses %>% filter(outcome == 1)
    } else if(input$crosses_outcome_filter == "unsuccessful") {
      crosses <- crosses %>% filter(outcome == 0)
    }
    # Si es "all", no filtrar por resultado
    
    # Para cada cross, buscar el evento previo (lógica de buildup)
    crosses_with_buildup <- list()
    
    for(i in 1:nrow(crosses)) {
      cross <- crosses[i,]
      cross_row <- which(crosses_data()$general_id == cross$general_id)
      
      if(length(cross_row) > 0 && cross_row > 1) {
        # Buscar el evento previo del mismo equipo (máximo 10 eventos atrás)
        for(j in 1:min(10, cross_row - 1)) {
          prev_idx <- cross_row - j
          prev_event <- crosses_data()[prev_idx,]
          
          # Si es del mismo equipo y mismo periodo
          if(prev_event$team_name == input$crosses_selected_team && 
             prev_event$period_id == cross$period_id) {
            
            # Calcular tiempo transcurrido
            time_diff <- (cross$time_min * 60 + cross$time_sec) - 
              (prev_event$time_min * 60 + prev_event$time_sec)
            
            # Si han pasado menos de 15 segundos
            if(time_diff <= 15) {
              # Agregar información del evento previo al cross
              cross$has_buildup <- TRUE
              cross$prev_event_type <- prev_event$event
              cross$prev_event_id <- prev_event$general_id
              cross$prev_x <- as.numeric(prev_event$x)
              cross$prev_y <- as.numeric(prev_event$y)
              cross$time_from_prev <- time_diff
              
              # Si el evento previo es un pase, agregar su destino
              if(prev_event$type_id == 1 && !is.na(prev_event$Pass.End.X)) {
                cross$prev_pass_end_x <- as.numeric(prev_event$Pass.End.X)
                cross$prev_pass_end_y <- as.numeric(prev_event$Pass.End.Y)
                cross$prev_pass_outcome <- as.integer(prev_event$outcome)
                
                # Detectar si hubo conducción entre el pase previo y el cross
                dist_carry <- sqrt((cross$x - cross$prev_pass_end_x)^2 + 
                                   (cross$y - cross$prev_pass_end_y)^2)
                cross$has_carry <- dist_carry > 3
              } else {
                cross$prev_pass_end_x <- NA
                cross$prev_pass_end_y <- NA
                cross$prev_pass_outcome <- NA
                cross$has_carry <- FALSE
              }
              
              break
            }
          }
        }
      }
      
      # Si no se encontró buildup
      if(!"has_buildup" %in% names(cross)) {
        cross$has_buildup <- FALSE
      }
      
      crosses_with_buildup[[i]] <- cross
    }
    
    # Combinar todos los crosses
    result <- bind_rows(crosses_with_buildup)
    
    return(result)
  })
  
  output$crosses_field_plot <- renderPlot({
    req(crosses_filtered())
    crosses <- crosses_filtered()
    
    if(nrow(crosses) == 0) {
      return(ggplot() + theme_void() + 
               geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), fill="#F5F5F5", color=NA) +
               annotate("text", x=0.5, y=0.5, label="No crosses found for selection", color="#333333"))
    }
    
    # Paleta de colores (idéntica a la función)
    colors <- list(
      background = "#F5F5F5",
      background_light = "#FFFFFF",
      text_primary = "#333333",
      text_secondary = "#777777",
      accent = "#1A78CF",
      home_team = "#E57373",
      away_team = "#64B5F6",
      cross_successful = "#00A300",
      cross_unsuccessful = "#FF4B44",
      prev_pass = "#2196F3",
      prev_carry = "#FF9800",
      prev_other = "#9C27B0",
      heatmap_start = "#E8F4F9",
      lines = "#777777"
    )
    
    # Estadísticas de banda y totales
    lane_stats <- crosses %>%
      mutate(lane = case_when(
        y < 33.33 ~ "Right Wing",
        y > 66.66 ~ "Left Wing",
        TRUE ~ "Central Lane"
      )) %>%
      group_by(lane) %>%
      summarise(
        count = n(),
        successful = sum(outcome == 1, na.rm = TRUE),
        percentage = sprintf("%.0f%%", 100 * n() / nrow(crosses)),
        success_percentage = sprintf("%.0f%%", 100 * sum(outcome == 1, na.rm = TRUE) / n())
      )
    
    total_crosses <- nrow(crosses)
    successful_crosses <- sum(crosses$outcome == 1, na.rm = TRUE)
    unsuccessful_crosses <- sum(crosses$outcome == 0, na.rm = TRUE)
    success_pct <- ifelse(total_crosses > 0, round(100 * successful_crosses / total_crosses, 1), 0)
    
    # Crear campo base con estadísticas de carril
    field_plot <- crosses_draw_soccer_field(
      fill_color = colors$background_light,
      line_color = colors$lines,
      lane_stats = lane_stats,
      is_home = TRUE
    )
    
    # Flecha de dirección de ataque
    attack_arrow_x <- c(20, 80)
    field_plot <- field_plot +
      geom_segment(
        aes(x = attack_arrow_x[1], y = 95, xend = attack_arrow_x[2], yend = 95),
        arrow = arrow(length = unit(0.5, "cm")), linewidth = 1.2,
        color = colors$home_team
      ) +
      annotate("text", x = mean(attack_arrow_x), y = 90, 
               label = "Direction of attack",
               color = colors$home_team,
               fontface = "bold", size = 3.5)
    
    # Dibujo de crosses y buildup (lógica exacta de la función)
    # 1. Buildup previo (pase, conducción, otro)
    for (i in 1:nrow(crosses)) {
      cross <- crosses[i,]
      if (cross$has_buildup) {
        if (!is.na(cross$prev_pass_end_x) && !is.na(cross$prev_pass_end_y) && cross$has_carry) {
          # Pase previo + conducción
          field_plot <- field_plot +
            geom_segment(data = cross, aes(x = prev_pass_end_x, y = prev_pass_end_y, xend = x, yend = y),
                         color = colors$prev_carry, linetype = "dotted", linewidth = 1.1, alpha = 0.8)
          field_plot <- field_plot +
            geom_segment(data = cross, aes(x = prev_x, y = prev_y, xend = prev_pass_end_x, yend = prev_pass_end_y),
                         color = colors$prev_pass, linetype = "dashed", linewidth = 1.1, alpha = 0.8)
        } else if (!is.na(cross$prev_pass_end_x) && !is.na(cross$prev_pass_end_y)) {
          # Solo pase previo
          field_plot <- field_plot +
            geom_segment(data = cross, aes(x = prev_x, y = prev_y, xend = prev_pass_end_x, yend = prev_pass_end_y),
                         color = colors$prev_pass, linetype = "dashed", linewidth = 1.1, alpha = 0.8)
        } else {
          # Otro evento previo
          field_plot <- field_plot +
            geom_segment(data = cross, aes(x = prev_x, y = prev_y, xend = x, yend = y),
                         color = colors$prev_other, linetype = "solid", linewidth = 1.1, alpha = 0.8)
        }
      }
    }
    
    # 2. Cross principal
    field_plot <- field_plot +
      geom_segment(data = crosses, aes(x = x, y = y, xend = Pass.End.X, yend = Pass.End.Y),
                   color = colors$lines, linewidth = 1.2, alpha = 0.8)
    
    # 3. Puntos de destino
    field_plot <- field_plot +
      geom_point(data = crosses, aes(x = Pass.End.X, y = Pass.End.Y, fill = factor(outcome)),
                 shape = 21, size = 4, color = "white", stroke = 1.2) +
      scale_fill_manual(values = c("1" = colors$cross_successful, "0" = colors$cross_unsuccessful), guide = "none")
    
    # Títulos y tema
    field_plot <- field_plot +
      labs(
        title = paste("Crosses with Buildup Analysis -", input$crosses_selected_team),
        subtitle = paste0("Total crosses: ", total_crosses, " (Success rate: ", success_pct, "%)\n",
                         "Successful: ", successful_crosses, " | Unsuccessful: ", unsuccessful_crosses)
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = colors$text_primary),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = colors$text_primary),
        legend.position = "none"
      )
    
    # Crear leyenda visual
    legend_plot <- ggplot() +
      # Leyenda de Buildup
      geom_segment(aes(x = 0.1, xend = 0.3, y = 0.7, yend = 0.7), 
                  color = colors$prev_pass, linetype = "dashed", linewidth = 1) +
      annotate("text", x = 0.35, y = 0.7, label = "Previous pass", 
               hjust = 0, size = 3, color = colors$text_primary) +
      
      geom_segment(aes(x = 0.1, xend = 0.3, y = 0.5, yend = 0.5), 
                  color = colors$prev_carry, linetype = "dotted", linewidth = 1) +
      annotate("text", x = 0.35, y = 0.5, label = "Ball carry", 
               hjust = 0, size = 3, color = colors$text_primary) +
      
      geom_segment(aes(x = 0.1, xend = 0.3, y = 0.3, yend = 0.3), 
                  color = colors$prev_other, linetype = "solid", linewidth = 1) +
      annotate("text", x = 0.35, y = 0.3, label = "Other event", 
               hjust = 0, size = 3, color = colors$text_primary) +
      
      # Leyenda de Cross Outcome
      geom_point(aes(x = 0.6, y = 0.6), color = colors$cross_successful, size = 3) +
      annotate("text", x = 0.65, y = 0.6, label = "Successful cross", 
               hjust = 0, size = 3, color = colors$text_primary) +
      
      geom_point(aes(x = 0.6, y = 0.4), color = colors$cross_unsuccessful, size = 3) +
      annotate("text", x = 0.65, y = 0.4, label = "Unsuccessful cross", 
               hjust = 0, size = 3, color = colors$text_primary) +
      
      # Títulos de las leyendas
      annotate("text", x = 0.2, y = 0.9, label = "Buildup Legend:", 
               hjust = 0, size = 3.5, fontface = "bold", color = colors$text_primary) +
      annotate("text", x = 0.6, y = 0.9, label = "Cross Outcome:", 
               hjust = 0, size = 3.5, fontface = "bold", color = colors$text_primary) +
      
      xlim(0, 1) + ylim(0, 1) + theme_void() +
      theme(plot.background = element_rect(fill = colors$background, color = NA))
    
    # Combinar campo y leyenda
    final_plot <- grid.arrange(
      field_plot,
      legend_plot,
      ncol = 1,
      heights = c(6, 1)
    )
    
    final_plot
  })

  output$crosses_lane_table <- renderTable({
    req(crosses_filtered())
    crosses <- crosses_filtered()
    if(nrow(crosses) == 0) return(data.frame())
    crosses <- crosses %>% mutate(
      lane = case_when(
        y < 33.33 ~ 'Right Wing',
        y > 66.66 ~ 'Left Wing',
        TRUE ~ 'Central Lane'
      )
    )
    summary <- crosses %>% group_by(lane) %>% summarise(Crosses = n(), Successful = sum(outcome == 1), SuccessRate = round(100*mean(outcome == 1),1))
    summary
  }, striped = TRUE, hover = TRUE)

  output$crosses_lane_plot <- renderPlot({
    req(crosses_filtered())
    crosses <- crosses_filtered()
    if(nrow(crosses) == 0) return(NULL)
    crosses <- crosses %>% mutate(
      lane = case_when(
        y < 33.33 ~ 'Right Wing',
        y > 66.66 ~ 'Left Wing',
        TRUE ~ 'Central Lane'
      )
    )
    summary <- crosses %>% group_by(lane) %>% summarise(Crosses = n(), Successful = sum(outcome == 1))
    ggplot(summary, aes(x = lane, y = Crosses, fill = lane)) +
      geom_bar(stat = "identity") +
      labs(title = "Crosses by Lane", x = "Lane", y = "Number of Crosses") +
      theme_minimal(base_size = 14) +
      scale_fill_brewer(palette = "Set1")
  })

  output$crosses_legend_plot <- renderPlot({
    # Leyenda visual (puedes mejorarla según el script original)
    ggplot() +
      annotate("text", x = 0.5, y = 0.8, label = "Green: Successful\nRed: Unsuccessful", size = 5, hjust = 0.5) +
      theme_void()
  })

  output$crosses_summary_panel <- renderPlot({
    req(crosses_filtered())
    crosses <- crosses_filtered()
    if(nrow(crosses) == 0) return(NULL)
    total <- nrow(crosses)
    successful <- sum(crosses$outcome == 1)
    unsuccessful <- sum(crosses$outcome == 0)
    ggplot() +
      annotate("text", x = 0.5, y = 0.7, label = paste0("Total Crosses: ", total), size = 6, hjust = 0.5) +
      annotate("text", x = 0.5, y = 0.5, label = paste0("Successful: ", successful), size = 5, hjust = 0.5, color = "#00A300") +
      annotate("text", x = 0.5, y = 0.3, label = paste0("Unsuccessful: ", unsuccessful), size = 5, hjust = 0.5, color = "#FF4B44") +
      theme_void()
  })

  # Cargar el mapping de fotos de jugadores EXACTAMENTE igual que en offensive transition
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

  output$crosses_top_players_plot <- DT::renderDataTable({
    req(crosses_filtered())
    crosses <- crosses_filtered()
    if(nrow(crosses) == 0) {
      return(DT::datatable(
        data.frame(Player = "No crosses found", Crosses = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    # Usar nombre abreviado (inicial y apellido)
    abbrev_name <- function(full_name) {
      parts <- strsplit(full_name, " ")[[1]]
      if(length(parts) == 1) return(parts[1])
      paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
    }
    top_players <- crosses %>%
      filter(!is.na(player_name) & player_name != "") %>%
      group_by(player_name) %>%
      summarise(crosses = n()) %>%
      arrange(desc(crosses)) %>%
      slice_head(n = 3) %>%
      mutate(
        Photo = sapply(player_name, function(name) {
          path <- get_player_photo(name)
          sprintf('<img src="%s" height="48" style="border-radius:50%%;margin-right:12px;box-shadow:0 1px 4px #bbb;">', path)
        }),
        Player = paste0('<div style="display:flex;align-items:center;"><div>', Photo, '</div><div style="font-size:18px;font-weight:600;color:#222;margin-left:8px;">', sapply(player_name, abbrev_name), '</div></div>')
      )
    DT::datatable(
      top_players[, c("Player", "crosses")],
      options = list(
        pageLength = 3,
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-left', targets = 0),
          list(className = 'dt-right', targets = 1)
        ),
        rowCallback = JS(
          'function(row, data, index) {',
          '  if(index < 2) {',
          '    $(row).css("border-bottom", "2px solid #e0e0e0");',
          '  }',
          '  $(row).css("height", "64px");',
          '}'
        )
      ),
      rownames = FALSE,
      colnames = c("Player", "Crosses"),
      escape = FALSE,
      class = 'stripe row-border hover'
    )
  })

  # Download handler para Top Players Panel
  output$download_crosses_top_players <- downloadHandler(
    filename = function() {
      paste0("crosses_top_players_", gsub(" ", "_", input$crosses_selected_team), ".png")
    },
    content = function(file) {
      req(crosses_filtered())
      crosses <- crosses_filtered()
      
      if(nrow(crosses) == 0) return(NULL)
      
      # Cargar mapa de fotos de jugadores
      player_photo_map <- read.csv('www/player_photo_map.csv', stringsAsFactors = FALSE, encoding = "UTF-8")
      
      # Obtener top 3 jugadores que más centros hicieron
      top_players <- crosses %>%
        filter(!is.na(player_name) & player_name != "") %>%
        group_by(player_name) %>%
        summarise(
          total_crosses = n(),
          successful_crosses = sum(outcome == 1, na.rm = TRUE),
          success_rate = round(100 * successful_crosses / total_crosses, 1),
          .groups = 'drop'
        ) %>%
        arrange(desc(total_crosses)) %>%
        head(3)
      
      if(nrow(top_players) == 0) return(NULL)
      
      # Paleta de colores
      colors <- list(
        background = "#F5F5F5",
        text_primary = "#333333",
        text_secondary = "#777777",
        accent = "#1A78CF"
      )
      
      # Crear panel de top jugadores
      p <- ggplot() +
        # Título
        annotate("text", x = 0.5, y = 0.95, 
                 label = "Top Crossers", 
                 size = 6, fontface = "bold", color = colors$text_primary) +
        
        # Jugador 1
        annotate("text", x = 0.2, y = 0.75, 
                 label = paste0("1. ", substr(top_players$player_name[1], 1, 20)), 
                 size = 4, fontface = "bold", color = colors$text_primary) +
        annotate("text", x = 0.2, y = 0.65, 
                 label = paste0("Crosses: ", top_players$total_crosses[1]), 
                 size = 3, color = colors$text_secondary) +
        annotate("text", x = 0.2, y = 0.55, 
                 label = paste0("Success: ", top_players$success_rate[1], "%"), 
                 size = 3, color = colors$text_secondary) +
        
        # Jugador 2
        annotate("text", x = 0.5, y = 0.75, 
                 label = paste0("2. ", substr(top_players$player_name[2], 1, 20)), 
                 size = 4, fontface = "bold", color = colors$text_primary) +
        annotate("text", x = 0.5, y = 0.65, 
                 label = paste0("Crosses: ", top_players$total_crosses[2]), 
                 size = 3, color = colors$text_secondary) +
        annotate("text", x = 0.5, y = 0.55, 
                 label = paste0("Success: ", top_players$success_rate[2], "%"), 
                 size = 3, color = colors$text_secondary) +
        
        # Jugador 3
        annotate("text", x = 0.8, y = 0.75, 
                 label = paste0("3. ", substr(top_players$player_name[3], 1, 20)), 
                 size = 4, fontface = "bold", color = colors$text_primary) +
        annotate("text", x = 0.8, y = 0.65, 
                 label = paste0("Crosses: ", top_players$total_crosses[3]), 
                 size = 3, color = colors$text_secondary) +
        annotate("text", x = 0.8, y = 0.55, 
                 label = paste0("Success: ", top_players$success_rate[3], "%"), 
                 size = 3, color = colors$text_secondary) +
        
        # Estadísticas generales
        annotate("text", x = 0.5, y = 0.35, 
                 label = paste0("Total Crosses: ", nrow(crosses)), 
                 size = 4, fontface = "bold", color = colors$accent) +
        annotate("text", x = 0.5, y = 0.25, 
                 label = paste0("Success Rate: ", round(100 * sum(crosses$outcome == 1) / nrow(crosses), 1), "%"), 
                 size = 4, color = colors$accent) +
        
        xlim(0, 1) + ylim(0, 1) + theme_void() +
        theme(plot.background = element_rect(fill = colors$background, color = NA))
      
      ggsave(file, p, width = 8, height = 6, dpi = 300, bg = colors$background)
    }
  )

  observeEvent(input$download_crosses_top_players, {
    session$sendCustomMessage("downloadPanel", list(
      selector = "#crosses_top_players_panel",
      filename = paste0("crosses_top_players_", Sys.Date(), ".png")
    ))
  })

  # Crosses & Build Field Plot
  output$crosses_buildup_field_plot <- renderPlot({
    req(crosses_filtered())
    crosses <- crosses_filtered()
    
    if(nrow(crosses) == 0) {
      return(ggplot() + theme_void() + 
               geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), fill="#F5F5F5", color=NA) +
               annotate("text", x=0.5, y=0.5, label="No crosses found for selection", color="#333333"))
    }
    
    # Paleta de colores (idéntica a la función)
    colors <- list(
      background = "#F5F5F5",
      background_light = "#FFFFFF",
      text_primary = "#333333",
      text_secondary = "#777777",
      accent = "#1A78CF",
      home_team = "#E57373",
      away_team = "#64B5F6",
      cross_successful = "#00A300",
      cross_unsuccessful = "#FF4B44",
      prev_pass = "#2196F3",
      prev_carry = "#FF9800",
      prev_other = "#9C27B0",
      heatmap_start = "#E8F4F9",
      lines = "#777777"
    )
    
    # Estadísticas de banda y totales
    lane_stats <- crosses %>%
      mutate(lane = case_when(
        y < 33.33 ~ "Right Wing",
        y > 66.66 ~ "Left Wing",
        TRUE ~ "Central Lane"
      )) %>%
      group_by(lane) %>%
      summarise(
        count = n(),
        successful = sum(outcome == 1, na.rm = TRUE),
        percentage = sprintf("%.0f%%", 100 * n() / nrow(crosses)),
        success_percentage = sprintf("%.0f%%", 100 * sum(outcome == 1, na.rm = TRUE) / n())
      )
    
    total_crosses <- nrow(crosses)
    successful_crosses <- sum(crosses$outcome == 1, na.rm = TRUE)
    unsuccessful_crosses <- sum(crosses$outcome == 0, na.rm = TRUE)
    success_pct <- ifelse(total_crosses > 0, round(100 * successful_crosses / total_crosses, 1), 0)
    
    # Crear campo base con estadísticas de carril
    field_plot <- crosses_draw_soccer_field(
      fill_color = colors$background_light,
      line_color = colors$lines,
      lane_stats = lane_stats,
      is_home = TRUE
    )
    
    # Flecha de dirección de ataque
    attack_arrow_x <- c(20, 80)
    field_plot <- field_plot +
      geom_segment(
        aes(x = attack_arrow_x[1], y = 95, xend = attack_arrow_x[2], yend = 95),
        arrow = arrow(length = unit(0.5, "cm")), linewidth = 1.2,
        color = colors$home_team
      ) +
      annotate("text", x = mean(attack_arrow_x), y = 90, 
               label = "Direction of attack",
               color = colors$home_team,
               fontface = "bold", size = 3.5)
    
    # Dibujo de crosses y buildup (lógica exacta de la función)
    # 1. Buildup previo (pase, conducción, otro)
    for (i in 1:nrow(crosses)) {
      cross <- crosses[i,]
      if (cross$has_buildup) {
        if (!is.na(cross$prev_pass_end_x) && !is.na(cross$prev_pass_end_y) && cross$has_carry) {
          # Pase previo + conducción
          field_plot <- field_plot +
            geom_segment(data = cross, aes(x = prev_pass_end_x, y = prev_pass_end_y, xend = x, yend = y),
                         color = colors$prev_carry, linetype = "dotted", linewidth = 1.1, alpha = 0.8)
          field_plot <- field_plot +
            geom_segment(data = cross, aes(x = prev_x, y = prev_y, xend = prev_pass_end_x, yend = prev_pass_end_y),
                         color = colors$prev_pass, linetype = "dashed", linewidth = 1.1, alpha = 0.8)
        } else if (!is.na(cross$prev_pass_end_x) && !is.na(cross$prev_pass_end_y)) {
          # Solo pase previo
          field_plot <- field_plot +
            geom_segment(data = cross, aes(x = prev_x, y = prev_y, xend = prev_pass_end_x, yend = prev_pass_end_y),
                         color = colors$prev_pass, linetype = "dashed", linewidth = 1.1, alpha = 0.8)
        } else {
          # Otro evento previo
          field_plot <- field_plot +
            geom_segment(data = cross, aes(x = prev_x, y = prev_y, xend = x, yend = y),
                         color = colors$prev_other, linetype = "solid", linewidth = 1.1, alpha = 0.8)
        }
      }
    }
    
    # 2. Cross principal
    field_plot <- field_plot +
      geom_segment(data = crosses, aes(x = x, y = y, xend = Pass.End.X, yend = Pass.End.Y),
                   color = colors$lines, linewidth = 1.2, alpha = 0.8)
    
    # 3. Puntos de destino
    field_plot <- field_plot +
      geom_point(data = crosses, aes(x = Pass.End.X, y = Pass.End.Y, fill = factor(outcome)),
                 shape = 21, size = 4, color = "white", stroke = 1.2) +
      scale_fill_manual(values = c("1" = colors$cross_successful, "0" = colors$cross_unsuccessful), guide = "none")
    
    # Títulos y tema
    field_plot <- field_plot +
      labs(
        title = paste("Crosses with Buildup Analysis -", input$crosses_selected_team),
        subtitle = paste0("Total crosses: ", total_crosses, " (Success rate: ", success_pct, "%)\n",
                         "Successful: ", successful_crosses, " | Unsuccessful: ", unsuccessful_crosses)
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = colors$text_primary),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = colors$text_primary),
        legend.position = "none"
      )
    
    # Crear leyenda visual
    legend_plot <- ggplot() +
      # Leyenda de Buildup
      geom_segment(aes(x = 0.1, xend = 0.3, y = 0.7, yend = 0.7), 
                  color = colors$prev_pass, linetype = "dashed", linewidth = 1) +
      annotate("text", x = 0.35, y = 0.7, label = "Previous pass", 
               hjust = 0, size = 3, color = colors$text_primary) +
      
      geom_segment(aes(x = 0.1, xend = 0.3, y = 0.5, yend = 0.5), 
                  color = colors$prev_carry, linetype = "dotted", linewidth = 1) +
      annotate("text", x = 0.35, y = 0.5, label = "Ball carry", 
               hjust = 0, size = 3, color = colors$text_primary) +
      
      geom_segment(aes(x = 0.1, xend = 0.3, y = 0.3, yend = 0.3), 
                  color = colors$prev_other, linetype = "solid", linewidth = 1) +
      annotate("text", x = 0.35, y = 0.3, label = "Other event", 
               hjust = 0, size = 3, color = colors$text_primary) +
      
      # Leyenda de Cross Outcome
      geom_point(aes(x = 0.6, y = 0.6), color = colors$cross_successful, size = 3) +
      annotate("text", x = 0.65, y = 0.6, label = "Successful cross", 
               hjust = 0, size = 3, color = colors$text_primary) +
      
      geom_point(aes(x = 0.6, y = 0.4), color = colors$cross_unsuccessful, size = 3) +
      annotate("text", x = 0.65, y = 0.4, label = "Unsuccessful cross", 
               hjust = 0, size = 3, color = colors$text_primary) +
      
      # Títulos de las leyendas
      annotate("text", x = 0.2, y = 0.9, label = "Buildup Legend:", 
               hjust = 0, size = 3.5, fontface = "bold", color = colors$text_primary) +
      annotate("text", x = 0.6, y = 0.9, label = "Cross Outcome:", 
               hjust = 0, size = 3.5, fontface = "bold", color = colors$text_primary) +
      
      xlim(0, 1) + ylim(0, 1) + theme_void() +
      theme(plot.background = element_rect(fill = colors$background, color = NA))
    
    # Combinar campo y leyenda
    final_plot <- grid.arrange(
      field_plot,
      legend_plot,
      ncol = 1,
      heights = c(6, 1)
    )
    
    final_plot
  })

  output$download_crosses_buildup_field <- downloadHandler(
    filename = function() {
      paste0("crosses_buildup_field_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 600, res = 150)
      print(output$crosses_buildup_field_plot())
      dev.off()
    }
  )

  output$download_crosses_lane_panel <- downloadHandler(
    filename = function() {
      paste0("crosses_lane_panel_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 900, height = 500, res = 150)
      print(output$crosses_lane_plot())
      dev.off()
    }
  )

  # Descargar Crosses & Build Field como imagen (html2canvas)
  observeEvent(input$download_crosses_buildup_field, {
    filename <- paste0("Crosses_Buildup_Field_", input$crosses_selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_crosses_buildup_field",
      filename = filename
    ))
  })

  # Descargar Crosses by Lane Statistics como imagen (html2canvas)
  observeEvent(input$download_crosses_lane_panel, {
    filename <- paste0("Crosses_Lane_Statistics_", input$crosses_selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#wrap_crosses_lane_panel",
      filename = filename
    ))
  })
}

shinyApp(ui, server) 