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
  away_team = "#FF0000",
  aerial_color = "#FF6B35",
  tackle_color = "#4CAF50",
  challenge_color = "#9C27B0"
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

# Función para obtener duelos ganados del equipo
get_team_won_duels <- function(match_data, team_name_filter, matchdays, duel_types) {
  team_duels <- match_data %>%
    filter(
      team_name == team_name_filter,
      event %in% duel_types,
      outcome == 1,  # outcome == 1 significa duelos ganados
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
  return(team_duels)
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
    if (nrow(row) > 0) {
      photo_file <- row$photo_file[1]
      if (!is.na(photo_file) && photo_file != "") {
        return(file.path("Jugadores", photo_file))
      }
    }
    return("Jugadores/placeholder.png")
  }, error = function(e) {
    return("Jugadores/placeholder.png")
  })
}

# ===== UI =====
ui <- dashboardPage(
  dashboardHeader(title = "Won Duels Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Duels Analysis", tabName = "analysis", icon = icon("futbol"))
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
            title = "Duels Type Selection", status = "success", solidHeader = TRUE, width = 12,
            checkboxGroupInput("duel_types", "Select Duels Types:", 
                              choices = c("Aerial" = "Aerial", 
                                        "Tackle" = "Tackle",
                                        "Challenge" = "Challenge"),
                              selected = c("Aerial", "Tackle", "Challenge"), 
                              inline = TRUE)
          )
        ),
        fluidRow(
          box(
            title = "Won Duels Heatmap", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("field_plot", height = "700px"),
            downloadButton("download_heatmap", "Download Heatmap as PNG", class = "btn-success", style = "margin-top: 10px;")
          )
        ),
        fluidRow(
          box(
            title = "Duels Statistics", status = "info", solidHeader = TRUE, width = 6,
            tableOutput("duels_stats"),
            downloadButton("download_duels_stats", "Download Duels Statistics as PNG", class = "btn-success", style = "margin-top: 10px;")
          ),
          box(
            title = "Duels by Field Zone", status = "info", solidHeader = TRUE, width = 6,
            tableOutput("zone_table"),
            plotOutput("zone_plot", height = "300px"),
            downloadButton("download_zone_plot", "Download Zone Plot as PNG", class = "btn-success", style = "margin-top: 10px;")
          )
        ),
        fluidRow(
          box(
            title = "Top Duels Winners", status = "info", solidHeader = TRUE, width = 6,
            div(id = "top_duelers_panel", style = "background: #fff; padding: 10px; border-radius: 8px;", 
                DT::dataTableOutput("top_duelers_plot", height = "220px")
            ),
            actionButton("download_top_duelers", "Download Top Duels Winners as PNG", class = "btn-success download-panel-btn", style = "margin-top: 10px; width:100%")
          ),
          box(
            title = "Duels by Type", status = "info", solidHeader = TRUE, width = 6,
            tableOutput("duel_type_table"),
            plotOutput("duel_type_plot", height = "200px"),
            downloadButton("download_duel_type_plot", "Download Duels by Type Plot as PNG", class = "btn-success", style = "margin-top: 10px;")
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
  
  # Datos reactivos para duelos ganados
  won_duels <- eventReactive(input$update_analysis, {
    req(team_season_data(), selected_matchdays(), input$duel_types)
    if(length(selected_matchdays()) == 0) return(data.frame())
    get_team_won_duels(team_season_data(), input$selected_team, selected_matchdays(), input$duel_types)
  }, ignoreNULL = FALSE)
  
  output$field_plot <- renderPlot({
    req(input$duel_types)
    
    duels <- won_duels()
    plot_title <- paste("Won Duels Heatmap -", input$selected_team)
    
    if(nrow(duels) == 0) {
      p <- draw_soccer_field() +
        labs(title = plot_title,
             subtitle = "No won duels found for selected matches")
      return(p)
    }
    
    zones <- create_field_zones()
    
    # Estadísticas para el subtítulo
    aerial_total <- sum(duels$event == "Aerial", na.rm = TRUE)
    tackle_total <- sum(duels$event == "Tackle", na.rm = TRUE)
    challenge_total <- sum(duels$event == "Challenge", na.rm = TRUE)
    def_duels <- sum(duels$field_zone == "Defensive Third", na.rm = TRUE)
    mid_duels <- sum(duels$field_zone == "Middle Third", na.rm = TRUE)
    att_duels <- sum(duels$field_zone == "Attacking Third", na.rm = TRUE)
    
    subtitle_text <- paste0(
      "Season: ", input$selected_season, " | Matches analyzed: ", length(selected_matchdays()), " | Total Won Duels: ", nrow(duels), "\n",
      "Aerial: ", aerial_total, " | Tackle: ", tackle_total, " | Challenge: ", challenge_total, "\n",
      "Zones: Def(", def_duels, ") Mid(", mid_duels, ") Att(", att_duels, ")"
    )
    
    # Crear heatmap
    duels_heatmap <- duels %>%
      mutate(
        bin_x = pmin(pmax(floor(x / (100/6)), 0), 5),
        bin_y = pmin(pmax(floor(y / (100/3)), 0), 2)
      ) %>%
      count(bin_x, bin_y) %>%
      mutate(id = bin_x + bin_y * 6) %>%
      right_join(zones, by = "id") %>%
      mutate(n = ifelse(is.na(n), 0, n))
    
    heatmap_colors <- colorRampPalette(c(colors$heatmap_start, colors$heatmap_end))(20)
    
    p <- draw_soccer_field() +
      geom_rect(
        data = duels_heatmap,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
        color = "white", linetype = "dashed", size = 0.3, alpha = 0.8
      ) +
      scale_fill_gradientn(colors = heatmap_colors, name = "Won Duels", guide = guide_colorbar(title.position = "top")) +
      geom_text(
        data = duels_heatmap %>% filter(n > 0),
        aes(x = center_x, y = center_y, label = n),
        color = "#222", fontface = "bold", size = 4
      ) +
      # --- LÍNEA PUNTEADA AMARILLA DE DISTANCIA PROMEDIO ---
      {
        avg_distance <- mean(duels$x, na.rm = TRUE)
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
      # --- PUNTOS DE DUELOS POR TIPO ---
      geom_point(
        data = duels,
        aes(x = x, y = y, color = event),
        size = 2, alpha = 0.7
      ) +
      scale_color_manual(
        values = c("Aerial" = colors$aerial_color, 
                   "Tackle" = colors$tackle_color, 
                   "Challenge" = colors$challenge_color),
        name = "Duel Type"
      ) +
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
  
  output$duels_stats <- renderTable({
    duels <- won_duels()
    if(nrow(duels) == 0) {
      return(data.frame(Metric = "No duels found", Value = "0"))
    }
    
    aerial_total <- sum(duels$event == "Aerial", na.rm = TRUE)
    tackle_total <- sum(duels$event == "Tackle", na.rm = TRUE)
    challenge_total <- sum(duels$event == "Challenge", na.rm = TRUE)
    def_duels <- sum(duels$field_zone == "Defensive Third", na.rm = TRUE)
    mid_duels <- sum(duels$field_zone == "Middle Third", na.rm = TRUE)
    att_duels <- sum(duels$field_zone == "Attacking Third", na.rm = TRUE)
    
    stats <- data.frame(
      Metric = c("Total Won Duels", "Aerial Duels", "Tackle Duels", "Challenge Duels", 
                 "Defensive Zone", "Middle Zone", "Attacking Zone", "Matches Analyzed"),
      Value = c(
        nrow(duels),
        aerial_total,
        tackle_total,
        challenge_total,
        def_duels,
        mid_duels,
        att_duels,
        length(selected_matchdays())
      )
    )
    stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$zone_table <- renderTable({
    duels <- won_duels()
    if(nrow(duels) == 0) {
      return(data.frame(Zone = "No duels found", Count = "0", Percentage = "0%"))
    }
    
    zone_stats <- duels %>%
      group_by(field_zone) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
      arrange(desc(Count))
    
    zone_stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$zone_plot <- renderPlot({
    duels <- won_duels()
    if(nrow(duels) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available") + theme_void())
    }
    
    zone_stats <- duels %>%
      group_by(field_zone) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = round(Count / sum(Count) * 100, 1))
    
    ggplot(zone_stats, aes(x = field_zone, y = Count, fill = field_zone)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")), 
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_manual(values = c("Defensive Third" = "#FF6B6B", 
                                   "Middle Third" = "#4ECDC4", 
                                   "Attacking Third" = "#45B7D1")) +
      labs(title = "Won Duels by Field Zone",
           x = "Field Zone", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$top_duelers_plot <- DT::renderDataTable({
    duels <- won_duels()
    
    if(nrow(duels) == 0) {
      return(DT::datatable(
        data.frame(Player = "No duels found", Won_Duels = NA),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    top_duelers <- duels %>%
      group_by(player_name) %>%
      summarise(won_duels = n()) %>%
      arrange(desc(won_duels)) %>%
      slice_head(n = 5) %>%
      mutate(
        Photo = sapply(player_name, function(name) {
          path <- get_player_photo(name)
          sprintf('<img src="%s" height="40" style="border-radius:50%%;margin-right:8px;">', path)
        }),
        Player = paste0(Photo, player_name)
      )
    
    DT::datatable(
      top_duelers[, c("Player", "won_duels")],
      options = list(pageLength = 5, dom = 't'),
      rownames = FALSE,
      colnames = c("Player", "Won Duels"),
      escape = FALSE
    )
  })
  
  output$duel_type_table <- renderTable({
    duels <- won_duels()
    if(nrow(duels) == 0) {
      return(data.frame(Type = "No duels found", Count = "0", Percentage = "0%"))
    }
    
    type_stats <- duels %>%
      group_by(event) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
      arrange(desc(Count))
    
    type_stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$duel_type_plot <- renderPlot({
    duels <- won_duels()
    if(nrow(duels) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available") + theme_void())
    }
    
    type_stats <- duels %>%
      group_by(event) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = round(Count / sum(Count) * 100, 1))
    
    ggplot(type_stats, aes(x = event, y = Count, fill = event)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")), 
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_manual(values = c("Aerial" = colors$aerial_color, 
                                   "Tackle" = colors$tackle_color, 
                                   "Challenge" = colors$challenge_color)) +
      labs(title = "Won Duels by Type",
           x = "Duel Type", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ===== FUNCIONES DE DESCARGA =====
  
  # Descargar heatmap principal
  output$download_heatmap <- downloadHandler(
    filename = function() {
      paste0("Won_Duels_Heatmap_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      req(won_duels())
      duels <- won_duels()
      
      if(nrow(duels) == 0) {
        p <- draw_soccer_field() +
          labs(title = paste("Won Duels Heatmap -", input$selected_team),
               subtitle = "No won duels found for selected matches")
        ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
        return()
      }
      
      zones <- create_field_zones()
      
      # Estadísticas para el subtítulo
      aerial_total <- sum(duels$event == "Aerial", na.rm = TRUE)
      tackle_total <- sum(duels$event == "Tackle", na.rm = TRUE)
      challenge_total <- sum(duels$event == "Challenge", na.rm = TRUE)
      def_duels <- sum(duels$field_zone == "Defensive Third", na.rm = TRUE)
      mid_duels <- sum(duels$field_zone == "Middle Third", na.rm = TRUE)
      att_duels <- sum(duels$field_zone == "Attacking Third", na.rm = TRUE)
      
      subtitle_text <- paste0(
        "Season: ", input$selected_season, " | Matches analyzed: ", length(selected_matchdays()), " | Total Won Duels: ", nrow(duels), "\n",
        "Aerial: ", aerial_total, " | Tackle: ", tackle_total, " | Challenge: ", challenge_total, "\n",
        "Zones: Def(", def_duels, ") Mid(", mid_duels, ") Att(", att_duels, ")"
      )
      
      # Crear heatmap
      duels_heatmap <- duels %>%
        mutate(
          bin_x = pmin(pmax(floor(x / (100/6)), 0), 5),
          bin_y = pmin(pmax(floor(y / (100/3)), 0), 2)
        ) %>%
        count(bin_x, bin_y) %>%
        mutate(id = bin_x + bin_y * 6) %>%
        right_join(zones, by = "id") %>%
        mutate(n = ifelse(is.na(n), 0, n))
      
      heatmap_colors <- colorRampPalette(c(colors$heatmap_start, colors$heatmap_end))(20)
      
      p <- draw_soccer_field() +
        geom_rect(
          data = duels_heatmap,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
          color = "white", linetype = "dashed", size = 0.3, alpha = 0.8
        ) +
        scale_fill_gradientn(colors = heatmap_colors, name = "Won Duels", guide = guide_colorbar(title.position = "top")) +
        geom_text(
          data = duels_heatmap %>% filter(n > 0),
          aes(x = center_x, y = center_y, label = n),
          color = "#222", fontface = "bold", size = 4
        ) +
        # Línea de distancia promedio
        {
          avg_distance <- mean(duels$x, na.rm = TRUE)
          list(
            geom_vline(xintercept = avg_distance, color = colors$avg_line, linetype = "dashed", linewidth = 0.8),
            annotate("text", x = avg_distance, y = 5,
                     label = paste("Avg. Distance:", round(avg_distance, 2)),
                     hjust = 0.5, vjust = 0, size = 4, fontface = "italic", color = colors$text_primary)
          )
        } +
        # Flecha de dirección de ataque
        {
          team_season_matches <- team_season_data() %>%
            filter(matchday_num %in% selected_matchdays()) %>%
            select(matchday_num, team_home, team_away) %>%
            distinct()
          
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
        # Puntos de duelos por tipo
        geom_point(
          data = duels,
          aes(x = x, y = y, color = event),
          size = 2, alpha = 0.7
        ) +
        scale_color_manual(
          values = c("Aerial" = colors$aerial_color, 
                     "Tackle" = colors$tackle_color, 
                     "Challenge" = colors$challenge_color),
          name = "Duel Type"
        ) +
        labs(title = paste("Won Duels Heatmap -", input$selected_team),
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
      
      ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
    }
  )
  
  # Descargar estadísticas de duelos
  output$download_duels_stats <- downloadHandler(
    filename = function() {
      paste0("Duels_Statistics_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      # Crear tabla visual
      duels <- won_duels()
      if(nrow(duels) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No duels found", size = 8) +
          theme_void()
        ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
        return()
      }
      
      aerial_total <- sum(duels$event == "Aerial", na.rm = TRUE)
      tackle_total <- sum(duels$event == "Tackle", na.rm = TRUE)
      challenge_total <- sum(duels$event == "Challenge", na.rm = TRUE)
      def_duels <- sum(duels$field_zone == "Defensive Third", na.rm = TRUE)
      mid_duels <- sum(duels$field_zone == "Middle Third", na.rm = TRUE)
      att_duels <- sum(duels$field_zone == "Attacking Third", na.rm = TRUE)
      
      stats_data <- data.frame(
        Metric = c("Total Won Duels", "Aerial Duels", "Tackle Duels", "Challenge Duels", 
                   "Defensive Zone", "Middle Zone", "Attacking Zone", "Matches Analyzed"),
        Value = c(
          nrow(duels),
          aerial_total,
          tackle_total,
          challenge_total,
          def_duels,
          mid_duels,
          att_duels,
          length(selected_matchdays())
        )
      )
      
      p <- ggplot(stats_data, aes(x = reorder(Metric, -Value), y = Value)) +
        geom_bar(stat = "identity", fill = colors$accent) +
        geom_text(aes(label = Value), vjust = -0.5, size = 4, fontface = "bold") +
        labs(title = paste("Duels Statistics -", input$selected_team),
             x = "Metric", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold"))
      
      ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
    }
  )
  
  # Descargar gráfico de zonas
  output$download_zone_plot <- downloadHandler(
    filename = function() {
      paste0("Zone_Plot_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      duels <- won_duels()
      if(nrow(duels) == 0) {
        p <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available") + theme_void()
        ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
        return()
      }
      
      zone_stats <- duels %>%
        group_by(field_zone) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = round(Count / sum(Count) * 100, 1))
      
      p <- ggplot(zone_stats, aes(x = field_zone, y = Count, fill = field_zone)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")), 
                  vjust = -0.5, size = 4, fontface = "bold") +
        scale_fill_manual(values = c("Defensive Third" = "#FF6B6B", 
                                     "Middle Third" = "#4ECDC4", 
                                     "Attacking Third" = "#45B7D1")) +
        labs(title = paste("Won Duels by Field Zone -", input$selected_team),
             x = "Field Zone", y = "Count") +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    }
  )
  
  # Descargar top duelistas
  observeEvent(input$download_top_duelers, {
    filename <- paste0("Top_Duels_Winners_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    session$sendCustomMessage("downloadPanel", list(
      selector = "#top_duelers_panel",
      filename = filename
    ))
  })
  
  # Descargar gráfico de tipos de duelo
  output$download_duel_type_plot <- downloadHandler(
    filename = function() {
      paste0("Duels_by_Type_", input$selected_team, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      duels <- won_duels()
      if(nrow(duels) == 0) {
        p <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available") + theme_void()
        ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
        return()
      }
      
      type_stats <- duels %>%
        group_by(event) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = round(Count / sum(Count) * 100, 1))
      
      p <- ggplot(type_stats, aes(x = event, y = Count, fill = event)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")), 
                  vjust = -0.5, size = 4, fontface = "bold") +
        scale_fill_manual(values = c("Aerial" = colors$aerial_color, 
                                     "Tackle" = colors$tackle_color, 
                                     "Challenge" = colors$challenge_color)) +
        labs(title = paste("Won Duels by Type -", input$selected_team),
             x = "Duel Type", y = "Count") +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    }
  )
}

# ===== EJECUTAR APP =====
shinyApp(ui = ui, server = server)