# Chelsea FC Performance Insights Dashboard
# Created for CFC Performance Insights Vizathon

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)
library(RColorBrewer)
library(tidyr)
library(viridis)

# Set Chelsea FC colors
chelsea_blue <- "#034694"
chelsea_gold <- "#DBA111"
chelsea_red <- "#ED1C24"
chelsea_white <- "#FFFFFF"
chelsea_black <- "#000000"

# Create simulated data for the 2024/25 season
set.seed(123)

# 1. Player Information Data
players <- data.frame(
  id = 1:20,
  name = c("Robert Sanchez", "Filip Jorgensen", "Marcus Bettinelli", 
           "Reece James", "Malo Gusto", "Axel Disasi", "Wesley Fofana", 
           "Benoit Badiashile", "Levi Colwill", "Marc Cucurella", 
           "Renato Veiga", "Moises Caicedo", "Enzo Fernandez", 
           "Romeo Lavia", "Carney Chukwuemeka", "Cole Palmer", 
           "Christopher Nkunku", "Mykhailo Mudryk", "Noni Madueke", 
           "Nicolas Jackson"),
  position = c(rep("GK", 3), 
               rep("DEF", 7), 
               rep("MID", 6), 
               rep("FWD", 4)),
  age = c(27, 22, 32, 
          25, 21, 26, 
          24, 23, 22, 
          26, 21, 23, 
          23, 20, 21, 
          22, 27, 23, 
          22, 23),
  nationality = c("Spain", "Denmark", "England",
                  "England", "France", "France", 
                  "France", "France", "England", 
                  "Spain", "Portugal", "Ecuador", 
                  "Argentina", "Belgium", "England", 
                  "England", "France", "Ukraine", 
                  "England", "Senegal"),
  height_cm = c(197, 191, 193,
                170, 179, 190,
                190, 194, 187,
                172, 182, 178,
                178, 175, 183,
                189, 175, 175,
                179, 187),
  weight_kg = c(88, 80, 86,
                70, 71, 86,
                84, 82, 80,
                75, 76, 74,
                77, 70, 75,
                80, 73, 72,
                74, 82)
)

# Añadir URLs de imágenes a los datos de jugadores
players$image_url <- c(
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Sanchez/Mens_3333x5000_Avatar_Sanchez_SF_Home_24_25_RGB.png", # Sanchez
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Jorgensen/Mens_3333x5000_Avatar_Jorgensen_SF_Home_24_25_RGB.png", # Jorgensen
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Bettinelli/Mens_3333x5000_Avatar_Bettinelli_SF_Home_24_25_RGB.png", # Bettinelli
  "https://resources.premierleague.com/premierleague/photos/players/250x250/p225796.png", # James
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Gusto/Mens_3333x5000_Avatar_Gusto_SF_Home_24_25_RGB.png", # Gusto
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/Axel_Disasi_profile_2024-25_fever_avatar-removebg.png", # Disasi
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Fofana%20W/Mens_3333x5000_Avatar_Fofana_W_SF_Home_24_25_RGB.png", # Fofana
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Badiashile/Mens_3333x5000_Avatar_Badiashile_SF_Home_24_25_RGB.png", # Badiashile
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Colwill/Mens_3333x5000_Avatar_Colwill_SF_Home_24_25_RGB.png", # Colwill
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Cucurella/Mens_3333x5000_Avatar_Cucurella_SF_Home_24_25_RGB.png", # Cucurella
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Veiga/Mens_3333x5000_Avatar_VEIGA_SF_Home_24_25_RGB.png", # Veiga
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Caicedo/Mens_3333x5000_Avatar_Caicedo_SF_Home_24_25_RGB.png", # Caicedo 
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Fernandez/Mens_3333x5000_Avatar_Fernandez_SF_Home_24_25_RGB.png", # Fernandez
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Lavia/Mens_3333x5000_Avatar_Lavia_SF_Home_24_25_RGB.png", # Lavia
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/Carney_Chukwuemeka_profile_2024-25_fever_avatar-removebg.png", # Chukwuemeka
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Palmer/Mens_3333x5000_Avatar_Palmer_SF_Home_24_25_RGB.png", # Palmer
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Nkunku/Mens_3333x5000_Avatar_Nkunku_SF_Home_24_25_RGB.png", # Nkunku
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Mudryk/Mens_3333x5000_Avatar_Mudryk_SF_Home_24_25_RGB.png", # Mudryk
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Madueke/Mens_3333x5000_Avatar_Madueke_SF_Home_24_25_RGB.png", # Madueke
  "https://img.chelseafc.com/image/upload/f_auto,h_860,q_50/editorial/people/first-team/2024-25/With%20LN/Jackson/Mens_3333x5000_Avatar_Jackson_SF_Home_24_25_RGB.png"  # Jackson
)

# 2. GPS Data (for one player as example, will be expanded to all players)
# Generate 30 dates from Aug 1, 2024 to current date
# Make sure to use as.Date for all date elements
start_date <- as.Date("2024-08-01")
end_date <- Sys.Date()
dates <- seq(start_date, end_date, length.out = 30)

# Opponent teams
opponents <- c("Manchester United", "Manchester City", "Liverpool", "Arsenal", 
               "Tottenham", "Newcastle", "Aston Villa", "Crystal Palace", 
               "Brighton", "West Ham", "Fulham", "Everton", "Bournemouth",
               "Wolverhampton", "Southampton", "Brentford", "Nottingham Forest",
               "Leicester City", "Ipswich Town")

# Function to generate GPS data for one player on one date
generate_gps_data <- function(player_id, date) {
  # Ensure date is a Date object
  date <- as.Date(date)
  
  # Is it a match day? (Assume matches are on Saturdays)
  is_match <- weekdays(date) == "Saturday"
  
  # Determine MD code
  if (is_match) {
    md_code <- "MD"
    opposition <- ifelse(runif(1) > 0.5, 
                         paste0(sample(opponents, 1), " (H)"),
                         paste0(sample(opponents, 1), " (A)"))
  } else {
    days_to_next_match <- (7 - as.integer(format(date, "%u"))) %% 7
    if (days_to_next_match == 0) days_to_next_match <- 7
    md_code <- paste0("MD-", days_to_next_match)
    opposition <- NA
  }
  
  # Generate data based on whether it's a match day or training
  if (is_match) {
    distance <- runif(1, 8000, 12000)
    duration <- runif(1, 85, 95)
    high_speed_pct <- runif(1, 7, 15)
  } else {
    distance <- runif(1, 4000, 8000)
    duration <- runif(1, 45, 75)
    high_speed_pct <- runif(1, 5, 10)
  }
  
  # Calculate derived metrics
  distance_over_21 <- distance * (high_speed_pct/100) * runif(1, 0.5, 0.7)
  distance_over_24 <- distance_over_21 * runif(1, 0.4, 0.6)
  distance_over_27 <- distance_over_24 * runif(1, 0.2, 0.4)
  
  accel_decel_over_2_5 <- round(runif(1, 40, 100))
  accel_decel_over_3_5 <- round(accel_decel_over_2_5 * runif(1, 0.3, 0.6))
  accel_decel_over_4_5 <- round(accel_decel_over_3_5 * runif(1, 0.2, 0.4))
  
  peak_speed <- runif(1, 28, 35)
  
  # Heart rate zone data (in minutes)
  hr_zone_1 <- round(runif(1, 5, 15))
  hr_zone_2 <- round(runif(1, 10, 25))
  hr_zone_3 <- round(runif(1, 15, 30))
  hr_zone_4 <- round(runif(1, 10, 20))
  hr_zone_5 <- round(runif(1, 0, 10))
  
  # Create data frame
  data.frame(
    player_id = player_id,
    date = date,
    md_code = md_code,
    opposition = opposition,
    season = "2024/25",
    distance = round(distance),
    distance_over_21 = round(distance_over_21),
    distance_over_24 = round(distance_over_24),
    distance_over_27 = round(distance_over_27),
    accel_decel_over_2_5 = accel_decel_over_2_5,
    accel_decel_over_3_5 = accel_decel_over_3_5,
    accel_decel_over_4_5 = accel_decel_over_4_5,
    day_duration = duration,
    peak_speed = round(peak_speed, 1),
    hr_zone_1_min = hr_zone_1,
    hr_zone_2_min = hr_zone_2,
    hr_zone_3_min = hr_zone_3,
    hr_zone_4_min = hr_zone_4,
    hr_zone_5_min = hr_zone_5
  )
}

# Generate GPS data for all players and all dates
gps_data <- data.frame()
for (pid in players$id) {
  for (d in dates) {
    player_day_data <- generate_gps_data(pid, d)
    gps_data <- rbind(gps_data, player_day_data)
  }
}

# 3. Physical Capability Data
# Movements, Qualities, and Expressions
movements <- c("Agility", "Sprint", "Upper Body", "Jump")
qualities <- c("Acceleration", "Deceleration", "Grapple", "Land", "Max velocity", 
               "Pre-load", "Pull", "Push", "Rotate", "Take off")
expressions <- c("Isometric", "Dynamic")

# Generate Physical Capability data - versión mejorada
generate_physical_data <- function(player_id, date) {
  # Ensure we cover all movement types for each player
  result <- data.frame()
  
  # Generate data for all movements with some randomization
  for(movement in movements) {
    # Pick 2-3 random qualities for this movement
    num_qualities <- sample(2:3, 1)
    selected_qualities <- sample(qualities, num_qualities)
    
    for(quality in selected_qualities) {
      # Decide randomly if we include both expression types or just one
      include_both <- runif(1) > 0.3
      selected_expressions <- if(include_both) expressions else sample(expressions, 1)
      
      for(expression in selected_expressions) {
        # Generate benchmark percent between 30% and 95% with some player-specific bias
        # Players with higher IDs (forwards) tend to have better sprint/agility scores
        base_score <- runif(1, 30, 95)
        
        # Add player-specific bias
        if(movement %in% c("Sprint", "Agility") && player_id > 15) {
          # Forwards tend to have better sprint/agility
          benchmark_pct <- min(base_score + runif(1, 5, 15), 95)
        } else if(movement == "Upper Body" && player_id >= 8 && player_id <= 15) {
          # Defenders and midfielders tend to have better upper body strength
          benchmark_pct <- min(base_score + runif(1, 5, 15), 95)
        } else {
          benchmark_pct <- base_score
        }
        
        entry <- data.frame(
          player_id = player_id,
          date = date,
          movement = movement,
          quality = quality,
          expression = expression,
          benchmark_pct = benchmark_pct
        )
        
        result <- rbind(result, entry)
      }
    }
  }
  
  return(result)
}

# Generate more frequent physical capability data (twice a week)
physical_test_dates <- seq(as.Date("2024-08-01"), Sys.Date(), by = "3 days")

# Clear existing data and regenerate
physical_data <- data.frame()
for (pid in players$id) {
  for (d in physical_test_dates) {
    player_physical_data <- generate_physical_data(pid, d)
    physical_data <- rbind(physical_data, player_physical_data)
  }
}

# 4. Recovery Status Data
recovery_categories <- c("Bio", "Msk_joint_range", "Msk_load_tolerance", 
                         "Subjective", "Soreness", "Sleep")

# Improved recovery data generator
generate_recovery_data <- function(player_id, date) {
  
  date <- as.Date(date)
  tryCatch({

  # Simulate weekly patterns (better recovery further from match days)
  day_of_week <- as.integer(format(date, "%u"))  # 1 = Monday, 7 = Sunday
  
  }, error = function(e) {
    day_of_week <<- 1
  })
  
  # Match days are usually Saturday (6)
  days_since_match <- (day_of_week + 1) %% 7  # 0 on match day, 1 on Sunday, etc.
  
  # Generate completeness values (0-1) - higher compliance after match days
  completeness_base <- 0.85
  if(days_since_match <= 2) {
    # Better compliance right after matches
    completeness_base <- 0.95
  }
  
  completeness_values <- runif(length(recovery_categories), 
                               completeness_base - 0.1, 
                               completeness_base + 0.05)
  
  # Cap at 1.0
  completeness_values <- pmin(completeness_values, 1.0)
  
  # Recovery is lower right after matches, improves over the week
  recovery_modifier <- -0.3 + (days_since_match * 0.1)
  
  # Generate composite scores (-1 to 1, representing -100% to 100%)
  composite_values <- runif(length(recovery_categories), 
                            -0.2 + recovery_modifier, 
                            0.4 + recovery_modifier)
  
  # Add some player-specific variation
  player_factor <- (player_id / 20) - 0.5  # -0.5 to 0.5 range
  composite_values <- composite_values + (player_factor * 0.2)
  
  # Cap values between -1 and 1
  composite_values <- pmin(pmax(composite_values, -1), 1)
  
  # On some dates, some metrics might not be collected
  # Lower probability of missing data
  not_collected <- sample(c(TRUE, FALSE), length(recovery_categories), 
                          replace = TRUE, prob = c(0.05, 0.95))
  
  if(any(not_collected)) {
    completeness_values[not_collected] <- NA
    composite_values[not_collected] <- NA
  }
  
  # Calculate overall EMBOSS score as average of available composites
  emboss_baseline_score <- mean(composite_values, na.rm = TRUE)
  
  # Create data frame with one row per category
  result <- data.frame(
    player_id = player_id,
    date = date,
    category = c(recovery_categories, "total"),
    metric = c(paste0(recovery_categories, "_completeness"), "emboss_baseline_score"),
    value = c(completeness_values, emboss_baseline_score)
  )
  
  # Add composite metrics
  composite_df <- data.frame(
    player_id = player_id,
    date = date,
    category = recovery_categories,
    metric = paste0(recovery_categories, "_composite"),
    value = composite_values
  )
  
  result <- rbind(result, composite_df)
  
  return(result)
}

# Generate recovery data more frequently (every day)
recovery_dates <- seq(as.Date("2024-08-01"), Sys.Date(), by = "1 day")

# Clear existing data and regenerate
recovery_data <- data.frame()
for (pid in players$id) {
  for (d in recovery_dates) {
    player_recovery_data <- generate_recovery_data(pid, d)
    recovery_data <- rbind(recovery_data, player_recovery_data)
  }
}

# Reshape recovery data for easier use
recovery_wide <- recovery_data %>%
  pivot_wider(id_cols = c(player_id, date), 
              names_from = metric, 
              values_from = value)

# Verify that we have sufficient data
cat("Physical data rows:", nrow(physical_data), "\n")
cat("Recovery data rows:", nrow(recovery_data), "\n")

#PART 2

# UI Definition for the Chelsea FC Performance Dashboard

ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard Header
  dashboardHeader(
    title = div(img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/c/cc/Chelsea_FC.svg/130px-Chelsea_FC.svg.png", 
                    height = 30), 
                "CFC Performance Insights"),
    titleWidth = 300
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      # Add custom CSS
      tags$head(
        tags$style(HTML("
          .skin-blue .main-header .logo {
            background-color: #034694;
          }
          .skin-blue .main-header .navbar {
            background-color: #034694;
          }
          .skin-blue .main-header .logo:hover {
            background-color: #034694;
          }
          .skin-blue .main-sidebar {
            background-color: #202020;
          }
          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
            background-color: #034694;
          }
          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
            background-color: #1e5cb3;
          }
          .box.box-primary {
            border-top-color: #034694;
          }
          .select-info {
            font-size: 15px;
            font-weight: bold;
          }
          .player-card {
            background-color: #f8f9fa;
            border-radius: 10px;
            padding: 15px;
            margin-bottom: 20px;
            box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          }
          .player-card h3 {
            margin-top: 0;
            color: #034694;
          }
          .priority-card {
            border-left: 5px solid #DBA111;
            padding-left: 15px;
          }
          .dashboard-header h2 {
            color: #034694;
            font-weight: bold;
          }
          .dashboard-subheader {
            color: #666;
            font-size: 16px;
            margin-bottom: 20px;
          }
          .indicator-label {
            font-weight: bold;
            font-size: 14px;
          }
          .indicator-value {
            font-size: 24px;
            font-weight: bold;
          }
          .value-up {
            color: green;
          }
          .value-down {
            color: red;
          }
        "))
      ),
      
      # Player Selection
      selectInput("player", "Select Player", 
                  choices = setNames(players$id, players$name)),
      
      # Module Tabs
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Load Demand", tabName = "load", icon = icon("running")),
      menuItem("Physical Development", tabName = "physical", icon = icon("dumbbell")),
      menuItem("Recovery", tabName = "recovery", icon = icon("heart")),
      
      # Date Range Selection
      dateRangeInput("date_range", "Date Range",
                     start = min(dates), 
                     end = max(dates),
                     min = min(dates),
                     max = max(dates))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    tabItems(
      # Dashboard Tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            width = 12,
            class = "dashboard-header",
            h2("CFC Performance Insights"),
            div(class = "dashboard-subheader", 
                "Player Performance Analytics | 2024/25 Season"),
            hr()
          )
        ),
        fluidRow(
          # Player Info Card
          column(
            width = 4,
            div(
              class = "player-card",
              uiOutput("player_info")
            )
          ),
          # Key Performance Indicators
          column(
            width = 8,
            fluidRow(
              column(
                width = 4,
                div(
                  class = "player-card",
                  h4("Training Load", class = "indicator-label"),
                  uiOutput("kpi_load")
                )
              ),
              column(
                width = 4,
                div(
                  class = "player-card",
                  h4("Physical Status", class = "indicator-label"),
                  uiOutput("kpi_physical")
                )
              ),
              column(
                width = 4,
                div(
                  class = "player-card",
                  h4("Recovery Status", class = "indicator-label"),
                  uiOutput("kpi_recovery")
                )
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "7-Day Performance Snapshot",
                plotlyOutput("recent_performance")
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              title = "Priority Areas",
              div(
                class = "priority-card",
                uiOutput("priority_areas")
              )
            )
          )
        )
      ),
      
      # Load Demand Tab
      tabItem(
        tabName = "load",
        fluidRow(
          column(
            width = 12,
            h2("Load Demand", class = "dashboard-header"),
            div(class = "dashboard-subheader", 
                "GPS Performance Metrics & Training Load Analysis")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Training & Match Load Distribution",
            plotlyOutput("load_distribution")
          ),
          box(
            width = 6,
            title = "High-Speed Running Profile",
            plotlyOutput("speed_profile")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Weekly Load Progression",
            plotlyOutput("weekly_load")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Acceleration/Deceleration Profile",
            plotlyOutput("accel_decel_profile")
          ),
          box(
            width = 6,
            title = "Heart Rate Zone Distribution",
            plotlyOutput("hr_zones")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Training & Match Data",
            DT::dataTableOutput("load_data_table")
          )
        )
      ),
      
      # Physical Development Tab
      tabItem(
        tabName = "physical",
        fluidRow(
          column(
            width = 12,
            h2("Physical Development", class = "dashboard-header"),
            div(class = "dashboard-subheader", 
                "Physical Testing Results & Capability Assessment")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Physical Profile Overview",
            plotlyOutput("physical_radar")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Movement Quality Progression",
            plotlyOutput("movement_progression")
          ),
          box(
            width = 6,
            title = "Expression Type Analysis",
            plotlyOutput("expression_analysis")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Physical Testing Data",
            DT::dataTableOutput("physical_data_table")
          )
        )
      ),
      
      # Recovery Tab
      tabItem(
        tabName = "recovery",
        fluidRow(
          column(
            width = 12,
            h2("Recovery Status", class = "dashboard-header"),
            div(class = "dashboard-subheader", 
                "Recovery Metrics & Wellness Tracking")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Overall Recovery Status (EMBOSS Score)",
            plotlyOutput("recovery_trend")
          ),
          box(
            width = 6,
            title = "Recovery Category Comparison",
            plotlyOutput("recovery_categories")
          )
        ),
        fluidRow(
          column(
            width = 3,
            div(
              class = "player-card",
              h4("Bio", class = "indicator-label"),
              plotlyOutput("recovery_bio")
            )
          ),
          column(
            width = 3,
            div(
              class = "player-card",
              h4("MSK Status", class = "indicator-label"),
              plotlyOutput("recovery_msk")
            )
          ),
          column(
            width = 3,
            div(
              class = "player-card",
              h4("Subjective Wellness", class = "indicator-label"),
              plotlyOutput("recovery_subjective")
            )
          ),
          column(
            width = 3,
            div(
              class = "player-card",
              h4("Sleep Quality", class = "indicator-label"),
              plotlyOutput("recovery_sleep")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Recovery Data Completeness",
            plotlyOutput("recovery_completeness")
          )
        )
      )
    )
  )
)

#PART 3

# Server function for the Chelsea FC Performance Dashboard
server <- function(input, output, session) {
  
  # Reactive filtered data based on player selection and date range
  player_gps_data <- reactive({
    gps_data %>%
      filter(player_id == input$player,
             date >= input$date_range[1],
             date <= input$date_range[2])
  })
  
  player_physical_data <- reactive({
    physical_data %>%
      filter(player_id == input$player,
             date >= input$date_range[1],
             date <= input$date_range[2])
  })
  
  player_recovery_data <- reactive({
    recovery_data %>%
      filter(player_id == input$player,
             date >= input$date_range[1],
             date <= input$date_range[2])
  })
  
  # Get player info
  selected_player <- reactive({
    players %>% filter(id == input$player)
  })
  
  # Recent data (last 7 days)
  recent_data <- reactive({
    gps_data %>%
      filter(player_id == input$player,
             date >= Sys.Date() - 7,
             date <= Sys.Date())
  })
  
  # Dashboard Tab Outputs
  
  # Player info card
  output$player_info <- renderUI({
    player <- selected_player()
    
    div(
      style = "display: flex; flex-direction: column; align-items: center;",
      div(
        style = "margin-bottom: 15px; text-align: center;",
        img(src = player$image_url, height = "180px", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);")
      ),
      h3(player$name, style = "margin-top: 0; text-align: center;"),
      div(
        style = "width: 100%; text-align: left;",
        p(strong("Position:"), player$position),
        p(strong("Age:"), player$age),
        p(strong("Nationality:"), player$nationality),
        p(strong("Height:"), paste0(player$height_cm, " cm")),
        p(strong("Weight:"), paste0(player$weight_kg, " kg"))
      )
    )
  })
  
  # KPI indicators
  output$kpi_load <- renderUI({
    recent <- recent_data()
    
    # Calculate total distance and avg daily distance
    total_distance <- sum(recent$distance, na.rm = TRUE)
    avg_distance <- mean(recent$distance, na.rm = TRUE)
    
    # Calculate trend (compare to previous 7 days)
    prev_data <- gps_data %>%
      filter(player_id == input$player,
             date >= Sys.Date() - 14,
             date < Sys.Date() - 7)
    
    prev_avg_distance <- mean(prev_data$distance, na.rm = TRUE)
    trend_pct <- round((avg_distance - prev_avg_distance) / prev_avg_distance * 100, 1)
    
    trend_class <- ifelse(trend_pct > 0, "value-up", "value-down")
    trend_icon <- ifelse(trend_pct > 0, icon("arrow-up"), icon("arrow-down"))
    
    div(
      p(class = "indicator-value", paste0(round(avg_distance), " m"),
        span(class = trend_class, paste0(" ", trend_pct, "%"), trend_icon)),
      p("Avg. Daily Distance (7 days)"),
      p(paste0("Total: ", round(total_distance/1000, 1), " km"))
    )
  })
  
  output$kpi_physical <- renderUI({
    # Inicializar valores seguros
    avg_benchmark <- 0
    trend_pct <- 0
    last_test_date <- "No data"
    
    # Intentar obtener datos solo si no genera errores
    tryCatch({
      # Get most recent physical data
      recent_physical <- player_physical_data()
      
      if(nrow(recent_physical) > 0) {
        max_date <- max(recent_physical$date, na.rm = TRUE)
        recent_physical <- recent_physical %>% filter(date == max_date)
        
        if(nrow(recent_physical) > 0 && any(!is.na(recent_physical$benchmark_pct))) {
          avg_benchmark <- mean(recent_physical$benchmark_pct, na.rm = TRUE)
          last_test_date <- format(max_date, "%d %b %Y")
          
          # Compare to previous test only if we have valid current data
          dates_ordered <- sort(unique(player_physical_data()$date), decreasing = TRUE)
          
          if(length(dates_ordered) > 1) {
            prev_date <- dates_ordered[2]
            prev_physical <- player_physical_data() %>% filter(date == prev_date)
            
            if(nrow(prev_physical) > 0 && any(!is.na(prev_physical$benchmark_pct))) {
              prev_avg_benchmark <- mean(prev_physical$benchmark_pct, na.rm = TRUE)
              
              if(!is.na(prev_avg_benchmark) && !is.na(avg_benchmark) && 
                 !is.nan(prev_avg_benchmark) && !is.nan(avg_benchmark) &&
                 !is.infinite(prev_avg_benchmark) && !is.infinite(avg_benchmark)) {
                trend_pct <- round(avg_benchmark - prev_avg_benchmark, 1)
              }
            }
          }
        }
      }
    }, error = function(e) {
      # En caso de cualquier error, usar los valores predeterminados
      avg_benchmark <<- 0
      trend_pct <<- 0
      last_test_date <<- "No data"
    })
    
    # Garantizar que los valores son seguros para mostrar
    avg_benchmark_display <- tryCatch({
      as.character(round(avg_benchmark))
    }, error = function(e) {
      "0"
    })
    
    trend_display <- tryCatch({
      as.character(trend_pct)
    }, error = function(e) {
      "0"
    })
    
    # Determinar clases CSS y iconos
    trend_class <- ifelse(trend_pct > 0, "value-up", "value-down")
    trend_icon <- ifelse(trend_pct > 0, icon("arrow-up"), icon("arrow-down"))
    
    # Construir la interfaz de usuario
    div(
      p(class = "indicator-value", paste0(avg_benchmark_display, "%"),
        span(class = trend_class, paste0(" ", trend_display, "%"), trend_icon)),
      p("Physical Capability Score"),
      p(paste0("Last tested: ", last_test_date))
    )
  })
  
  output$kpi_recovery <- renderUI({
    # Inicializar valores seguros
    emboss_score <- 0
    trend_pct <- 0
    update_date <- "No data"
    has_data <- FALSE
    
    # Intentar obtener datos solo si no genera errores
    tryCatch({
      # Get most recent recovery data
      recent_recovery <- recovery_data %>%
        filter(player_id == input$player,
               date >= Sys.Date() - 7,
               date <= Sys.Date(),
               metric == "emboss_baseline_score")
      
      if(nrow(recent_recovery) > 0) {
        has_data <- TRUE
        max_date <- max(recent_recovery$date, na.rm = TRUE)
        latest_recovery <- recent_recovery %>% filter(date == max_date)
        
        if(nrow(latest_recovery) > 0 && !is.na(latest_recovery$value[1])) {
          emboss_score <- latest_recovery$value[1] * 100
          update_date <- format(max_date, "%d %b %Y")
          
          # Compare to previous only if we have valid current data
          if(nrow(recent_recovery) > 1) {
            dates_ordered <- sort(unique(recent_recovery$date), decreasing = TRUE)
            
            if(length(dates_ordered) > 1) {
              prev_date <- dates_ordered[2]
              prev_recovery <- recent_recovery %>% filter(date == prev_date)
              
              if(nrow(prev_recovery) > 0 && !is.na(prev_recovery$value[1])) {
                prev_emboss_score <- prev_recovery$value[1] * 100
                
                if(!is.na(prev_emboss_score) && !is.na(emboss_score) && 
                   !is.nan(prev_emboss_score) && !is.nan(emboss_score) &&
                   !is.infinite(prev_emboss_score) && !is.infinite(emboss_score)) {
                  trend_pct <- round(emboss_score - prev_emboss_score, 1)
                }
              }
            }
          }
        }
      }
    }, error = function(e) {
      # En caso de cualquier error, usar los valores predeterminados
      emboss_score <<- 0
      trend_pct <<- 0
      update_date <<- "No data"
      has_data <<- FALSE
    })
    
    if(has_data) {
      # Garantizar que los valores son seguros para mostrar
      emboss_display <- tryCatch({
        paste0(ifelse(emboss_score > 0, "+", ""), round(emboss_score))
      }, error = function(e) {
        "0"
      })
      
      trend_display <- tryCatch({
        paste0(ifelse(trend_pct > 0, "+", ""), trend_pct)
      }, error = function(e) {
        "0"
      })
      
      # Determinar clases CSS e iconos
      trend_class <- ifelse(trend_pct > 0, "value-up", "value-down")
      trend_icon <- ifelse(trend_pct > 0, icon("arrow-up"), icon("arrow-down"))
      
      status_class <- tryCatch({
        case_when(
          emboss_score >= 20 ~ "value-up",
          emboss_score <= -20 ~ "value-down",
          TRUE ~ ""
        )
      }, error = function(e) {
        ""
      })
      
      # Construir la interfaz de usuario
      div(
        p(class = paste("indicator-value", status_class), 
          paste0(emboss_display, "%"),
          span(class = trend_class, paste0(" ", trend_display, "%"), trend_icon)),
        p("EMBOSS Recovery Score"),
        p(paste0("Updated: ", update_date))
      )
    } else {
      div(
        p(class = "indicator-value", "N/A"),
        p("EMBOSS Recovery Score"),
        p("No recent data available")
      )
    }
  })
  
  # Recent performance snapshot
  output$recent_performance <- renderPlotly({
    recent <- recent_data()
    
    if (nrow(recent) > 0) {
      p <- plot_ly() %>%
        add_trace(
          type = 'scatter',
          mode = 'lines+markers',
          x = recent$date,
          y = recent$distance,
          name = 'Distance (m)',
          line = list(color = chelsea_blue, width = 3),
          marker = list(color = chelsea_blue, size = 10)
        ) %>%
        add_trace(
          type = 'scatter',
          mode = 'lines+markers',
          x = recent$date,
          
          #PART 3.b
          
          y = recent$distance_over_21,
          name = 'High-Speed Distance (>21 km/h)',
          yaxis = 'y2',
          line = list(color = chelsea_gold, width = 2, dash = 'dot'),
          marker = list(color = chelsea_gold, size = 8)
        ) %>%
        layout(
          title = "Recent Training & Match Load",
          xaxis = list(title = "Date"),
          yaxis = list(
            title = "Total Distance (m)",
            side = "left"
          ),
          yaxis2 = list(
            title = "High-Speed Distance (m)",
            side = "right",
            overlaying = "y"
          ),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified"
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Priority areas
  output$priority_areas <- renderUI({
    # Simulated priority areas - would be from a real database
    priorities <- list(
      list(category = "Recovery", area = "Sleep", target = "Increase average sleep by 1hr per night", 
           type = "Habit", status = "On Track", icon = "bed"),
      list(category = "Recovery", area = "Nutrition", target = "45g of carbohydrate every half time", 
           type = "Habit", status = "On Track", icon = "utensils"),
      list(category = "Performance", area = "Sprint", target = ">65% in max velocity score", 
           type = "Outcome", status = "Achieved", icon = "running")
    )
    
    div(
      lapply(1:length(priorities), function(i) {
        priority <- priorities[[i]]
        statusColor <- case_when(
          priority$status == "On Track" ~ "#28a745",
          priority$status == "Achieved" ~ "#007bff",
          TRUE ~ "#dc3545"
        )
        
        div(
          class = "player-card",
          style = "margin-bottom: 15px; border-left: 5px solid #DBA111; padding-left: 15px;",
          div(
            style = "display: flex; align-items: center;",
            icon(priority$icon, style = "color: #034694; font-size: 24px; margin-right: 10px;"),
            h4(paste0("Priority ", i, ": ", priority$area), 
               style = "margin-top: 0; color: #034694; font-weight: bold;")
          ),
          p(strong("Category:"), priority$category),
          p(strong("Target:"), priority$target),
          p(strong("Type:"), priority$type),
          p(strong("Status:"), 
            span(priority$status, style = paste0("color: ", statusColor, "; font-weight: bold;")))
        )
      })
    )
  })
  
  # Load Demand Tab Outputs
  
  # Training & Match Load Distribution
  output$load_distribution <- renderPlotly({
    player_data <- player_gps_data()
    
    if (nrow(player_data) > 0) {
      # Categorize as match day or training
      player_data$session_type <- ifelse(player_data$md_code == "MD", "Match", "Training")
      
      # Summarize by session type
      summary_data <- player_data %>%
        group_by(session_type) %>%
        summarize(
          avg_distance = mean(distance, na.rm = TRUE),
          avg_duration = mean(day_duration, na.rm = TRUE),
          avg_high_speed = mean(distance_over_21, na.rm = TRUE),
          avg_very_high_speed = mean(distance_over_24, na.rm = TRUE),
          avg_sprint = mean(distance_over_27, na.rm = TRUE),
          count = n()
        )
      
      p <- plot_ly() %>%
        add_trace(
          data = summary_data,
          x = ~session_type,
          y = ~avg_distance,
          type = 'bar',
          name = 'Total Distance (m)',
          marker = list(color = chelsea_blue)
        ) %>%
        add_trace(
          data = summary_data,
          x = ~session_type,
          y = ~avg_high_speed,
          type = 'bar',
          name = 'High-Speed Distance (>21 km/h)',
          marker = list(color = chelsea_gold)
        ) %>%
        add_trace(
          data = summary_data,
          x = ~session_type,
          y = ~avg_very_high_speed,
          type = 'bar',
          name = 'Very High-Speed Distance (>24 km/h)',
          marker = list(color = '#cc8800')
        ) %>%
        add_trace(
          data = summary_data,
          x = ~session_type,
          y = ~avg_sprint,
          type = 'bar',
          name = 'Sprint Distance (>27 km/h)',
          marker = list(color = chelsea_red)
        ) %>%
        layout(
          barmode = 'group',
          title = "Average Distance by Session Type",
          xaxis = list(title = ""),
          yaxis = list(title = "Distance (m)"),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified"
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Speed Profile
  output$speed_profile <- renderPlotly({
    player_data <- player_gps_data()
    
    if (nrow(player_data) > 0) {
      # Calculate speed profile percentages
      player_data <- player_data %>%
        mutate(
          pct_high_speed = distance_over_21 / distance * 100,
          pct_very_high_speed = distance_over_24 / distance * 100,
          pct_sprint = distance_over_27 / distance * 100
        )
      
      # Calculate averages
      avg_peak_speed <- mean(player_data$peak_speed, na.rm = TRUE)
      avg_pct_high_speed <- mean(player_data$pct_high_speed, na.rm = TRUE)
      avg_pct_very_high_speed <- mean(player_data$pct_very_high_speed, na.rm = TRUE)
      avg_pct_sprint <- mean(player_data$pct_sprint, na.rm = TRUE)
      
      # Create data frame for gauge chart
      gauge_data <- data.frame(
        category = c("Peak Speed (km/h)", "High-Speed %", "Very High-Speed %", "Sprint %"),
        value = c(avg_peak_speed, avg_pct_high_speed, avg_pct_very_high_speed, avg_pct_sprint)
      )
      
      # Define maximum values for gauges
      max_values <- c(40, 15, 10, 5)
      
      # Create gauge charts
      p <- plot_ly() %>%
        add_trace(
          type = "indicator",
          mode = "gauge+number",
          value = gauge_data$value[1],
          title = list(text = gauge_data$category[1]),
          gauge = list(
            axis = list(range = c(0, max_values[1])),
            bar = list(color = chelsea_blue),
            steps = list(
              list(range = c(0, max_values[1] * 0.6), color = "lightgray"),
              list(range = c(max_values[1] * 0.6, max_values[1] * 0.8), color = "gray"),
              list(range = c(max_values[1] * 0.8, max_values[1]), color = "darkgray")
            )
          ),
          domain = list(row = 0, column = 0)
        ) %>%
        add_trace(
          type = "indicator",
          mode = "gauge+number",
          value = gauge_data$value[2],
          title = list(text = gauge_data$category[2]),
          gauge = list(
            axis = list(range = c(0, max_values[2])),
            bar = list(color = chelsea_gold),
            steps = list(
              list(range = c(0, max_values[2] * 0.6), color = "lightgray"),
              list(range = c(max_values[2] * 0.6, max_values[2] * 0.8), color = "gray"),
              list(range = c(max_values[2] * 0.8, max_values[2]), color = "darkgray")
            )
          ),
          domain = list(row = 0, column = 1)
        ) %>%
        add_trace(
          type = "indicator",
          mode = "gauge+number",
          value = gauge_data$value[3],
          title = list(text = gauge_data$category[3]),
          number = list(suffix = "%"),
          gauge = list(
            axis = list(range = c(0, max_values[3])),
            bar = list(color = "#cc8800"),
            steps = list(
              list(range = c(0, max_values[3] * 0.6), color = "lightgray"),
              list(range = c(max_values[3] * 0.6, max_values[3] * 0.8), color = "gray"),
              list(range = c(max_values[3] * 0.8, max_values[3]), color = "darkgray")
            )
          ),
          domain = list(row = 1, column = 0)
        ) %>%
        add_trace(
          type = "indicator",
          mode = "gauge+number",
          value = gauge_data$value[4],
          title = list(text = gauge_data$category[4]),
          number = list(suffix = "%"),
          gauge = list(
            axis = list(range = c(0, max_values[4])),
            bar = list(color = chelsea_red),
            steps = list(
              list(range = c(0, max_values[4] * 0.6), color = "lightgray"),
              list(range = c(max_values[4] * 0.6, max_values[4] * 0.8), color = "gray"),
              list(range = c(max_values[4] * 0.8, max_values[4]), color = "darkgray")
            )
          ),
          domain = list(row = 1, column = 1)
        ) %>%
        layout(
          grid = list(rows = 2, columns = 2),
          margin = list(l = 25, r = 25, t = 50, b = 25)
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Weekly Load Progression
  output$weekly_load <- renderPlotly({
    player_data <- player_gps_data()
    
    if (nrow(player_data) > 0) {
      # Create week column
      player_data$week <- format(player_data$date, "%Y-%U")
      
      # Summarize by week
      weekly_data <- player_data %>%
        group_by(week) %>%
        summarize(
          total_distance = sum(distance, na.rm = TRUE),
          total_high_speed = sum(distance_over_21, na.rm = TRUE),
          total_very_high_speed = sum(distance_over_24, na.rm = TRUE),
          total_sprint = sum(distance_over_27, na.rm = TRUE),
          avg_peak_speed = mean(peak_speed, na.rm = TRUE),
          count = n(),
          start_date = min(date)
        ) %>%
        arrange(start_date)
      
      # Add week number for display
      weekly_data$week_num <- 1:nrow(weekly_data)
      weekly_data$week_label <- paste0("Week ", weekly_data$week_num, "\n", 
                                       format(weekly_data$start_date, "%d %b"))
      
      p <- plot_ly() %>%
        add_trace(
          data = weekly_data,
          x = ~week_label,
          y = ~total_distance / 1000,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Total Distance (km)',
          line = list(color = chelsea_blue, width = 3),
          marker = list(color = chelsea_blue, size = 10)
        ) %>%
        add_trace(
          data = weekly_data,
          x = ~week_label,
          y = ~total_high_speed / 1000,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'High-Speed Distance (km)',
          line = list(color = chelsea_gold, width = 2),
          marker = list(color = chelsea_gold, size = 8)
        ) %>%
        layout(
          title = "Weekly Load Progression",
          xaxis = list(title = ""),
          yaxis = list(title = "Distance (km)"),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified"
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  #PART 4
  
  # Acceleration/Deceleration Profile
  output$accel_decel_profile <- renderPlotly({
    player_data <- player_gps_data()
    
    if (nrow(player_data) > 0) {
      # Categorize as match day or training
      player_data$session_type <- ifelse(player_data$md_code == "MD", "Match", "Training")
      
      # Summarize by session type
      summary_data <- player_data %>%
        group_by(session_type) %>%
        summarize(
          avg_accel_2_5 = mean(accel_decel_over_2_5, na.rm = TRUE),
          avg_accel_3_5 = mean(accel_decel_over_3_5, na.rm = TRUE),
          avg_accel_4_5 = mean(accel_decel_over_4_5, na.rm = TRUE),
          count = n()
        )
      
      p <- plot_ly() %>%
        add_trace(
          data = summary_data,
          x = ~session_type,
          y = ~avg_accel_2_5,
          type = 'bar',
          name = 'Accel/Decel >2.5 m/s²',
          marker = list(color = chelsea_blue)
        ) %>%
        add_trace(
          data = summary_data,
          x = ~session_type,
          y = ~avg_accel_3_5,
          type = 'bar',
          name = 'Accel/Decel >3.5 m/s²',
          marker = list(color = chelsea_gold)
        ) %>%
        add_trace(
          data = summary_data,
          x = ~session_type,
          y = ~avg_accel_4_5,
          type = 'bar',
          name = 'Accel/Decel >4.5 m/s²',
          marker = list(color = chelsea_red)
        ) %>%
        layout(
          barmode = 'group',
          title = "Acceleration/Deceleration Profile",
          xaxis = list(title = ""),
          yaxis = list(title = "Count (n)"),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified"
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Heart Rate Zone Distribution
  output$hr_zones <- renderPlotly({
    player_data <- player_gps_data()
    
    if (nrow(player_data) > 0) {
      # Reshape HR zone data
      hr_data <- player_data %>%
        select(date, hr_zone_1_min, hr_zone_2_min, hr_zone_3_min, hr_zone_4_min, hr_zone_5_min) %>%
        summarize(
          zone_1 = sum(hr_zone_1_min, na.rm = TRUE),
          zone_2 = sum(hr_zone_2_min, na.rm = TRUE),
          zone_3 = sum(hr_zone_3_min, na.rm = TRUE),
          zone_4 = sum(hr_zone_4_min, na.rm = TRUE),
          zone_5 = sum(hr_zone_5_min, na.rm = TRUE)
        ) %>%
        pivot_longer(cols = starts_with("zone_"), 
                     names_to = "zone", 
                     values_to = "minutes")
      
      # Define zone labels and colors
      hr_data$zone_label <- factor(hr_data$zone, 
                                   levels = c("zone_1", "zone_2", "zone_3", "zone_4", "zone_5"),
                                   labels = c("Zone 1 (50-60%)", 
                                              "Zone 2 (60-70%)", 
                                              "Zone 3 (70-80%)", 
                                              "Zone 4 (80-90%)", 
                                              "Zone 5 (90-100%)"))
      
      zone_colors <- c("#3498db", "#2ecc71", "#f1c40f", "#e67e22", "#e74c3c")
      
      p <- plot_ly() %>%
        add_pie(
          data = hr_data,
          labels = ~zone_label,
          values = ~minutes,
          marker = list(colors = zone_colors),
          textinfo = 'label+percent',
          hoverinfo = 'label+value+percent',
          textposition = 'inside',
          insidetextfont = list(color = "#FFFFFF")
        ) %>%
        layout(
          title = "Heart Rate Zone Distribution",
          showlegend = FALSE
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Load Data Table
  output$load_data_table <- renderDT({
    player_data <- player_gps_data()
    
    if (nrow(player_data) > 0) {
      # Format data for display
      display_data <- player_data %>%
        select(date, md_code, opposition, distance, distance_over_21, 
               distance_over_24, day_duration, peak_speed) %>%
        mutate(
          date = format(date, "%d %b %Y"),
          opposition = ifelse(is.na(opposition), "", opposition),
          distance = round(distance),
          distance_over_21 = round(distance_over_21),
          distance_over_24 = round(distance_over_24)
        ) %>%
        arrange(desc(date))
      
      # Rename columns for display
      colnames(display_data) <- c("Date", "Session Type", "Opposition", 
                                  "Total Distance (m)", "High-Speed Distance (m)",
                                  "Very High-Speed Distance (m)", "Duration (min)",
                                  "Peak Speed (km/h)")
      
      datatable(
        display_data,
        options = list(
          pageLength = 10,
          dom = "ftip",
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    } else {
      datatable(
        data.frame(Message = "No data available for the selected period"),
        options = list(dom = "t"),
        rownames = FALSE
      )
    }
  })
  
  # Physical Development Tab Outputs
  
  # Physical Profile Radar Chart
  output$physical_radar <- renderPlotly({
    player_data <- player_physical_data()
    
    if (nrow(player_data) > 0) {
      # Get most recent data for each movement-quality combination
      latest_data <- player_data %>%
        group_by(movement, quality) %>%
        filter(date == max(date, na.rm = TRUE)) %>%
        ungroup()
      
      # Calculate average benchmark percentage by movement
      movement_summary <- latest_data %>%
        group_by(movement) %>%
        summarize(
          avg_benchmark = mean(benchmark_pct, na.rm = TRUE)
        )
      
      # Ensure we have all movements for a complete radar
      all_movements <- data.frame(
        movement = c("Agility", "Sprint", "Upper Body", "Jump"),
        avg_benchmark = c(0, 0, 0, 0)
      )
      
      # Merge to ensure all categories exist
      movement_summary <- movement_summary %>%
        full_join(all_movements, by = "movement") %>%
        mutate(avg_benchmark = coalesce(avg_benchmark.x, avg_benchmark.y)) %>%
        select(movement, avg_benchmark)
      
      # Get the player's name
      player_name <- players$name[players$id == input$player]
      
      # Create radar chart
      p <- plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
      ) %>%
        add_trace(
          r = movement_summary$avg_benchmark,
          theta = movement_summary$movement,
          name = player_name,
          fillcolor = paste0(chelsea_blue, "80"),  # with 50% opacity
          line = list(color = chelsea_blue)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 100)
            )
          ),
          showlegend = TRUE
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Movement Quality Progression
  output$movement_progression <- renderPlotly({
    player_data <- player_physical_data()
    
    if (nrow(player_data) > 0) {
      # Calculate average by movement and date
      movement_trend <- player_data %>%
        group_by(movement, date) %>%
        summarize(
          avg_benchmark = mean(benchmark_pct, na.rm = TRUE)
        ) %>%
        ungroup()
      
      p <- plot_ly() %>%
        add_trace(
          data = movement_trend %>% filter(movement == "Agility"),
          x = ~date,
          y = ~avg_benchmark,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Agility',
          line = list(color = chelsea_blue),
          marker = list(color = chelsea_blue)
        ) %>%
        add_trace(
          data = movement_trend %>% filter(movement == "Sprint"),
          x = ~date,
          y = ~avg_benchmark,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Sprint',
          line = list(color = chelsea_gold),
          marker = list(color = chelsea_gold)
        ) %>%
        add_trace(
          data = movement_trend %>% filter(movement == "Upper Body"),
          x = ~date,
          y = ~avg_benchmark,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Upper Body',
          line = list(color = chelsea_red),
          marker = list(color = chelsea_red)
        ) %>%
        add_trace(
          data = movement_trend %>% filter(movement == "Jump"),
          x = ~date,
          y = ~avg_benchmark,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Jump',
          line = list(color = "#2ecc71"),
          marker = list(color = "#2ecc71")
        ) %>%
        layout(
          title = "Movement Quality Progression",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Benchmark %", range = c(0, 100)),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified"
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Expression Type Analysis
  output$expression_analysis <- renderPlotly({
    # Crear un dataset básico de reserva
    default_data <- expand.grid(
      expression = c("Isometric", "Dynamic"),
      quality = c("Acceleration", "Deceleration", "Max velocity", "Pre-load", "Take off")
    )
    default_data$avg_benchmark <- runif(nrow(default_data), 30, 80)
    
    tryCatch({
      player_data <- player_physical_data()
      
      if (nrow(player_data) > 0) {
        # Get most recent data - safely
        latest_date <- max(player_data$date, na.rm = TRUE)
        latest_data <- player_data %>% filter(date == latest_date)
        
        # Solo si tenemos datos suficientes, procesamos
        if(nrow(latest_data) >= 5) {
          # Calculate average by expression type and quality
          expression_summary <- latest_data %>%
            group_by(expression, quality) %>%
            summarize(
              avg_benchmark = mean(benchmark_pct, na.rm = TRUE),
              .groups = 'drop'
            )
          
          if(nrow(expression_summary) >= 5) {
            date_title <- format(latest_date, "%d %b %Y")
            
            p <- plot_ly() %>%
              add_trace(
                data = expression_summary %>% filter(expression == "Isometric"),
                x = ~quality,
                y = ~avg_benchmark,
                type = 'bar',
                name = 'Isometric',
                marker = list(color = chelsea_blue)
              ) %>%
              add_trace(
                data = expression_summary %>% filter(expression == "Dynamic"),
                x = ~quality,
                y = ~avg_benchmark,
                type = 'bar',
                name = 'Dynamic',
                marker = list(color = chelsea_gold)
              ) %>%
              layout(
                title = paste0("Expression Type Analysis (", date_title, ")"),
                xaxis = list(title = "", categoryorder = "trace"),
                yaxis = list(title = "Benchmark %", range = c(0, 100)),
                barmode = 'group',
                legend = list(orientation = "h", y = -0.2),
                hovermode = "x unified"
              )
            
            return(p)
          }
        }
      }
      
      # Si llegamos aquí, usamos los datos por defecto
      date_title <- format(Sys.Date(), "%d %b %Y")
      
      p <- plot_ly() %>%
        add_trace(
          data = default_data %>% filter(expression == "Isometric"),
          x = ~quality,
          y = ~avg_benchmark,
          type = 'bar',
          name = 'Isometric',
          marker = list(color = chelsea_blue)
        ) %>%
        add_trace(
          data = default_data %>% filter(expression == "Dynamic"),
          x = ~quality,
          y = ~avg_benchmark,
          type = 'bar',
          name = 'Dynamic',
          marker = list(color = chelsea_gold)
        ) %>%
        layout(
          title = paste0("Expression Type Analysis (Sample Data)"),
          xaxis = list(title = ""),
          yaxis = list(title = "Benchmark %", range = c(0, 100)),
          barmode = 'group',
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified"
        )
      
      return(p)
      
    }, error = function(e) {
      # En caso de cualquier error, mostrar datos por defecto
      p <- plot_ly() %>%
        add_trace(
          data = default_data %>% filter(expression == "Isometric"),
          x = ~quality,
          y = ~avg_benchmark,
          type = 'bar',
          name = 'Isometric',
          marker = list(color = chelsea_blue)
        ) %>%
        add_trace(
          data = default_data %>% filter(expression == "Dynamic"),
          x = ~quality,
          y = ~avg_benchmark,
          type = 'bar',
          name = 'Dynamic',
          marker = list(color = chelsea_gold)
        ) %>%
        layout(
          title = "Expression Type Analysis (Sample Data)",
          xaxis = list(title = ""),
          yaxis = list(title = "Benchmark %", range = c(0, 100)),
          barmode = 'group',
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified"
        )
      
      return(p)
    })
  })
  
  # Physical Data Table
  output$physical_data_table <- renderDT({
    tryCatch({
      player_data <- player_physical_data()
      
      if (nrow(player_data) > 0) {
        # Construir tabla manualmente para evitar errores
        dates_char <- sapply(player_data$date, function(d) {
          tryCatch({
            format(as.Date(d), "%d %b %Y")
          }, error = function(e) {
            "Unknown date"
          })
        })
        
        benchmarks_char <- sapply(player_data$benchmark_pct, function(b) {
          tryCatch({
            paste0(round(as.numeric(b), 1), "%")
          }, error = function(e) {
            "N/A"
          })
        })
        
        # Crear dataframe simple con solo caracteres
        display_data <- data.frame(
          "Test Date" = dates_char,
          "Movement" = as.character(player_data$movement),
          "Quality" = as.character(player_data$quality),
          "Expression" = as.character(player_data$expression),
          "Benchmark %" = benchmarks_char,
          stringsAsFactors = FALSE
        )
        
        # Ordenar manualmente por fecha (descendente)
        sorted_data <- display_data[order(player_data$date, decreasing = TRUE),]
        
        datatable(
          sorted_data,
          options = list(
            pageLength = 10,
            dom = "ftip",
            scrollX = TRUE
          ),
          escape = FALSE,  # Permitir HTML en celdas si es necesario
          rownames = FALSE
        )
      } else {
        datatable(
          data.frame(Message = "No data available for the selected period"),
          options = list(dom = "t"),
          rownames = FALSE
        )
      }
    }, error = function(e) {
      # En caso de error, mostrar un mensaje
      datatable(
        data.frame(Message = "Error loading data. Please try again."),
        options = list(dom = "t"),
        rownames = FALSE
      )
    })
  })
  
  #PART 5
  
  # Recovery Tab Outputs
  
  # Overall Recovery Status Trend
  output$recovery_trend <- renderPlotly({
    # Get all the emboss_baseline_score values
    recovery_data_filtered <- recovery_data %>%
      filter(
        player_id == input$player,
        date >= input$date_range[1],
        date <= input$date_range[2],
        metric == "emboss_baseline_score"
      ) %>%
      arrange(date)
    
    if (nrow(recovery_data_filtered) > 0) {
      # Convert to percentage
      recovery_data_filtered$percent_value <- recovery_data_filtered$value * 100
      
      # Create color gradient based on values
      colors <- ifelse(recovery_data_filtered$percent_value >= 0, chelsea_blue, chelsea_red)
      
      p <- plot_ly() %>%
        add_trace(
          data = recovery_data_filtered,
          x = ~date,
          y = ~percent_value,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = chelsea_blue, width = 2),
          marker = list(color = colors, size = 8),
          name = 'EMBOSS Score'
        ) %>%
        add_trace(
          x = recovery_data_filtered$date,
          y = rep(0, nrow(recovery_data_filtered)),
          type = 'scatter',
          mode = 'lines',
          line = list(color = 'gray', dash = 'dot'),
          showlegend = FALSE
        ) %>%
        layout(
          title = "Recovery Status Trend",
          xaxis = list(title = "Date"),
          yaxis = list(
            title = "EMBOSS Score (%)",
            zeroline = TRUE,
            zerolinecolor = 'gray',
            zerolinewidth = 2
          ),
          hovermode = "x unified"
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Recovery Category Comparison
  output$recovery_categories <- renderPlotly({
    # Get the most recent recovery data for each category
    
    #PART 5.b
    
    recovery_data_composite <- recovery_data %>%
      filter(
        player_id == input$player,
        date >= input$date_range[1],
        date <= input$date_range[2],
        grepl("_composite$", metric)
      )
    
    if (nrow(recovery_data_composite) > 0) {
      # Get the latest date
      latest_date <- max(recovery_data_composite$date)
      latest_data <- recovery_data_composite %>% filter(date == latest_date)
      
      # Extract category names and convert values to percentages
      latest_data$category_name <- sub("_composite$", "", latest_data$metric)
      latest_data$percent_value <- latest_data$value * 100
      
      # Create color gradient based on values
      colors <- colorRampPalette(c(chelsea_red, "gray", chelsea_blue))(100)
      color_indices <- round((latest_data$percent_value + 100) / 2) + 1
      color_indices[color_indices < 1] <- 1
      color_indices[color_indices > 100] <- 100
      bar_colors <- colors[color_indices]
      
      p <- plot_ly() %>%
        add_trace(
          data = latest_data,
          x = ~reorder(category_name, percent_value),
          y = ~percent_value,
          type = 'bar',
          marker = list(color = bar_colors),
          name = 'Recovery Score'
        ) %>%
        add_trace(
          x = latest_data$category_name,
          y = rep(0, nrow(latest_data)),
          type = 'scatter',
          mode = 'lines',
          line = list(color = 'gray', dash = 'dot'),
          showlegend = FALSE
        ) %>%
        layout(
          title = paste0("Recovery Categories (", format(latest_date, "%d %b %Y"), ")"),
          xaxis = list(title = ""),
          yaxis = list(
            title = "Score (%)",
            zeroline = TRUE,
            zerolinecolor = 'gray',
            zerolinewidth = 2
          ),
          hovermode = "closest"
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Bio Recovery Gauge
  output$recovery_bio <- renderPlotly({
    create_recovery_gauge("Bio")
  })
  
  # MSK Recovery Gauge
  output$recovery_msk <- renderPlotly({
    create_recovery_gauge("Msk_joint_range")
  })
  
  # Subjective Recovery Gauge
  output$recovery_subjective <- renderPlotly({
    create_recovery_gauge("Subjective")
  })
  
  # Sleep Recovery Gauge
  output$recovery_sleep <- renderPlotly({
    create_recovery_gauge("Sleep")
  })
  
  # Helper function to create recovery gauges
  create_recovery_gauge <- function(category_name) {
    # Get the most recent recovery data for this category
    recovery_data_filtered <- recovery_data %>%
      filter(
        player_id == input$player,
        category == category_name,
        metric == paste0(category_name, "_composite")
      )
    
    if (nrow(recovery_data_filtered) > 0) {
      # Get the latest date
      latest_date <- max(recovery_data_filtered$date, na.rm = TRUE)
      latest_value <- recovery_data_filtered %>% 
        filter(date == latest_date) %>%
        pull(value)
      
      # Handle potential NA values
      if (is.na(latest_value) || length(latest_value) == 0) {
        latest_value <- 0
      }
      
      # Convert to percentage
      percent_value <- latest_value * 100
      
      # Choose color based on value
      gauge_color <- if(percent_value >= 20) {
        chelsea_blue
      } else if(percent_value <= -20) {
        chelsea_red
      } else {
        "#FFA500"  # orange for neutral/caution
      }
      
      p <- plot_ly() %>%
        add_trace(
          type = "indicator",
          mode = "gauge+number",
          value = percent_value,
          number = list(
            suffix = "%",
            font = list(color = gauge_color)
          ),
          gauge = list(
            axis = list(range = list(-100, 100)),
            bar = list(color = gauge_color),
            steps = list(
              list(range = c(-100, -20), color = "#ffcccc"),
              list(range = c(-20, 20), color = "#ffffcc"),
              list(range = c(20, 100), color = "#ccffcc")
            ),
            threshold = list(
              line = list(color = "black", width = 4),
              thickness = 0.75,
              value = 0
            )
          )
        ) %>%
        layout(
          margin = list(l = 20, r = 30, t = 50, b = 20)
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data",
          showarrow = FALSE,
          font = list(size = 14)
        )
    }
  }
  
  # Recovery Data Completeness
  output$recovery_completeness <- renderPlotly({
    # Get completeness data for each category
    recovery_completeness_data <- recovery_data %>%
      filter(
        player_id == input$player,
        date >= input$date_range[1],
        date <= input$date_range[2],
        grepl("_completeness$", metric)
      )
    
    if (nrow(recovery_completeness_data) > 0) {
      # Extract category names and calculate average completeness
      recovery_completeness_data$category_name <- sub("_completeness$", "", recovery_completeness_data$metric)
      
      # Calculate average completeness per category
      avg_completeness <- recovery_completeness_data %>%
        group_by(category_name) %>%
        summarize(avg_completeness = mean(value, na.rm = TRUE) * 100)
      
      p <- plot_ly() %>%
        add_trace(
          data = avg_completeness,
          x = ~category_name,
          y = ~avg_completeness,
          type = 'bar',
          marker = list(color = chelsea_blue),
          name = 'Completeness'
        ) %>%
        layout(
          title = "Recovery Data Completeness",
          xaxis = list(title = ""),
          yaxis = list(
            title = "Completeness (%)",
            range = c(0, 100)
          ),
          hovermode = "closest"
        )
      
      return(p)
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected period",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)