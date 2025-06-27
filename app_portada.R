library(shiny)
library(shinyjs)
library(callr)

# Define rutas y puertos de cada app
apps <- list(
  offensive = list(
    file = "shiny_offensive_transition.R",
    port = 8081
  ),
  deffensive = list(
    file = "shiny_deffensive_transition.R",
    port = 8082
  ),
  corner = list(
    file = "shiny_corner.R",
    port = 8083
  ),
  attacking_phase = list(
    file = "shiny_offensive_phase.R",
    port = 8084
  ),
  defensive_phase = list(
    file = "Shiny Heatmaps Won Duels PL.R",
    port = 8085
  )
)

# Guardar procesos lanzados
launched <- new.env()

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML('
    .main-title { text-align: center; font-size: 2.5em; margin-top: 40px; margin-bottom: 40px; }
    .menu-btn {
      width: 100%;
      height: 120px;
      font-size: 2em;
      margin-bottom: 30px;
      border-radius: 15px;
      font-weight: bold;
    }
    .center-row { display: flex; justify-content: center; align-items: center; }
    .menu-col { max-width: 400px; margin: 0 20px; }
  '))),
  div(class = 'main-title', 'Main Analysis Menu'),
  fluidRow(
    column(4, class = 'menu-col',
           actionButton('go_offensive', 'Offensive Transition', class = 'btn-primary menu-btn')
    ),
    column(4, class = 'menu-col',
           actionButton('go_deffensive', 'Defensive Transition', class = 'btn-danger menu-btn')
    ),
    column(4, class = 'menu-col',
           actionButton('go_corner', 'Set Pieces', class = 'btn-warning menu-btn')
    )
  ),
  fluidRow(
    column(4, class = 'menu-col',
           actionButton('go_attacking_phase', 'Attacking Phase', class = 'btn-success menu-btn')
    ),
    column(4, class = 'menu-col',
           actionButton('go_defensive_phase', 'Defensive Phase', class = 'btn-info menu-btn')
    ),
    column(4, class = 'menu-col'
    )
  ),
  br(),
  div(style = 'text-align:center; color:#888; margin-top:40px;',
      'Select the analysis you want to visualize. Each one will open in a new tab.')
)

server <- function(input, output, session) {
  launch_app <- function(app_key) {
    app <- apps[[app_key]]
    if (is.null(launched[[app_key]]) || !launched[[app_key]]$is_alive()) {
      # Lanza la app en un proceso de R en segundo plano
      launched[[app_key]] <<- callr::r_bg(function(file, port) {
        shiny::runApp(file, port = port, launch.browser = FALSE)
      }, args = list(file = app$file, port = app$port))
      Sys.sleep(2) # Espera un poco para que la app arranque
    }
    runjs(sprintf("window.open('http://127.0.0.1:%d', '_blank')", app$port))
  }
  observeEvent(input$go_offensive, { launch_app("offensive") })
  observeEvent(input$go_deffensive, { launch_app("deffensive") })
  observeEvent(input$go_corner, { launch_app("corner") })
  observeEvent(input$go_attacking_phase, { launch_app("attacking_phase") })
  observeEvent(input$go_defensive_phase, { launch_app("defensive_phase") })
}

shinyApp(ui, server)