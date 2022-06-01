shiny_server <- function(input, output, session) {
  # produce the landscape plot
  output$l <- shiny::renderPlot({
    original_network <- make_2d_Isingland(input$coef_t * MDDThresholds,
      input$coef_c * MDDConnectivity,
      beta = input$beta, transform = TRUE
    )
    plot(original_network)
  })

  # produce the simulation animation
  make_s <- shiny::eventReactive(input$start_sim, {
    original_network <- make_2d_Isingland(input$coef_t * MDDThresholds,
      input$coef_c * MDDConnectivity,
      beta = input$beta,
      transform = TRUE
    )
    s <- simulate_Isingland(
      l = original_network, mode = input$mode, beta2 = input$beta2,
      length = input$length, initial = input$initial
    )
    outfile <- tempfile(fileext = ".gif")
    gganimate::anim_save(outfile, gganimate::animate(plot(s), fps = input$fps))

    list(
      src = outfile,
      contentType = "image/gif"
    )
  })

  output$s <- shiny::renderImage(make_s(), deleteFile = TRUE)

  # produce UIs
  output$beta2 <- shiny::renderUI({
    shiny::sliderInput("beta2",
      label = "Beta2",
      min = 0.1, max = 3.0, step = 0.1, value = input$beta
    )
  })
}

shiny_ui <- function() {
  shiny::fluidPage(
    theme = shinythemes::shinytheme("simplex"),
    shiny::navbarPage(
      "Landscape for MDD Ising network",
      shiny::tabPanel(
        "Landscape & Simulation",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::h2("Network parameters"),
            shiny::sliderInput("coef_t",
              label = "Coefficient for thresholds",
              min = 0.1, max = 2.0, step = 0.1, value = 1
            ),
            shiny::sliderInput("coef_c",
              label = "Coefficient for connectivity",
              min = 0.1, max = 2.0, step = 0.1, value = 1
            ),
            shiny::sliderInput("beta",
              label = "Beta",
              min = 0.1, max = 3.0, step = 0.1, value = 1.5
            ),
            shiny::h2("Simulation settings"),
            shiny::selectInput("mode",
              label = "Simulation mode", choices = c("single", "distribution"),
              selected = "single"
            ),
            shiny::sliderInput("initial",
              label = "Initial value",
              min = 0, max = 9, step = 1, value = 0
            ),
            shiny::uiOutput("beta2"),
            shiny::numericInput("length",
              label = "Length",
              value = 100,
              min = 1,
              max = 1000, step = 1
            ),
            shiny::numericInput("fps",
              label = "fps",
              value = 10,
              min = 1,
              max = 100, step = 1
            ),
            shiny::actionButton("start_sim", "Start simulation")
          ),
          shiny::mainPanel(
            shiny::plotOutput("l") %>% shinycssloaders::withSpinner(),
            shiny::plotOutput("s") %>% shinycssloaders::withSpinner()
          )
        )
      ),
      shiny::tabPanel(
        "About",
        shiny::p("This Shiny app is a part of the", shiny::strong("Isinglandr"), "package."),
        shiny::p(
          "See the", shiny::a("Github repo", href = "https://github.com/Sciurus365/Isinglandr"),
          "for more information."
        )
      )
    )
  )
}


#' A shiny app that shows the landscape for
#' the Ising network of major depressive disorder
#'
#' @export
#'
shiny_Isingland_MDD <- function() {
  app <- shiny::shinyApp(shiny_ui, shiny_server)
  shiny::runApp(app)
}
