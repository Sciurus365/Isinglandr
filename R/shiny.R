shiny_server <- function(input, output, session) {
  output$l <- shiny::renderPlot({
    original_network <- make_2d_Isingland(input$coef_t * MDDThresholds,
      input$coef_c * MDDConnectivity,
      beta = input$beta, transform = TRUE
    )
    plot(original_network)
  })
}

shiny_ui <- function() {
  shiny::fluidPage(
    theme = shinythemes::shinytheme("simplex"),

    shiny::navbarPage(
      "Landscape for MDD Ising network",
      shiny::tabPanel(
        "Landscape",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            sliderInput("coef_t",
              label = "Coefficient for threshouds",
              min = 0.1, max = 2.0, step = 0.1, value = 1
            ),
            sliderInput("coef_c",
              label = "Coefficient for connectivity",
              min = 0.1, max = 2.0, step = 0.1, value = 1
            ),
            sliderInput("beta",
              label = "beta",
              min = 0.1, max = 3.0, step = 0.1, value = 1.5
            )
          ),
          shiny::mainPanel(
            shiny::plotOutput("l")
          )
        )
      ),
      shiny::tabPanel("About",
      									p("This Shiny app is a part of the", strong("Isinglandr"), "package."),
      								  p("See the", a("Github repo", href = "https://github.com/Sciurus365/Isinglandr"),
      								  	"for more information.")
      									)
    )
  )
}


#' @export
shiny_Isingland_MDD <- function() {
  app <- shiny::shinyApp(shiny_ui, shiny_server)
  shiny::runApp(app)
}
