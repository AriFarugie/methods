library(shiny)

ui <- fluidPage(
  titlePanel("Mini Shinylive Demo"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Stichprobengröße n", min = 10, max = 500, value = 100),
      sliderInput("rho", "Ziel-Korrelation r", min = -1, max = 1, value = 0.5, step = 0.1),
      actionButton("resample", "Neue Stichprobe ziehen")
    ),
    mainPanel(
      plotOutput("scatter", height = "350px"),
      verbatimTextOutput("corVal")
    )
  )
)

server <- function(input, output, session) {
  
  # Daten generieren (nur Base-R)
  generate_data <- function(n, r) {
    x <- rnorm(n)
    z <- rnorm(n)
    y <- r * x + sqrt(1 - r^2) * z
    data.frame(x = x, y = y)
  }
  
  data <- eventReactive(input$resample, {
    generate_data(input$n, input$rho)
  }, ignoreNULL = FALSE)
  
  output$scatter <- renderPlot({
    d <- data()
    plot(
      d$x, d$y,
      xlab = "X", ylab = "Y",
      pch = 16, col = "#004C99",
      main = paste0("Ziel-r = ", round(input$rho, 2))
    )
    abline(lm(y ~ x, data = d), lwd = 2, col = "black")
  })
  
  output$corVal <- renderPrint({
    d <- data()
    r_emp <- cor(d$x, d$y)
    cat("Empirische Korrelation:", round(r_emp, 3), "\n")
  })
}

shinyApp(ui, server)
