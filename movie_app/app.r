library(shiny)
library(miniUI)

source("utils.r")

ui <- miniPage(
  gadgetTitleBar("Movie analysis app"),
  miniTabstripPanel(
    miniTabPanel("Parameters", icon=icon("sliders"),
      miniContentPanel(
        textInput("actor", "Actor", "Diego Luna"),
        numericInput("k", "Num. clusters", 3)
      )
    ),
    miniTabPanel("Plot", icon=icon("area-chart"),
      miniContentPanel(
        plotOutput("movie_plot", height="100%")
      )
    ),
    miniTabPanel("Data", icon=icon("table"),
      miniContentPanel(
        DT::dataTableOutput("table", height="100%")
      )
    )
  )
)

server <- function(input, output, session) {
  scrapeTable <- reactive({
    scrape_table(input$actor)
  })
  
  clusterMovies <- reactive({
    scrapeTable() %>%
      cluster_movies(input$k)
  })
  
  displayTable <- reactive({
    clusterMovies() %>%
      select(title, rating, domestic_gross, cluster)
  })
  
  output$movie_plot <- renderPlot({
    clusterMovies() %>% plot_movies(input$actor)
  })
  
  output$table <- DT::renderDataTable({
    displayTable()
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}