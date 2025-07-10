library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Basic Shiny Application"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Controls"),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      selectInput("dataset",
                  "Choose a dataset:",
                  choices = c("mtcars", "iris", "faithful")),
      
      actionButton("refresh", "Refresh Data", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", 
                 h4("Histogram of Selected Data"),
                 plotOutput("distPlot")),
        
        tabPanel("Data Table",
                 h4("Dataset Preview"),
                 DT::dataTableOutput("dataTable")),
        
        tabPanel("Summary",
                 h4("Data Summary"),
                 verbatimTextOutput("summary"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to get selected dataset
  selectedData <- reactive({
    switch(input$dataset,
           "mtcars" = mtcars,
           "iris" = iris,
           "faithful" = faithful)
  })
  
  # Render histogram
  output$distPlot <- renderPlot({
    data <- selectedData()
    
    # Use first numeric column for histogram
    numeric_cols <- sapply(data, is.numeric)
    if (any(numeric_cols)) {
      x <- data[, which(numeric_cols)[1]]
      hist(x, 
           breaks = input$bins,
           col = 'steelblue',
           border = 'white',
           main = paste("Histogram of", names(data)[which(numeric_cols)[1]]),
           xlab = names(data)[which(numeric_cols)[1]])
    }
  })
  
  # Render data table
  output$dataTable <- DT::renderDataTable({
    selectedData()
  }, options = list(pageLength = 10))
  
  # Render summary
  output$summary <- renderPrint({
    summary(selectedData())
  })
}

# Run the application
shinyApp(ui = ui, server = server)