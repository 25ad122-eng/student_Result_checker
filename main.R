library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Student Grade & Result Processor"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File (Name, Mark)",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      helpText("Ensure your CSV has columns named 'Name' and 'Mark'."),
      hr(),
      
      selectInput("filter_grade", "Filter by Grade:",
                  choices = c("All", "A", "B", "C", "D", "F")),
      
      downloadButton("download_data", "Download Processed Results")
    ),
    
    mainPanel(
      h4("Processed Student Results"),
      tableOutput("result_table")
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive function to process input CSV data
  processed_data <- reactive({
    req(input$file)
    
    df <- read.csv(input$file$datapath)
    
    # Validate required columns
    validate(
      need(all(c("Name", "Mark") %in% colnames(df)), "CSV must contain 'Name' and 'Mark' columns.")
    )
    
    # Calculate Result and Grade
    df$Result <- ifelse(df$Mark >= 50, "PASS", "FAIL")
    df$Grade  <- ifelse(df$Mark >= 90, "A",
                 ifelse(df$Mark >= 75, "B",
                 ifelse(df$Mark >= 60, "C",
                 ifelse(df$Mark >= 50, "D", "F"))))
    
    return(df)
  })
  
  # Reactive output filtered by grade
  filtered_data <- reactive({
    df <- processed_data()
    if (input$filter_grade != "All") {
      df <- df[df$Grade == input$filter_grade, ]
    }
    return(df)
  })
  
  # Render Table Output
  output$result_table <- renderTable({
    filtered_data()
  })
  
  # Download Handler for processed CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste("processed_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny application
shinyApp(ui = ui, server = server)