library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# Define the data file path
data_file <- "donut_survey_data.csv"

# Function to load existing data
load_data <- function() {
  if (file.exists(data_file)) {
    tryCatch({
      data <- read.csv(data_file, stringsAsFactors = FALSE)
      data$timestamp <- as.POSIXct(data$timestamp)
      return(data)
    }, error = function(e) {
      # Return empty data frame if file is corrupted
      return(data.frame(
        type = character(0),
        rating = numeric(0),
        timestamp = as.POSIXct(character(0)),
        stringsAsFactors = FALSE
      ))
    })
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      type = character(0),
      rating = numeric(0),
      timestamp = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }
}

# Function to save data safely
save_data <- function(data) {
  tryCatch({
    write.csv(data, data_file, row.names = FALSE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Define UI
ui <- fluidPage(
  titlePanel("Donut Survey"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Rate Your Donut Experience"),
      
      # Donut type selection
      selectInput("donut_type", 
                  "Which  donut did you have?",
                  choices = c("Choose a type..." = "",
                              "Mikiko Mochi Donuts",
                              "Coco Donuts",
                              "Voodoo doughnut",
                              "Bluestar Donuts",
                              "Doe Donuts"),
                  selected = ""),
      
      # Rating input
      radioButtons("rating",
                   "Rate it on a 5-star scale:",
                   choices = list("⭐" = 1,
                                  "⭐⭐" = 2,
                                  "⭐⭐⭐" = 3,
                                  "⭐⭐⭐⭐" = 4,
                                  "⭐⭐⭐⭐⭐" = 5),
                   selected = character(0)),
      
      # Submit button
      actionButton("submit", "Submit Rating", 
                   class = "btn-primary"),
      
      br(), br(),
      p("Thank you for participating in our donut survey!"),
      
      # Refresh button for live updates
      br(),
      actionButton("refresh", "Refresh Results", 
                   class = "btn-secondary"),
      
      # Debug info
      br(), br(),
      verbatimTextOutput("debug_info")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Results Summary",
                 h3("Survey Results"),
                 br(),
                 fluidRow(
                   column(6, 
                          h4("Average Rating by type"),
                          plotOutput("avg_rating_plot")),
                   column(6,
                          h4("Number of Responses by type"),
                          plotOutput("response_count_plot"))
                 ),
                 br(),
                 h4("Summary Statistics"),
                 tableOutput("summary_table"),
                 br(),
                 textOutput("total_responses")
        ),
        
        tabPanel("Raw Data",
                 h3("All Survey Responses"),
                 br(),
                 downloadButton("download_data", "Download Data as CSV", 
                                class = "btn-info"),
                 br(), br(),
                 DT::dataTableOutput("raw_data_table")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Initialize reactive values to store survey data
  survey_data <- reactiveValues(
    responses = load_data(),
    last_update = Sys.time()
  )
  
  # Handle form submission
  observeEvent(input$submit, {
    
    # Validate inputs
    if(input$donut_type == "" || is.null(input$rating)) {
      showNotification("Please select both a type and rating before submitting.",
                       type = "warning")
      return()
    }
    
    # Create new response
    new_response <- data.frame(
      type = input$donut_type,
      rating = as.numeric(input$rating),
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    # Add to current data
    updated_data <- rbind(survey_data$responses, new_response)
    
    # Try to save to file
    save_success <- save_data(updated_data)
    
    if (save_success) {
      # Update reactive values
      survey_data$responses <- updated_data
      survey_data$last_update <- Sys.time()
      
      # Reset form
      updateSelectInput(session, "donut_type", selected = "")
      updateRadioButtons(session, "rating", selected = character(0))
      
      # Show success message
      showNotification("Thank you! Your rating has been submitted.", 
                       type = "message")
    } else {
      # If file save failed, still keep in memory for this session
      survey_data$responses <- updated_data
      survey_data$last_update <- Sys.time()
      
      # Reset form
      updateSelectInput(session, "donut_type", selected = "")
      updateRadioButtons(session, "rating", selected = character(0))
      
      # Show warning about file save
      showNotification("Rating submitted (saved in memory only - file save failed)", 
                       type = "warning")
    }
  })
  
  # Handle manual refresh
  observeEvent(input$refresh, {
    # Reload data from file
    file_data <- load_data()
    survey_data$responses <- file_data
    survey_data$last_update <- Sys.time()
    showNotification("Results refreshed from file!", type = "message")
  })
  
  # Debug information
  output$debug_info <- renderText({
    paste0(
      "Responses in memory: ", nrow(survey_data$responses), "\n",
      "File exists: ", file.exists(data_file), "\n",
      "Working directory: ", getwd(), "\n",
      "Last update: ", format(survey_data$last_update, "%H:%M:%S")
    )
  })
  
  # Create average rating plot
  output$avg_rating_plot <- renderPlot({
    data <- survey_data$responses
    
    if(nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 1, y = 1, label = "No data yet", size = 6) +
               theme_void())
    }
    
    avg_ratings <- data %>%
      group_by(type) %>%
      summarise(avg_rating = mean(rating), .groups = 'drop')
    
    ggplot(avg_ratings, aes(x = reorder(type, avg_rating), y = avg_rating)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      geom_text(aes(label = round(avg_rating, 1)), 
                hjust = -0.1, size = 4) +
      #coord_flip() +
      ylim(0, 5.5) +
      labs(x = "type", y = "Average Rating", 
           title = "Average Rating by Donut type") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # Create response count plot
  output$response_count_plot <- renderPlot({
    data <- survey_data$responses
    
    if(nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 1, y = 1, label = "No data yet", size = 6) +
               theme_void())
    }
    
    count_data <- data %>%
      count(type)
    
    ggplot(count_data, aes(x = reorder(type, n), y = n)) +
      geom_col(fill = "darkgreen", alpha = 0.7) +
      geom_text(aes(label = n), hjust = -0.1, size = 4) +
      #coord_flip() +
      labs(x = "type", y = "Number of Responses", 
           title = "Response Count by type") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # Create summary table
  output$summary_table <- renderTable({
    data <- survey_data$responses
    
    if(nrow(data) == 0) {
      return(data.frame(Message = "No survey responses yet"))
    }
    
    data %>%
      group_by(type = type) %>%
      summarise(
        `Responses` = n(),
        `Avg Rating` = round(mean(rating), 2),
        `Std Dev` = round(sd(rating), 2),
        `Min` = min(rating),
        `Max` = max(rating),
        .groups = 'drop'
      ) %>%
      arrange(desc(`Avg Rating`))
  })
  
  # Display total responses
  output$total_responses <- renderText({
    data <- survey_data$responses
    paste("Total Survey Responses:", nrow(data))
  })
  
  # Display raw data table
  output$raw_data_table <- DT::renderDataTable({
    data <- survey_data$responses
    
    if(nrow(data) == 0) {
      return(data.frame(Message = "No survey responses yet"))
    }
    
    data %>%
      arrange(desc(timestamp)) %>%
      mutate(
        type = type,
        Rating = rating,
        `Submitted At` = format(timestamp, "%Y-%m-%d %H:%M:%S")
      ) %>%
      select(type, Rating, `Submitted At`)
    
  }, options = list(
    pageLength = 15,
    order = list(list(2, 'desc'))
  ))
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("donut_survey_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(survey_data$responses, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
