# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("IMDB rating"          = "imdb_rating", 
                              "IMDB number of votes" = "imdb_num_votes", 
                              "Critics score"        = "critics_score", 
                              "Audience score"       = "audience_score", 
                              "Runtime"              = "runtime"), 
                  selected = "audience_score"),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c(
                    "IMDB rating"          = "imdb_rating", 
                    "IMDB number of votes" = "imdb_num_votes", 
                    "Critics score"        = "critics_score", 
                    "Audience score"       = "audience_score", 
                    "Runtime"              = "runtime"), 
                  selected = "critics_score"),
      
      # Select variable for color
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c(
                    "Title type" = "title_type", 
                    "Genre" = "genre", 
                    "MPAA rating" = "mpaa_rating", 
                    "Critics rating" = "critics_rating", 
                    "Audience rating" = "audience_rating"),
                  selected = "mpaa_rating"),
      
      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table", 
                    value = TRUE),
      
      HTML('<div class="form-group shiny-input-container">
  <div class="checkbox">
    <label>
      <input id="show_data" type="checkbox" checked="checked"/>
      <span>Show data table</span>
    </label>
  </div>
</div>'),
      
      # Subset for title types
      checkboxGroupInput(inputId = "selected_title_type", 
                         label = "Select title type:", 
                         choices = levels(movies$title_type),
                         selected = levels(movies$title_type))
    ),
    
    # Output
    mainPanel(
      # Show scatterplot
      plotOutput(outputId = "scatterplot"),
      
      # Show data table
      dataTableOutput(outputId = "moviestable")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Subset data for summary
  summary_table <- reactive({
    req(input$selected_title_type)
    movies %>%
      group_by(title_type, genre) %>% 
      summarise(avg_rating = round(mean(imdb_rating), 2), 
                SD = round(sd(imdb_rating), 2), n = n()) %>% 
      filter(title_type %in% input$selected_title_type)
  })
  
  # Plot
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y,
                                     color = input$z)) +
      geom_point()
  })
  
  # Print data table if checked 
  output$moviestable <- renderDataTable({
    if(input$show_data){
      DT::datatable(data = summary_table(),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    } 
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)