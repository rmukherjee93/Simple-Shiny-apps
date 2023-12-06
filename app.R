install.packages(c("shiny","plotly"))
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  titlePanel("Kansas Counties Analysis"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_counties", "Select Counties", 
                         choices = kansas_county_area$County),
      checkboxInput("sort_by_area", "Sort Plots by Area", value = FALSE)
      # Add any other input controls needed for the correlation analysis
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualizations", 
                 plotlyOutput("areaPlot"),
                 plotlyOutput("crimeRatePlot"),
                 plotlyOutput("popplot")),
        tabPanel("Correlation Analysis",
                 verbatimTextOutput("correlationResult"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to filter the data based on selected counties
  filtered_areas <- reactive({
    kansas_county_area %>% filter(County %in% input$selected_counties)
  })
  
  filtered_crime_rates <- reactive({
    df_crime %>% filter(County %in% input$selected_counties)
  })
  
  filtered_pop <- reactive({
    df_pop %>% filter(County %in% input$selected_counties)
  })
  
  # Render the area plot
  output$areaPlot <- renderPlotly({
    ggplot(filtered_areas(), aes(x = County, y = Area)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Area of Selected Counties", x = "County", y = "Area")
  })
  
  # Render the crime rate time series plot
  output$crimeRatePlot <- renderPlotly({
    ggplot(filtered_crime_rates(), aes(x = Year, y = Crime_Rate, color = County, group = County)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Crime Rate Time Series for Selected Counties", x = "Year", y = "Crime Rate")
  })
  
  output$popplot <- renderPlotly({
    ggplot(filtered_pop(), aes(x = Year, y = Population, color = County)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Population Time Series for Selected Counties", x = "Year", y = "Population")
  })
  
  # Output for correlation analysis
  output$correlationResult <- renderPrint({
    # Filter the data based on selected counties
    selected_crime_data <- df_crime %>% filter(County %in% input$selected_counties)
    selected_pop_data <- df_pop %>% filter(County %in% input$selected_counties)
    
    # Merge the datasets
    merged_data <- merge(selected_crime_data, selected_pop_data, by = c("County", "Year"))
    
    # Perform correlation analysis
    if(nrow(merged_data) > 0) {
      correlation_analysis <- cor.test(merged_data$Crime_Rate, merged_data$Population)
      return(correlation_analysis)
    } else {
      return("Not enough data for correlation analysis.")
    }
  })
  
  
}

shinyApp(ui = ui, server = server)
