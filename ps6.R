library(shiny)
library(ggplot2)
library(dplyr)

data <- read.delim("UAH-lower-troposphere-long.csv")

ui <- fluidPage(
  titlePanel("Global Temperature Data"),
  
  tabsetPanel(
    tabPanel("Overview",
             h3("About this app"),
             p("This app displays global temperature data from UAH."),
             p("Temperature is measured as deviation from 1981-2010 baseline."),
             p("The dataset contains 14310 observations and 5 variables."),
             h3("Sample data"),
             tableOutput("random_sample")
    ),
    

    tabPanel("Plots",
             h3("Global temperature trends by region"),
             p("Select the regions you are interested in, and choose whether to display trend lines."),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("regions", "Select regions:", choices = unique(data$region)),
                 br(),
                 checkboxInput("show_trend", "Display trend lines", value = FALSE)
               ),
               mainPanel(
                 plotOutput("plot"),
                 h4("Summary"),
                 textOutput("summary")
               )
             )
    ),
    
    tabPanel("Tables",
             h3("Average global temperature by time period"),
             p("Select the time period over which to calculate the averages:"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("time_period", "Average over:", choices = c("month", "year", "decade"), selected = "month")
               ),
               mainPanel(
                 tableOutput("table"),
                 h4("Temperature range"),
                 textOutput("temperature_range")
               )
             )
           )
       )
    )


server <- function(input, output) {
  
  output$random_sample <- renderTable({
    sample_n(data, 5)
  })
  
  output$plot <- renderPlot({
    ggplot(data %>% filter(region %in% input$regions), aes(x = year, y = temp, color = region)) +
      geom_point(alpha = 0.5, size = 2) +
      scale_color_brewer(palette = "Set1") +
      labs(x = "Year", y = "Temperature") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      if (input$show_trend) {
        geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, size = 1.2)
      }
  })
  
  output$summary <- renderText({
    data_subset <- data %>% filter(region %in% input$regions)
    paste("Time period: All. Total non-missing observations:", nrow(data_subset))
  })
  
  output$table <- renderTable({
    if (input$time_period == "month") {
      data %>% 
        group_by(year, month) %>% 
        summarise(average_temp = mean(temp))
    } else if (input$time_period == "year") {
      data %>% 
        group_by(year) %>% 
        summarise(average_temp = mean(temp))
    } else {
      data %>% 
        mutate(decade = floor(year/10) * 10) %>% 
        group_by(decade) %>% 
        summarise(average_temp = mean(temp))
    }
  })
  
  output$temperature_range <- renderText({
    if (input$time_period == "month") {
      data_subset <- data %>% 
        group_by(year, month) %>% 
        summarise(average_temp = mean(temp))
    } else if (input$time_period == "year") {
      data_subset <- data %>% 
        group_by(year) %>% 
        summarise(average_temp = mean(temp))
    } else {
      data_subset <- data %>% 
        mutate(decade = floor(year/10) * 10) %>% 
        group_by(decade) %>% 
        summarise(average_temp = mean(temp))
    }
    paste("Temperature range: ", round(min(data_subset$average_temp), 2), "-", round(max(data_subset$average_temp), 2), "deg C")
  })
  
}

shinyApp(ui = ui, server = server)