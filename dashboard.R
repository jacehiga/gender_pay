# Load necessary libraries
library(shiny)
library(tidyverse)
library(plotly)
library(colorspace)

# Load the pay dataset
pay <- read_csv("https://raw.githubusercontent.com/jacehiga/DataViz/refs/heads/main/Glassdoor%20Gender%20Pay%20Gap.csv")

# Define the UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Gender Pay Analysis"),
  
  # Sidebar with control for selecting an age range and job title
  sidebarLayout(
    sidebarPanel(
      sliderInput("ageRange",
                  h3("Select Age Range:"),
                  min = 20,
                  max = 65,
                  value = c(20, 65),
                  step = 1,
                  sep = ""),
      
      # Dropdown to select by JobTitle   
      selectInput("Education",
                  h4("Select Education Level:"),
                  choices = unique(na.omit(pay$Education)),
                  selected = "High School")
    ),
    
    # Display both plots in the main panel
    mainPanel(
      plotOutput("payHeatmap"),
      plotlyOutput("payScatterPlot")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Generate the pay heatmap plot
  output$payHeatmap <- renderPlot({
    # Filter data based on the selected age range
    pay_filtered <- pay %>%
      filter(Age >= input$ageRange[1] & Age <= input$ageRange[2])
    
    ggplot(pay_filtered, aes(x = Age, y = Gender, fill = (BasePay + Bonus))) +
      geom_tile(width = 1, height = 0.75) +
      scale_fill_continuous_sequential("Emrld", name = "Salary",
                                       labels = scales::label_dollar()) +
      labs(title = "Age-Based Pay Distribution by Gender",
           subtitle = "Salaries of Male and Female Tech Employees Over Various Ages",
           caption = "Source: Kaggle (Glassdoor - Analyze Gender Pay Gap)",
           x = "Age",
           y = "Gender") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "sans"), 
        plot.subtitle = element_text(size = 10, hjust = 0.5, family = "sans"),
        axis.title.x = element_text(size = 9, face = "bold", family = "sans"), 
        axis.title.y = element_text(size = 9, face = "bold", family = "sans"), 
        axis.text.y = element_text(size = 10),
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank())
  })
  
  # Generate the pay scatter plot with Plotly
  output$payScatterPlot <- renderPlotly({
    # Filter data based on selected age range and job title
    pay_filtered <- pay %>%
      filter(Age >= input$ageRange[1] & Age <= input$ageRange[2],
             Education == input$Education)
    
    mod<-lm(BasePay~Age+Gender, data= pay_filtered)
    
    # Create the ggplot object with custom tooltips
    p <- ggplot(pay_filtered, aes(x = Age, y = BasePay + Bonus, color = Gender,
                                  text = paste("Age:", Age,
                                               "<br>Base Pay + Bonus:", BasePay + Bonus,
                                               "<br>Gender:", Gender,
                                               "<br>Education:", Education))) +
      geom_point(size = 1) +
      geom_smooth(aes(group = Gender), se = FALSE, method = "lm") +
      labs(x = "Age",
           y = "Base Pay + Bonus ($)",
           color = "Gender") +
      scale_color_manual(values = c("purple", "darkblue")) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 9, face = "bold", family = "sans"), 
        axis.title.y = element_text(size = 9, face = "bold", family = "sans"), 
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(hjust = 0),
        legend.title = element_text(size = 10, face = "bold")
      ) +
      ylim(c(40000, 180000))
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p, tooltip = "text") %>% 
      layout(
        title = list(
          text = paste0(
            "<span style='font-family: Sans; font-size:16px; font-weight:bold;'>Pay Distribution by Age for Education Level: ", input$Education, "</span>",
            "<br><span style='font-family: Sans; font-size:12px'>On average Male employees make $", 
            round(mod$coefficients[3], 2), 
            " more than Female employees</span>")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
