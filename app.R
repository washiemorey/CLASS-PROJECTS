##...........Created by Washie Morey.........................#
#...........On 3 May 2025.............................#
#...........Premiums Linear Regression Model..................#

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Insurance Premium Estimator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("experience", 
                   "Years of Driving Experience:", 
                   value = 0, min = 0),
      
      numericInput("violations", 
                   "Number of Traffic Violations:", 
                   value = 0, min = 0),
      
      selectInput("gender", 
                  "Gender:", 
                  choices = list("Male" = 0, "Female" = 1)),
      
      actionButton("submit", "Calculate Premium")
    ),
    
    mainPanel(
      h3("Estimated Insurance Premium:"),
      verbatimTextOutput("result")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Regression coefficients from your model
  intercept <- 58.795
  coef_experience <- -1.534
  coef_violations <- 7.674
  coef_gender <- -2.954
  
  result_premium <- eventReactive(input$submit, {
    # Calculate premium
    premium <- intercept +
      coef_experience * input$experience +
      coef_violations * input$violations +
      coef_gender * as.numeric(input$gender)
    round(premium, 2)
  })
  
  output$result <- renderText({
    paste("Ksh", result_premium())
  })
}

# Run the App
shinyApp(ui = ui, server = server)
