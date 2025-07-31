# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(titlePanel("mtcars dataset"),
  
sidebarLayout(
sidebarPanel(
selectInput("var", "Variable Selection:", choices = names(mtcars),
selected = "mpg"), selectInput("group", "Variable Selection",
choices = names(mtcars), selected = "cyl")),
    
mainPanel(
tabsetPanel(
tabPanel("Data", tableOutput("table")),
tabPanel("Summary", verbatimTextOutput("summary")),
tabPanel("BoxPlot", plotOutput("boxplot")),
tabPanel("Bar", plotOutput("barplot")),
tabPanel("Histogram", plotOutput("hist"))))))

# Server
server <- function(input, output) {
  
# Pull two columns
selected_data <- reactive({mtcars |> select(all_of(c(input$var, input$group)))})
  
# Show data
output$table <- renderTable({selected_data()})
  
output$summary <- renderPrint({
  var_data <- mtcars[[input$var]]
  if (is.numeric(var_data)) {
cat("Summary for continuous variable:\n\n")
print(summary(var_data))
  } else {
    cat("Summary for discrete variable:\n\n")
print(table(var_data))}})
  
# Boxplot
output$boxplot <- renderPlot({
ggplot(mtcars, aes_string(x = input$group, y = input$var)) +
geom_boxplot(fill = "black") +
labs(title = paste("Boxplot:", input$var, "by", input$group),
     x = input$group, y = input$var)})
  
# Bar plot
output$barplot <- renderPlot({
ggplot(mtcars, aes_string(x = input$var)) +
      geom_bar(fill = "black") +
      labs(title = paste("Bar chart of", input$var),
           x = input$var, y = "Count")})
  
# Histogram
output$hist <- renderPlot({
    ggplot(mtcars, aes_string(x = input$var)) +
      geom_histogram(fill = "green", bins = 10, color = "black") +
      labs(title = paste("Histogram of", input$var),
           x = input$var, y = "Frequency")})}

# Run the app
shinyApp(ui = ui, server = server)

