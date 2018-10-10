library(shiny)
library(wordcloud)

# User Interface
ui <- fluidPage(
  
  h1("Online pricing suggestion engine"),
  p(style = "font-family:Calibri",
    "Online Shopping Product Price Prediction"),
  tags$br(),
  tags$br(),
  
  fluidRow(
      column(5, textInput(inputId = "Item_name", 
                label = "Please Put Your Item name Here",
                value = "Item Name")),
      column(5, textOutput(outputId, 
                 container = div))
  ),
  
  fluidRow(
      column(4,selectInput(inputId = 'First_category',
                         label = 'Please select your first category',
                         choices = c(1,2,3),
                         selected = NULL)),
      
      column(4,selectInput(inputId = 'Second_category',
                 label = 'Please select your second category',
                 choices = c(1,2,3),
                 selected = NULL)),
      
      column(4,selectInput(inputId = 'Third_category',
                  label = 'Please select your third category',
                  choices = c(1,2,3),
                  selected = NULL))
      
     ),

  textInput(inputId = "item_description", 
            label = "Please Put Your Item Description Here",
            value = "Item Description",
            width='80%'),
  
  actionButton(inputId = "clicks", 
               label = "Predict Price"),
  
  wordcloud_rep <- repeatable(wordcloud)
  
  
  
  output$plot <- renderPlot({
      v <- terms()
      wordcloud_rep(names(v), v, scale=c(4,0.5),
                    min.freq = input$freq, max.words=input$max,
                    colors=brewer.pal(8, "Dark2"))
  })
  
  )

### Question: how to show our output? Just a number?

# Server
server <- function(input, output) {
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
}


# Run the app
shinyApp(ui = ui, server = server)   #Wrap with runApp() if calling from a sourced script or inside a function

# shiny app dashboard
rsconnect::setAccountInfo(name='sky456',
                          token='4AB1077CA346CD9E22E2098860FB25FD',
                          secret='<SECRET>')
library(rsconnect)
rsconnect::deployApp('path/to/your/app')





#######################################

ui <- fluidPage(
  
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num), main = input$title)
  })
}




