library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Text Prediction in R"),
  h5("by Winston Mwabumba"),
  ("-----------------------------------------------------------------"),
  h5("NB: Please a minute for the word suggestion algorithm to load"),
  mainPanel(
    textInput("textIn", 
              label = h4("Enter your text here:"), 
              value = "",
              placeholder = "Enter text here"),
    h5("Your suggested word is: "),
    textOutput("textOut"),
    
  )
)
)