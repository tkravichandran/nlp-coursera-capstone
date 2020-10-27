library(shiny)
library(dplyr)
library(shinycssloaders)

shinyUI(fluidPage(
  titlePanel("Text prediction app"),
  sidebarLayout(
    sidebarPanel(
        h3("Input your text from Twitter "),
        textInput(inputId="i1", label="Input for prediction"),
        actionButton("do", "Predict Go!"),
        actionButton("clear","Clear All")
    ),
    mainPanel(
        h3("The predictions in order are:"),
        uiOutput("t1") %>% withSpinner(color="#0dc5c1"),
        uiOutput("t2"),
        uiOutput("t3"),
        textOutput("t4"),
        tableOutput("table1"),
        h3("Instructions for using the app"),
        h4("This App predicts the next word after you press Submit. The top 3 words predicted are shown along with time taken for prediction. Also a table showing the probability from corpus is shown along with 5 predictions and ngram. 'Clear', removes the current prediction. It is not necessary to clear before you submit again.",
           style = "word-wrap: break-word;")

    )
  )
))
