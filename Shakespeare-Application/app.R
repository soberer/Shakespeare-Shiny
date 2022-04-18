#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)


books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

#begin get frequency function
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}
#end get frequency function


# Define UI for application that draws a histogram
ui <- fluidPage(
  
    theme = shinytheme("darkly"),

    # Application title
    titlePanel("Shakespeare's Plays Word Frequencies"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          selectInput(inputId = "books", label = "Choose a book:",
                      choices = books,
                      selected = "summer"),
          
          checkboxInput(inputId = "stopwords", label = "Stop words:",
                        value = TRUE),
          
          actionButton(inputId = "rerun", 
                       label = "Rerun"),
          
          hr(),
          
          h3("Word Cloud Settings"),
          
          sliderInput(inputId = "max_words", label = "Max # of Words:",
                      min = 10, max = 200, value = 100, step = 10),
          
          sliderInput(inputId = "largest_words", label = "Size of largest words:",
                      min = 1, max = 8, value = 4),
          
          sliderInput(inputId = "smallest_words", label = "Size of smallest words:",
                      min = 0.1, max = 4, value = 0.5),
          
          hr(),
          
          h3("Word Count Settings"),
          
          sliderInput(inputId = "minimum_wordcount", label = "Minimum words for Counts Chart:",
                      min = 10, max = 100, value = 25),
          
          sliderInput(inputId = "word_size", label = "Word size for Counts Chart:",
                      min = 8, max = 30, value = 14)
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel(title = "Word Cloud",
                      plotOutput(outputId = "cloud", height = "600px")),
                      tabPanel(title = "Word Counts",
                      plotOutput(outputId = "freq", height = "600px"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

freq <- eventReactive(
  input$rerun, {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$books, input$stopwords) # ... = replace with the two inputs from Task 2
    })
})

output$cloud <- renderPlot({
  
  v <- freq()
  pal <- brewer.pal(8,"Dark2")
  
  v %>% 
    with(
      wordcloud(
        word, 
        n, 
        scale = c(input$largest_words, input$smallest_words),
        random.order = FALSE, 
        max.words = input$max_words, 
        colors=pal))
})

output$freq <- renderPlot({
  
  v <- freq()
  
  v %>%
    filter(n > input$minimum_wordcount) %>%
    ggplot(v, mapping = aes(x = reorder(word, n), y = n)) + 
    coord_flip() +
    theme(text = element_text(size = input$word_size),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()
           ) +
    geom_bar(stat="identity")
  
}) 



}




# Run the application ################################################################################################
shinyApp(ui = ui, server = server)
