
# This is Version 1 of the German Hate Speech Filter
# Incorporating a learning algorithm into the filter.
# DATE: 08.11.2017
# Introducing probabilities and coloring for various types of text

## NOTE: I used:
# set_opts(ignored.packages = "tidyverse")
# to make Packrat ignore tidyverse.
# The latest update is having problems with RStudio (that is v1.2.0)
# so I want to keep v.1.1.1

library(tidyverse)
library(plyr)
library(stringr)
library(tm)
library(RWeka)
library(RColorBrewer)

library(caret)   # For learning algorithm training
library(kernlab) # For SVM


attach("../Data/Algo_with_Probs.RData")
Algo <- svmModel
varNames <- setdiff(names(Algo$trainingData), ".outcome")
detach("file:../Data/Algo_with_Probs.RData")

### We need this to access the NGramTokenizer options from RWeka,
### to be used with documenttermmatrix

myTokenizer <- function(c) {
  NGramTokenizer(x = c, control = Weka_control(max = 6))
}

## Create color palettes for use in comment labelling:
badColor <- brewer.pal(n = 5, "Reds")
goodColor <- brewer.pal(n = 5, "Greens")
neutralColor <- brewer.pal(n = 7, "RdYlGn")


ui <- fluidPage( 
  
  titlePanel("Hate Speech Filter v.0"),
  
 br(),
 br(),
 br(),
 br(),
  
  fluidRow(    
    column(width = 7, offset = 2,
           wellPanel(
             textAreaInput("originalText", "Geben Sie bitte den zu evaluierenden Text ein:")
           )       
    ), 
    column(width = 1, 
           br(),
           br(),
           br(),
           br(),
           actionButton("submit", "Text absenden"))),
  
  br(),  
  br(),
  br(),
  
 fluidRow(column(width = 7, offset = 2,
                 wellPanel(
                   helpText(strong("Evaluierter Text:")),
                   htmlOutput("filteredText")
                 )
 )),
 
 br(),
 br(),
 
 br() 
)





server <- function(input, output) {

  comment <- eventReactive(input$submit, {
    req(input$originalText)
    enteredComment <- VectorSource(input$originalText)
    corpusComment <- VCorpus(enteredComment, readerControl = list(language = "ger"))
    inspect(corpusComment)
    inspect(corpusComment[[1]])
    return(corpusComment)
  })
  
  printText <- eventReactive(input$submit, {
    req(input$originalText)
    newvec <- str_split(input$originalText, "\\n")[[1]]
    return(newvec)
  })
  
  transformedComment <- reactive({
    req(comment())
    trComment <- comment() %>%
      tm_map(stripWhitespace) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removePunctuation)
    return(trComment)
  })
  
  docTermMatrix <- reactive({
    req(comment())
    dtm <- DocumentTermMatrix(transformedComment(), control = list(tokenize = myTokenizer, wordLengths = c(2, Inf)))
    return(dtm)
  })
  
  flagIt <- reactive({
    req(comment())
    X <- as.matrix(docTermMatrix())
    newNames <- intersect(colnames(X), varNames)

    temp <- matrix(0, nrow = nrow(X), ncol = length(varNames))
    colnames(temp) <- varNames
    
    if (!is_empty(newNames)) {
      temp[ , newNames] <- X[ , newNames]
    }
    FinalX <- temp
    
    prediction <- predict(Algo, newdata = FinalX, type = "prob")
    
#    prediction <- predict(Algo, newdata = FinalX)
#    flagged <- any(prediction == "Bad")
    flagged <- max(prediction$Bad, na.rm = TRUE)
    return(flagged)
  })
  

  output$filteredText <- renderUI({  
    req(input$originalText)
    if (flagIt() > 0.65) {
#      if (flagIt() == TRUE) {
      withTags({
        div(class = "Bad Comment", style = if (flagIt() > 0.85) {
          "background-color: #DE2D26; color: #f4a582;"
          } else {
            "background-color: #FCAE91; color: #fddbc7;"
          },
            # style = "background-color: #fcc; color: #b61919;",
            br(),
            h4("Wollen Sie wirklich diesen Kommentar absenden?  "),
            h5(paste0("Es besteht eine ", round(100*flagIt()), "% Möglichkeit dass dieser Kommentar als Hassposting betrachtet wird.")),
            hr(),
            lapply(printText(), function(x) p(em(x))),
            br()
        )
      })
    } else if (flagIt() < 0.35) {
      withTags({
        div(class = "Good Comment", style = "background-color: #70ce88; color: #065006a;",
            br(),
            h4("Alles OK, Sie können diesen Text ruhig absenden!  "),
            h5(paste0("Es besteht eine ", round(100 - 100*flagIt()), "% Möglichkeit dass dieser Kommentar kein Hassposting ist.")),
            hr(),
            lapply(printText(), function(x) p(em(x))),
            br()
        )
      })
    } else {
      withTags({
        div(class = "Good Comment", style = "background-color: #70ce88; color: #065006a;",
            br(),
            h4("Ich bin mir nicht ganz sicher, aber wahrscheinlich können Sie diesen Text absenden.  "),
            h5(paste0("Es besteht eine ", round(100 - 100*flagIt()), "% Möglichkeit dass dieser Kommentar kein Hassposting ist.")),
            hr(),
            lapply(printText(), function(x) p(em(x))),
            br()
        )
      })
     }
  })
  
  

}


shinyApp(ui, server)

