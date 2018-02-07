#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
require(stringr)
library(sqldf)

twoGrams <- read.csv('twoGramTable.csv', header = TRUE, stringsAsFactors = FALSE)
threeGrams <- read.csv('threeGramTable.csv', header = TRUE, stringsAsFactors = FALSE)
fourGrams <- read.csv('fourGramTable.csv', header = TRUE, stringsAsFactors = FALSE)
fiveGrams <- read.csv('fiveGramTable.csv', header = TRUE, stringsAsFactors = FALSE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$inputText <- renderText({ input$txt }, quoted = FALSE)
  observeEvent(input$Submit, {
    txt <- gsub("\'","\'\'",input$txt)
    nwords <- str_count(txt, "\\S+")
    formattedTxt <- paste(unlist(strsplit(isolate(txt),' ')), collapse = '_')
    output$suggestions  <- renderPrint({
      if(nwords >= 5){
      print(getPreds(formattedTxt, 5))
      }
      else{
        
        print(getPreds(formattedTxt, nwords + 1))
      }
      })
  })
  
  getPreds <- function(str, nGrams){
    if (nGrams == 1) {
      return('Not found')
    }
    if (length(unlist(strsplit(str, "_"))) > nGrams - 1) {
      str <-
        paste(tail(unlist(strsplit(str, "_")), nGrams - 1), collapse = '_')
    }
    if (nGrams == 5) {
      query = sprintf("select Pred from fiveGrams where nGrams = '%s' order by Frequency desc limit 3",
                      str)
    }
    else if (nGrams == 4) {
      query = sprintf("select Pred from fourGrams where nGrams = '%s' order by Frequency desc limit 3",
                      str)
    }
    else if (nGrams == 3) {
      query = sprintf("select Pred from threeGrams where nGrams = '%s' order by Frequency desc limit 3",
                      str)
    }
    else if (nGrams == 2) {
      query = sprintf("select Pred from twoGrams where nGrams = '%s' order by Frequency desc limit 3",
                      str)
    }
    res <- sqldf(query)
    if (nrow(res) == 0) {
      getPreds(str, nGrams - 1)
    }
    else {
      return(res)
    }
  } 
  
})
