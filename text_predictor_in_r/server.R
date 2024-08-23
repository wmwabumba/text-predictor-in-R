library(shiny)
library(ggplot2)
library(reshape2)
library(tm)
library(slam)
library(data.table)
library(RWeka)
library(dtplyr)
library(e1071)
library(dplyr)
library(magrittr)
library(stringr)


# Comment out the pre-processing of text when running in Shiny
# preprocessing <- TRUE
# if(preprocessing) 
# {
# setting a working directory and   loadng data
setwd( "data")
con <- file("en_US.blogs.txt",open="r" )
en_US.blogs.data <- readLines(con)
close(con)
con <- file("en_US.news.txt",open="r" )
en_US.news.data <- readLines(con) 
close(con)
con <- file("en_US.twitter.txt",open="r" )
en_US.twitter.data <- readLines(con) 
close(con)

# data sampling and consolidation
set.seed(123)
en_US.blogs.sample <- sample(en_US.blogs.data, 3000, replace=FALSE)
en_US.news.sample <- sample(en_US.news.data, 3000, replace=FALSE)
en_US.twitter.sample <- sample(en_US.twitter.data, 3000, replace=FALSE)
sample.data <- c(en_US.blogs.sample,en_US.news.sample,en_US.twitter.sample)

# Corpus transformation function
# removing whitespace
# converting all text to lowercase
# removing punctuations
# removing numeric values
getCorpus <- function(v) 
{
  corpus <- VCorpus(VectorSource(v))
  corpus <- tm_map(corpus, stripWhitespace)  
  corpus <- tm_map(corpus, content_transformer(tolower))  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers) 
  corpus 
}

# Applying the corpus transformation function
aCorp <- getCorpus(sample.data)

# Defining functions to tokenize the corpus into N-grams
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# Creating term document matrices of 2, 3, and 4 N-grams
tdm.2 <- TermDocumentMatrix(aCorp, control = list(tokenize = BigramTokenizer)) 
tdm.3 <- TermDocumentMatrix(aCorp, control = list(tokenize = TrigramTokenizer))
tdm.4 <- TermDocumentMatrix(aCorp, control = list(tokenize = QuadgramTokenizer))

# Defining a function that will convert the term document matrices into frequencies
tdmToFreq <- function(tdm) 
{
  freq <- sort(row_sums(tdm, na.rm=TRUE), decreasing=TRUE)
  word <- names(freq)
  data.table(word=word, freq=freq)
}

# Defininf a function to process the N-grams to create two columns for the pre-term and current-term
processGram <- function(dt) 
{
  dt[, c("pre", "cur"):=list(unlist(strsplit(word, "[ ]+?[a-z]+$")), 
                             unlist(strsplit(word, "^([a-z]+[ ])+"))[2]), 
     by=word]
}

# Convert term document matrices into frequencies and processing the N-grams
mvs1 <- tdmToFreq(tdm.2)
processGram(mvs1)
mvs2 <- tdmToFreq(tdm.3)
processGram(mvs2)
mvs3 <- tdmToFreq(tdm.4)
processGram(mvs3)

# Save the N-gram frequencies to disk
save(mvs1, file = "mvs1.RData")
save(mvs2, file = "mvs2.RData")
save(mvs3, file = "mvs3.RData")
# }



# Define my function to take raw text input and return the predicted next word
word_suggestion <- function(raw) 
{
  sentence <- tolower(raw) %>%
    removePunctuation %>%
    removeNumbers %>%
    stripWhitespace %>%
    str_trim %>%
    strsplit(split=" ") %>%
    unlist
  
  length(sentence)
  
  if(length(sentence)>=3)
  {
    gram <- paste(tail(sentence, 3), collapse=" ")
    myattempt4 <- mvs3[ which(pre==gram),]$cur[1]
    
    if(is.na(myattempt4))
    {
      gram <- paste(tail(sentence, 2), collapse=" ")
      myattempt3 <- mvs2[ which(pre==gram),]$cur[1]
      
      if(is.na(myattempt3))
      {
        gram <- paste(tail(sentence, 1), collapse=" ")
        myattempt2 <- mvs1[ which(pre==gram),]$cur[1]
        
        if(is.na(myattempt2))
        {
          return("--no match--")
        }
        else
        {
          return(myattempt2)
        }
      }
      else
      {
        return(myattempt3)
      }
    }
    else
    {
      return(myattempt4)
    }
  }
  
  if(length(sentence)==2)
  {
    gram <- paste(tail(sentence, 2), collapse=" ")
    myattempt3 <- mvs2[ which(pre==gram),]$cur[1]
    
    if(is.na(myattempt3))
    {
      gram <- paste(tail(sentence, 1), collapse=" ")
      myattempt2 <- mvs1[ which(pre==gram),]$cur[1]
      
      if(is.na(myattempt2))
      { 
        return("--no match--")
      }
      else
      {
        return(myattempt2)
      }
    }
    else
    {
      return(myattempt3)
    }
  }
  
  if(length(sentence)==1)
  {
    gram <- paste(tail(sentence, 1), collapse=" ")
    myattempt2 <- mvs1[ which(pre==gram),]$cur[1]
    
    if(is.na(myattempt2))
    { 
      return("--no match--")
    }
    else
    {
      return(myattempt2)
    }
  }
  
  else
  {
    return("--no suggestion--")
  }
  
}


# Define the Shiny Server
shinyServer(function(input, output)
{
  
  observe({
    
    output$textOut <- renderText(
      { 
        paste("", word_suggestion(input$textIn))
      })
    
  })
  
})