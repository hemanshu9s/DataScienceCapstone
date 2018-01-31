mergeFiles <- function(){
  if(!file.exists("Coursera-SwiftKey/final/en_US/mergedfile.txt")){
  conn <- file("Coursera-SwiftKey/final/en_US/mergedfile.txt", "r")
  fulltext <- readLines(conn, encoding ="UTF-8")
  nlines <- length(fulltext)
  close(conn)
  
  conn <- file("Coursera-SwiftKey/final/en_US/sampledfile.txt", "w")
  selection <- rbinom(nlines, 1, .1)
  for (i in 1:nlines) {
    if (selection[i]==1) { cat(fulltext[i], file=conn, sep="\n") }
  }
  close(conn)
  paste("Saved", sum(selection), "lines to file", "Coursera-SwiftKey/final/en_US/sampledfile.txt")
  }
}

makeCorpus <- function(){
  mytf3 <- readLines("Coursera-SwiftKey/final/en_US/sampledfile.txt", encoding ="UTF-8")
  myCorpus <- corpus(mytf3)
}

makeSentences <- function(filecorpus){
  sentences <- tokens(filecorpus, what="sentence",remove_punct = TRUE, remove_twitter = TRUE)
  sentences <-  tokens_remove(sentences, getProfanities())
  unlist(lapply(sentences, function(a) paste('S', char_tolower(a), 'E')))
}

makeNgrams <- function(sentences, n = 1L){
  words <- tokens(sentences, ngrams = n, remove_separators = TRUE,remove_punct = TRUE, remove_twitter = TRUE, what = "word", remove_hyphens = TRUE, remove_numbers = TRUE)
  words <-  tokens_remove(words, getProfanities())
}

getProfanities <- function(){
  profanityFile <- "profanities.txt"
  if(!file.exists(profanityFile)){
         download.file('http://www.cs.cmu.edu/~biglou/resources/bad-words.txt',profanityFile)
  }
  profanities <- read.csv("profanities.txt", header = FALSE, stringsAsFactors = FALSE)
  profanities$V1
}

createnGramLookupTable <- function(ngram){
 tokenList <-  unlist(ngram, recursive = FALSE, use.names = FALSE)
 wordFreq <- table(tokenList)
 nGramTable <- as.data.frame(wordFreq)
}


main <- function(){
  library(quanteda)
  require(readtext)
  library(sqldf)
  mergeFiles()
  fileCorpus <- makeCorpus()
  sentences <- makeSentences(fileCorpus)
  oneGrams <- makeNgrams(sentences, 1)
  oneGramTable <- createnGramLookupTable(oneGrams)
  write.table(oneGramTable, "oneGramTable.dat", sep = ",", quote = FALSE)
  oneGrams<- NULL
  twoGrams <- makeNgrams(sentences, 2)
  twoGramTable <- createnGramLookupTable(twoGrams)
  write.table(twoGramTable, "twoGramTable.dat", sep = ",", quote = FALSE)
  twoGrams <- NULL
  threeGrams <- makeNgrams(sentences, 3)
  threeGramTable <- createnGramLookupTable(threeGrams)
  write.table(threeGramTable, "threeGramTable.dat", sep = ",", quote = FALSE)
  threeGrams <- NULL
  fourGrams <- makeNgrams(sentences, 4)
  fourGramTable <- createnGramLookupTable(fourGrams)
  write.table(fourGramTable, "fourGramTable.dat", sep = ",", quote = FALSE)
  fourGrams <- NULL
}
