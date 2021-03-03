library(pdftools)
library(tidyverse)
library(tm)

# folder with 1000s of PDFs
dest <- "PDF_tests"

# make a vector of PDF file names
myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)

# read in pdf files and convert them to a corpus of text
corp <- Corpus(URISource(myfiles),
               readerControl = list(reader = readPDF))

#clean up documents
corp.tdm <- TermDocumentMatrix(corp, 
                                  control = 
                                    list(stopwords = TRUE,
                                         removeNumbers = TRUE,
                                         bounds = list(global = c(3, Inf)))) 

findFreqTerms(corp.tdm, lowfreq = 100, highfreq = Inf)

