library(mallet)
library(dplyr)
library(readtext)
remotes::install_github("ChengMengli/tf-idf")
library(textmineR)

#run all functions in file mallet_functions.R

#1.  Make a trainer object and supply the trainer with documents###########

setwd("~/OneDrive - Newcastle University/RMallet/") #set wd

#read in training text
data<-readtext(paste0("./classifier/sources/cl_base_new/*")) 

#read in stopwords file
stopwords <- read.table("~/OneDrive - Newcastle University/RMallet/project_sim/workshop/extra-exclude-words_alt.txt", quote="\"", comment.char="")

#create instances
SDG.instances <- 
  mallet.import(id.array = row.names(data), 
                text.array = data[["text"]], 
                stoplist = "~/OneDrive - Newcastle University/RMallet/project_sim/workshop/extra-exclude-words_alt.txt",
                token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

#Train model
topic.model <- MalletLDA(num.topics=18, alpha.sum = 1, beta = 0.1) # create model specify no of topics

topic.model$loadDocuments(SDG.instances)

vocabulary <- topic.model$getVocabulary()# look at vocab
head(vocabulary)

word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)

topic.model$setAlphaOptimization(20, 50) #optimise hyperparameters

topic.model$train(2000) #train the model with set number of iterations

topic.model$maximize(18)

doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)

#2.###### read in text to classifiy and convert to dtm #######

# Read all the files and create a FileName column to store filenames
test<-readtext::readtext("~/OneDrive - Newcastle University/RMallet/project_sim/full_texts_test/*.txt")

dtm <- CreateDtm(doc_vec = test$text, # character vector of documents
                 doc_names = test$doc_id, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(tm::stopwords("english"), # stopwords from tm
                                  tm::stopwords("smart"),stopwords),
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE) # numbers - this is the default

# remove any tokens that were in 3 or fewer documents
dtm <- dtm[ , colSums(dtm > 0) > 3 ] # alternatively: dtm[ , tf_mat$term_

#3. create compatible inferences and inferencer ######

com<-compatible_instances(test$text, ids = test$doc_id, SDG.instances)#compatible instances
inf<-inferencer(topic.model)# inferencer

#4. apply model to infer topics from imported text documents
hoo<-infer_topics(inf, com)

