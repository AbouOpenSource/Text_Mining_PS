library(tm)
library(XML)
library(SnowballC)
library(NLP)
library(slam)
rm(list=ls(all=TRUE))

doc1 <- Corpus(DirSource("/home/abou/R/data/Datatest2016"))


inspect(doc1)

ovid<- system.file("texts", "txt", package = "tm")
ovidCorpus<- Corpus(DirSource(ovid), readerControl = list(reader = readPlain))
inspect(ovidCorpus)


docs <- c("This is a text.", "This another one.")
corpus1<-Corpus(VectorSource(docs))

reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- Corpus(DirSource(reut21578),readerControl = list(reader = readReut21578XML))
inspect(reuters)

class(doc1)

class(doc1) 
inspect(doc1)
summary(doc1)
writeLines(as.character(X[[numdoc]]))

writeLines(as.character(reuters[[1]]))

writeLines(as.character(ovidCorpus[[1]]))

#
#Pre-processing f the corpus or document
#

reuters2 <- tm_map(reuters, PlainTextDocument)
inspect(reuters2)
stopwords("english")

#put in lowercase
reuters3 <- tm_map(reuters2, content_transformer(tolower))
writeLines(as.character(reuters3[[1]]))


#remove number

reuters4 <- tm_map(reuters3, removeNumbers)
writeLines(as.character(reuters4[[1]]))


#Remove punctuation

reuters5 <- tm_map(reuters4, removePunctuation)
writeLines(as.character(reuters5[[1]]))

#Elimination of stop words
reuters6<- tm_map(reuters5, removeWords, stopwords("english"))


length(stopwords("english"))

#Creation of a dictionary (multi-sets of sequences of characters)

mondico <- c( "crude", "oil")

#Creation of corpus based on mondico
Doc1RW <- tm_map(reuters5, removeWords, mondico)

Doc1RW2 <- tm_map(reuters5, removeWords, c("prices"))

###############################Stemming ############################

reuters7 <- tm_map(reuters6, stemDocument)
writeLines(as.character(reuters7[[1]]))

#Elimination of Extra whitespace

reuters8<- tm_map(reuters7, stripWhitespace)
writeLines(as.character(reuters8[[1]]))

dtm <- DocumentTermMatrix(reuters8)
dtm
class(dtm)
dim(dtm)
mondico <- c("prices", "crude", "oil")

#Creation of a matrix dtm_mondico from reuters8 using only the dictionary terms

dtm_mondico<-DocumentTermMatrix(reuters8, list(dictionary = c("prices", "crude", "oil") ))

dtm_mondico

#Elimination of sparse terms ( ie belonging to few documents)

inspect(removeSparseTerms(dtm, 0.4))

dtmsparse<- removeSparseTerms(dtm, 0.05)

dtmsparse

dtm2 <- removeSparseTerms(dtm, 0.7)

write.table(matrix(dtm2), file="/home/abou/R/data/matrix")

write.csv(as.matrix(dtm2), file="/home/abou/R/data/dtm2.csv")


#####Text mining ############
library(ggplot2)
library(wordcloud)


##Computing the term frequencies as a vector by converting the document term matrix into a
##matrix and summing the column counts

freq <- colSums(as.matrix(dtm2))

length(freq)
