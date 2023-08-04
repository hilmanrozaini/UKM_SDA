# finalised codes
# STQD project 2 - p121535 - hilman

# ///////////// TASK 1 /////////////////

#install.packages("text")
library(tm)
library(textstem)
library(SnowballC)
library(tidyverse)
library(stringr)
library(topicmodels)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidytext)

#to open a folder of docs
docs<-Corpus(DirSource("C:/Users/User/Desktop/STQD6114_ unstructured data/project_2/dataset_proj2_stqd6114_p121535/text_folder_task1"))
writeLines(as.character(docs[[30]])) #inspect a particular document

# data preprocessing
docs <- tm_map(docs,content_transformer(tolower)) #Transform to lower case
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})

docs <- tm_map(docs, toSpace, " ' ")
docs <- tm_map(docs, toSpace, "  '")
docs <- tm_map(docs, toSpace, '  "')
docs <- tm_map(docs, toSpace, ' " ')
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, '"')
docs <- tm_map(docs, toSpace, " \" ")
docs <- tm_map(docs, toSpace, " “ ")
docs <- tm_map(docs, toSpace, " ” ")
docs <- tm_map(docs, toSpace, " ’ ")
docs <- tm_map(docs, toSpace, " ‘ ")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "”")
docs <- tm_map(docs, toSpace, "’")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, '"")')
docs <- tm_map(docs, toSpace, '""')
docs <- tm_map(docs, toSpace, ')')

docs <- tm_map(docs, removePunctuation) #remove punctuation
docs <- tm_map(docs, removeNumbers) #Strip digits
docs <- tm_map(docs, removeWords, stopwords("english")) #remove stopwords
docs <- tm_map(docs, stripWhitespace) #remove whitespace

myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","'ve ",
                 "'re ") #, "txt' ", 'txt', "txt`", " txt` ", "txt` = \" ")

docs <- tm_map(docs, removeWords, myStopwords) #remove custom stopwords

# Lemmatize the words in the documents

library(textTinyR)

# the 'docs' is a list of character vectors where each element represents a document

# Create an empty list to store the lemmatized documents
lemmatized_docs <- vector("list", length(docs))

# Loop over each document
for (i in seq_along(docs)) {
  # Lemmatize the current document
  lemmatized_docs[[i]] <- lemmatize_strings(docs[[i]])
}

# 'lemmatized_docs' now contains the lemmatized version of each document


# creating DTM for the lemmatized docs

dtm<-DocumentTermMatrix(lemmatized_docs)
dtm

# /////// task 1 : topic modelling using LDA //////////

# i) creating k=4
ap_lda<-LDA(dtm,k=4,control=list(seed=1234)) #create two-topic LDA model
ap_lda

ap_topics<-tidy(ap_lda,matrix="beta") #Extract the per-topic-per-word-probabilities
ap_topics

ap_top_terms <- ap_topics %>% group_by(topic) %>% top_n(10,beta) %>% ungroup () %>% arrange (topic, -beta)
ap_top_terms

subset(ap_top_terms, topic == 1, select = c(term, beta))
subset(ap_top_terms, topic == 2, select = c(term, beta))
subset(ap_top_terms, topic == 3, select = c(term, beta))
subset(ap_top_terms, topic == 4, select = c(term, beta))



ap_top_terms%>% mutate(term=reorder(term,beta))%>%
  ggplot(aes(term,beta,fill=factor(topic)))+geom_col(show.legend=FALSE)+
  facet_wrap(~topic,scales="free")+scale_x_reordered()+coord_flip() #visualize the above

# ii) Find terms that are most common within each topics
# topic 2 & topic 3

beta_spread_4 <- ap_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic2>0.004 | topic3 > 0.004) %>% mutate(log_ratio = log2(topic3/topic2))

beta_spread_4%>% mutate(term=reorder(term,log_ratio))%>%
  ggplot(aes(term,log_ratio))+geom_col(show.legend=FALSE)+coord_flip()


# iii) Extract the per-document-per-topic-probabilities
ap_documents<-tidy(ap_lda,matrix="gamma") 
ap_documents

# to see the which documents belong to which topic based on the gamma value
sorted_gamma <- ap_documents[order(-ap_documents$gamma), ]
document_topic <- sorted_gamma[, c("document", "topic", "gamma")]
print(document_topic)

# iv) #Check the most common words in the document, eg: document 6

tidy(dtm)%>%filter(document==6)%>%arrange(desc(count)) 


# ///////////// task 2 : text clustering ////////////////////////

#Present text data numerically, weighted TF-IDF

tdm.tfidf <- weightTfIdf(dtm)
tdm.tfidf <- removeSparseTerms(tdm.tfidf, 0.999)  
# 'removeSparseTerms' is to remove word with similar meaning where the occurence is 0.999 (meaning very almost similar to one another)
tfidf.matrix <- as.matrix(tdm.tfidf) 
tfidf.matrix
# Cosine distance matrix (useful for specific clustering algorithms) 
library(proxy)
dist.matrix <- dist(tfidf.matrix, method = "cosine")

truth.K=3  # we can manipulate the k-value for k-means
# reminder, for hierarchical, we cant manipulate the k-value coz it is based on dendogram

#Perform clustering
set.seed(1234)
library(dbscan) #useful for hdbscan function
clustering.kmeans <- kmeans(tfidf.matrix, truth.K) 
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.dbscan <- hdbscan(dist.matrix, minPts = 10)

library(cluster)
clusplot(as.matrix(dist.matrix),clustering.kmeans$cluster,color=T,shade=T,labels=2,lines=0)

plot(clustering.hierarchical)
rect.hclust(clustering.hierarchical,3)

plot(as.matrix(dist.matrix),col=clustering.dbscan$cluster+1L, main="dbscan clustering")

#Combine results
master.cluster <- clustering.kmeans$cluster 
slave.hierarchical <- cutree(clustering.hierarchical, k = truth.K) 
slave.dbscan <- clustering.dbscan$cluster 

#plotting results
library(colorspace)
points <- cmdscale(dist.matrix, k = 2) 
palette <- diverge_hcl(truth.K) # Creating a color palette, need library(colorspace)
#layout(matrix(1:3,ncol=1))

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
# k-means clustering plot assumes that the one black point is a single cluster, while the rest red colour dot is another cluster
# in hierarchical clustering, the 2 red dots are one cluster while the rest black is another cluster
# density based clustering consist only one colour,  so we need to identify whether is it a single cluster or is it that all does not belong to any cluster
# we can see the table to understand the behaviour of density based clustering

table(master.cluster) # it means one dot belong to 1 cluster, while 29 other belongs to another cluster
table(slave.hierarchical)  # it means 2 dot belong to 1 cluster, while 28 other belongs to another cluster
table(slave.dbscan)  # it means that there is no cluster, so in this case, it says all 30 dots are noise, and they do not belong to any cluster


# //////////////////////////////////////////////////////

# task 3: sentiment analysis

library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
library("syuzhet")
library("ggplot2")

sent_dat <- read.csv("C:/Users/User/Desktop/STQD6114_ unstructured data/project_2/dataset_proj2_stqd6114_p121535/sentiment_dataset/preprocessed_kindle_review.csv")
head(sent_dat)
TextDoc  <- Corpus(VectorSource(sent_dat$reviewText))
inspect(TextDoc)


# cleaning data 

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
TextDoc <- tm_map(TextDoc, removeNumbers)
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company","team")) # Remove your own stop word # specify your stopwords as a character vector
TextDoc <- tm_map(TextDoc, removePunctuation) # Remove punctuations
TextDoc <- tm_map(TextDoc, stripWhitespace) # Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stemDocument) # Text stemming - which reduces words to their root form

# Build a term-document matrix
dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(dtm)
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE) # Sort by descending value of frequency
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
head(dtm_d, 5) # Display the top 5 most frequent words

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Word Association
findAssocs(dtm, terms = c("book","stori", "read", "like" ,"one"), corlimit = 0.25) # Find associations 
#findAssocs(dtm, terms = findFreqTerms(dtm, lowfreq = 100), corlimit = 0.25) # Find associations for words that occur at least 50 times

## Sentiment scores
# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods have different scales
syuzhet_vector <- get_sentiment(sent_dat$reviewText, method="syuzhet")
head(syuzhet_vector)
head(syuzhet_vector,10) # see the first 10 elements of the vector
# the 2.60 is positive and it is positive for the first line of the text
# although it is ranged between -1 to +1, the 2.60 is the total of the value in the line
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(sent_dat$reviewText, method="bing")
head(bing_vector)
# the first line gives u a total value of +3
# the 2nd line gives u a total of 1
# note, bing method does not give u decimal value
summary(bing_vector)

#afinn
afinn_vector <- get_sentiment(sent_dat$reviewText, method="afinn")
head(afinn_vector)
# for affin, normally we get higher.. coz the range value is between -5 to +5
# so thats why the sum of value in first line is high
# also, affin only give whole number, no decimal
# if not mistaken, there will be no zero value.. meaning no netural value will be assigned to any word
summary(afinn_vector)


#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)
# row is the method, so the 1st row is syuzhet method, 2nd row is bing, and so on
# the column is the review.. meaning column 1 is review 1, column 2 is review 2
# take note, we cannot compare one lexicon to another lexicon.. because each lexicon has their own theory application
# but, the compare of each vector is that we compare just to see the sign for all 3 method
# eg: for 1st review, all 3 method give u +ve sign
# and for 2nd review (2nd column), all 3 method also give u +ve sign
# while in 4th review, 2nd method give u -ve sign



## Emotion classification
# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score : 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# and if the sentiment is positive or negative


# from here, we see that there are 12,000 rows of reviews
length(sent_dat$reviewText) 

# so, we Combine 'sent_dat$reviewText' into a single column, so that the text processing is faster
combined_reviews <- paste(sent_dat$reviewText, collapse = " ")

# Print the combined reviews
#print(combined_reviews)

d<-get_nrc_sentiment(combined_reviews)
head(d,10) # head(d,10) - just to see top 10 lines

#Visualization
td<-data.frame(t(d)) #transpose
td_new <- data.frame(rowSums(td)) #The function rowSums computes column sums across rows for each level of a grouping variable.

names(td_new)[1] <- "count" #Transformation and cleaning
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

#Plot 1 - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment,ylab="count")+ggtitle("Survey sentiments")

#Plot 2 - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)












