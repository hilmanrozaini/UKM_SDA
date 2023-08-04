# //////////////// unstrucutred data task 4: 3 songs analysis /////////////////////////////

# /////////////// DATA EXPLORATION ///////////////////

library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("ggplot2")
library("tidyverse")
library("tm")
library("wordcloud2")
library("wordcloud")

# ---------- SONG 1 -----------------
song1<- read.csv(file.choose(), header=FALSE)  # song1.csv
head(song1, 10)  

# cleaning and preprocessing
docs<-Corpus(VectorSource(song1))
docs
getTransformations()
docs<-tm_map(docs,toSpace,"'")
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,content_transformer(tolower))  # to lowercase, easy for preprocessing
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removeWords,stopwords("english"))
docs<-tm_map(docs,stripWhitespace)
inspect(docs)
song1 # just to compare the original file

# convert into DTM
dtm<-DocumentTermMatrix(docs)
inspect(dtm)
freq<-colSums(as.matrix(dtm))
length(freq)
ord<-order(freq,decreasing=T)
head(ord)
freq[ord]
freq[head(ord)]
freq

#once we have all above, we can insert to data frame
wf<-data.frame(names(freq),freq)
wf
names(wf)<-c("TERM","FREQ")
head(wf)
wf1_1<- wf[order(wf$FREQ, decreasing=T),]   # TO SEE THE DATA FRAME WORD FREQUENCY IN DESCENDING ORDER
head(wf1_1,10)

#findFreqTerms(dtm,lowfreq=10)
#findAssocs(dtm,"get",0.3)

#Subs<-subset(wf,FREQ>=5)
#Subs
#ggplot(Subs,aes(x=TERM,y=FREQ))+geom_bar(stat="identity")+
#  theme(axis.text.x=element_text(angle=45,hjust=1))
#ggplot(wf,aes(x=TERM,y=FREQ))+geom_bar(stat="identity")+
#  theme(axis.text.x=element_text(angle=45,hjust=1)) #Show all, include terms that hv small freq


#wordcloud(words=Subs$TERM, freq=Subs$FREQ, min.freq=1, max.words = 200, random.order = FALSE, color=brewer.pal(8,"Dark2")) 
wordcloud(words=wf$TERM, freq=wf$FREQ, min.freq=1, max.words = 200, random.order = FALSE, color=brewer.pal(8,"Dark2"))


# ------------ SONG 2 ------------------------

song2<- read.csv(file.choose(), header=FALSE)  # song2.csv
head(song2, 10)  

# cleaning and preprocessing
docs2<-Corpus(VectorSource(song2))
docs2
#getTransformations()
docs2<-tm_map(docs2,toSpace,"'")
docs2<-tm_map(docs2,removePunctuation)
docs2<-tm_map(docs2,content_transformer(tolower))  # to lowercase, easy for preprocessing
docs2<-tm_map(docs2,removeNumbers)
docs2<-tm_map(docs2,removeWords,stopwords("english"))
docs2<-tm_map(docs2,stripWhitespace)
inspect(docs2)
song2 # just to compare the original file

# convert into DTM
dtm2<-DocumentTermMatrix(docs2)
inspect(dtm2)
freq2<-colSums(as.matrix(dtm2))
length(freq2)
ord2<-order(freq2,decreasing=T)
head(ord2)
freq2[ord2]
freq2[head(ord2)]
freq2

#once we have all above, we can insert to data frame
wf2<-data.frame(names(freq2),freq2)
wf2
names(wf2)<-c("TERM","FREQ")
head(wf2)
wf2_2<- wf2[order(wf2$FREQ, decreasing=T),]
head(wf2_2,10)

#findFreqTerms(dtm2,lowfreq=10)
#findAssocs(dtm2,"get",0.3)

#Subs<-subset(wf,FREQ>=5)
#Subs
#ggplot(Subs,aes(x=TERM,y=FREQ))+geom_bar(stat="identity")+
#  theme(axis.text.x=element_text(angle=45,hjust=1))
#ggplot(wf,aes(x=TERM,y=FREQ))+geom_bar(stat="identity")+
#  theme(axis.text.x=element_text(angle=45,hjust=1)) #Show all, include terms that hv small freq
#wordcloud(words=Subs$TERM, freq=Subs$FREQ, min.freq=1, max.words = 200, random.order = FALSE, color=brewer.pal(8,"Dark2")) 

wordcloud(words=wf2$TERM, freq=wf2$FREQ, min.freq=1, max.words = 200, random.order = FALSE, color=brewer.pal(8,"Dark2"))


# ------------ SONG 3 ------------------------

song3<- read.csv(file.choose(), header=FALSE)  # song3.csv
head(song3, 10)  

# cleaning and preprocessing
docs3<-Corpus(VectorSource(song3))
docs3
#getTransformations()
docs3<-tm_map(docs3,toSpace,"'")
docs3<-tm_map(docs3,removePunctuation)
docs3<-tm_map(docs3,content_transformer(tolower))  # to lowercase, easy for preprocessing
docs3<-tm_map(docs3,removeNumbers)
docs3<-tm_map(docs3,removeWords,stopwords("english"))
docs3<-tm_map(docs3,stripWhitespace)
inspect(docs3)
song3 # just to compare the original file

# convert into DTM
dtm3<-DocumentTermMatrix(docs3)
inspect(dtm3)
freq3<-colSums(as.matrix(dtm3))
length(freq3)
ord3<-order(freq3,decreasing=T)
head(ord3)
freq3[ord3]
freq3[head(ord3)]
freq3

#once we have all above, we can insert to data frame
wf3<-data.frame(names(freq3),freq3)
wf3
names(wf3)<-c("TERM","FREQ")
head(wf3)
wf3_3<- wf3[order(wf3$FREQ, decreasing=T),]
head(wf3_3,10)

#findFreqTerms(dtm3,lowfreq=10)
#findAssocs(dtm3,"get",0.3)

#Subs<-subset(wf,FREQ>=5)
#Subs
#ggplot(Subs,aes(x=TERM,y=FREQ))+geom_bar(stat="identity")+
#  theme(axis.text.x=element_text(angle=45,hjust=1))
#ggplot(wf,aes(x=TERM,y=FREQ))+geom_bar(stat="identity")+
#  theme(axis.text.x=element_text(angle=45,hjust=1)) #Show all, include terms that hv small freq
#wordcloud(words=Subs$TERM, freq=Subs$FREQ, min.freq=1, max.words = 200, random.order = FALSE, color=brewer.pal(8,"Dark2")) 

wordcloud(words=wf3$TERM, freq=wf3$FREQ, min.freq=1, max.words = 200, random.order = FALSE, color=brewer.pal(8,"Dark2"))

# ALL WORDCLOUD FOR ALL 3 SONGS
# SONG 1
wordcloud(words=wf$TERM, freq=wf$FREQ, min.freq=1, max.words = 200, random.order = FALSE, color=brewer.pal(8,"Dark2"))
# SONG 2
wordcloud(words=wf2$TERM, freq=wf2$FREQ, min.freq=1, max.words = 200, random.order = FALSE, color=brewer.pal(8,"Dark2"))
# SONG 3
wordcloud(words=wf3$TERM, freq=wf3$FREQ, min.freq=1, max.words = 200, random.order = FALSE, color=brewer.pal(8,"Dark2"))
