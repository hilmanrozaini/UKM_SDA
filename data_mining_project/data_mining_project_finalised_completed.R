# //////////////// DATA MINING PROJECT /////////////////////////////

# /////////////// DATA EXPLORATION ///////////////////

retail_data<- read.csv(file.choose())  # OnlineRetail.csv file
head(retail_data, 15)  

library(tidyverse)
library("tm") #install text mining package
library("quanteda")
library("quanteda.textstats")
library("quanteda.textplots")
library("wordcloud")
library("RColorBrewer")
library(mice)


# ------------ Checking NA values -------------
sum(is.na(retail_data))
which(is.na(retail_data))
is.na(retail_data)

#to visualise the NA values using MICE
mice::md.pattern(project_data) 
# it is found that the NA is revolve around unit price and customerID
# since the quantity of NA for unit price is insignificant,
# and the customerID is a variable that is cannot be imputed with prediction value, 
# thus we decide to omit the NA value by dropping all the NA values

# decide to drop all NA values
dummy<- retail_data
df <- dummy %>% drop_na()
sum(is.na(df))
head(df)
mice::md.pattern(df)


# ////////////////////////// DATA MANIPULATION ///////////////////////////

df<-df%>%
  filter(!stringr::str_detect("TEST001|TEST002|POST|DOT|gift|DOT|C2|ADJUST|BANK CHARGESt|PADS", df$StockCode))


df$totalcost<- df$UnitPrice*df$Quantity
#dat1 <- df%>% group_by(CustomerID) %>% summarise(AvgSpending = mean(totalcost))
dat2<- df%>% group_by(Description) %>% summarise(total_spending = sum(totalcost))
dat3<- df%>% group_by(Description) %>% summarise(total_quantity = sum(Quantity))
dat4<- df%>% select(CustomerID, Country, Description) #to select customerID and Country in project dataset
#dat4<- unique(dat4) # to filter only unique country with unique customerID ===>> not applicable because the description is unique
head(dat4,15)

dat_combine<- merge(dat2,dat3, by="Description")
dat_final_combine<-merge(dat_combine,dat4, by="Description")
#dat_final_with_country<- merge(dat_final_combine, dat4, by="CustomerID")

dat_final_combine      #dataset with unique customerID only
#head(dat_final_with_country)  #dataset with unique customerID and countries included
#unique(dat_final_with_country$Country)  # to identify how many unique countries in the dataset
#summary(dat_final_with_country)


# ----------- remove negative quantity rows (also remove 0 values because there's double entry, unsure to keep or not) -------------  

#(this is because we only take into account the positive counterpart of the invoice, 
#because it is mentioned that the negaive qty is cancelled order, but we do not know the sentimen behind the cancelled order, 
# thus is removing the whole invoice, which includes both positive & negative value, it might not be a best representative of showing the customer's demand
# therefore, we just keep the positive because it was a demanded item, and the cancel may be due to other reasons instead of categorizing as a 'not-in-demand' item

max_arng_dat<- dat_final_combine %>% group_by(Description) %>%  # group by Name
  mutate(QtyMax = max(total_quantity)) %>%  # create a temp variable holding the max of each Name
  arrange(desc(QtyMax), desc(total_quantity)) %>%  # arrange with two columns
  select(Description, QtyMax, total_spending, CustomerID, Country) # select only the two input columns

df_dat<-as.data.frame(max_arng_dat)
tail(df_dat1,100) #it is determine that the negative quantity starts from row 406356

df_dat1<- subset(df_dat, QtyMax>0)
tail(df_dat1, 10) #the negative and zero quantity has been filtered.. so now min quantity > 0
unique(df_dat1$Country)


# ---------------- just to find which unique country has top quantity demand ---------------


sum_country_qty<- dat_final_combine%>% group_by(Country) %>% summarise(summ_qty=sum(total_quantity))

max_qty_country<- sum_country_qty %>% group_by(Country) %>%  # group by Name
  mutate(QtyMax = max(summ_qty)) %>%  # create a temp variable holding the max of each Name
  arrange(desc(QtyMax), desc(summ_qty)) %>%  # arrange with two columns
  select( QtyMax, Country) # select only the two input columns

max_qty_country1<- max_qty_country%>%distinct()
df_country_dat<- as.data.frame(max_qty_country1)
df_country_dat  # to see the sum of product quantity for each unique country 

# we can see the arrangement of which country has the highest quantity demand for online retail products


# this is just to make sure that the total quantity is still remain the same 
sum(dat_final_combine$total_quantity) #first data
sum(df_country_dat$QtyMax) #latest manipulated data
sum(df_dat$QtyMax)   #data before removing quantity == 0

# all the total quantity are the same.




# ////////////// TEXT MINING ///////////////////////// 

# ---------------- DATA PREPARATION FOR TEXT ANALYSIS -----------------

# from quanteda package, load corpus
corp_retail_dat<- corpus(df_dat1, text_field="Description")  #converting the dataset into corpus format for text analysis purposes
corp_retail_dat


## -- step 1 --
# tokenize the text
text2<- tokens(corp_retail_dat)
text2


# preprocessing DTM (cleaning DTM)  (diff method from internet, just wanna see the diff)
text3 <- corp_retail_dat |>
  tokens(remove_punct = T, remove_numbers = T, remove_symbols = T) |>   ## tokenize, removing unnecessary noise
  tokens_tolower() |>                                                   ## normalize
  tokens_remove(stopwords('en')) |>                                     ## remove stopwords (English)
  tokens_wordstem()                                                  ## stemming
text3

# both text2 & text3 are the same
# both output: Tokens consisting of 406,319 documents and 4 docvars.



# Construct the document-feature matrix (DTM) based on the tokenised text
dtm<- dfm(text3)  #we use text3 because we want to use a cleaned corpus
dtm

# output: Document-feature matrix of: 406,319 documents, 1,735 features (99.77% sparse) and 4 docvars.


# filtering DTM

dtm1 <- corp_retail_dat |>
  tokens(remove_punct = T, remove_numbers = T, remove_symbols = T) |>   
  tokens_tolower() |>                                                    
  tokens_remove(stopwords('en')) |>                                     
  tokens_wordstem() |>
  dfm()
dtm1


dtm2<- dfm_trim(dtm1, min_termfreq = 10) # we wanna trim the frequency of the word hat occues less than 10 times
dtm2 #after we trim it,our features reduce to 1572


# --------------------- TEXT ANALYSIS --------------------------------

# using quanteda.textplot & quanteda.textstats package for wordcloud

textplot_wordcloud(dtm2, max_words = 100, color = c('blue','darkorange'))  #top 100 most frequency used words

textstat_frequency(dtm2, n=20)    # word frequencies


# now we look at the document where country is "United Kingdom", we focus on UK because it has the highest retail quantity demanded in the dataset
#docvars is we get a dataframe with the document variables, in this case we get dtm2 document of variable Country where the value is "United Kingdom"
# its like a subset

#choosing UK as the highest product demand country
is_UK <- docvars(dtm2)$Country == 'United Kingdom'  
UK_dtm<- dtm2[is_UK,]
textplot_wordcloud(UK_dtm, max_words = 100, color = c('darkgreen','black')) 
textstat_frequency(UK_dtm, n=20) 

#choosing singapore as the lowest product demand country
is_SG <- docvars(dtm2)$Country == 'Singapore'  
SG_dtm<- dtm2[is_SG,]
textplot_wordcloud(SG_dtm, max_words = 100, color = c('darkmagenta','black')) 
textstat_frequency(SG_dtm, n=20) 


#using textstat_keyness  (for uk)
ts_keyness_UK<- textstat_keyness(dtm2, is_UK)
head(ts_keyness_UK,5)
tail(ts_keyness_UK,5)

# using textplot_keyness   (for uk)
textplot_keyness(ts_keyness_UK)
# we might need to remove the word postag, because it looks like an outlier

#from here, we can say that product description with the word "heart" is mostly used in the UK, at around 31991 times(n_target)
# n reference shows how many other countries that has the word "heart" in the dataframe, in this case, there are only 1902 times of the word "heart" is used in the product description in other country
# meaning the product with the description "heart" is not as highly demanded in other country as compared to UK
# Chi2 means the probability of UK towards the product item with the word heart is high and observation is much more often as compare to 'sign' and others in the dataframe

# for tail, it is showed that postag is lowest, and woodland is 2nd lowest.
# the chi2 shows the relative frequency and its negative value means it is under-represented
# UK use the word "woodland" & "postag" relatively less often than in other country

#using textstat_keyness  (for sg)
ts_keyness_SG<- textstat_keyness(dtm2, is_SG)
head(ts_keyness_SG,5)
tail(ts_keyness_SG,5)

# using textplot_keyness   (for sg)
textplot_keyness(ts_keyness_SG)
# we might need to remove the word postag, because it looks like an outlier



# ------------ key-word in context --------------------
# just now we just see a per word analysis
# now we try to see the keyword in context
# a keyword-in-context listing shows a given keyword in the context of its use.
# note: the kwic() function requires a tokenized corpus object as input.

kwic_all_heart<- kwic(tokens(text3), "heart", window=5 )  #top used word in UK
head(kwic_all_heart,20)

kwic_all_pizza<- kwic(tokens(text3), "pizza", window=5 )   # top used word in SG
head(kwic_all_pizza,20)


# ---------- specific word deeper analysis ---------------

heart_kwic <- kwic(tokens(text3), 'heart*')
heart_corp <- corpus(heart_kwic)
heart_dtm <- heart_corp |>
  tokens(remove_punct = T, remove_numbers = T, remove_symbols = T) |>
  tokens_tolower() |>
  tokens_remove(stopwords('en')) |>
  tokens_wordstem() |>
  dfm()

textplot_wordcloud(heart_dtm, max_words=100, color = c("darkgreen", "black"))

# so these are the words that can be related to "heart" in the product description. 


pizza_kwic <- kwic(tokens(text3), 'pizza*')
pizza_corp <- corpus(pizza_kwic)
pizza_dtm <- pizza_corp |>
  tokens(remove_punct = T, remove_numbers = T, remove_symbols = T) |>
  tokens_tolower() |>
  tokens_remove(stopwords('en')) |>
  tokens_wordstem() |>
  dfm()

textplot_wordcloud(pizza_dtm, max_words=100, color = c("darkmagenta", "black"))


# so these are the words that can be related to "heart" in the product description. 



