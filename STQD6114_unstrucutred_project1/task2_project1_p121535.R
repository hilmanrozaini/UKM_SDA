# WEB ANALYSIS

# ------------------------ 
library("tidyverse")
library("rvest")
library("xml2")
library("stringr")


#fiction books
for (page_result in seq(from=1, to=3, by=1)){
  #link
  link=paste("https://www.amazon.com/s?k=fiction+books&rh=n%3A283155%2Cp_72%3A1250224011&dc&ds=v1%3AUse%2FBPEB0ypr6nT2SEVRLjlT7HekanNIbHxMUzrf2I4&crid=2GVSE9YS1ZS7F&qid=1684041965&rnid=1250219011&sprefix=fiction+book%2Caps%2C359&ref=sr_nr_p_72_4")
  page=read_html(link)
  #book name
  book_name=page %>%
    html_nodes(".a-color-base.a-text-normal") %>%
    html_text()
  #book price
  book_price=page %>% html_nodes(".a-price-whole") %>%
    html_text()
  # reviews
  review=page %>% html_nodes(".aok-align-bottom") %>%
    html_text()
  
  # number of review written
  review_written=page%>% html_nodes(".s-link-style .s-underline-text")%>%
    html_text()
  
  # convert into dataframe
  
  fiction_dat<-data.frame(book_name,book_price,review,review_written)
  
}


# children books
for (page_result in seq(from=1, to=3, by=1)){
  #link
  link=paste("https://www.amazon.com/s?k=children+books&i=stripbooks&rh=n%3A4%2Cp_72%3A1250224011&dc&ds=v1%3AYUFA7xUglZ5UMVcxssioqteFgiyXhNpNX69drqBCBVU&qid=1684042237&rnid=1250219011&ref=sr_nr_p_72_4")
  page=read_html(link)
  #book name
  n_book_name=page %>%
    html_nodes(".a-color-base.a-text-normal") %>%
    html_text()
  #book price
  n_book_price=page %>% html_nodes(".a-price-whole") %>%
    html_text()
  # reviews
  n_review=page %>% html_nodes(".aok-align-bottom") %>%
    html_text()
  
  # number of review written
  n_review_written=page%>% html_nodes(".s-link-style .s-underline-text")%>%
    html_text()
  
  # convert into dataframe
  
  child_dat<-data.frame(n_book_name,n_book_price,n_review,n_review_written)
  
}

head(fiction_dat)
head(child_dat)

# analysis
# average book price
# average review rating
# highest & lowest book price (with name of book)


# data strucutre cleaning
str(fiction_dat)
fiction_dat$book_price<-as.numeric(fiction_dat$book_price)
fiction_dat[,4] <- str_replace_all(fiction_dat[,4], ",", "")    # remove commas in the number to convert into numeric
fiction_dat$review_written<-as.numeric(fiction_dat$review_written)  # convert into numeric
fiction_dat$review<- as.numeric(str_extract(fiction_dat$review, "[0-9]+"))

str(child_dat)
child_dat$n_book_price<- as.numeric(child_dat$n_book_price)
child_dat[,4] <- str_replace_all(child_dat[,4], ",", "")    # remove commas in the number to convert into numeric
child_dat$n_review_written<-as.numeric(child_dat$n_review_written)  # convert into numeric
child_dat$n_review<- as.numeric(str_extract(child_dat$n_review, "[0-9]+"))


# analysis
# 1) average book price, rating and number of rating
summary(fiction_dat)
summary(child_dat)

# 2) most expensive books & cheapest books
# fiction
head(fiction_dat%>% arrange(book_price),1) #cheapest fiction book ($)
head(fiction_dat%>% arrange(desc(book_price)),1)  # most expensive fiction books ($)

#child book
head(child_dat%>%arrange(n_book_price),1)  #cheapest child book 
head(child_dat%>%arrange(desc(n_book_price)),1)    # most expensive child book


# 3) top 3 with rating 5 books
# fiction book
head(fiction_dat%>% arrange(review))   # fiction lowest review
head(fiction_dat%>% arrange(desc(review)))   #fiction highest review

#child book
head(child_dat%>%arrange(n_review))     # child loweset review
head(child_dat%>%arrange(desc(n_review)),3)   # child highest review


# 4) top and lowest most written review books
# fiction book
head(fiction_dat%>% arrange(review_written))   # book with lowest review written
head(fiction_dat%>% arrange(desc(review_written)))  # book with highest review written

# child book
head(child_dat%>% arrange(n_review_written))    # book with lowest review written
head(child_dat%>% arrange(desc(n_review_written)))   #book with highest review written



