#install packages

install.packages("SnowballC")
install.packages("NLP")
install.packages("tm")
install.packages("syuzhet")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("RSentiment")
install.packages("DT")

#load and use the packages
library(SnowballC)
library(NLP)
library(tm)
library(syuzhet)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(RSentiment)
library(DT)

#get and set working directory
getwd()
setwd("C:\\Users\\liuch\\Desktop\\assignment\\DS6501 Assignment1\\Political Parties")

######## Green Party ########
#load dataset
tweets.greens <- read.csv("NZGreens_tweets.csv")

#verify the data
head(tweets.greens)
tail(tweets.greens)

############ data pre-processsing ########
#display the "text" column
head(tweets.greens$text)

#use gsub function to remove URL
tweets.greens$text <- iconv(tweets.greens$text, "UTF-8", "UTF-8", sub = "byte")
tweets.greens$text <- gsub("http.*","",tweets.greens$text)
tweets.greens$text <- gsub("https.*","",tweets.greens$text)
tweets.greens$text <- gsub("[\\]+.*","",tweets.greens$text)
tweets.greens$text <- gsub("#","",tweets.greens$text)
tweets.greens$text <- gsub("@.*","",tweets.greens$text)
tweets.greens$text <- gsub("…","",tweets.greens$text)

head(tweets.greens$text)

#remove punctuation and numbers
tweets.greens$text <- gsub("[[:punct:]]", "", tweets.greens$text)
tweets.greens$text <- gsub("[[:digit:]]+", "", tweets.greens$text)

head(tweets.greens$text)
greens.df <- tweets.greens$text #store the cleaned data to a new variable

############ set sentiment score ########
#convert the clean data to a vector using as.vector
word.greens <- as.vector(greens.df)

#use get_nrc_sentiment function to generate score for each tweet
emotion.greens <- get_nrc_sentiment(word.greens)

#join these scores together with the original tweets to check the score of each tweet's emotion
emotion.greens2 <- cbind(greens.df, emotion.greens)

View(emotion.greens2)

#use get_sentiment function to extract sentiment score and store positive, negative, and neutral tweets
sent.value <- get_sentiment(word.greens)
postitive.greens <- word.greens[sent.value > 0]
negative.greens <- word.greens[sent.value < 0]
neutral.greens <- word.greens[sent.value == 0]

head(postitive.greens)
head(negative.greens)
head(neutral.greens)

############ display the most positive and negative tweets ########
#most positive (maxium/highest sent.value) and negative tweets(minium/lowest sent.value)
most.positive.greens <- word.greens[sent.value == max(sent.value)]
most.positive.greens
most.negative.greens <- word.greens[sent.value == min(sent.value)]
most.negative.greens

#combine the tweets with their scores, can check if the most positive/negative tweet is correct
sent.value_greens <- cbind(greens.df, sent.value)
sent.value_greens

############ plot a pie chart of sentiment scores  ########
#generate a pie to show the proportion of positive, negative, and neutral
#count the number of positive, negative and neutral tweets
pos <- length(postitive.greens)
neg <- length(negative.greens)
neut <- length(neutral.greens)

#combine the count number, define label, then plot a pie chart
score.count <- c(pos, neg, neut)
label <- c("Positive", "Negative", "Neutral")#text
pie(score.count, label, main= "Sentiment Analysis of Greens Party", col = (rainbow(length(score.count))))


#create a bar chart and use text() funtion to display the exact number of each sentiment
greens.bar <- barplot(colSums(emotion.greens),las=2,col = brewer.pal(10, "Spectral"),
        ylab = "Count", main = "Sentiment Scores of Greens Party")
text(x=greens.bar,y=colSums(emotion.greens), labels=colSums(emotion.greens), pos = 3, cex = 0.8, col = "black")



#create a table   
greens.table <- data.frame(Sentiment = names(colSums(emotion.greens)), Count = colSums(emotion.greens))
DT::datatable(greens.table)


############ create a TDM ########
#create a corpus from the cleaned tweets: greens.df 
greens_corpus <- Corpus(VectorSource(greens.df))

#create a TDM
tdm <- TermDocumentMatrix(greens_corpus, 
                          control = list(removePunctuation=TRUE, 
                                         wordLengths=c(5, 15), 
                                         stopwords = c("green", "party","greens","government", "kiwis", "today", "zealand", stopwords("english")), 
                                         removeNumbers=TRUE, 
                                         tolower=TRUE))
tdm
#define TDM as matrix to calculate word frequency
tdm.matrix <- as.matrix(tdm)

#sort word frequency in decreasing order
word_freq <- sort(rowSums(tdm.matrix), decreasing = TRUE)

#create a data frame with frequency
dm <- data.frame(word=names(word_freq), freq=word_freq)

############ create a word cloud by using the TDM and display the most frequently occurring word ########
#plot a word cloud whit words what appear about 50 times
wordcloud(dm$word, dm$freq, min.freq = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

warnings() # this can help me to filter some stop words

#display the most frequently occurring word
head(word_freq)
findAssocs(tdm, "public", 0.3)# a correlation of 0.6 and above does not have any words displayed 
findAssocs(tdm, "covid", 0.6)
findAssocs(tdm, "people", 0.4)
findAssocs(tdm, "price", 0.6)


######## Labour Party & National Party  ########
#will do these two parties together, since detailed steps have been done above

#load and verify dataset
tweets.labour <- read.csv("nzlabour_tweets.csv")
head(tweets.labour)
tail(tweets.labour)

tweets.national <- read.csv("NZNationalParty_tweets.csv")
head(tweets.national)
tail(tweets.national)

############## data pre-processsing ############# 
#labour: remove URL, punctuation and numbers
labour.df <- tweets.labour$text 
labour.df <- iconv(labour.df, "UTF-8", "UTF-8", sub = "byte")
labour.df <- gsub("http.*","",labour.df)
labour.df <- gsub("https.*","",labour.df)
labour.df <- gsub("[\\]+.*","",labour.df)
labour.df <- gsub("#","",labour.df)
labour.df <- gsub("@.*","",labour.df)
labour.df <- gsub("…","",labour.df)
labour.df <- gsub("[[:punct:]]", "", labour.df)
labour.df <- gsub("[[:digit:]]+", "", labour.df)
head(labour.df)

#national: remove URL, punctuation and numbers
national.df <- tweets.national$text 
national.df <- iconv(national.df, "UTF-8", "UTF-8", sub = "byte")
national.df <- gsub("http.*","",national.df)
national.df <- gsub("https.*","",national.df)
national.df <- gsub("[\\]+.*","",national.df)
national.df <- gsub("#","",national.df)
national.df <- gsub("@.*","",national.df)
national.df <- gsub("…","",national.df)
national.df <- gsub("[[:punct:]]", "", national.df)
national.df <- gsub("[[:digit:]]+", "", national.df)
head(national.df)

############## set sentiment score ############# 
#convert the clean data to a vector using as.vector
word.labour <- as.vector(labour.df)
word.national <- as.vector(national.df)

#use get_nrc_sentiment function to generate score for each tweet
emotion.labour <- get_nrc_sentiment(word.labour)
emotion.national <- get_nrc_sentiment(word.national)

#join these scores together with the original tweets to check the score of each tweet's emotion
emotion.labour2 <- cbind(labour.df, emotion.labour)
emotion.national2 <- cbind(national.df, emotion.national)

View(emotion.labour2)
View(emotion.national2)

#use get_sentiment function to extract sentiment score and store positive, negative, and neutral tweets
sent.value <- get_sentiment(word.labour)
postitive.labour <- word.labour[sent.value > 0]
negative.labour <- word.labour[sent.value < 0]
neutral.labour <- word.labour[sent.value == 0]
head(postitive.labour)
head(negative.labour)
head(neutral.labour)

sent.value <- get_sentiment(word.national)
postitive.national <- word.national[sent.value > 0]
negative.national <- word.national[sent.value < 0]
neutral.national <- word.national[sent.value == 0]
head(postitive.national)
head(negative.national)
head(neutral.national)

##############  display the most positive and negative tweets ############# 
most.positive.labour <- word.labour[sent.value == max(sent.value)]
most.positive.labour
most.negative.labour <- word.labour[sent.value == min(sent.value)]
most.negative.labour

sent.value_labour <- cbind(labour.df, sent.value)
sent.value_labour


most.positive.national <- word.national[sent.value == max(sent.value)]
most.positive.national
most.negative.national <- word.national[sent.value == min(sent.value)]
most.negative.national

sent.value_national <- cbind(national.df, sent.value)
sent.value_national


##############  plot a pie chart of sentiment scores ############# 
#labour party
#count the number of positive, negative and neutral tweets
pos <- length(postitive.labour)
neg <- length(negative.labour)
neut <- length(neutral.labour)

#combine the count number, define label, then plot a pie chart
score.count <- c(pos, neg, neut)
label <- c("Positive", "Negative", "Neutral")
pie(score.count, label, main= "Sentiment Analysis of Labour Party", col = (rainbow(length(score.count))))

#create a bar chart
labour.bar <- barplot(colSums(emotion.labour),las=2,col = brewer.pal(10, "Spectral"),
                      ylab = "Count", main = "Sentiment Scores of Labour Party")
text(x=labour.bar,y=colSums(emotion.labour), labels=colSums(emotion.labour), pos = 3, cex = 0.8, col = "black")

#create a table 
labour.table <- data.frame(Sentiment = names(colSums(emotion.labour)), Count = colSums(emotion.labour))
DT::datatable(labour.table)


#national party
#count the number of positive, negative and neutral tweets
pos <- length(postitive.national)
neg <- length(negative.national)
neut <- length(neutral.national)

#combine the count number, define label, then plot a pie chart
score.count <- c(pos, neg, neut)
label <- c("Positive", "Negative", "Neutral")#text
pie(score.count, label, main= "Sentiment Analysis of National Party", col = (rainbow(length(score.count))))

#create a bar chart
national.bar <- barplot(colSums(emotion.national),las=2,col = brewer.pal(10, "Spectral"),
                      ylab = "Count", main = "Sentiment Scores of national Party")
text(x=national.bar,y=colSums(emotion.national), labels=colSums(emotion.national), pos = 3, cex = 0.8, col = "black")

#create a table 
national.table <- data.frame(Sentiment = names(colSums(emotion.national)), Count = colSums(emotion.national))
DT::datatable(national.table)

##############  create TDM and word cloud ############# 
#labour party
#create a corpus from the cleaned tweets 
labour_corpus <- Corpus(VectorSource(labour.df))
#create a TDM
tdm.labour <- TermDocumentMatrix(labour_corpus, 
                          control = list(removePunctuation=TRUE, 
                                         wordLengths=c(5, 15), 
                                         stopwords = c("kiwis", "today", "zealand", "party","labour","government", stopwords("english")), 
                                         removeNumbers=TRUE, 
                                         tolower=TRUE))
tdm.labour
#define TDM as matrix to calculate word frequency
tdm.matrix.labour <- as.matrix(tdm.labour)
#sort word frequency in decreasing order
word_freq.labour <- sort(rowSums(tdm.matrix.labour), decreasing = TRUE)
#create a data frame with frequency
dm.labour <- data.frame(word=names(word_freq.labour), freq=word_freq.labour)

#plot a word cloud whit words what appear about 50 times
wordcloud(dm.labour$word, dm.labour$freq, min.freq = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
warnings()
#display the most frequently occurring word
head(word_freq.labour)
findAssocs(tdm.labour, "people", 0.45)# a correlation of 0.45 and above does not have any words displayed 
findAssocs(tdm.labour, "support", 0.35)
findAssocs(tdm.labour, "covid", 0.45)
findAssocs(tdm.labour, "boost", 0.45)
findAssocs(tdm.labour, "families", 0.35)

#national party
#create a corpus from the cleaned tweets 
national_corpus <- Corpus(VectorSource(national.df))
#create a TDM
tdm.national <- TermDocumentMatrix(national_corpus, 
                                 control = list(removePunctuation=TRUE, 
                                                wordLengths=c(5, 15), 
                                                stopwords = c("kiwis", "today", "zealand", "party","national","nationals", "government","governments", stopwords("english")), 
                                                removeNumbers=TRUE, 
                                                tolower=TRUE))
tdm.national
#define TDM as matrix to calculate word frequency
tdm.matrix.national <- as.matrix(tdm.national)
#sort word frequency in decreasing order
word_freq.national <- sort(rowSums(tdm.matrix.national), decreasing = TRUE)
#create a data frame with frequency
dm.national <- data.frame(word=names(word_freq.national), freq=word_freq.national)

#plot a word cloud whit words what appear about 50 times
wordcloud(dm.national$word, dm.national$freq, min.freq = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
warnings()

#display the most frequently occurring word
head(word_freq.national)
findAssocs(tdm.national, "labours", 0.3) 
findAssocs(tdm.national, "labour", 0.3)
findAssocs(tdm.national, "living", 0.5)
findAssocs(tdm.national, "emissions", 0.6)
findAssocs(tdm.national, "inflation", 0.6)


#in the word cloud above, I find out that stemming should be used to stem works like "labour" and "labours"
#first convert data.frame to corpus by using VectorSource() and Corpus(), then stem
national_corpus <- Corpus(VectorSource(dm.national$word))
national_corpus <- tm_map(national_corpus, stemDocument)

#create a DTM using corpus, then generate word cloud
dtm_national <- DocumentTermMatrix(national_corpus, control = list(wordLengths=c(5, 15), stopwords = c("kiwis", "today", "zealand", "party","national","nationals", "government","governments")))
freq <- colSums(as.matrix(dtm_national))

wordcloud(names(freq),freq, min.freq=50, random.order = FALSE, colors = brewer.pal(8, "Dark2"))





