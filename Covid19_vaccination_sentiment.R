library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)
library(aws.comprehend)
library(dplyr)

# accessKeys.csv == the CSV downloaded from AWS containing your Access & Secret keys
keyTable <- read.csv("your Access & Secret key.csv", header = T)

# Extract keys from access key table and assign to R environment variables
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

# Set up your AWS credentials and region for activation
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 

#Note: Replace below with your credentials following above reference
api_key <- "xxx"
api_secret <- 'xxx'
access_token <- "xxx"
access_token_secret <- "xxx"
#Note: This will ask us permission for direct authentication, type '1' for yes:
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# extracting 5000 tweets 
tweets <- searchTwitter("#covidvaccine", n=5000, lang="en")
n.tweet <- length(tweets)
tweetsDf <- twListToDF(tweets)

tweets.txt <- sapply(tweets, function(t)t$getText())
# Ignore graphical Parameters to avoid input errors
tweets.txt <- str_replace_all(tweets.txt,"[^[:graph:]]", " ")

tweetsDf$text <- gsub("http.*","", tweetsDf$text) # Remove http
tweetsDf$text <- gsub("https.*","", tweetsDf$text) # Remove https
tweetsDf$text <- gsub("RT","", tweetsDf$text)  # remove RT
tweetsDf$text <- gsub("rt","", tweetsDf$text)  # remove rt
tweetsDf$text <- gsub("[[:punct:]]","", tweetsDf$text) # remove punctuation
tweetsDf$text <- gsub("^ ","",tweetsDf$text)   # remove blank spaces at the beginning
tweetsDf$text <- enc2native(tweetsDf$text) # Covnert emojis to native encoding
tweetsDf$text <- gsub("<.*.>", "", tweetsDf$text)  # remove tabs
tweetsDf$text <- gsub("@\\w+", "", tweetsDf$text) #remove at
tweetsDf$text <- gsub("http\\w+", "", tweetsDf$text) #remove links
tweetsDf$text <- trimws(tweetsDf$text) # Remove leading whitespaces from the beginning
tweetsDf$text <- plain_tweets(tweetsDf$text) #rtweets function for cleaning the tweets
tweetsDf <- tweetsDf %>% filter(nchar(text)!=0) # remove blank rows


tweetsDf %<>% mutate( created = created %>% 
      # Remove zeros.
      str_remove_all(pattern = '\\+0000') %>%
      # Parse date.
      parse_date_time(orders = '%y-%m-%d %H%M%S') )

tweetsDf %<>% 
  mutate(Created_At_Round = created%>% round(units = 'hours') %>% as.POSIXct())

tweetsDf %>% pull(created) %>% min()


p1 <- tweetsDf %>% 
  dplyr::count(Created_At_Round) %>% 
  ggplot(mapping = aes(x = Created_At_Round, y = n)) +
  theme_light() +
  geom_line() +
  xlab(label = 'Date') +
  ylab(label = NULL) +
  ggtitle(label = 'Number of Tweets per Hour')
p1 %>% ggplotly()


sentiment_func <- function(x){
  
  sentiment_type = detect_sentiment(x) %>% pull(Sentiment)
  return(sentiment_type)
}

sentiment_data <- data.frame(unlist(unname(lapply(tweetsDf$text,sentiment_func))))
sentiment_df <- cbind(tweetsDf$text,sentiment_data)
colnames(sentiment_df) <- c("tweet","sentiment")

# Visualization for sentiments of tweets
sentiment_df %>%
  group_by(sentiment) %>%
  summarize(count =n()) %>%
  ggplot(aes(x = sentiment, y = count, fill = sentiment, label = count)) +
  geom_col() + geom_text(vjust = -0.5) +
  ggtitle("Tweets by Sentiments")


entity <- function(row, df) {
  record <- tweetsDf[row,]
  entities <- detect_entities(as.character(record$text))   # Get sentiment from Amazon's Comprehemd
  merged <- merge(entities, record) # Merge the sentiment result to the original data
  return (merged)}
row_seq <- seq(1,nrow(tweetsDf)) # Define argument for lapply
entities <- lapply(row_seq, entity, df=tweetsDf)
entities_df <- rbindlist(entities, fill = TRUE) # Append list of dataframes together

entities_df %>%
  filter(!is.na(Text)) %>%
  filter(!Type == "QUANTITY") %>%
  filter(!Type == "DATE") %>%
  filter(!Text == '@') %>%
  filter(!Text == '/') %>%
  group_by(Text) %>%
  summarize(count = n()) %>%
  arrange(-count) %>% 
  head(10) %>%
  ggplot(aes(x = count, y = Text, fill = Text)) +
  geom_col() + labs (title = "Most entities mentioned on Twitter")
