
#
#
# Load and Install required packages
#
#
#



#install.packages('rtweet')
#install.packages('igraph')
#install.packages('tidytext')
#install.packages("tidytext")
#install.packages("tm")
#install.packages("textclean")

library(rtweet)
library(tidytext)
library(dplyr)
library(tidyr)
library(tm)
library(ggplot2)
library(ggthemes)




api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""


create_token(
  app = "BCU_Data_Viz",
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret)

since <- '2019-04-09'
until <- '2019-05-09'


mueller.tweets <- search_tweets("#muellerreport OR #mueller", n=10000, lang = "en", since = since, until = until,
                                 retryonratelimit = TRUE, type = 'mixed', include_rts = F)

ts_plot(mueller.tweets)



mueller.tweets.raw <- mueller.tweets

# Save data
setwd("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Social_Network/")
write_as_csv(mueller.tweets.raw, "mueller.tweets.raw.csv")

#
#
# Cleaning Twitter Data for Mueller Report Hashtags
#
#
#

# Clean Text using corpus tools
mueller.tweets$text.cl <- gsub("http.*","",  mueller.tweets$text)
mueller.tweets$text.cl <- gsub("https.*","", mueller.tweets$text.cl)

docs <- mueller.tweets$text.cl
docs2 <- textclean::replace_non_ascii(docs, replacement = "")

text.cl<- VCorpus(VectorSource(docs2))

text.cl<-tm_map(text.cl,content_transformer(tolower))
text.cl<-tm_map(text.cl, removePunctuation)
text.cl<-tm_map(text.cl, removeWords, stopwords("english"))
text.cl<-tm_map(text.cl, stemDocument, language = "english") # Will this change sentiment results?
text.cl<-tm_map(text.cl, stripWhitespace)
text.cl<-tm_map(text.cl, removeNumbers)

# Bring back into tidytext dataframe
text.cl.char<- lapply(text.cl, as.character)
text.cl.char.cl <- unlist(text.cl.char, use.names = F)

mueller.tweets$text.cl <- text.cl.char.cl

mueller.tweets.word <- mueller.tweets %>%
  select(text.cl) %>%
  unnest_tokens(word, text.cl)

words.remove <- data.frame(c("muellerreport", "mueller", "s", "amp", "rt"))
names(words.remove) <- c('word')

mueller.tweets.word.cl <- mueller.tweets.word %>%
  anti_join(words.remove)

#
#
#
# Descriptive Analysis 
#
#
#

# Plot of most popular words
mueller.tweets.word.cl %>%
  filter(word != "") %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "",
       y = "Count",
       title = "Count of unique words found in tweets") + 
  theme_tufte(base_size = 18)


# Ngrams Paired words 

mueller.paired.words <- mueller.tweets %>%
  dplyr::select(text.cl) %>%
  unnest_tokens(paired_words, text.cl, token = "ngrams", n = 2)

mueller.paired.words %>%
  count(paired_words, sort = TRUE)

mueller.paired.sep.words <- mueller.paired.words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

mueller.paired.words %>%
  count(paired_words, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(paired_words, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of paired words found in tweets") + 
  theme_tufte()

# Save clean data
write.csv(mueller.tweets.word.cl, file = "mueller.unigram.csv")
write.csv(mueller.paired.words, file = "mueller.biigram.csv")


#
#
#
# Sentiment Analysis 
#
#
#

## Bag of Words with NRC

nrc <- get_sentiments("nrc")
table(nrc$sentiment)

mueller.tweet.sent.nrc <- mueller.tweets.word.cl %>% 
  inner_join(nrc) 

# Visualize
mueller.tweet.sent.nrc %>% 
  group_by(sentiment) %>% 
  summarise(Count = n()) %>% 
  mutate(sentiment = reorder(sentiment, Count)) %>% 
  filter(sentiment != c('positive', 'negative')) %>% 
  ggplot(aes(x = sentiment, y = Count)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Emotions related to Mueller Report")

# Closer look at words included in Trust
mueller.tweet.sent.nrc %>% 
  filter(sentiment == 'trust') %>% 
  group_by(word) %>% 
  summarise(Count = n()) %>% 
  mutate(word = reorder(word, Count)) %>% 
  top_n(10) %>% 
  ggplot(aes(x = word, y = Count)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Trust Words") # Trust words are all government related words, but this is probably not trustworthy for the way we are thinking. 

## Remove words which may be mislabeled government words for Trust
words.remove <- data.frame(c("congress", "president", "attorney", "counsel", "general", "law"))
names(words.remove) <- c('word')

mueller.tweets.word.cl2 <- mueller.tweets.word.cl %>%
  anti_join(data.frame(words.remove))

mueller.tweet.sent.nrc <- mueller.tweets.word.cl2 %>% 
  inner_join(nrc)

mueller.tweet.sent.nrc %>% 
  group_by(sentiment) %>% 
  summarise(Count = n()) %>% 
  mutate(sentiment = reorder(sentiment, Count)) %>% 
  filter(sentiment != c('positive', 'negative')) %>% 
  ggplot(aes(x = sentiment, y = Count)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Emotions related to Mueller Report")

mueller.tweet.sent.nrc %>% 
  filter(sentiment == 'fear') %>% 
  group_by(word) %>% 
  summarise(Count = n()) %>% 
  mutate(word = reorder(word, Count)) %>% 
  top_n(10) %>% 
  ggplot(aes(x = word, y = Count)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Fear Words")

mueller.tweet.sent.nrc %>% 
  group_by(sentiment) %>% 
  summarise(Count = n()) %>% 
  mutate(sentiment = reorder(sentiment, Count)) %>% 
  filter(sentiment == c('positive', 'negative')) %>% 
  ggplot(aes(x = sentiment, y = Count)) +
  geom_col() +
  xlab(NULL) +
  labs(x = "Count",
       y = "Unique words",
       title = "NRC Sentiment Mueller Report")

# Wordcloud of difference between NRC cats

install.packages("wordcloud")
library(wordcloud)

mueller.sent.nrc.compare <- mueller.tweet.sent.nrc %>% 
  filter(sentiment == 'fear' | sentiment == 'trust') %>% 
  count(word, sentiment, sort = T) %>% 
  filter(n > 20)

mueller.sent.nrc.compare %>% 
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("#e15759", "#76b7b2"), max.words = 100, 
                   match.colors = T, scale = c(5, 0.7))


## Bag of Words with Bing
mueller.tweet.sent.bing <- mueller.tweets.word.cl %>% 
  inner_join(get_sentiments('bing'))

# Visualize
mueller.tweet.sent.bing %>% 
  group_by(sentiment) %>% 
  summarise(Count = n()) %>% 
  mutate(sentiment = reorder(sentiment, Count)) %>% 
  ggplot(aes(x = sentiment, y = Count)) +
  geom_col() +
  xlab(NULL) +
  labs(x = "Sentiment",
       y = "Count",
       title = "Bing Sentiment Mueller Report")

## Bag of Words with afinn
mueller.tweet.sent.afinn <- mueller.tweets.word.cl %>% 
  inner_join(get_sentiments('afinn'))

# Visualize
mueller.tweet.sent.afinn %>% 
  group_by(score) %>% 
  summarise(Count = n()) %>% 
  mutate(score = reorder(score, as.numeric(score))) %>% 
  ggplot(aes(x = score, y = Count)) +
  geom_col() +
  xlab(NULL) +
  labs(x = "Sentiment",
       y = "Count",
       title = "Emotions related to Mueller Report")

# Save clean data
write.csv(mueller.tweet.sent.nrc, file = "mueller.sent.nrc.csv")
write.csv(mueller.tweet.sent.afinn, file = "mueller.sent.afinn.csv")
write.csv(mueller.tweet.sent.bing, file = "mueller.sent.bing.csv")

# Reverse AFINN scores by removing common negation words 

negation.words <- c("not", "no", "never", "without", "unlike", "untrue", "false")

negated.scores.afinn.list <- mueller.paired.sep.words %>% 
  filter(word1 %in% negation.words) %>% 
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>% 
  count(word1, word2, score, sort = T) %>% 
  ungroup()

negated.scores.afinn.list %>% 
  mutate(word2 = reorder(word2, n)) %>% 
  ggplot(aes(word2, n)) +
  geom_col() + 
  coord_flip() +
  facet_wrap(~word1) +
  labs(x = "Count",
       y = "Word",
       title = "Common Positive or Negative Words to Follow a Negation")


# Switch score for negated words
negated.scores.afinn <- mueller.paired.sep.words %>% 
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>% 
  mutate(score.cl = ifelse(word1 %in% negation.words, score * -1, score))

negated.scores.afinn %>% 
  group_by(score.cl) %>% 
  summarise(Count = n()) %>% 
  mutate(score.cl = reorder(score.cl, as.numeric(score.cl))) %>% 
  ggplot(aes(x = score.cl, y = Count)) +
  geom_col() +
  xlab(NULL) +
  labs(x = "Sentiment",
       y = "Count",
       title = "Emotions related to Mueller Report")


write.csv(negated.scores.afinn, file = "mueller.sent.afinn.cl.csv")

# Bigram network

library(igraph)
#install.packages("ggraph")
library(ggraph)

bigram_counts_gr <- mueller.paired.sep.words %>% 
  filter(word1 != "amp") %>% 
  filter(word2 != "amp") %>%
  group_by(word1, word2) %>% 
  summarise(Count = n()) %>% 
  filter(Count > 85) %>% 
  graph_from_data_frame()




set.seed(005)

a <- grid::arrow(type = "closed", length = unit(.08, "inches"))

ggraph(bigram_counts_gr, layout = "fr") +
  geom_edge_link(arrow = a, colour = 'grey', 
                 end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = .6*(degree(bigram_counts_gr, mode="total"))) + 
  geom_node_text(aes(label = name), vjust = 0, hjust = 1) +
  labs(x = "",
       y = "",
       title = "Common Bigrams in Mueller Report Tweets") +
  theme_void()













