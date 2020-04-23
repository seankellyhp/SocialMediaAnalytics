
#
#
# Load and Install required packages
#
#
#

#install.packages("devtools")
#devtools::install_github("mkearney/nytimes")
#install.packages("jsonlite")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("stringr")

library(nytimes) # access news api
library(jsonlite) # web query
library(dplyr) # data cleaning
library(ggplot2) # data vis
library(lubridate) # data cleaning
library(tidyr) # data cleaning
#library(robotstxt)  # get robots.txt
library(httr)       # http requests
library(rvest)      # web scraping tools
library(stringr) # data cleaning


#
#
#
# Create helper functions
#
#
#

#id <- ""
api <- ''
#secret <- ""


## Function 1: Query NY Times 

nytime=function (keyword, year){
  searchQ=URLencode(keyword)
  url=paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,'&begin_date=',year,'0101&end_date=',year,'1231&api-key=',api,sep="") 
  #get the total number of search results
  initialsearch=fromJSON(url,flatten=T)
  maxPages=round((initialsearch$response$meta$hits/10)-1) 
  #try with the max page limit at 10
  
  maxPages=ifelse(maxPages>=10,10,maxPages)#create an empty data frame
  df=data.frame(id=as.numeric(), article_url=character(),created_time=character(),snippet=character(),headline=character(), section=character())
  
  #save search results into data frame
  
  for(i in 0:maxPages){
    #get the search results of each page
    
    nytSearch= fromJSON(paste0(url,"&page=",i),flatten=T)
    temp= data.frame(id=1:nrow(nytSearch$response$docs),
                     article_url=nytSearch$response$docs$web_url,
                     created_time=nytSearch$response$docs$pub_date,
                     snippet=nytSearch$response$docs$snippet,
                     headline=nytSearch$response$docs$headline.main, 
                     section=nytSearch$response$docs$section_name)
    df=rbind(df,temp)
    Sys.sleep(5) #sleep for 5 second
  }
  return(df)
}


## Function 2: Web Crawl to get text from NY Times articles 

get_article_body <- function (url) {
  
  # download article page
  response <- GET(url)
  
  # check if request was successful
  if (response$status_code != 200) return(NA)
  
  # extract html
  html <- content(x        = response, 
                  type     = "text", 
                  encoding = "UTF-8")
  
  # parse html
  parsed_html <- read_html(html)                   
  
  # define paragraph DOM selector
  selector <- "article#story div.StoryBodyCompanionColumn div p"
  
  # parse content
  parsed_html %>% 
    html_nodes(selector) %>%      # extract all paragraphs within class 'article-section'
    html_text() %>%               # extract content of the <p> tags
    str_replace_all("\n", "") %>% # replace all line breaks
    paste(collapse = " ")         # join all paragraphs into one string
}


# Get Data
mueller.articles=nytime('mueller+report',2019)


#
#
#
## Descriptive Analysis of Full Metadata 
#
#
#

# Clean data
mueller.articles.all <- mueller.articles %>% 
  separate(created_time, c("created_date", "time_stamp"), sep = "T") %>% 
  mutate(created_time.d = as.Date.factor(.$created_date, "%Y-%m-%d"))

# Visualize
mueller.articles.all %>% 
  group_by(created_time.d) %>% 
  summarize(Count = n()) %>% 
  ggplot(aes(x = created_time.d, y = Count)) +
  geom_line() +
  geom_point() +
  labs(x = "Date",
       y = "Num. of articles",
       title = "Mueller Report Articles on NY Times Website")

mueller.important.dates <- mueller.articles.all %>% 
  group_by(created_time.d) %>% 
  summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  top_n(5)

mueller.articles.all %>% 
  group_by(section) %>%
  summarize(count=n()) %>%
  mutate(percent=(count/sum(count))*100) %>%
  ggplot() + 
  geom_bar(aes(y=percent,x=section),stat="identity") + 
  coord_flip() +
  labs(x = "Percent",
       y = "Section",
       title = "Section of NY Times Website with Mueller Report Articles")

setwd("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Social_Network/")
write.csv(mueller.articles.all, file = "mueller.articles.all.csv")

#
#
#
# Sample Data and retrieve full article body from url for Text Analysis
#
#
#

# Subset using above information about important sections and dates
mueller.articles.cl <- mueller.articles.all %>% 
  filter(!grepl('video|interactive|podcasts|books|arts|briefing|reader', article_url)) %>% 
  filter(created_time.d >= as.Date("2019-03-22", "%Y-%m-%d"))

# Sample for 10 articles
set.seed(005)
mueller.articles.sample <- mueller.articles.cl %>%
  sample_n(10)

# Get URLs
mueller.articles.sample$article_url <- as.character(mueller.articles.sample$article_url)
url.list <- mueller.articles.sample$article_url
url.names <- basename(url.list)


## Get full article text

# initialize vector
mueller.articles.sample$body <- NA

# initialize progress bar
pb <- txtProgressBar(min     = 1, 
                     max     = nrow(mueller.articles.sample), 
                     initial = 1, 
                     style   = 3)

# loop through URLs and scrape full content of NY Times page
for (i in 1:nrow(mueller.articles.sample)) {
  mueller.articles.sample$body[i] <- get_article_body(mueller.articles.sample$article_url[i])
  setTxtProgressBar(pb, i)
  Sys.sleep(1)
}

# We now have full text and meta-data in the same dataframe 
View(mueller.articles.sample)


# Install and Load required packages 

#install.packages('tm')
#install.packages('topicmodels')
library(tm)
library(topicmodels)

#
#
#
# Clean Sample Article Text
#
#
#

# Get text of articles in a vector from reference
articles <- mueller.articles.sample$body

# Create corpus from vector
docs<-VCorpus(VectorSource(mueller.articles.sample$body))

# Clean corpus
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs, removePunctuation)
docs<-tm_map(docs, removeWords, stopwords("english"))
docs<-tm_map(docs, stemDocument, language = "english")
docs<-tm_map(docs, stripWhitespace)
docs<-tm_map(docs, removeNumbers)

dtm<-DocumentTermMatrix(docs)

#convert rownames to url names
rownames(dtm)<- url.names

#collapse matrix by summing over columns
freq<-colSums(as.matrix(dtm))

#length should be total number of terms
length(freq)

#create sort order (descending)
ord<-order(freq,decreasing=TRUE)

#List all terms in decreasing order of freq 
num.words <- freq[ord]

#
#
#
# Descriptive Analysis of Sample Articles
#
#
#

# Add to tidytext dataframe 
ny.articles.words <- lapply(docs, as.character)
ny.articles.words <- unlist(ny.articles.words, use.names = F)

mueller.articles.sample$body.cl <- ny.articles.words

mueller.articles.word <- mueller.articles.sample %>%
  select(body.cl) %>%
  tidytext::unnest_tokens(word, body.cl)

mueller.articles.word %>%
  filter(word != "") %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in articles")


mueller.articles.sample.group.words <- mueller.articles.sample %>% 
  mutate(url.name = basename(mueller.articles.sample$article_url)) %>% 
  select(url.name, body.cl) %>%
  tidytext::unnest_tokens(word, body.cl) 

mueller.articles.words.count <- mueller.articles.sample.group.words %>% 
  group_by(url.name, word) %>% 
  summarise(Count = n())

mueller.articles.total.words <- mueller.articles.sample.group.words %>% 
  group_by(url.name) %>% 
  summarise(Count = n())

sort.mueller.words <- mueller.articles.sample.group.words %>% 
  group_by(url.name, word) %>% 
  summarise(Count = n()) %>% 
  mutate(word = reorder(word, Count)) %>% 
  arrange(desc(url.name), desc(Count)) %>% 
  filter(Count >1) %>% 
  top_n(10)

# Bigram across articles

mueller.articles.paired.words <- mueller.articles.sample %>%
  dplyr::select(body.cl) %>%
  tidytext::unnest_tokens(paired_words, body.cl, token = "ngrams", n = 2)

mueller.articles.paired.words %>%
  count(paired_words, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(paired_words, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of paired words found in articles")

#
#
#
# Topic Modelling with Sample Articles
#
#
#

k <- 5
#ldaOut<-LDA(dtm,k,method="Gibbs") # Tries basic but did not give clear topics

#Set parameters for Gibbs sampling
burnin<-4000
iter<-2000
thin<-500
seed<-list(2004,6,64,10001,766)
nstart<-5
best<-TRUE

#Run LDA using Gibbs sampling
ldaOut<-LDA(dtm,k,method="Gibbs",control=list(nstart=nstart,
                                              seed=seed,
                                              best=best,
                                              burnin=burnin,
                                              iter=iter,
                                              thin=thin))

# Get output
ldaOut.terms<-as.matrix(terms(ldaOut,10))

ldaOut.terms.beta <-as.matrix(terms(ldaOut,10))

ldaOut.topics<-as.matrix(topics(ldaOut))
ldaOut.prob<-as.matrix(ldaOut@gamma)
ldaOut.terms.beta<-as.matrix(ldaOut@beta)
ldaOut.terms.beta<-ldaOut@beta
ldaOut.terms<-ldaOut@terms
topicProbabilities<-as.data.frame(ldaOut@gamma)

mueller.lda <- LDA(dtm, k=4, control = list(nstart=nstart,
                                            seed=seed,
                                            best=best))

# Tidy Output to test Vem - not as accurate

#library(tidytext)

mueller.topics <- tidy(mueller.lda, matrix = "beta")

mueller.topics.top <- mueller.topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

mueller.topics.top %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

mueller.topics.docs <- tidy(mueller.lda, matrix = "gamma")

mueller.topics.docs %>%
  #mutate(document = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), 100 * gamma)) +
  geom_boxplot() +
  facet_wrap(~ document)


write.csv(mueller.topics, file = "mueller.topics.csv")
write.csv(mueller.topics.docs, file = "mueller.topics.docs.csv")


# Visualize






## Now do it with bigrams/ something cool




#
#
#
# Text Summarization of Mueller Report
#
#
#

# The PDF of the full Mueller Report can be downloaded for free from the secure U.S. Department of Justice Website. They do not allow web scraping.
# Load the Mueller report pdf from your path

#install.packages('pdftools')
library(pdftools)

path <- "C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Social_Network/mueller-report.pdf"
mueller.text <- pdf_text(path)

mueller.text.cl <- strsplit(mueller.text, "\n")
#mueller.text.cl2 <- paste(mueller.text,collapse="\n")

## Clean text 

mueller.text.cl4 <- mueller.text.cl
mueller.total <- length(mueller.text.cl)

for(i in 1:mueller.total) {

# Remove 2 Header lines
mueller.text.cl4[[i]] <- mueller.text.cl4[[i]][-1]
mueller.text.cl4[[i]] <- mueller.text.cl4[[i]][-1]

# Remove Footer
footer.page <- length(mueller.text.cl4[[i]])
mueller.text.cl4[[i]] <- mueller.text.cl4[[i]][-footer.page]

# Remove sentence stop 
mueller.text.cl4[[i]] <- gsub("\r", "", mueller.text.cl4[[i]])

# Remove censor text
mueller.text.cl4[[i]] <- gsub("Harm to Ongoing", "", mueller.text.cl4[[i]])
}

mueller.text.cl5 <- unlist(mueller.text.cl4, recursive=TRUE)
mueller.text.cl6 <- paste(mueller.text.cl5,collapse=" ")


## Summarize 

#install.packages('textrank')

library(textrank)

#install.packages('udpipe')

library(udpipe)

tagger<-udpipe_download_model("english")
tagger<-udpipe_load_model(tagger$file_model)

#Apply tagger
mueller.tag<-udpipe_annotate(tagger,mueller.text.cl6)

#Convert to dataframe
mueller.tag.df<-as.data.frame(mueller.tag)

head(mueller.tag.df[,c("sentence_id","lemma","upos")],10)

sentences<-unique(mueller.tag.df[,c("sentence_id","sentence")])

#Use nouns, pronouns and adjectives to find important people and events in the report
terminology<-subset(mueller.tag.df,upos %in% c("NOUN","ADJ", "PROPN"))
terminology<-terminology[,c("sentence_id","lemma")]

head(terminology)

#install.packages("textreuse")
library(textreuse)

# Create a set of candidate solutions in a hash table to decrease computation needs for the large text file 

minhash <- minhash_generator(n = 10000, seed = 123456789)
candidates <- textrank_candidates_lsh(x = terminology$lemma, 
                                      sentence_id = terminology$sentence_id,
                                      minhashFUN = minhash, 
                                      bands = 500)

# Textrank algorithm with candidates
tr<-textrank_sentences(data=sentences, terminology=terminology, textrank_candidates = candidates)
names(tr)

s<-summary(tr,n=10,keep.sentence.order=TRUE)
cat(s,sep="\n")















