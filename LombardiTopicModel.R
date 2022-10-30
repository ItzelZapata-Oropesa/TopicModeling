#set Working Directory
#setwd("~/Desktop/Dr. Friedmans Project")

#libraries
library(readxl)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(topicmodels)
library(stm)
library(ldatuning)
library(knitr)
library(tm)
library(lubridate)
library(kableExtra)
library(BTM)
library(textplot)
library(concaveman)
library(udpipe)
library(data.table)
library(stopwords)
library(quanteda)
#install.packages("LDAvis")
library(LDAvis)
#install.packages("servr")
library(servr)


#Reading excel file into R
articles <- read_excel("Mark_Lombardi_study- citation_summary.xlsx", col_names = TRUE)

articles <- articles[ , -c(7:10)]

#creating tidytext Corpus

tidyArticles <- articles %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, Title, drop = FALSE) %>%
  anti_join(stop_words, by = "word")

tidy_top_terms <- tidyArticles %>%
  count(word, sort = TRUE)
tidy_top_terms

#stemming

tidyArticles <- tidyArticles %>% 
  mutate(word = wordStem(word))

#Document Term Matrix

tidyArticle_DTM <- tidyArticles %>%
  count(Title, word) %>%
  cast_dtm(Title, word, n)

tidyArticle_DTM

#Tokenize
articles_bigrams <- articles %>%
  unnest_tokens(bigrams, Title, token = "ngrams", n = 2)

articles_bigrams

#Remove stop words
articles_bigramsSeperate <- articles_bigrams %>%
  separate(bigrams, c("word1", "word2"), sep = " ")

articles_bigramsSeperate

stopW <- stop_words

articles_bigramsSeperate <- articles_bigramsSeperate %>%
  filter(!word1 %in% stopW$word) %>%
  filter(!word2 %in% stopW$word)

articles_bigramsSeperate

#checking for N/A's
is.na(articles_bigramsSeperate$word1)
sum(is.na(articles_bigramsSeperate$word1))
articles_bigramsSeperate[209, ] #Whole row is empty, we need to remove NAs 

articles_bigramsSeperate <- articles_bigramsSeperate %>%
  filter(!is.na(word1)) %>%
  filter(!is.na(word2))
sum(is.na(articles_bigramsSeperate$word1))
sum(is.na(articles_bigramsSeperate$word2))

#Re-combine the bigrams
articles_bigrams <- articles_bigramsSeperate %>%
  unite(bigram, word1, word2, sep = " ")

articles_bigrams

arBigramCount <- articles_bigrams %>%
  count(bigram, sort = TRUE)
arBigramCount


#Word Counts by Year

tidyArticles %>%
  group_by(Year) %>%
  count(word, sort = TRUE) %>%
  slice_max(word, n = 10) %>%
  ungroup %>%
  mutate(word = reorder_within(word, n, Year)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Year, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Count",
       x = "Unique Terms",
       title = "Most frequent Terms found in article titles",
       subtitle = "Stop words excluded")

#Bigram Word Count by Year

articles_bigrams %>%
  group_by(Year) %>%
  count(bigram, sort = TRUE) %>%
  slice_max(bigram, n = 10) %>%
  ungroup %>%
  mutate(word = reorder_within(bigram, n, Year)) %>%
  ggplot(aes(x = word, y = n, fill = bigram)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Year, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Count",
       x = "Unique Terms",
       title = "Most frequent Terms found in article titles",
       subtitle = "Stop words excluded")
  

#Finding K
k_metrics <- FindTopicsNumber(
  tidyArticle_DTM,
  topics = seq(5, 50, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL
)

FindTopicsNumber_plot(k_metrics)
  
#LDA Model
articlelda <- LDA(tidyArticle_DTM, 
               k = 10)

terms(articlelda, 5)


articlesDfm <- tidyArticles %>%
  count(Title, word, sort = TRUE) %>%
  cast_dfm(Title, word, n)


topicM <- stm(articlesDfm, K = 10, init.type = "Spectral")
summary(topicM)


tidyA <- tidy(topicM)
tidyA

tidyA %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(x = term, y = beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Count",
       x = "Unique Terms",
       title = "Most frequent Terms found in article titles",
       subtitle = "Stop words excluded")

art <- textProcessor(articles$Title,
                     metadata = articles, 
                     lowercase = TRUE,
                     removenumbers = TRUE,
                     removepunctuation = TRUE,
                     removestopwords = TRUE,
                     wordLengths = c(3, Inf),
                     stem = TRUE,
                     striphtml = TRUE,
                     onlycharacter = FALSE,
                     customstopwords = NULL)
docs <- art$documents
meta <- art$meta
vocab <- art$vocab

artStm <- stm(documents = docs,
              data = meta,
              vocab = vocab,
              prevalence = ~ Year,
              K =10,
              max.em.its = 25,
              verbose = FALSE)

toLDAvis(mod = artStm, docs = docs)
