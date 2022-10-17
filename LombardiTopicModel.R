#set Working Directory
setwd("~/Desktop/Dr. Friedmans Project")

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


#Reading excel file into R
articles <- read_excel("Mark_Lombardi_study- citation_summary.xlsx", col_names = TRUE)

articles <- articles[ , -c(7:10)]

#creating tidytext Corpus

tidyArticles <- articles %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, Title, drop = FALSE) %>%
  anti_join(stop_words)

tidy_top_tokens <- tidyArticles %>%
  count(word, sort = TRUE)
tidy_top_tokens

#stemming

tidyArticles <- tidyArticles %>% 
  mutate(word = wordStem(word))

#Document Term Matrix

tidyArticle_DTM <- tidyArticles %>%
  count(Title, word) %>%
  cast_dtm(Title, word, n)

tidyArticle_DTM

# #Tokenize
# 
# tds_bigrams <- articles %>%   
#   unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2)
# 
# tds_bigrams <- tds_bigrams %>% 
#   separate(bigram, into = c("word1", "word2"), sep = " ") %>%
#   filter(!word1 %in% stop_words$word) %>%
#   filter(!word2 %in% stop_words$word) %>%
#   mutate(word1 = wordStem(word1)) %>% 
#   mutate(word2 = wordStem(word2)) %>% 
#   unite(bigram, c(word1, word2), sep = " ")

# bigram_top_tokens <- tds_bigrams %>% 
#   count(bigram, sort = TRUE) %>% 
#   top_n(10)
# 
# bigram_top_tokens

#Word Counts by Year

tidyArticles %>%
  group_by(Year) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder_within(word, n, Year)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Year, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0))


# Determining K
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
  
