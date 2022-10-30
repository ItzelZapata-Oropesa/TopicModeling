#set Working Directory
#setwd("~/Desktop/Dr. Friedmans Project")

#load packages
library(readxl)
library(tidyverse)
library(tidytext)
#install.packages("igraph")
library(igraph)
library(ggraph)

#Reading excel file into R
articles <- read_excel("Mark_Lombardi_study- citation_summary.xlsx", col_names = TRUE)
articles <- articles[ , -c(7:10)]

#Tokenise bigrams
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

#To Visulize we will use the dataframe that contains the separated bigrams
arBigramCountS <- articles_bigramsSeperate %>%
  count(word1, word2, sort = TRUE)

arBigramCountS

#Turn into directed graph
#arBigramCountS$num <-arBigramCountS$n
articleBigramGraph <-arBigramCountS %>%
  filter(n >=2 ) %>%
  graph_from_data_frame()

articleBigramGraph

#Visulaize the directed graph
ggraph(articleBigramGraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = grid::arrow(type = "closed", length = unit(3, "mm")),
                 end_cap = circle (1, "mm")) +
  geom_node_point(color = "plum2", size = 3) +
  geom_node_text(aes(label = name), size = 2) +
  theme_void()

#Second Viz using terms that appear together more than 2 times
articleBigramGraph2 <-arBigramCountS %>%
  filter(n >2 ) %>%
  graph_from_data_frame()

articleBigramGraph2

ggraph(articleBigramGraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = grid::arrow(type = "closed", length = unit(3, "mm")),
                 end_cap = circle (1, "mm")) +
  geom_node_point(color = "plum2", size = 3) +
  geom_node_text(aes(label = name), size = 2) +
  theme_void()

