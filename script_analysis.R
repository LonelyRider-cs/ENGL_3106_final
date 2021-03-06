# Step 1: Load Libraries
library(tidyverse)
library(tidytext)
library(textdata)

library(pdftools)
library(widyr)
library(ggpubr)
library(tm)
library(eply)

##To import the script (as it is a csv file), convert it to an excel worksheet before uploading into R studio.
##Once there is an xlsx file in the working directory, it can be imported through the file tab.

##removal of unneccessary (for now) columns
GOTscript <- Game_of_Thrones_Script[-c(1,1:5)]

##cleaning
GOTscript_clean <- GOTscript%>%
  unnest_tokens(word,Sentence)

data("stop_words")
BING <- get_sentiments("bing")

##Sentiment analysis
Script_sentiment <- GOTscript_clean%>%
  inner_join(BING)%>%
  count(sentiment)
Script_overall <- Script_sentiment[2,2] - Script_sentiment[1,2]

Script_wordcount <- GOTscript_clean%>%
  inner_join(BING)%>%
  count(word, sentiment)
Script_topwords <- Script_wordcount%>%
  group_by(sentiment)%>%
  top_n(15)%>%
  ungroup() %>%
  mutate(word = reorder(word, n))
ggplot(GOT_topwords, aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +            
  coord_flip()

##NRC sentiment analysis
NRC <- lexicon_nrc()
Script_NRC <- GOTscript_clean %>%   
  inner_join(NRC) %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))
  
ggplot(Script_NRC,aes(word, n, fill = sentiment)) +
  geom_col(color="black",show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip()

##bigrams
Script_bigrams <- GOTscript%>%
  unnest_tokens(bigram,Sentence,token = "ngrams",n=2)
view(Script_bigrams %>%
       count(bigram, sort = TRUE))
Scriptbigrams_filtered <- Script_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% SMART$word) %>%
  filter(!word2 %in% SMART$word)
Scriptbigrams_scrubbed <- Scriptbigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") 

Scriptbigrams_count <- count(Scriptbigrams_scrubbed, bigram, sort = TRUE)
Scriptbigrams_count[1:25,] %>%                 
  mutate(bigram = reorder(bigram, n)) %>% 
  ggplot() +
  aes(bigram, n) +
  geom_col(color="coral3", fill="cornsilk") +
  coord_flip()
