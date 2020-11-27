##shakespeare sentiment analysis
library(tidyverse)
library(gutenbergr)
library(tidytext)
library(textdata)
library(pdftools)
library(widyr)
library(ggpubr)
library(tm)
library(eply)
data("stop_words")
SMART  <-  stop_words%>%
  filter(lexicon == "SMART")

##cleaning and combining

S_corpus <- rbind(sp01,sp02,sp03,sp04,sp05,sp06,sp07,sp08,sp09,sp10,sp11,sp12,sp13,sp14,sp15)
S_corpus_clean <- S_corpus%>%
  unnest_tokens(word,text)

##Bing Sentiment analysis
BING <- get_sentiments("bing")
S_sentiment <- S_corpus_clean%>%
  inner_join(BING)%>%
  count(sentiment)
S_overall <- S_sentiment[2,2]-S_sentiment[1,2]

S_wordcount <- S_corpus_clean%>%
  inner_join(BING)%>%
  count(word,sentiment)

S_topwords <- S_wordcount%>%
  group_by(sentiment)%>%
  top_n(15)%>%
  ungroup()%>%
  mutate(word=reorder(word,n))

ggplot(S_topwords, aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +            
  coord_flip()+

##NRC lexicon analysis
NRC <- lexicon_nrc()

S_NRC <- S_corpus_clean%>%
  inner_join(NRC)%>%
  count(word,sentiment)%>%
  group_by(sentiment)%>%
  top_n(10)%>%
  ungroup()%>%
  mutate(word=reorder(word,n))

ggplot(S_NRC,aes(word,n,fill=sentiment))+
  geom_col(color="black",show.legend=FALSE)+
  facet_wrap(~sentiment,scales="free")+
  coord_flip()

##bigrams
Sbigrams <- S_corpus%>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

view(Sbigrams %>%
       count(bigram, sort = TRUE))

Sbigrams_filtered <- Sbigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% SMART$word) %>%
  filter(!word2 %in% SMART$word)

Sbigrams_scrubbed <- Sbigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") 
Sbigrams_count <- count(Sbigrams_scrubbed, bigram, sort = TRUE)

Sbigrams_count[1:25,] %>%                 
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot() +
  aes(bigram, n) +
  geom_col(color="coral3", fill="cornsilk") +
  coord_flip()
