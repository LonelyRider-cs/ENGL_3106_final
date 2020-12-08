library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(topicmodels)
library(reshape)
library(reshape2)

data("stop_words")
SMART  <-  stop_words%>%
  filter(lexicon == "SMART")


##Bing Sentiment analysis
BING <- get_sentiments("bing")
mp_sentiment <- all_medieval_plays_unnested%>%
  inner_join(BING)%>%
  count(sentiment)
mp_overall <- mp_sentiment[2,2]- mp_sentiment[1,2]

mp_wordcount <- all_medieval_plays_unnested%>%
  inner_join(BING)%>%
  count(word,sentiment)

mp_topwords <-mp_wordcount%>%
  group_by(sentiment)%>%
  top_n(15)%>%
  ungroup()%>%
  mutate(word=reorder(word,n))

ggplot(mp_topwords, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip()

##NRC lexicon analysis
NRC <- lexicon_nrc()

mp_NRC <- all_medieval_plays_unnested%>%
  inner_join(NRC)%>%
  count(word,sentiment)%>%
  group_by(sentiment)%>%
  top_n(10)%>%
  ungroup()%>%
  mutate(word=reorder(word,n))

ggplot(mp_NRC,aes(word,n,fill=sentiment))+
  geom_col(color="black",show.legend=FALSE)+
  facet_wrap(~sentiment,scales="free")+
  coord_flip()

##bigrams
mp_bigrams <- all_medieval_plays_nested%>%
  unnest_tokens(bigram, V1, token = "ngrams", n = 2)

view(mp_bigrams %>%
       count(bigram, sort = TRUE))

mp_bigrams_filtered <- mp_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% SMART$word) %>%
  filter(!word2 %in% SMART$word)

mp_bigrams_scrubbed <- mp_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
mp_bigrams_count <- count(mp_bigrams_scrubbed, bigram, sort = TRUE)

mp_bigrams_count[1:25,] %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot() +
  aes(bigram, n) +
  geom_col(color="coral3", fill="cornsilk") +
  coord_flip()




mp_playWordCounts <- all_medieval_plays_unnested %>%
  anti_join(stop_words) %>%
  count(playID, word, sort = TRUE) %>%
  ungroup()

mp_playDTM <- mp_playWordCounts %>%
  cast_dtm(playID, word, n)

mp_playLDA <- LDA(mp_playDTM, k = 4, control = list(seed = 1234))

mp_playTopics <- tidy(mp_playLDA, matrix = "beta")

mp_topTopicTerms <- mp_playTopics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

mp_topTopicTerms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
