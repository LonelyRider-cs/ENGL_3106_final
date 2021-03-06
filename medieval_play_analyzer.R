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


stop_words_topic_models <- rbind(stop_words, c("thy", NA))
stop_words_topic_models <- rbind(stop_words_topic_models, c("thou", NA))
stop_words_topic_models <- rbind(stop_words_topic_models, c("thee", NA))

mp_playWordCounts <- all_medieval_plays_unnested %>%
  anti_join(stop_words_topic_models) %>%
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


#work for comparing GoT corpus to medieval plays to get percent of same words in both corpora
GOTC_unique <- GOT_corpus_clean %>% 
  count(word, sort=TRUE)
#remove count, not needed
GOTC_unique <- GOTC_unique[-2]

mp_unique <- all_medieval_plays_unnested %>% 
  count(word, sort=TRUE)
#remove count, not needed
mp_unique <- mp_unique[-2]

#combine unique to figure out later which words are the same in both corpora
unique_combined <- rbind(GOTC_unique, mp_unique)
#words with count = 2 are found in both corpra
unique_combined_count <- unique_combined %>% 
  count(word, sort=TRUE)
#remove all elements that are numbers(not written out)
unique_combined_count <- unique_combined_count[259:129775, ]

number_unique_total <- nrow(unique_combined_count)

number_similar <- nrow(subset(unique_combined_count,n==2))

percent_similar <- number_similar / number_unique_total
