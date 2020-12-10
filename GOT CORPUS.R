library(dplyr)
library(tidyverse)
library(tidytext)
library(textdata)
library(pdftools)


##analysis on all GOT book texts
GOT <- pdf_text("Game of Thrones (1).pdf")%>%
  readr::read_lines()
GOT <- GOT[71:22477]
GOT <- tibble(GOT)%>%
  mutate(lineID = row_number())%>%    # Adding the row number to the data
  mutate(title = "Game of Thrones")   # Adding a title column to the data also
colnames(GOT)[colnames(GOT) == 'GOT'] = 'text'  

COK <- pdf_text("A Clash of Kings (2).pdf")%>%
  readr::read_lines()
COK <- COK[5:24206]
COK <- tibble(COK)%>%
  mutate(lineID = row_number())%>%    # Adding the row number to the data
  mutate(title = "A Clash of Kings")   # Adding a title column to the data also
colnames(COK)[colnames(COK) == 'COK'] = 'text'  

SOS <- pdf_text("A Storm of Swords (3).pdf")%>%
  readr::read_lines()
SOS <- SOS[18:29250]
SOS <- tibble(SOS)%>%
  mutate(lineID = row_number())%>%    # Adding the row number to the data
  mutate(title = "A Storm of Swords")   # Adding a title column to the data also
colnames(SOS)[colnames(SOS) == 'SOS'] = 'text'  # Changing column name

FFC <- pdf_text("A Feast for Crows (4).pdf")%>%
  readr::read_lines()
FFC <- FFC[5:20812]
FFC <- tibble(FFC)%>%
  mutate(lineID = row_number())%>%    # Adding the row number to the data
  mutate(title = "A Feast for Crows")   # Adding a title column to the data also
colnames(FFC)[colnames(FFC) == 'FFC'] = 'text'  # Changing column name

DWD <- pdf_text("A Dance with Dragons (5)-1 (1).pdf")%>%
  readr::read_lines()
DWD <- DWD[198:31414]
DWD = tibble(DWD)%>%
  mutate(lineID = row_number())%>%    # Adding the row number to the data
  mutate(title = "A Dance with Dragons")   # Adding a title column to the data also
colnames(DWD)[colnames(DWD) == 'DWD'] = 'text'  # Changing column name

##Whole corpus
GOT_corpus = rbind(GOT, COK, SOS, FFC, DWD)

##Sentiment Analysis BING
BING <- get_sentiments("bing")
GOT_corpus_clean <- GOT_corpus%>%
  unnest_tokens(word,text)

GOTC_sentiment <- GOT_corpus_clean%>%
  inner_join(BING)%>%
  count(sentiment)
  GOTC_overall <- GOTC_sentiment[2,2] - GOTC_sentiment[1,2]

GOTC_wordcount <- GOT_corpus_clean%>%
  inner_join(BING)%>%
  count(word, sentiment)
GOTC_topwords <- GOTC_wordcount%>%
  group_by(sentiment)%>%
  top_n(15)%>%
  ungroup() %>%
  mutate(word = reorder(word, n))
ggplot(GOTC_topwords, aes(word, n, fill = sentiment)) + # graph by word and n, color different for each sentiment
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +             # new function to display multiple plots
  coord_flip()+
  labs(y= "Number of Occurences", title = "Top Positive and Negative Words in A Game of Thrones",
       subtitle = "With Bing lexicon")

##Sentiment Analysis NRC
NRC <- lexicon_nrc()

GOTC_NRC <- GOT_corpus_clean%>%
  inner_join(NRC)%>%
  count(word,sentiment)%>%
  group_by(sentiment)%>%
  top_n(n=10)%>%
  ungroup()%>%
  mutate(word=reorder(word,n))

  ggplot(GOTC_NRC,aes(word,n,fill=sentiment))+
  geom_col(color="black",show.legend=FALSE)+
  facet_wrap(~sentiment,scales="free")+
  coord_flip()

##Bigram analysis
GOTCbigrams <- GOT_corpus%>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
view(GOTCbigrams %>%
       count(bigram, sort = TRUE))
GOTCbigrams_filtered <- GOTCbigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% SMART$word) %>%
  filter(!word2 %in% SMART$word)
GOTCbigrams_scrubbed <- GOTCbigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
GOTCbigrams_count <- count(GOTCbigrams_scrubbed, bigram, sort = TRUE)
GOTCbigrams_count <- GOTCbigrams_count[-1, ]
GOTCbigrams_count[1:25,] %>%                 
  mutate(bigram = reorder(bigram, n)) %>%   # sort bigram column by value of the n column
  ggplot() +
  aes(bigram, n) +
  geom_col(color="coral3", fill="cornsilk") +
  coord_flip()

