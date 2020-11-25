#Here's some code for analyzing "A Game of Thrones" using the tools we've learned
#in lecture, plus something else.
#   -Blue, Nov. 24

# Step 1: Load libraries, prepare stop words and sentiment analysis lexica.
library(tidyverse)
library(tidytext)
library(textdata)

library(pdftools)
library(widyr)
library(ggpubr)
library(tm)
library(eply)

data("stop_words")


# Step 2: Get the PDF of the novel. Make sure the filenames match and the novel is in 
#your working directory.
GOT_pdf <- pdf_text("Game of Thrones (1).pdf")%>%
  readr::read_lines()
GOT_pdf <- GOT_pdf[70:22477] #Removing the first 69 lines of contents, maps, and other
#non-text content.

# Step 3: Clean and format text into manageable objects. 
GOT_tibble <- tibble(GOT_pdf)%>%
  mutate(lineID = row_number()) %>% #Index by row number, just in case.
  mutate(chapternum = cumsum(str_detect(GOT_pdf, 
                                     regex("^[\\s]*BRAN[\\s]*$|^[\\s]*CATELYN[\\s]*$|^[\\s]*EDDARD[\\s]*$|^[\\s]*JON[\\s]*$|^[\\s]*DAENERYS[\\s]*$|^[\\s]*SANSA[\\s]*$|^[\\s]*ARYA[\\s]*$|^[\\s]*TYRION[\\s]*$", 
                                           ignore_case = FALSE))))  #If one of these names, in all caps, appears alongside
                                                                 #whitespace, increment the chapter by 1.
colnames(GOT_tibble)[colnames(GOT_tibble) == 'GOT_pdf'] = 'text' #Rename the "GOT_pdf" column to "text," for clarity.
#The original experiments with AGoT in class divided the text into indexes based on
#number of lines. This has been changed to indexing by chapter, to better understand
#how point of view character affects vocabulary, theme, setting, etc.
#Note: the Prologue is counted as Chapter 0.
#If there's a way to get each chapter indexed by name, that'd be useful for isolating
#one character at a time.
GOT_tibbleClean <- GOT_tibble %>% 
  unnest_tokens(word, GOT_pdf) #unnest by word.
#The first novel is now loaded, cleaned, and indexed by chapter number.

#Let's do an overall sentiment analysis with BING. (Overall positive/negative)
BING <- get_sentiments("bing")
GOT_sentiment <- GOT_tibbleClean%>%
inner_join(BING)%>%
  count(sentiment)
GOT_overall <- GOT_sentiment[2,2] - GOT_sentiment[1,2] #Subtracting the negative value from the positive value
#Sentiment analysis by chapter number
GOT_byChapter <- GOT_tibbleClean%>%
  inner_join(BING) %>% 
  count(index = chapternum, sentiment) %>%
  spread(sentiment, n) %>%                    
  mutate(sentiment = positive - negative)
#Okay, let's do a count of the top positive and negative words.
GOT_wordcount <- GOT_tibbleClean%>%
  inner_join(BING)%>%
  count(word, sentiment) #Filter the novel through BING, count word occurrences.
GOT_topwords <- GOT_wordcount%>%
  group_by(sentiment)%>%
  top_n(15)%>% #Take the top 15.
  ungroup() %>%
  mutate(word = reorder(word, n)) #Sort from high to low.
#And give it a plot:
ggplot(GOT_topwords, aes(word, n, fill = sentiment)) + # graph by word and n, color different for each sentiment
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +             # new function to display multiple plots
  coord_flip()+
  labs(y= "Number of Occurences", title = "Top Positive and Negative Words in A Game of Thrones",
       subtitle = "With Bing lexicon")

#Ok that was fun, let's do it with AFINN (each word has a value, from +/-5).
AFINN <-  lexicon_afinn()
#Overall, full novel analysis.
GOT_AFINN <-  GOT_tibbleClean%>%
  inner_join(AFINN)%>%
  summarise(sentiment = sum(value))
#Gotta email the LA who wrote this code about doing this from chapter to chapter,
#since we found a way to index the book by chapter.

#And while we're at it, NRC (joy, anger, surprise, etc.)
NRC <- lexicon_nrc()
#Anger, top 20
NRC_anger <- NRC%>%
  filter(sentiment == "anger")
GOT_anger <- GOT_tibbleClean%>%
  inner_join(NRC_anger)%>%
  count(word, sort = TRUE) %>%
  top_n(n = 20)
ggplot(GOT_anger, aes(word, n))+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  coord_flip()+ #making the words appear on the y axis
  ggtitle("Top 20 Anger Words in Game of Thrones")+ #adding a title
  ylab("Number of Occurrences")+ 
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)
#Let's just count all of the top 10 words by emotion, and graph it too.
GOT_NRC <- GOT_tibbleClean %>%    ## notice that here we are creating a new ggplot object
  inner_join(NRC) %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  # now plot everything
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(color="black",show.legend = FALSE) +
  # we use the facet_wrap function
  facet_wrap(~sentiment, scales = "free") +
  coord_flip()

#Let's try bigram and skipgram analysis. This is better suited to whole corpus analysis,
#but it's worth trying.
SMART  <-  stop_words%>%
  filter(lexicon == "SMART") %>% #We use this lexicon for bigram cleaning. Here is where
                             #you'd filter stop words you specifically want to see,
                             #like names or pronouns.
GOTbigrams <- GOT_tibble%>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) #Tokenize by bigrams.
view(GOTbigrams %>%
       count(bigram, sort = TRUE))
#The process for cleaning bigrams is different than whole texts.
GOTbigrams_filtered <- GOTbigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  ## "bigram" is the column to be separated
  ## "word1" and "word2" are the two new columns to be created
  ## "sep = " "" tells R to split the bigram where it finds a space
  
  filter(!word1 %in% SMART$word) %>%
  filter(!word2 %in% SMART$word)
GOTbigrams_scrubbed <- GOTbigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")   ## joining everything back together with unite()
#And cleaned bigrams. Now let's count occurrences,
GOTbigrams_count <- count(GOTbigrams_scrubbed, bigram, sort = TRUE) #Count occurrences of bigrams.
#And plot them.
GOTbigrams_count[1:25,] %>%    #uses only the top 25 values.             
mutate(bigram = reorder(bigram, n)) %>%   # sort bigram column by value of the n column
  ggplot() +
  aes(bigram, n) +
  geom_col(color="coral3", fill="cornsilk") +
  coord_flip()

#Let's do skipgrams and word probability next.
#First, find each word's probability of appearing.
GOT_wordProb <- GOT_tibble %>%
  unnest_tokens(word, text) %>%  
  count(word, sort = TRUE) %>%      #count occurrences
  mutate(p = n/sum(n))              #find probability
#Now, we create the actual skipgrams from the untokenized text. We'll do groups of 8 words.
GOTskipgrams <- GOT_tibble %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%     # create 8-grams
  mutate(ngramID = row_number()) %>%                          # create an index of each 8-gram
  unite(skipgramID, lineID, ngramID)%>%                       # create a nex cobined index joining lineID and ngramID
  unnest_tokens(word, ngram)                                  # re-tokenize the text by word
#Then we find the probability of two words appearing in one 8-gram.
GOT_skipgram_probs <- GOTskipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE)%>%   # counting word co-occurence
  mutate(p= n/sum(n))
#And then normalize the probabilities, filtering out ones that are too small.
GOT_normalized_prob <- GOT_skipgram_probs %>%
  filter(n>10) %>%                            # select word pairs that occur more than 10 times
  rename(word1 = item1, word2 = item2) %>%    # rename the columns of data for clarity
  left_join(GOT_wordProb %>%              # use a left_join() to add probabilties
              select(word1 = word, p1 = p),   # instructing r which columns of data to match
            by = "word1") %>%
  left_join(GOT_wordProb %>%              # now do the same thing for word #2
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p/p1/p2) #Remember: p = overall appearance probability.
#Let's use this to find words related to, say, Tyrion.
tyrion_probs<- GOT_normalized_prob %>%
  filter(word1 == "tyrion") %>%  #here I am finding all of the instances where word1 is "dragon"
  arrange(-p_together)
tyrion_probs <- tyrion_probs[ ,c(2,7)]%>% #Only shows columns 2 and 7, the words and p_together
  top_n(25)

#~

#So far, all of what we've done here is stuff we've done before in class and for 
#the sentiment analysis project. Let's try something different.
#Warning: this stuff gets kinda processor intensive, it could crash jupyterhub. I'm
#running this on my local version of RStudio.

#Let's try topic modeling. The tutorial I'm following/adapting is here:
# https://www.tidytextmining.com/topicmodeling.html#topicmodeling
#Topic modeling is a method of essentially letting the computer read our data and
#tell us what it sees in it, instead of telling the computer to look for what we
#want to see. In essence, topic modeling reads "documents," like Twitter posts,
#newspaper articles, or in our case, chapters in a novel. Every document is made of
#a set number of topics, and each topic has certain words in it. These topics aren't
#pre-defined, but interpreted on the spot. Running the same topic model on the 
#same data can yield slightly different results when you run it.

#We're using a new package, "topicmodels."
install.packages("topicmodels")
library(topicmodels)
#We've already indexed our text by chapter, let's have a quick look at the most
#common words, indexed by chapter, sorted from most appearances in one chapter to least.
GOT_chapterWordCounts <- GOT_tibbleClean %>%
  anti_join(stop_words) %>%
  count(chapternum, word, sort = TRUE) %>%
  ungroup()
#This word count is going to be the basis for our topic model. Before we can do
#the actual topic modeling, we need to convert our data from a tibble to 
#something called a DocumentTermMatrix, for the topicmodel to be able to read it.
GOT_chaptersDTM <- GOT_chapterWordCounts %>%
  cast_dtm(chapternum, word, n)
#The actual method of topic modeling we're using is called Latent Dirichelet
#Allocation. Do not ask me what that precisely means, I couldn't tell you.
#But let's try it anyway!
GOT_chaptersLDA <- LDA(GOT_chaptersDTM, k = 4, control = list(seed = 1234))
#For the above, k = number of topics to generate. 4 is just our example, 
#Change the seed value for different results! The creation process is the heavy 
#part, especially when working with large corpora. Give it a few seconds.
#Once it's done, let's clean it up and put it back in tibble form.
GOT_chapterTopics <- tidy(GOT_chaptersLDA, matrix = "beta")
#One topic per term per row. Note that the topics have numbers. The program doesn't
#necessarily know what unifies these terms under these topics, that's for us to 
#interpret.
#Let's look at the top 10 terms per topic.
GOT_topTopicTerms <- GOT_chapterTopics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) 
GOT_topTopicTerms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
#This is only one way of topic modeling or using LDA. The results here are skewed
#and imperfect because of the size of the dataset. Long as it is, AGoT is still
#just one text. Things tend to improve with the size of the dataset used (the whole
#ASoIaF series, or the early modern drama corpus, for example).
