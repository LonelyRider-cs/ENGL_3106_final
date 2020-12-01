library(tidyverse)
library(gutenbergr)
library(tidytext)
library(textdata)

all_medieval_plays <- data.frame(matrix(ncol=0,nrow=0))
for (i in 723:1000) {
  if (nchar(i) == 3) {
    try({
      #get the name of the file
      file_name <- paste("A00",toString(i),".headed.txt", sep="")
      
      #name of file for unnested tokens
      file_name_unnested <- paste("A00",toString(i),".headed.txt.u", sep="")
      
      #print current file being worked on
      print(file_name)
      
      #read in file
      assign(file_name, read.delim(file_name, header = FALSE, sep="\n", quote = ""))
      
      #assignt to temp and remove unnecassary info
      temp_file <- get(file_name)[1:nrow(get(file_name)), ]
      
      #unnest words in play
      assign(file_name_unnested, tibble(temp_file) %>%
               mutate(lineID = row_number()) %>%
               mutate(playID = i) %>% 
               unnest_tokens(word,temp_file,token ="ngrams",n=1))
      
      all_medieval_plays <- rbind(all_medieval_plays, get(file_name_unnested))
    })
  }
  
  if (nchar(i) == 4) {
    try({
      #get the name of the file
      file_name <- paste("A0",toString(i),".headed.txt", sep="")
      
      #name of file for unnested tokens
      file_name_unnested <- paste("A0",toString(i),".headed.txt.u", sep="")
      
      #print current file being worked on
      print(file_name)
      
      #read in file
      assign(file_name, read.delim(file_name, header = FALSE, sep="\n", quote = ""))
      
      #assignt to temp and remove unnecassary info
      temp_file <- get(file_name)[1:nrow(get(file_name)), ]
      
      #unnest words in play
      assign(file_name_unnested, tibble(temp_file) %>%
               mutate(lineID = row_number()) %>%
               mutate(playID = i) %>% 
               unnest_tokens(word,temp_file,token ="ngrams",n=1))
      
      all_medieval_plays <- rbind(all_medieval_plays, get(file_name_unnested))
    })
  }
  
  if (nchar(i) == 5) {
    try({
      #get the name of the file
      file_name <- paste("A",toString(i),".headed.txt", sep="")
      
      #name of file for unnested tokens
      file_name_unnested <- paste("A",toString(i),".headed.txt.u", sep="")
      
      #print current file being worked on
      print(file_name)
      
      #read in file
      assign(file_name, read.delim(file_name, header = FALSE, sep="\n", quote = ""))
      
      #assignt to temp and remove unnecassary info
      temp_file <- get(file_name)[1:nrow(get(file_name)), ]
      
      #unnest words in play
      assign(file_name_unnested, tibble(temp_file) %>%
               mutate(lineID = row_number()) %>%
               mutate(playID = i) %>% 
               unnest_tokens(word,temp_file,token ="ngrams",n=1))
      
      all_medieval_plays <- rbind(all_medieval_plays, get(file_name_unnested))
    })
  }
}



