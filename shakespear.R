library(tidyverse)
library(gutenbergr)
library(tidytext)
library(textdata)
data("stop_words")

#add blank tokens into stop words
blank_token <- data.frame(NA, "blank")
names(blank_token) <- c("word", "lexicon")
stop_words <- rbind(stop_words, blank_token)

#downloading all american corups files individually and then cleaningthe rows

#henry IV part 1
sp01 <- gutenberg_download(1780, meta_fields = c("title", "author"))
sp01 <- sp01[226:4093, ]

#henry IV part 2
sp02 <- gutenberg_download(1782, meta_fields = c("title", "author"))
sp02 <- sp02[359:4443, ]

#henry V
sp03 <- gutenberg_download(2253, meta_fields = c("title", "author"))
sp03 <- sp03[136:4223, ]

#henry VI part 1
sp04 <- gutenberg_download(1765, meta_fields = c("title", "author"))
sp04 <- sp04[301:3641, ]

#henry VI part 2
sp05 <- gutenberg_download(2255, meta_fields = c("title", "author"))
sp05 <- sp05[100:4282, ]

#henry VI part 3
sp06 <- gutenberg_download(2256, meta_fields = c("title", "author"))
sp06 <- sp06[99:4155, ]

#henry VIII
sp07 <- gutenberg_download(2258, meta_fields = c("title", "author"))
sp07 <- sp07[133:4317, ]

#King John
sp08 <- gutenberg_download(1511, meta_fields = c("title", "author"))
sp08 <- sp08[46:4084, ]

#Richard II 
sp09 <- gutenberg_download(2250, meta_fields = c("title", "author"))
sp09 <- sp09[97:3537, ]

#Richard III
sp10 <- gutenberg_download(1103, meta_fields = c("title", "author"))
sp10 <- sp10[105:4576, ]

#Julius Caesar
sp11 <- gutenberg_download(1785, meta_fields = c("title", "author"))
sp11 <- sp11[292:3239, ]

#Hamlet
sp12 <- gutenberg_download(2265, meta_fields = c("title", "author"))
sp12 <- sp12[95:5010, ]

#King Lear
sp13 <- gutenberg_download(2266, meta_fields = c("title", "author"))
sp13 <- sp13[96:4376, ]

#Coriolanus
sp14 <- gutenberg_download(2259, meta_fields = c("title", "author"))
sp14 <- sp14[94:5066, ]

#Macbeth
sp15 <- gutenberg_download(2264, meta_fields = c("title", "author"))
sp15 <- sp15[95:3373, ]
