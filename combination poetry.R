library(tidyverse)
library(tm)
library(NLP)
library(openNLP)
library(gsubfn)
setwd('/Users/nataliemcgartland/Desktop/MayaAngelou/')
stillirise <- read.delim('stillirise.txt')
cagedbird <- read.delim('CagedBird.txt')
kin <- read.delim('Kin.txt')
themotheringblackness <- read.delim('TheMotheringBlackness.txt')
phenomenalwoman <- read.delim('PhenomenalWoman.txt')
raven <- read.delim('raven.txt')

#preprocess to remove  punctuation and capitalization
raven <- gsub("—"," ",raven)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
removeNonWords <- function(x) gsub("[^a-zA-Z]+"," ",x)

raven_clean <- removeNumPunct(raven)
raven_clean <- removeNonWords(raven_clean)
raven_clean <- as.String(raven_clean)

bird_clean <- removeNumPunct(cagedbird)
bird_clean <- removeNonWords(bird_clean)
bird_clean <- as.String(bird_clean)

##this is how we tagged part of speech into the correct column format
bird_string = unlist(bird_clean) %>%
  paste(collapse=' ') %>% 
  as.String

init_s_w = annotate(bird_string, list(Maxent_Sent_Token_Annotator(),
                                      Maxent_Word_Token_Annotator()))
pos_res = annotate(bird_string, Maxent_POS_Tag_Annotator(), init_s_w)
word_subset = subset(pos_res, type=='word')
tags = sapply(word_subset$features , '[[', "POS")

bird_pos = data_frame(word=bird_string[word_subset], pos=tags) %>% 
  filter(!str_detect(pos, pattern='[[:punct:]]'))

##doing it on raven now
raven_string = unlist(raven_clean) %>% 
  paste(collapse=' ') %>% 
  as.String

init_s_w = annotate(raven_string, list(Maxent_Sent_Token_Annotator(),
                                       Maxent_Word_Token_Annotator()))
pos_res = annotate(raven_string, Maxent_POS_Tag_Annotator(), init_s_w)
word_subset = subset(pos_res, type=='word')
tags = sapply(word_subset$features , '[[', "POS")

raven_pos = data_frame(word=raven_string[word_subset], pos=tags) %>% 
  filter(!str_detect(pos, pattern='[[:punct:]]'))

##combining  them
parts_of_speech <- c("NN")
raven_tmp <- filter(raven_pos,pos %in% parts_of_speech)
bird_tmp <- filter(bird_pos,pos %in% parts_of_speech)
replacements <- c((raven_tmp$word))
test <- c((bird_tmp$word))
test.df <- as.data.frame(test)
v1 <- sample(replacements,nrow(bird_tmp),replace=TRUE)
v1 <- as.data.frame(v1)
tmppoem <- cagedbird
for (val in c(1:(length(test.df$test)))){
  newpoem <- gsub(pattern=(test.df$test[val]), replace=(v1$v1[val]), x=tmppoem)
  tmppoem <- newpoem
}
