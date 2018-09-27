library(data.table)
library(magrittr)
library(ggplot2)
library(scales)
library(stringr)
library(quanteda)
library(gridExtra)
library(tm)

setwd('/home/dou6/Desktop/Data Mining/Project/')

dtrain <- fread('train.tsv', showProgress = FALSE)
#dim(dtrain)
#summary(dtrain)

#covert "no description" to NA
dtrain[item_description == 'No description yet', item_description := NA]

###tokenize###
toks <- tokens(dtrain$item_description, 
               remove_punct = TRUE, 
               remove_separators = TRUE, 
               remove_symbols = TRUE)
head(toks[[1]], 50)

###token selection###
#remove stop words from toks
nostop_toks <- tokens_select(toks, stopwords('en'), selection = 'remove')

###covert token to ngrams###
ngram <- tokens_ngrams(nostop_toks, n = 2:4)
head(ngram[[1]], 50)
ngram

####construct dfm####
dfm2=dfm(ngram)
#head(dfm2,n=5)

#see top features in dfm2
topfeatures(dfm2, 50)

#select features based on frequency
freq_dfm2 <- dfm_trim(dfm2, min_termfreq = 5000)#set as more than 5000 times
topfeatures(freq_dfm2, 50)

###plot###
# get 25 most common bigrams
tf <- topfeatures(dfm2, n = 25)
data.frame(term = names(tf), freq = unname(tf)) %>%
  ggplot(aes(x = reorder(term, freq), y = freq/1000)) + 
  geom_bar(stat = 'identity', fill = 'orangered2') + 
  labs(x = '', y = 'Frequency (000s)', title = '25 most common description bigrams') + 
  coord_flip() 

#freq_dfm2=data.frame(dfm2)
topfeatures_df=convert(freq_dfm2,to="data.frame")#######topfeatures_df is a dataframe for features more than 5000 times
head(topfeatures_df,n=5)



