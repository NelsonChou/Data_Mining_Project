load('dwithallbrands.Rda')
load('feedcaret_d_modelbw.Rda')
load(d_modelng2, file='feedcaret_d_modelng2.Rda')


################################################################################
# VISUALLIZATION
################################################################################
##Wordcloud
library(wordcloud)
wordcloud(words = highfreq$keyword, freq = highfreq$freq, scale=c(4,0.1), max.words = 20,
          colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))
