load('dwithallbrands.Rda')
load('feedcaret_d_modelbw.Rda')
load('feedcaret_d_modelng2.Rda')

#add brand dummies to bag of words set
#d_bw_brand<-cbind(d_modelbw,d_model[,24:54])
################################################################################
# PCA 
################################################################################
## for bag of words
names(d_modelbw)[27]<-'shipping_cost'

d_modelbw$shipping_cost<-ifelse(d_modelbw$shipping_cost=='0',0,1)
d_modelbw$DummyBrand<-ifelse(d_modelbw$DummyBrand=='0',0,1)
d_modelbw$DummyItemDescription<-ifelse(d_modelbw$DummyItemDescription=='0',0,1)
d_modelbw$item_condition_id1<-ifelse(d_modelbw$item_condition_id1=='0',0,1)
d_modelbw$item_condition_id2<-ifelse(d_modelbw$item_condition_id2=='0',0,1)
d_modelbw$item_condition_id3<-ifelse(d_modelbw$item_condition_id3=='0',0,1)
d_modelbw$FirstCategoryBeauty<-ifelse(d_modelbw$FirstCategoryBeauty=='0',0,1)
d_modelbw$FirstCategoryElectronics<-ifelse(d_modelbw$FirstCategoryElectronics=='0',0,1)
d_modelbw$FirstCategoryKids<-ifelse(d_modelbw$FirstCategoryKids=='0',0,1)
d_modelbw$FirstCategoryMen<-ifelse(d_modelbw$FirstCategoryMen=='0',0,1)
d_modelbw$FirstCategoryWomen<-ifelse(d_modelbw$FirstCategoryWomen=='0',0,1)
d_modelbw$filt_brandBrandMissing<-ifelse(d_modelbw$filt_brandBrandMissing=='0',0,1)
d_modelbw$filt_brandothers<-ifelse(d_modelbw$filt_brandothers=='0',0,1)
set.seed(2019)
inTrain <- createDataPartition(y = d_modelbw$y,   # outcome variable
                               p = .80,   # doing a 5-fold
                               list = F)
train <- d_modelbw[inTrain,]  # training data set
test <- d_modelbw[-inTrain,]  # test data set

library(psych)
pcabw<- principal(train[,2:ncol(train)], 
                 nfactors = 38,
                 rotate = 'none', 
                 scores = T
)
pcabw$loadings

## for n-gram
d_modelng2$shipping<-ifelse(d_modelng2$shipping=='0',0,1)
d_modelng2$DummyBrand<-ifelse(d_modelng2$DummyBrand=='0',0,1)
d_modelng2$DummyItemDescription<-ifelse(d_modelng2$DummyItemDescription=='0',0,1)
d_modelng2$item_condition_id1<-ifelse(d_modelng2$item_condition_id1=='0',0,1)
d_modelng2$item_condition_id2<-ifelse(d_modelng2$item_condition_id2=='0',0,1)
d_modelng2$item_condition_id3<-ifelse(d_modelng2$item_condition_id3=='0',0,1)
d_modelng2$FirstCategoryBeauty<-ifelse(d_modelng2$FirstCategoryBeauty=='0',0,1)
d_modelng2$FirstCategoryElectronics<-ifelse(d_modelng2$FirstCategoryElectronics=='0',0,1)
d_modelng2$FirstCategoryKids<-ifelse(d_modelng2$FirstCategoryKids=='0',0,1)
d_modelng2$FirstCategoryMen<-ifelse(d_modelng2$FirstCategoryMen=='0',0,1)
d_modelng2$FirstCategoryWomen<-ifelse(d_modelng2$FirstCategoryWomen=='0',0,1)
d_modelng2$filt_brandBrandMissing<-ifelse(d_modelng2$filt_brandBrandMissing=='0',0,1)
d_modelng2$filt_brandothers<-ifelse(d_modelng2$filt_brandothers=='0',0,1)
set.seed(2019)
inTrain <- createDataPartition(y = d_modelng2$y,   # outcome variable
                               p = .80,   # doing a 5-fold
                               list = F)
train <- d_modelng2[inTrain,]  # training data set
test <- d_modelng2[-inTrain,]  # test data set

library(psych)
pcang2<- principal(train[,2:ncol(train)], 
                 nfactors = 15,
                 rotate = 'none', 
                 scores = T
)
pcang2$loadings

################################################################################
# LASSO ON Bag of words
###############################################################################
library(caret)
ctrl<-trainControl(method='cv',number=5,classProbs = F, 
                   summaryFunction = defaultSummary)

lassofit<- train(y~., data=d_modelbw,
                 method='lars', trControl=ctrl,
                 tuneLength=38,metric='RMSE')
lassofit


################################################################################
# VISUALLIZATION
################################################################################
##Wordcloud
library(wordcloud)
wordcloud(words = highfreq$keyword, freq = highfreq$freq, scale=c(4,0.1), max.words = 20,
          colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))
