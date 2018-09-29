##############################
####Data Mining Final Project
#############################

setwd('/home/chou71/Downloads')

rm(list=ls())
###############################
#load required packages
###############################
#install.packages('qdap'); install.packages('data.table'); install.packages('stringi'); install.packages('qdap')
#install.packages('tm'); install.packages('wordcloud'); install.packages('plotrix'); install.packages('dendextend')
#install.packages('ggthemes'); install.packages('RWeka'); install.packages('rlang'); install.packages('magrittr')
#install.packages('scales'); install.packages('stringr'); install.packages('quanteda'); install.packages('gridExtra')
#install.packages('quanteda', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(data.table)
library(ggplot2)
library(dplyr)
library(stringi)
library(qdap)
library(tm)
library(plotrix)
library(dendextend)
library(ggthemes)
library(RWeka)
library(rlang)
library(magrittr)
library(scales)
library(stringr)
library(quanteda)
library(gridExtra)


#load data, combine two data set since in column 8 the tab delimiter can cause problem
d<-fread('train.tsv', sep='\t', header=TRUE, select = c(1:7))
text<-fread("train.tsv",sep="\t",header=TRUE, select = c(8))
d$item_description<-text
rm(text)

################################################################################
## Handling missing values
################################################################################

#####Intepret NA correctly
d$category_name<-ifelse(d$category_name=="", NA, d$category_name)
d$brand_name<-ifelse(d$brand_name=="", NA, d$brand_name)
d$name<-ifelse(d$name=="", NA, d$name)
d$item_description<-ifelse(d$item_description=="No description yet", 
                           NA, d$item_description)

################################################################################
## Split category
################################################################################

#Create three columns to capture 1st, 2nd, and 3rd categories
d$FirstCategory<-sapply(d$category_name, FUN = function(x) strsplit(x, split='/')[[1]][1])
d$SecondCategory<-sapply(d$category_name, FUN = function(x) strsplit(x, split='/')[[1]][2])
d$ThirdCategory<-sapply(d$category_name, FUN = function(x) strsplit(x, split='/')[[1]][3])

#Filling in missing for later dummy variable creation
d$FirstCategory<-ifelse(is.na(d$FirstCategory),'Missing', d$FirstCategory)
d$SecondCategory<-ifelse(is.na(d$SecondCategory),'Missing', d$SecondCategory)
d$ThirdCategory<-ifelse(is.na(d$ThirdCategory),'Missing', d$ThirdCategory)

################################################################################
## Generate potential features
################################################################################

##create number of character for item name
d$lengthofitemname<-ifelse(is.na(d$name), 0,nchar(d$name))

#item_condition_id: set level for item_condition_id
#set 1<2<3<4<5 first, but the definition of the levels are unclear, can try 1>2>3>4>5 later
d$item_condition_id<-ordered(d$item_condition_id, levels=c(1,2,3,4,5))

#Shipping: 1 if shipping fee is paid by seller and 0 by buyer
#Treat 1 as better than 0
d$shipping<-ordered(d$shipping, levels=c(0, 1))

#Dummy variables
#e.g. if brand name is not empty, 1, else 0
d$DummyBrand<-ifelse(is.na(d$brand),0,1)
d$DummyItemDescription<-ifelse(is.na(d$item_description),0,1)
d$DummyFirstCategory<-ifelse(is.na(d$FirstCategory),0,1)
d$DummySecondCategory<-ifelse(is.na(d$SecondCategory),0,1)
d$DummyThirdCategory<-ifelse(is.na(d$ThirdCategory),0,1)

#Process brand name for dummy variables generation
d$brand_name<-as.character(d$brand_name)
#d$brand_name<-tolower(d$brand_name)   ##because not using bag of words on brand
#d$brand_name<-removePunctuation(d$brand_name)
#d$brand_name<-stripWhitespace(d$brand_name)
#d$brand_name<-replace_symbol(d$brand_name)
#d$brand_name<-replace_number(d$brand_name)

#Filling in missing for later dummy variable creation
d$brand_name<-ifelse(is.na(d$brand_name),'BrandMissing', d$brand_name)


save(d, file="d.Rda")
#load('d.Rda')

#########################
####Bag of Words
#########################

###Item_description
###################

#words transformation for Corpros creation
d$item_description_processed<-tolower(d$item_description)
d$item_description_processed<-removePunctuation(d$item_description_processed)
d$item_description_processed<-stripWhitespace(d$item_description_processed)
d$item_description_processed<-replace_abbreviation(d$item_description_processed)
d$item_description_processed<-replace_contraction(d$item_description_processed)
d$item_description_processed<-replace_symbol(d$item_description_processed)

#Identify the most used words to see if any word should be removed from stop word list
#term_count <- freq_terms(d$item_description_processed, 40)

#term_count$WORD
#[1] "and"       "the"       "a"         "for"       "in"        "new"       "to"        "with"     
#[9] "size"      "is"        "of"        "no"        "i"         "on"        "brand"     "free"     
#[17] "condition" "you"       "it"        "shipping"  "rm"        "or"        "are"       "this"     
#[25] "worn"      "used"      "my"        "will"      "all"       "never"     "but"       "great"    
#[33] "not"       "black"     "price"     "have"      "from"      "your"      "bundle"    "one" 

#Remove the stopwords from our item_description
d$item_description_processed<-removeWords(d$item_description_processed, stopwords('en'))

#head(d$item_description_processed)

#check the top 40 frequent used words again, makes more sense to keep now
#term_count<-freq_terms(d$item_description_processed, 40)
#term_count$WORD
#[1] "new"       "size"      "brand"     "free"      "condition" "shipping"  "rm"        "worn"     
#[9] "used"      "will"      "never"     "great"     "black"     "price"     "bundle"    "one"      
#[17] "color"     "pink"      "please"    "small"     "like"      "good"      "can"       "box"      
#[25] "tags"      "x"         "firm"      "items"     "just"      "white"     "medium"    "perfect"  
#[33] "cute"      "blue"      "comes"     "large"     ""        "home"      "super"     "ship"  

#save the file
save(d, file="item_description_processed.Rda")
#load("item_description_processed.Rda")

#list out stop words for English
#stopwords("en")

#Make the term_count into VCorpus 
term_count_source<-VectorSource(d$item_description_processed)
term_count_corpus <- VCorpus(term_count_source)
dtm <- DocumentTermMatrix(term_count_corpus)

#Set the degree of sparseness to keep, e.g. 0.96 means a variable that is more sparse than 0.96 will be
#removed. The lower the sparse, the fewer words are retained
DegreeofSparse<-0.95
dtm <- removeSparseTerms(dtm, DegreeofSparse)

#The terms that are kept in the end, rooms for further improvement
dtm$dimnames
#[1] "black"     "box"       "brand"     "bundle"    "can"       "color"     "condition" "firm"     
#[9] "free"      "good"      "great"     "like"      "never"     "new"       "one"       "pink"     
#[17] "please"    "price"     "shipping"  "size"      "small"     "tags"      "used"      "will"     
#[25] "worn" 

#create term dataframe to be matched with original dataset
labeledTerms <- as.data.frame(as.matrix(dtm))
rm(term_count_source, term_count_corpus, dtm)

save(labeledTerms, file="Bag_of_words.Rda")
#load('Bag_of_words.Rda')

###Name: do the same for item name
###################

#words transformation for Corpros creation
d$name_processed<-tolower(d$name)
d$name_processed<-removePunctuation(d$name_processed)
d$name_processed<-stripWhitespace(d$name_processed)
d$name_processed<-replace_abbreviation(d$name_processed)
d$name_processed<-replace_contraction(d$name_processed)
d$name_processed<-replace_symbol(d$name_processed)

#remove stop words
d$name_processed<-removeWords(d$name_processed, stopwords('en'))

#Make the term_count into VCorpus 
term_count_source<-VectorSource(d$name_processed)
term_count_corpus <- VCorpus(term_count_source)
dtm_name <- DocumentTermMatrix(term_count_corpus)

#Set the degree of sparseness to keep, e.g. 0.96 means a variable that is more sparse than 0.96 will be
#removed. The lower the sparse, the fewer words are retained
#0.99 seems ok to use to name
DegreeofSparse<-0.99
dtm_name <- removeSparseTerms(dtm_name, DegreeofSparse)

#[1] "american"  "baby"      "bag"       "black"     "blue"      "boots"     "bra"       "bundle"   
#[9] "case"      "dress"     "free"      "gold"      "hold"      "iphone"    "jacket"    "jeans"    
#[17] "large"     "leggings"  "lot"       "lularoe"   "mens"      "new"       "nike"      "nwt"      
#[25] "pink"      "secret"    "set"       "ship"      "shirt"     "shoes"     "shorts"    "size"     
#[33] "small"     "tank"      "tee"       "top"       "victoria"  "victorias" "white"     "womens"

rm(term_count_source, term_count_corpus)
rm(dtm_name)
#create term dataframe to be matched with original dataset
labeledTerms_name <- as.data.frame(as.matrix(dtm_name))

save(labeledTerms_name, file="Bag_of_words_name.Rda")
#load('Bag_of_words_name.Rda')
rm(dtm_name)
#########################
####ngram
#########################

###tokenize###
toks <- tokens(d$item_description, 
               remove_punct = TRUE, 
               remove_separators = TRUE, 
               remove_symbols = TRUE)

###token selection###
#remove stop words from toks
nostop_toks <- tokens_select(toks, stopwords('en'), selection = 'remove')

###covert token to ngrams###
#try2-4
ngram <- tokens_ngrams(nostop_toks, n = 2:4)

#try bigram
bigram <- tokens_ngrams(nostop_toks, n = 2)
#try trigram
trigram <- tokens_ngrams(nostop_toks, n = 3)

####construct dfm####
dfm24=dfm(ngram)#2:4
dfm2=dfm(bigram)#2
dfm3=dfm(trigram)#3

#select features based on frequency
freq_dfm24 <- dfm_trim(dfm24, min_termfreq = 5000)#set as more than 5000 times
freq_dfm24_1k <- dfm_trim(dfm24, min_termfreq = 1000)#set as more than 5000 times

freq_dfm2 <- dfm_trim(dfm2, min_termfreq = 5000)#set as more than 5000 times
freq_dfm2_1k <- dfm_trim(dfm2, min_termfreq = 1000)#set as more than 5000 times

freq_dfm3 <- dfm_trim(dfm3, min_termfreq = 5000)#set as more than 5000 times
freq_dfm3_1k <- dfm_trim(dfm3, min_termfreq = 1000)#set as more than 5000 times

###plots###
#24gram plot
# get 25 most common
tf24 <- topfeatures(dfm24, n = 25)
#data.frame(term = names(tf24), freq = unname(tf24)) %>%
#ggplot(aes(x = reorder(term, freq), y = freq/1000)) + 
#geom_bar(stat = 'identity', fill = 'orangered2') + 
#labs(x = '', y = 'Frequency (000s)', title = '25 most common description bigrams') + 
#coord_flip() 

#bigram plot
# get 25 most common
tf2 <- topfeatures(dfm2, n = 25)
#data.frame(term = names(tf2), freq = unname(tf2)) %>%
#ggplot(aes(x = reorder(term, freq), y = freq/1000)) + 
#geom_bar(stat = 'identity', fill = 'orangered2') + 
#labs(x = '', y = 'Frequency (000s)', title = '25 most common description bigrams') + 
#coord_flip() 

#trigram plot
# get 25 most common
tf3 <- topfeatures(dfm3, n = 25)
#data.frame(term = names(tf3), freq = unname(tf3)) %>%
#ggplot(aes(x = reorder(term, freq), y = freq/1000)) + 
#geom_bar(stat = 'identity', fill = 'orangered2') + 
#labs(x = '', y = 'Frequency (000s)', title = '25 most common description bigrams') + 
#coord_flip()

rm(dfm2, dfm3, dfm24, toks, nostop_toks, ngram, bigram, trigram)

###covert to dataframe, feed the model###
#24gram to df24
#freq_dfm2=data.frame(dfm2)
topfeatures_df24=convert(freq_dfm24,to="data.frame")#######topfeatures_df is a dataframe for features more than 5000 times
topfeatures_df24<-topfeatures_df24[, 2:ncol(topfeatures_df24)]

#bigram to df2
#freq_dfm2=data.frame(dfm2)
topfeatures_df2=convert(freq_dfm2,to="data.frame")#######topfeatures_df is a dataframe for features more than 5000 times
topfeatures_df2<-topfeatures_df24[, 2:ncol(topfeatures_df2)]

#trigram to df3
#freq_dfm3=data.frame(dfm3)
topfeatures_df3=convert(freq_dfm3,to="data.frame")#######topfeatures_df is a dataframe for features more than 5000 times
topfeatures_df3<-topfeatures_df24[, 2:ncol(topfeatures_df3)]

save(topfeatures_df24, file='topfeatures_df24.Rda')
save(topfeatures_df2, file='topfeatures_df2.Rda')
save(topfeatures_df3, file='topfeatures_df3.Rda')
#load('topfeatures_df24.Rda')
#load('topfeatures_df2.Rda')
#load('topfeatures_df3.Rda')

rm(freq_dfm24, freq_dfm24_1k, freq_dfm2, freq_dfm2_1k, freq_dfm3, freq_dfm3_1k)
rm(tf24, tf3, tf2)

####### DON'T RUN!!! ######### DON'T RUN!!! ############# DON'T RUN!!! #####################
#### Noun phrases ###WILL CRASH THE SERVER

####### DON'T RUN!!! ######### DON'T RUN!!! ############# DON'T RUN!!! #####################

                        

############################################################################################
###################### Brand_Name selection ################################
test<-fread("test.tsv",sep="\t",header=TRUE)
test$brand_name<-ifelse(test$brand_name=="", NA, test$brand_name)
test$brand_name<-as.character(test$brand_name)
test$brand_name<-ifelse(is.na(test$brand_name),'BrandMissing', test$brand_name)

d_brand<-d%>%group_by(brand_name)%>%
        summarise(n=n())%>%
        arrange(desc(n))
t_brand<-test%>%group_by(brand_name)%>%
        summarise(n=n())%>%
        arrange(desc(n))
all<-merge(d_brand,t_brand, by='brand_name',all = T)%>% ##keeping both brands appeared in train and in test
        mutate(n.x=ifelse(is.na(all$n.x),0,all$n.x))%>%
        mutate(n.y=ifelse(is.na(all$n.y),0,all$n.y))%>%
        mutate(count=n.x+n.y)%>%arrange(desc(count))
want<-all[1:30,]            ##keep only 30 most frequently listed brands
rm(d_brand,t_brand,all)
d$filt_brand<-ifelse(d$brand_name%in%want$brand_name,d$brand_name,'others') ##USE d$filt_brand to generate brand dummies for d and test
############################################################################################

load('labeledTerms_name.Rda')
load('labeledTerms.Rda')
load('topfeatures_df2.Rda')
load('topfeatures_df24.Rda')
load('topfeatures_df3.Rda')
                        
                        
#########################
####Dummy variables
#########################

#subset columns for dummy variables creation, only first category
names(d)[6]<-'y'
d_dummy <- d[,c(6,3,9)]
names(d)
#Create dummy based on item_condition, first to third category
library(caret)

#convert item_condition_id to character for dummy variable
d_dummy$item_condition_id<-as.character(d_dummy$item_condition_id)

dummies <- dummyVars(y ~ ., data = d_dummy)            # create dummyes for Xs
ex <- data.frame(predict(dummies, newdata = d_dummy))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))                # removes dots from col names
d_dummy <- cbind(d_dummy$y, ex) 
rm(ex, dummies)

#Convert dummy columns to factor first before merging
names(d_dummy)[1]<-'y'
col<-names(d_dummy)[2:ncol(d_dummy)]

d_dummy[col] <- lapply(d_dummy[col], factor)

save(d_dummy, file='d_dummy.Rda')

#load('d_dummy.Rda')
#merge d_dummy back with d

#Convert factors in the original data frame into factors
#select from original data frame the required columns to be merged with d_dummy

d_bind<-d[,c(6, 13:17)]

d_bind$DummyBrand<-as.factor(d_bind$DummyBrand)
d_bind$DummyItemDescription<-as.factor(d_bind$DummyItemDescription)
d_bind$DummyFirstCategory<-as.factor(d_bind$DummyFirstCategory)
d_bind$DummySecondCategory<-as.factor(d_bind$DummySecondCategory)
d_bind$DummyThirdCategory<-as.factor(d_bind$DummyThirdCategory)

d_model<-cbind(d_bind, d_dummy[,c(2:ncol(d_dummy))])

rm(col, d_dummy, d_bind)

save(d_model, file='d_model.Rda')
#########################
####Merge with Bag of Words, ngram, and noun phrases
#########################

#merge with bag of words
d_modelbw<-cbind(d_model, labeledTerms, labeledTerms_name)

#merge with ngram
d_modelng24<-cbind(d_model, topfeatures_df24)
d_modelng2<-cbind(d_model, topfeatures_df2)
d_modelng3<-cbind(d_model, topfeatures_df3)

#merge with noun phrases



rm(labeledTerms, labeledTerms_name, topfeatures_df24, topfeatures_df2, topfeatures_df3)

################################################################################
# Remove Zero- and Near Zero-Variance Predictors
################################################################################

#bag of words
nzv <- nearZeroVar(d_modelbw[,2:ncol(d_modelbw)], uniqueCut=10) # identify columns that are "near zero"

d_select<-d_modelbw[, 2:ncol(d_modelbw)]
d_filtered<-subset(d_select, select=c(-nzv))

# remove those columns from your dataset
dim(d_filtered) 

# Remove those features that nearZeroVar() identified for removal from dataset.
d_modelbw <- cbind(d_modelbw$y, d_filtered)   # combine y with the Xs
names(d_modelbw)[1] <- "y"         # fix the y variable name

rm(d_filtered, nzv, d_select)           # clean up  

#ngram2
nzv <- nearZeroVar(d_modelng2[,2:ncol(d_modelng2)], uniqueCut=10) # identify columns that are "near zero"

d_select<-d_modelng2[, 2:ncol(d_modelng2)]
d_filtered<-subset(d_select, select=c(-nzv))

# Remove those features that nearZeroVar() identified for removal from dataset.
d_modelng2 <- cbind(d_modelng2$y, d_filtered)   # combine y with the Xs
names(d_modelng2)[1] <- "y"         # fix the y variable name

rm(d_filtered, nzv, d_select)   

#ngram24
nzv <- nearZeroVar(d_modelng24[,2:ncol(d_modelng24)], uniqueCut=10) # identify columns that are "near zero"

d_select<-d_modelng24[, 2:ncol(d_modelng24)]
d_filtered<-subset(d_select, select=c(-nzv))

# Remove those features that nearZeroVar() identified for removal from dataset.
d_modelng24 <- cbind(d_modelng24$y, d_filtered)   # combine y with the Xs
names(d_modelng24)[1] <- "y"         # fix the y variable name

rm(d_filtered, nzv, d_select)   

#ngram3
nzv <- nearZeroVar(d_modelng3[,2:ncol(d_modelng3)], uniqueCut=10) # identify columns that are "near zero"

d_select<-d_modelng3[, 2:ncol(d_modelng3)]
d_filtered<-subset(d_select, select=c(-nzv))

# Remove those features that nearZeroVar() identified for removal from dataset.
d_modelng3 <- cbind(d_modelng3$y, d_filtered)   # combine y with the Xs
names(d_modelng3)[1] <- "y"         # fix the y variable name

rm(d_filtered, nzv, d_select)

save(d_modelbw, file='d_modelbw.Rda')
save(d_modelng2, file='d_modelng2.Rda')
save(d_modelng24, file='d_modelng24.Rda')
save(d_modelng3, file='d_modelng3.Rda')

################################################################################
# Remove Correlated Predictors
################################################################################

#No high correlation for bag of words and ngram


################################################################################
# Remove linear dependencies and remove them
################################################################################
save(d_modelbw, file='d_modelbw.Rda')
load('d_modelbw.Rda')

save(d_modelng2, file='d_modelng2.Rda')
load('d_modelng2.Rda')
#No linear dependencies left

################################################################################
# Standardize (and/ normalize) your input features.
################################################################################

#Bag of words
#create a dataset that keeps unstandarized variables
d_uns <- d_modelbw[,c(2:ncol(d_modelbw), 1)] 

# Keep dummy variables aside so that those are not standarized
# dCats => contains the 0/1 variable, dNums => contains numeric features 
d_modelbw<-as.data.frame(d_modelbw)

numcols <- apply(X=d_modelbw, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(d)
catcols <- apply(X=d_modelbw, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(d)
dNums <- d_modelbw[,numcols]
dCats <- d_modelbw[,catcols]

# Z-score: This will make all the numeric features centered at 0 and have a standard
# deviation of 1. method = c("center", "scale")

#YeoJohnson: makes the distribution of features more bell-shaped c("YeoJohnson)

# Identify the means, standard deviations, other parameters, etc. for transformation
preProcValues <- preProcess(dNums[,2:ncol(dNums)], method = c("center","scale", "YeoJohnson"))

# Transforma variables using the predict() function
dNums <- predict(preProcValues, dNums)

# combine the standardized numeric features with the dummy vars
d_modelbw <- cbind(dNums, dCats)

#Clean-up the environment
rm(preProcValues, numcols, catcols, dNums, dCats)  # clean up


#ngram
#create a dataset that keeps unstandarized variables
d_uns <- d_modelng2[,c(2:ncol(d_modelng2), 1)] 

# Keep dummy variables aside so that those are not standarized
# dCats => contains the 0/1 variable, dNums => contains numeric features 
d_modelng2<-as.data.frame(d_modelng2)

numcols <- apply(X=d_modelng2, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(d)
catcols <- apply(X=d_modelng2, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(d)
dNums <- d_modelng2[,numcols]
dCats <- d_modelng2[,catcols]

# Z-score: This will make all the numeric features centered at 0 and have a standard
# deviation of 1. method = c("center", "scale")

#YeoJohnson: makes the distribution of features more bell-shaped c("YeoJohnson)

# Identify the means, standard deviations, other parameters, etc. for transformation
preProcValues <- preProcess(dNums[,2:ncol(dNums)], method = c("center","scale", "YeoJohnson"))

# Transforma variables using the predict() function
dNums <- predict(preProcValues, dNums)

# combine the standardized numeric features with the dummy vars
d_modelng2 <- cbind(dNums, dCats)

#Clean-up the environment
rm(preProcValues, numcols, catcols, dNums, dCats)  # clean up

#same result as d_modelng2, thus remove other two
rm(d_modelng24, d_modelng3)



