setwd('D:/Master Program/03. Begin/Course/07. Data Mining/Project/Data')

rm(list=ls())
###############################
#load required packages
###############################
#install.packages('qdap')
#install.packages('data.table')
#install.packages('stringi')
#install.packages('qdap')
#install.packages('tm')
#install.packages('wordcloud')
#install.packages('plotrix')
#install.packages('dendextend')
#install.packages('ggthemes')
#install.packages('RWeka')
#install.packages('rlang')
library(data.table)
library(ggplot2)
library(dplyr)
library(stringi)
library(qdap)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggthemes)
library(RWeka)
library(rlang)
library(sqldf)

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
d$brand_name<-tolower(d$brand_name)
d$brand_name<-removePunctuation(d$brand_name)
d$brand_name<-stripWhitespace(d$brand_name)
d$brand_name<-replace_symbol(d$brand_name)
d$brand_name<-replace_number(d$brand_name)
#Filling in missing for later dummy variable creation
d$brand_name<-ifelse(is.na(d$brand_name),'Missing', d$brand_name)

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

#create term dataframe to be matched with original dataset
labeledTerms_name <- as.data.frame(as.matrix(dtm_name))
rm(term_count_source, term_count_corpus, dtm_name)

save(dtm_name, file="Bag_of_words_name.Rda")

#########################
####ngram
#########################




###Subset required columns only
#change column name and sort column, drop item_description_processed here


#########################
####Dummy variables
#########################

#subset columns for dummy variables creation
names(d)[6]<-'y'
d_dummy <- d[,c(6,3,9:11, 13)]

#Create dummy based on item_condition, first to third category
library(caret)

#convert item_condition_id to character for dummy variable
d_dummy$item_condition_id<-as.character(d_dummy$item_condition_id)

dummies <- dummyVars(y ~ ., data = d_dummy)            # create dummyes for Xs
ex <- data.frame(predict(dummies, newdata = d_dummy))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))                # removes dots from col names
d_dummy <- cbind(d_dummy$y, ex) 
rm(ex)

save(d_dummy, file='d_dummy.Rda')
save(d, file='d.Rda')
save(labeledTerms, file='labeledTerms.Rda')
save(labeledTerms_name, file='labeledTerms_name.Rda')

#merge d_dummy back with d

d2<-d[, c(6, 5,7, 14:15)]
d_dummy<-subset(d_dummy, select=-c(item_condition_id5))
d2<-cbind(d, d_dummy)





#Identify the top brands with more than 1%
#brand_percentage<-aggregate(data.frame(count = d$brand_name_processed), list(value = d$brand_name_processed), length)
#brand_percentage$Percentage<-brand_percentage$count/sum(brand_percentage$count)

#temp<-head(brand_percentage[order(brand_percentage$Percentage, decreasing = TRUE),], 17)
#sum(temp$Percentage)




#########################
#### Modeling
#########################


#install.packages('plm')
library(plm)
library(foreign)

#Fixed effect fixed effect
fixed_effect<-plm(Sev_scores_unnecessary_drug_adj ~ OTPOST, data = df2, index = c('provnum', 'surveyyear'), model = 'within', effect = 'twoways')
summary(fixed_effect_Sev_scores_unnecessary_drug_adj)

#Select the columns required and write to a csv file for Tableau
#Kaggled<-d[,c(3,5:7, 10:14)]
#write.table(Kaggled, file='Kaggled.csv', sep='|', row.names = FALSE, eol='\n')


#d[is.null(d$price)==TRUE,]

#summary(d)



#bins=seq(from=0, to=max(Kaggled$price)+65, by = 65)
#hist(Kaggled$price, breaks = bins)



