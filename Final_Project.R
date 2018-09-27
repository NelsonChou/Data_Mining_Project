setwd('D:/Master Program/03. Begin/Course/07. Data Mining/Project/Data')

#load required packages
library(data.table)
library(ggplot2)
library(dplyr)
#install.packages('stringi')
library(stringi)

#load data, combine two data set since in column 8 the tab delimiter can cause problem
train<-fread('train.tsv', sep='\t', header=TRUE, select = c(1:7))
text<-fread("train.tsv",sep="\t",header=TRUE, select = c(8))
train$item_description<-text

#create category split
#1. split category and then count how many categories for each row
train$category_Split<-sapply(train$category_name, FUN = function(x) strsplit(x, split='/')[[1]])
train$NumofCategory<-lengths(train$category_Split)

#2. Create three columns to capture 1st, 2nd, and 3rd categories
train$FirstCategory<-sapply(train$category_name, FUN = function(x) strsplit(x, split='/')[[1]][1])
train$SecondCategory<-sapply(train$category_name, FUN = function(x) strsplit(x, split='/')[[1]][2])
train$ThirdCategory<-sapply(train$category_name, FUN = function(x) strsplit(x, split='/')[[1]][3])

#3. Check how long are each descriptions, maybe can draw some plots out of it
train$lenthofdescription<-nchar(train$item_description)

#Select the columns required and write to a csv file for Tableau
KaggleTrain<-train[,c(1:7, 10:14)]
write.table(KaggleTrain, file='KaggleTrain.csv', sep='|')

