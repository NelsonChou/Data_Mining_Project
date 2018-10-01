library(Matrix)
library(tidyverse)
library(lightgbm)
library(quanteda)
library(stringr)
library(tictoc)
library(glue)
library(data.table)
library(caret)

train_cols <- cols(
  train_id = col_integer(),
  name = col_character(),
  item_condition_id = col_integer(),
  category_name = col_character(),
  brand_name = col_character(),
  price = col_double(),
  shipping = col_integer(),
  item_description = col_character()
)

train <- read_tsv("/Users/douzhi/Desktop/Fall Mod1 2018/MGMT 571 Data Mining/Project/Datasets/train.tsv", col_types = train_cols)

##############log the price

train <- train %>%
  filter(price != 0)


###############
###############
###############
## Handling categories

temp <- as_tibble(str_split(train$category_name, "/", n = 3, simplify = TRUE))
names(temp) <- paste0("category", 1:3)
train <- bind_cols(train, temp)


# Cleaning and some new features

train <- train %>% 
  mutate(item_description = if_else(is.na(item_description) | 
                                      str_to_lower(item_description) == "no description yet", 
                                    "nodescription", 
                                    item_description),
         desc_length = if_else(item_description == "nodescription", 0L, str_length(item_description)),
         na_brand = is.na(brand_name),
         brand_name = if_else(brand_name == "Céline", "Celine", brand_name)
  )


# expensive_brands <- train %>% 
#   group_by(brand_name) %>% 
#   summarize(n= n(), median = median(price), mean = mean(price)) %>% 
#   filter(n >= 10 & median >= 100)

## Handling missing values

train$category1[is.na(train$category1)] <- "missing" 
train$category2[is.na(train$category2)] <- "missing" 
train$category3[is.na(train$category3)] <- "missing" 

train$brand_name[is.na(train$brand_name)] <- "missing"

train$category_name[is.na(train$category_name)] <- "missing"


names(train)[1] <- "item_id"
head(train,5)
###############partition###########
set.seed(2019)
inTrain <- createDataPartition(y=train$price,   # outcome variable
                               p = .7,
                               list = F)
train <- train[inTrain,]  # training data set
test <- train[-inTrain,]  


######delete log price and store
train$price_log  <- log(train$price + 1)
log_prices  <- train$price_log
train$price_log <- NULL
head(train,5)


all <- bind_rows(train, test)




##############
head(all,5)



##################################################
##############ngrams for description##############
##################################################



###tokenize###
toks <- tokens(all$item_description, 
               remove_punct = TRUE, 
               remove_separators = TRUE, 
               remove_symbols = TRUE)

###token selection###
#remove stop words from toks
nostop_toks <- tokens_select(toks, stopwords('en'), selection = 'remove')
save(nostop_toks,file='nostop_toks')
load('nostop_toks')
###covert token to ngrams###
#try bag of word
onetwogram=tokens_ngrams(nostop_toks, n =1:2)
#try2-4
ngram <- tokens_ngrams(nostop_toks, n = 2:4)

####construct dfm####
dfm24=dfm(ngram)#2:4
save(dfm24,file='dfm24.Rda')
#select features based on frequency
freq_dfm24 <- dfm_trim(dfm24, min_termfreq = 1000)#set as more than 1000 times
save(freq_dfm24, file='freq_dfm24.Rda')

dfm12=dfm(onetwogram)
save(dfm12,file='dfm12.Rda')
freq_dfm12 <- dfm_trim(dfm12, min_termfreq = 1000)#set as more than 1000 times
save(freq_dfm12, file='freq_dfm12.Rda')

rm(nostop_toks,ngram)

################################################
##########ngram for name #######################
################################################
names_tokens <- tokens(tokens_remove(tokens(all$name,    
                       remove_numbers = TRUE, 
                       remove_punct = TRUE,
                       remove_symbols = TRUE, 
                       remove_separators = TRUE), 
                stopwords("english")), 
  ngrams = 1)

names_dfm <- dfm(names_tokens)
trimmed_names_dfm <- dfm_trim(names_dfm, min_count = 30)

################################################
##########preparing data for modeling###########
################################################

#############Essentially creat dummy for catogries
sparse_matrix <- sparse.model.matrix(
  ~item_condition_id + 
    shipping + 
    na_brand + 
    category1 + 
    category2 + 
    category3 + 
    desc_length + 
    brand_name,
  data = all)

#covert dfms to sparse matrix
#name
class(trimmed_names_dfm) <- class(sparse_matrix)
#24gram
class(freq_dfm24) <- class(sparse_matrix)
#12gram
class(freq_dfm12) <- class(sparse_matrix)


aaa12 <- cbind(
  sparse_matrix, # basic features
  freq_dfm12,  # description
  trimmed_names_dfm)
aaa24 <- cbind(
  sparse_matrix, # basic features
  freq_dfm24,  # description
  trimmed_names_dfm)

rownames(aaa12) <- NULL
rownames(aaa24) <- NULL

glue("Number of features: {dim(aaa12)[2]}")
glue("Number of features: {dim(aaa24)[2]}")

sparse_train12 <- aaa12[seq_len(nrow(train)), ]
sparse_test12  <- aaa12[seq(from = (nrow(train) + 1), to = nrow(aaa12)), ]
sparse_train24 <- aaa24[seq_len(nrow(train)), ]
sparse_test24  <- aaa24[seq(from = (nrow(train) + 1), to = nrow(aaa24)), ]

load("/Users/douzhi/Desktop/Fall Mod1 2018/MGMT 571 Data Mining/Project/LightGBM-DATA/sparse_train12.Rda")
load("/Users/douzhi/Desktop/Fall Mod1 2018/MGMT 571 Data Mining/Project/LightGBM-DATA/sparse_train24.Rda")
load("/Users/douzhi/Desktop/Fall Mod1 2018/MGMT 571 Data Mining/Project/LightGBM-DATA/sparse_test12.Rda")
load("/Users/douzhi/Desktop/Fall Mod1 2018/MGMT 571 Data Mining/Project/LightGBM-DATA/sparse_test24.Rda")

dtrain12 <- lgb.Dataset(sparse_train12, label=log_prices)#feed model
dtrain24 <- lgb.Dataset(sparse_train24, label=log_prices)#feed model
##############################################################
##############################################################




nrounds <- 8000
param <- list(
  objective = "regression",
  metric = "rmse"
)

set.seed(333)

modelng12 <- lgb.train(
  params = param,
  data = dtrain12,
  nrounds = nrounds,
  learning_rate = 1,
  subsample = 0.7,
  max_depth = 4,
  eval_freq = 50,
  verbose = -1,
  nthread = 6)

modelng24 <- lgb.train(
  params = param,
  data = dtrain24,
  nrounds = nrounds,
  learning_rate = 1,
  subsample = 0.7,
  max_depth = 4,
  eval_freq = 50,
  verbose = -1,
  nthread = 4)

#############Predict#############
log_predicted12 <- predict(modelng12, sparse_test12)
log_predicted24 <- predict(modelng24, sparse_test24)

#########model measuring############
#save the model
save(modelng12,file="/Users/douzhi/Desktop/Fall Mod1 2018/MGMT 571 Data Mining/Project/modelng12.Rda")

#log the price in test set
testlog=test
test$price_log  <- log(test$price + 1)

postResample(pred =log_predicted12, obs = test$price_log)
postResample(pred =log_predicted24, obs = test$price_log)


predicted12 <- exp(log_predicted12) - 1
predicted24 <- exp(log_predicted24) - 1

postResample(pred =predicted12, obs = test$price)
postResample(pred =predicted24, obs = test$price)
