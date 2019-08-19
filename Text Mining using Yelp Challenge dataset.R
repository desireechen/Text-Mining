# Set working directory.
setwd("~/MSBA/IS5152 Decision Making Technologies/Assignment 3/G12_e0056186_e0130739_e0056094_e0056268")
########## Below are the steps for data pre-processing. Tallmadge city in Ohio state is selected.
## Loading original dataset.
# business <- read.csv("yelp_academic_dataset_business.csv")
# user <- read.csv("yelp_academic_dataset_user.csv")
# review <- read.csv("yelp_review.csv")
# tip <- read.csv("yelp_tip.csv")
# checkin <- read.csv("yelp_checkin.csv")
## Get reviews for the chosen city Tallmadge.
# library("plyr")
# count <- ddply(review,.(business_id), summarize, freq=length(business_id))
# business_new <- merge(x = business, y = count, by = "business_id",all.x = TRUE)
# count1 <- ddply(business_new,.(city), summarize, freq=length(city))
# count2 <- ddply(business_new,.(city), summarize, tot=sum(freq))
# output <- merge(x=count1,y=count2,by = "city",all.x = TRUE)
# business_TM <- subset(business,city=="Tallmadge")
# data_TM <- merge(x = business_TM, y = review, by = "business_id",all.x = TRUE)
# data_TM_1 <- data_TM[,-c(16,20,26)]
# write.csv(data_TM_1,file = "DATA_TM.csv")
## Separate "DATA_TM.csv" into training and testing data in Excel.
## For removing the potential selection bias, "Stratified Random Sampling" is applied.
## Ratio of Train:Test data = 7:3
## Manually create a new column in Train and Test datasets, "repeat."
## According to review, if it is written by customer who had visited previously, label as "1", otherwise, "0".
## As the original datasets are too big, only the subset "TMtrain_repeatdone" & "TMtest_repeatdone" are uploaded.
########## Load Train and Test data relating to Tallmadge city in Ohio state in the United States
TM_train <- read.csv("TMtrain_repeatdone.csv")
TM_test <- read.csv("TMtest_repeatdone.csv")
########## Word Cloud
library("tm")
library("wordcloud")
docs <- Corpus(VectorSource(TM_train$text))
# Text transformation: Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower)) # Convert the text to lower case
docs <- tm_map(docs, removeNumbers) # Remove numbers
docs <- tm_map(docs, removeWords, stopwords("english")) # Remove english common stopwords
# Remove our own stop words
docs <- tm_map(docs, removeWords, c("food", "place", "area", "menu", "one", "will", "also")) 
docs <- tm_map(docs, removePunctuation) # Remove punctuations
docs <- tm_map(docs, stripWhitespace) # Eliminate extra white spaces
docs <- tm_map(docs, stemDocument) # Text stemming
# Build term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
# Generate the Word Cloud
set.seed(123)
wordcloud(words = d$word, freq = d$freq, min.freq = 0,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
########## Dictionary Approach
### Use Positive and Negative words dictionaries to calculate Sentiment Score
library("plyr")
library("stringr") # for string processing
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list) 
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}
# Import dictionaries of positive and negative words
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")
# Calculate sentiment score for Train Data
scores_train = score.sentiment(TM_train$text, pos, neg, .progress='text')
TM_train$senti_score <- scores_train$score
DA_train_senti <- ddply(TM_train,.(business_id),summarize,senti_train=mean(senti_score))
# Calculate sentiment score for Test Data
scores_test = score.sentiment(TM_test$text, pos, neg, .progress='text')
TM_test$senti_score <- scores_test$score
DA_test_senti <- ddply(TM_test,.(business_id),summarize,senti_test=mean(senti_score))
### Use 3 different dictionaries to calculate Social Indexes
score.others = function(sentences, freq.words, .progress='none')
{
  scores1 = laply(sentences,
                  function(sentence, freq.words)
                  {
                    # remove punctuation
                    sentence = gsub("[[:punct:]]", "", sentence)
                    # remove control characters
                    sentence = gsub("[[:cntrl:]]", "", sentence)
                    # remove digits?
                    sentence = gsub('\\d+', '', sentence)
                    # define error handling function when trying tolower
                    tryTolower = function(x)
                    {
                      # create missing value
                      y = NA
                      # tryCatch error
                      try_error = tryCatch(tolower(x), error=function(e) e)
                      # if not an error
                      if (!inherits(try_error, "error"))
                        y = tolower(x)
                      # result
                      return(y)
                    }
                    # use tryTolower with sapply 
                    sentence = sapply(sentence, tryTolower)
                    # split sentence into words with str_split (stringr package)
                    word.list = str_split(sentence, "\\s+")
                    words = unlist(word.list)
                    # compare words to the dictionaries
                    freq.matches = match(words, freq.words)
                    # get the position of the matched term or NA
                    # we just want a TRUE/FALSE
                    freq.matches = !is.na(freq.matches)
                    score1 = sum(freq.matches)
                    return(score1)
                  }, freq.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df1 = data.frame(text=sentences, score1=scores1)
  return(scores.df1)
}
# Import dictionary Affpt containing affection words valuing love and friendship
Affpt = readLines("Affpt.txt")
# Calculate Affpt score for Train Data
scores_Affpt_train = score.others(TM_train$text, Affpt, .progress='text')
TM_train$Affpt_score <- scores_Affpt_train$score1
DA_train_Affpt <- ddply(TM_train,.(business_id),summarize,Affpt_train=mean(Affpt_score))
# Calculate Affpt score for Test Data
scores_Affpt_test = score.others(TM_test$text, Affpt, .progress='text')
TM_test$Affpt_score <- scores_Affpt_test$score1
DA_test_Affpt <- ddply(TM_test,.(business_id),summarize,Affpt_test=mean(Affpt_score))
# Import dictionary Kin containing words denoting kinship
kin = readLines("kin.txt")
# Calculate Kin score for Train Data
scores_kin_train = score.others(TM_train$text, kin, .progress='text')
TM_train$kin_score <- scores_kin_train$score1
DA_train_kin <- ddply(TM_train,.(business_id),summarize,kin_train=mean(kin_score))
# Calculate Kin score for Test Data
scores_kin_test = score.others(TM_test$text, kin, .progress='text')
TM_test$kin_score <- scores_kin_test$score1
DA_test_kin <- ddply(TM_test,.(business_id),summarize,kin_test=mean(kin_score))
# Import dictionary HU containing words referencing social categories of humans
HU = readLines("HU.txt")
# Calculate HU score for Train Data
scores_HU_train = score.others(TM_train$text, HU, .progress='text')
TM_train$HU_score <- scores_HU_train$score1
DA_train_HU <- ddply(TM_train,.(business_id),summarize,HU_train=mean(HU_score))
# Calculate HU score for Test Data
scores_HU_test = score.others(TM_test$text, HU, .progress='text')
TM_test$HU_score <- scores_HU_test$score1
DA_test_HU <- ddply(TM_test,.(business_id),summarize,HU_test=mean(HU_score))
########## Text Classification
library(RTextTools)
TM <- rbind(TM_train,TM_test)
# Prepare document term matrix for Text Classification
dtMatrix <- create_matrix(TM["text"], language="english", removeNumbers=TRUE, stemWords=TRUE, 
                          removeSparseTerms=.998) 
container <- create_container(dtMatrix, TM$repeat., trainSize=1:263,testSize=264:373, virgin=FALSE)
# Train 8 models using 8 algorithms
set.seed(123)
SVM <- train_model(container,"SVM", kernel="linear", cost=1)
set.seed(123)
SLDA <- train_model(container,"SLDA") # this line takes a few minutes to run
set.seed(123)
BOOSTING <- train_model(container,"BOOSTING")
set.seed(123)
BAGGING <- train_model(container,"BAGGING") # this line takes a few minutes to run
set.seed(123)
RF <- train_model(container,"RF") # this line takes a moment to run
set.seed(123)
GLMNET <- train_model(container,"GLMNET")
set.seed(123)
TREE <- train_model(container,"TREE")
set.seed(123)
MAXENT <- train_model(container,"MAXENT")
# Make predictions using 8 trained models
SVM_CLASSIFY <- classify_model(container, SVM)
SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
TREE_CLASSIFY <- classify_model(container, TREE)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
# Output classification results
analytics <- create_analytics(container,
                              cbind(SVM_CLASSIFY, SLDA_CLASSIFY, BOOSTING_CLASSIFY, 
                                    BAGGING_CLASSIFY, RF_CLASSIFY, GLMNET_CLASSIFY, 
                                    TREE_CLASSIFY, MAXENT_CLASSIFY),b=1)
summary(analytics)
# write.csv(analytics@document_summary, "DocumentSummary.csv") # save to csv output file
# For each of the 25 food businesses in Tallmadge city, get the repeat index from Manual Coding.
TC_train_manual <- ddply(TM_train,.(business_id),summarize,repeat_train_manual=mean(repeat.))
TC_test_manual <- ddply(TM_test,.(business_id),summarize,repeat_test_manual=mean(repeat.))
########## Topic Modelling
# install.packages("ldatuning")
library(ldatuning)
library(topicmodels)
# Prepare document term matrix for Topic Modelling by LDA
text1 <- create_matrix(TM["text"],language="english", removeNumbers=TRUE, stemWords=TRUE, 
                       removeSparseTerms=.995)
# Choose optimal K which is the number of topics. This line takes a few minutes to run.
result <- FindTopicsNumber(
  text1,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 0622),
  mc.cores = 2L,
  verbose = TRUE
)
result
FindTopicsNumber_plot(result) # Plot metrics to find the optimal K
# Fit LDA model
set.seed(204)
lda_out <- LDA(text1, k=5) # this line takes a moment to run
lda_out
term <- terms(lda_out,30) # get top 30 frequent terms under each topic
term
topic <- topics(lda_out, 1) # most likely topic identified for each document
topic[1] # to see which topic the 1st document is identified as
# For each document, get the probability distribution of each of the topics
gamma_out <- as.data.frame(lda_out@gamma) 
names(gamma_out) <- c(1:5) 
head(gamma_out)
# Create variables for the each of the 5 topics
prob <- as.data.frame(gamma_out)
colnames(prob)<-c("mgr_svc","mexican","normal","breakfast_waffles","positive")
train_index <- prob[1:263,]
test_index <- prob[264:373,]
topic_train_index <- cbind(TM_train,train_index)
topic_test_index <- cbind(TM_test,test_index)
topic_train <- ddply(topic_train_index,.(business_id),summarize,t1=mean(mgr_svc),t2=mean(mexican),
                     t3=mean(normal),t4=mean(breakfast_waffles),t5=mean(positive))
topic_test <- ddply(topic_test_index,.(business_id),summarize,t1=mean(mgr_svc),t2=mean(mexican),
                    t3=mean(normal),t4=mean(breakfast_waffles),t5=mean(positive))
########## Combine variables from Dictionary Approach, Text Classification, Topic Modelling.
# Combined Training data
final_train <- cbind(DA_train_senti,DA_train_Affpt,DA_train_kin,DA_train_HU,TC_train_manual,topic_train)
final_train <- final_train[,-c(3,5,7,9,11)]
# write.csv(final_train, "Finaltrain.csv") # save to csv output file
# Combined Test data
final_test <- cbind(DA_test_senti,DA_test_Affpt,DA_test_kin,DA_test_HU,TC_test_manual,topic_test)
final_test <- final_test[,-c(3,5,7,9,11)]
# write.csv(final_test, "Finaltest.csv") # save to csv output file
########## Load data file that contains data from external sources
final_train_new <- read.csv("Finaltrain_withpricerank1.csv")
final_test_new <- read.csv("Finaltest_withpricerank1.csv")
# Get the stars of each business
stars_train <- ddply(TM_train,.(business_id),summarize,star=mean(stars.x))
stars_test <- ddply(TM_test,.(business_id),summarize,star=mean(stars.x))
final_train_new <- merge(x=final_train_new,y=stars_train,by="business_id", all.x = TRUE)
final_test_new <- merge(x=final_test_new,y=stars_test,by="business_id", all.x = TRUE)                 
final_train_new <- final_train_new[,-2]
final_test_new <- final_test_new[,-2]
########## Baseline model
baseline <- lm(star ~ senti + Affpt + kin + HU + repeat_manual + t1 + t2 + t3 + t4 + t5 + price, 
               data = final_train_new) 
summary(baseline)
final_test_new$baselinepred <- predict.lm(baseline,newdata = final_test_new)
# install.packages("hydroGOF")
library("hydroGOF")
rmse_test_baseline <- sqrt(mse(final_test_new$baselinepred,final_test_new$star))
########## Variable transformation of Senti * repeat index
final_train_new$impression <- final_train_new$senti * final_train_new$repeat_manual
final_test_new$impression <- final_test_new$senti * final_test_new$repeat_manual
########### Build model with new variable obtained from Features Transformation
lm1 <- lm(star ~ senti + Affpt + kin + HU + repeat_manual + t1 + t2 + t3 + t4 + t5 + 
            price + impression, data = final_train_new) 
summary(lm1)
final_test_new$lm1pred <- predict.lm(lm1,newdata = final_test_new)
rmse_test_lm1 <- sqrt(mse(final_test_new$lm1pred,final_test_new$star))
# StepAIC for variable selection
library(MASS)
step_lm1 <- stepAIC(lm1,direction="both")
summary(step_lm1) # lm(formula=star~repeat_manual+t1+t2+t3+t4+price+impression,data=final_train_new)
final_test_new$steplm1pred <- predict.lm(step_lm1,newdata = final_test_new)
rmse_test_steplm1 <- sqrt(mse(final_test_new$steplm1pred,final_test_new$star))
# Predict missing rank values in Train Data using the 5 variables not in model after stepAIC
withrank_train <- subset(final_train_new,rank!='')
withoutrank_train <- subset(final_train_new,is.na(rank)==TRUE)
train_rank <- lm(rank ~ senti + Affpt + kin + HU + t5, data = withrank_train)
step_train <- stepAIC(train_rank,direction="both")
withoutrank_train$rank <- predict.lm(step_train,newdata = withoutrank_train)
final_train_new <- rbind(withrank_train,withoutrank_train)
# Predict missing rank values in Test Data using the 5 variables not in model after stepAIC
withrank_test <- subset(final_test_new,rank!='')
withoutrank_test <- subset(final_test_new,is.na(rank)==TRUE)
test_rank <- lm(rank ~ senti + Affpt + kin + HU + t5, data = withrank_test)
step_test <- stepAIC(test_rank,direction = "both")
withoutrank_test$rank <- predict.lm(step_test,newdata = withoutrank_test)
final_test_new <- rbind(withrank_test,withoutrank_test)
# Build model with all the missing rank values filled up, and use StepAIC for variable selection
lm2 <- lm(formula = star ~ repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, 
          data = final_train_new)
summary(lm2)
final_test_new$lm2pred <- predict.lm(lm2,newdata = final_test_new)
rmse_test_lm2 <- sqrt(mse(final_test_new$lm2pred,final_test_new$star))
step_lm2 <- stepAIC(lm2,direction = "both")
summary(step_lm2)
# final model: lm(formula=star~repeat_manual+t1+t2+t3+t4+price+impression+rank,data=final_train_new)
final_test_new$steplm2pred <- predict.lm(step_lm2,newdata = final_test_new)
rmse_test_steplm2 <- sqrt(mse(final_test_new$steplm2pred,final_test_new$star))
########## Variables scaling for Train Data
for (i in 1:25) {
  final_train_new$repeat_manual.[i] <- (final_train_new$repeat_manual[i]-min(final_train_new$repeat_manual))/(max(final_train_new$repeat_manual)-min(final_train_new$repeat_manual))
  final_train_new$t1.[i] <- (final_train_new$t1[i]-min(final_train_new$t1))/(max(final_train_new$t1)-min(final_train_new$t1))
  final_train_new$t2.[i] <- (final_train_new$t2[i]-min(final_train_new$t2))/(max(final_train_new$t2)-min(final_train_new$t2))
  final_train_new$t3.[i] <- (final_train_new$t3[i]-min(final_train_new$t3))/(max(final_train_new$t3)-min(final_train_new$t3))
  final_train_new$t4.[i] <- (final_train_new$t4[i]-min(final_train_new$t4))/(max(final_train_new$t4)-min(final_train_new$t4))
  final_train_new$price.[i] <- (final_train_new$price[i]-min(final_train_new$price))/(max(final_train_new$price)-min(final_train_new$price))
  final_train_new$impression.[i] <- (final_train_new$impression[i]-min(final_train_new$impression))/(max(final_train_new$impression)-min(final_train_new$impression))
  final_train_new$rank.[i] <- (final_train_new$rank[i]-min(final_train_new$rank))/(max(final_train_new$rank)-min(final_train_new$rank))
}
# Variable scaling for Test Data
for (i in 1:25) {
  final_test_new$repeat_manual.[i] <- (final_test_new$repeat_manual[i]-min(final_test_new$repeat_manual))/(max(final_test_new$repeat_manual)-min(final_test_new$repeat_manual))
  final_test_new$t1.[i] <- (final_test_new$t1[i]-min(final_test_new$t1))/(max(final_test_new$t1)-min(final_test_new$t1))
  final_test_new$t2.[i] <- (final_test_new$t2[i]-min(final_test_new$t2))/(max(final_test_new$t2)-min(final_test_new$t2))
  final_test_new$t3.[i] <- (final_test_new$t3[i]-min(final_test_new$t3))/(max(final_test_new$t3)-min(final_test_new$t3))
  final_test_new$t4.[i] <- (final_test_new$t4[i]-min(final_test_new$t4))/(max(final_test_new$t4)-min(final_test_new$t4))
  final_test_new$price.[i] <- (final_test_new$price[i]-min(final_test_new$price))/(max(final_test_new$price)-min(final_test_new$price))
  final_test_new$impression.[i] <- (final_test_new$impression[i]-min(final_test_new$impression))/(max(final_test_new$impression)-min(final_test_new$impression))
  final_test_new$rank.[i] <- (final_test_new$rank[i]-min(final_test_new$rank))/(max(final_test_new$rank)-min(final_test_new$rank))
}
# Construct the data file of before-scaling variables for training the model
train_before_scaling <- final_train_new[,c(1,6,7,8,9,10,12,13,14,15)]
test_before_scaling <- final_test_new[,c(1,6,7,8,9,10,12,13,14,16)]
# Construct the data file of after-scaling variables for training the model
train_after_scaling <- final_train_new[,c(1,14,16:23)]
test_after_scaling <- final_test_new[,c(1,14,21:28)]
########## Fit SVM model
# Cross validation
n <- 0622
set.seed(n)
library(caret)
train_control <- trainControl(method="cv", number=10)
# Use SVM on before-scaled variables
library(kernlab)
set.seed(n)
svm_before <- train(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, 
                    data=train_before_scaling, trControl=train_control, method="svmRadial")
print(svm_before)
# Use SVM on after-scaled variables
set.seed(n)
svm_after <- train(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., 
                   data=train_after_scaling, trControl=train_control, method="svmRadial")
print(svm_after)
# Get prediction using model built on before-scaled variables
train_before_scaling$pred1 <- predict(svm_before,train_before_scaling[,-c(1,9)],type = "raw")
test_before_scaling$pred1 <- predict(svm_before,test_before_scaling[,-c(1,9)],type = "raw")
# Get prediction using model built on after-scaled variables
train_after_scaling$pred1 <- predict(svm_after,train_after_scaling[,-c(1,2)],type = "raw")
test_after_scaling$pred1 <- predict(svm_after,test_after_scaling[,-c(1,2)],type = "raw")
# Get the RMSE of model using before-scaled variables
train_rmse_before_scaling <- sqrt(mse(train_before_scaling$pred1,train_before_scaling$star))
test_rmse_before_scaling <- sqrt(mse(test_before_scaling$pred1,test_before_scaling$star))
# Get the RMSE of model using after-scaled variables
train_rmse_after_scaling <- sqrt(mse(train_after_scaling$pred1,train_after_scaling$star))
test_rmse_after_scaling <- sqrt(mse(test_after_scaling$pred1,test_after_scaling$star))
# Tune SVM used on before-scaled variables
grid1 <- expand.grid(.sigma=0.05701049,.C=c(1,1.5,5)) 
set.seed(n)
svm_tune1 <- train(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, 
                   data=train_before_scaling, trControl=train_control, 
                   method="svmRadial", metric="RMSE", tuneGrid=grid1) 
print(svm_tune1) # best tuned parameters using before-scaled variables are sigma = 0.05701049 and C = 5. 
# Tune SVM used on after-scaled variables
grid2 <- expand.grid(.sigma=0.05701049,.C=c(1,1.5,5)) 
set.seed(n)
svm_tune2 <- train(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., 
                   data=train_after_scaling, trControl=train_control, 
                   method="svmRadial", metric="RMSE", tuneGrid=grid2) 
print(svm_tune2) # best tuned parameters using after-scaled variables are sigma = 0.05701049 and C = 5. 
# Get prediction using tuned model built on before-scaled variables
train_before_scaling$pred2 <- predict(svm_tune1,train_before_scaling[,-c(1,9,11)],type = "raw")
test_before_scaling$pred2 <- predict(svm_tune1,test_before_scaling[,-c(1,9,11)],type = "raw")
# Get prediction using tuned model built on after-scaled variables
train_after_scaling$pred2 <- predict(svm_tune2,train_after_scaling[,-c(1,2,11)],type = "raw")
test_after_scaling$pred2 <- predict(svm_tune2,test_after_scaling[,-c(1,2,11)],type = "raw")
# Get the RMSE of tuned model using before-scaled variables
train_rmse1_before_scaling <- sqrt(mse(train_before_scaling$pred2,train_before_scaling$star))
test_rmse1_before_scaling <- sqrt(mse(test_before_scaling$pred2,test_before_scaling$star))
# Get the RMSE of tuned model using after-scaled variables
train_rmse1_after_scaling <- sqrt(mse(train_after_scaling$pred2,train_after_scaling$star))
test_rmse1_after_scaling <- sqrt(mse(test_after_scaling$pred2,test_after_scaling$star))
########## Ensemble method of bagging and SVM.
# Create the empty 25-by-10 matrix to contain the predicted values.
predictions1_1 <- matrix(0, nrow=25, ncol=10)
predictions2_1 <- matrix(0, nrow=25, ncol=10)
predictions3_1 <- matrix(0, nrow=25, ncol=10)
predictions4_1 <- matrix(0, nrow=25, ncol=10)
predictions1_2 <- matrix(0, nrow=25, ncol=10)
predictions2_2 <- matrix(0, nrow=25, ncol=10)
predictions3_2 <- matrix(0, nrow=25, ncol=10)
predictions4_2 <- matrix(0, nrow=25, ncol=10)
trainpredictions1_1 <- matrix(0, nrow=25, ncol=10)
trainpredictions2_1 <- matrix(0, nrow=25, ncol=10)
trainpredictions3_1 <- matrix(0, nrow=25, ncol=10)
trainpredictions4_1 <- matrix(0, nrow=25, ncol=10)
trainpredictions1_2 <- matrix(0, nrow=25, ncol=10)
trainpredictions2_2 <- matrix(0, nrow=25, ncol=10)
trainpredictions3_2 <- matrix(0, nrow=25, ncol=10)
trainpredictions4_2 <- matrix(0, nrow=25, ncol=10)
# Ensemble method on before-scaled variables
library(e1071)
k <- 10 # There are 10 bags we would like to have.
# Get predictions for Ensemble method. Predict on test data.
set.seed(n)
for (j in 1:k) {
  # sample k random number between 10 and 25 with replacement. Minimum number is 10, because we have 10 bags and each bag has to have at least 1 row of data.
  set.seed(n)
  boot <- sample(k:25,k,replace=TRUE) 
  sboot <- train_before_scaling[1:boot[j],] # If boot value is 24, this takes the rows 1 to 24 for the kth bag.
  set.seed(n)
  tuned <- tune.svm(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, data = sboot, 
                    gamma = 10^(-6:-1), cost = 10^(-3:3))
  cc <- as.numeric(tuned$best.parameters[2])
  gg <- as.numeric(tuned$best.parameters[1])
  set.seed(n)
  modellin1_1 <- svm(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, sboot, 
                     type = "eps-regression", cost = cc, gamma = gg, kernel = "linear")
  set.seed(n)
  modelpoly2_1 <- svm(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, sboot, 
                      type = "eps-regression", cost = cc, gamma = gg, kernel = "polynomial")
  set.seed(n)
  modelrad3_1 <- svm(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, sboot, 
                     type = "eps-regression", cost = cc,gamma = gg, kernel = "radial")
  set.seed(n)
  modeltan4_1 <- svm(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, sboot, 
                     type = "eps-regression", cost = cc,gamma = gg, kernel = "sigmoid")
  # Store the 25 prediction values in the kth column.
  predictions1_1[,j] <- predict(modellin1_1,newdata=test_before_scaling, decision.values=F) 
  predictions2_1[,j] <- predict(modelpoly2_1,newdata=test_before_scaling, decision.values=F)
  predictions3_1[,j] <- predict(modelrad3_1,newdata=test_before_scaling, decision.values=F)
  predictions4_1[,j] <- predict(modeltan4_1,newdata=test_before_scaling, decision.values=F)
}
# Get predictions for Ensemble method. Predict on train data.
set.seed(n)
for (j in 1:k) {
  set.seed(n)
  boot <- sample(k:25,k,replace=TRUE) 
  sboot <- train_before_scaling[1:boot[j],] 
  set.seed(n)
  tuned <- tune.svm(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, data = sboot, 
                    gamma = 10^(-6:-1), cost = 10^(-3:3))
  cc <- as.numeric(tuned$best.parameters[2])
  gg <- as.numeric(tuned$best.parameters[1])
  set.seed(n)
  modellin1_1 <- svm(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, sboot, 
                     type = "eps-regression", cost = cc, gamma = gg, kernel = "linear")
  set.seed(n)
  modelpoly2_1 <- svm(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, sboot, 
                      type = "eps-regression", cost = cc, gamma = gg, kernel = "polynomial")
  set.seed(n)
  modelrad3_1 <- svm(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, sboot, 
                     type = "eps-regression", cost = cc,gamma = gg, kernel = "radial")
  set.seed(n)
  modeltan4_1 <- svm(star~repeat_manual + t1 + t2 + t3 + t4 + price + impression + rank, sboot, 
                     type = "eps-regression", cost = cc,gamma = gg, kernel = "sigmoid")
  trainpredictions1_1[,j] <- predict(modellin1_1,newdata=train_before_scaling, decision.values=F) 
  trainpredictions2_1[,j] <- predict(modelpoly2_1,newdata=train_before_scaling, decision.values=F)
  trainpredictions3_1[,j] <- predict(modelrad3_1,newdata=train_before_scaling, decision.values=F)
  trainpredictions4_1[,j] <- predict(modeltan4_1,newdata=train_before_scaling, decision.values=F)
}
# Ensemble method on after-scaled variables
m <- 10 # Again, we would like to have 10 bags.
# Get predictions for Ensemble method. Predict on test data.
set.seed(n)
for (i in 1:m) {
  set.seed(n)
  boot <- sample(m:25,k,replace=TRUE) 
  sboot <- train_after_scaling[1:boot[i],]
  set.seed(n)
  tuned <- tune.svm(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., data = sboot, 
                    gamma = 10^(-6:-1), cost = 10^(-3:3))
  cc <- as.numeric(tuned$best.parameters[2])
  gg <- as.numeric(tuned$best.parameters[1])
  set.seed(n)
  modellin1_2 <- svm(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., sboot, 
                     type = "eps-regression", cost = cc, gamma = gg, kernel = "linear")
  set.seed(n)
  modelpoly2_2 <- svm(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., sboot, 
                      type = "eps-regression", cost = cc, gamma = gg, kernel = "polynomial")
  set.seed(n)
  modelrad3_2 <- svm(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., sboot, 
                     type = "eps-regression", cost = cc, gamma = gg, kernel = "radial")
  set.seed(n)
  modeltan4_2 <- svm(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., sboot, 
                     type = "eps-regression", cost = cc, gamma = gg, kernel = "sigmoid")
  predictions1_2[,i] <- predict(modellin1_2,newdata=test_after_scaling, decision.values=F)
  predictions2_2[,i] <- predict(modelpoly2_2,newdata=test_after_scaling, decision.values=F)
  predictions3_2[,i] <- predict(modelrad3_2,newdata=test_after_scaling, decision.values=F)
  predictions4_2[,i] <- predict(modeltan4_2,newdata=test_after_scaling, decision.values=F)
}
# Get predictions for Ensemble method. Predict on train data.
set.seed(n)
for (i in 1:m) {
  set.seed(n)
  boot <- sample(m:25,k,replace=TRUE) 
  sboot <- train_after_scaling[1:boot[i],]
  set.seed(n)
  tuned <- tune.svm(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., data = sboot, 
                    gamma = 10^(-6:-1), cost = 10^(-3:3))
  cc <- as.numeric(tuned$best.parameters[2])
  gg <- as.numeric(tuned$best.parameters[1])
  set.seed(n)
  modellin1_2 <- svm(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., sboot, 
                     type = "eps-regression", cost = cc, gamma = gg, kernel = "linear")
  set.seed(n)
  modelpoly2_2 <- svm(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., sboot, 
                      type = "eps-regression", cost = cc, gamma = gg, kernel = "polynomial")
  set.seed(n)
  modelrad3_2 <- svm(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., sboot, 
                     type = "eps-regression", cost = cc, gamma = gg, kernel = "radial")
  set.seed(n)
  modeltan4_2 <- svm(star~repeat_manual. + t1. + t2. + t3. + t4. + price. + impression. + rank., sboot, 
                     type = "eps-regression", cost = cc, gamma = gg, kernel = "sigmoid")
  trainpredictions1_2[,i] <- predict(modellin1_2,newdata=train_after_scaling, decision.values=F)
  trainpredictions2_2[,i] <- predict(modelpoly2_2,newdata=train_after_scaling, decision.values=F)
  trainpredictions3_2[,i] <- predict(modelrad3_2,newdata=train_after_scaling, decision.values=F)
  trainpredictions4_2[,i] <- predict(modeltan4_2,newdata=train_after_scaling, decision.values=F)
}
# Calculate average number of each row
predictions1_1 <- as.data.frame(predictions1_1)
predictions1_1$average <- rowMeans(predictions1_1[,1:10])
predictions2_1 <- as.data.frame(predictions2_1)
predictions2_1$average <- rowMeans(predictions2_1[,1:10])
predictions3_1 <- as.data.frame(predictions3_1)
predictions3_1$average <- rowMeans(predictions3_1[,1:10])
predictions4_1 <- as.data.frame(predictions4_1)
predictions4_1$average <- rowMeans(predictions4_1[,1:10]) 
predictions1_2 <- as.data.frame(predictions1_2)
predictions1_2$average <- rowMeans(predictions1_2[,1:10])
predictions2_2 <- as.data.frame(predictions2_2)
predictions2_2$average <- rowMeans(predictions2_2[,1:10])
predictions3_2 <- as.data.frame(predictions3_2)
predictions3_2$average <- rowMeans(predictions3_2[,1:10])
predictions4_2 <- as.data.frame(predictions4_2)
predictions4_2$average <- rowMeans(predictions4_2[,1:10])
trainpredictions1_1 <- as.data.frame(trainpredictions1_1)
trainpredictions1_1$average <- rowMeans(trainpredictions1_1[,1:10])
trainpredictions2_1 <- as.data.frame(trainpredictions2_1)
trainpredictions2_1$average <- rowMeans(trainpredictions2_1[,1:10])
trainpredictions3_1 <- as.data.frame(trainpredictions3_1)
trainpredictions3_1$average <- rowMeans(trainpredictions3_1[,1:10])
trainpredictions4_1 <- as.data.frame(trainpredictions4_1)
trainpredictions4_1$average <- rowMeans(trainpredictions4_1[,1:10])
trainpredictions1_2 <- as.data.frame(trainpredictions1_2)
trainpredictions1_2$average <- rowMeans(trainpredictions1_2[,1:10])
trainpredictions2_2 <- as.data.frame(trainpredictions2_2)
trainpredictions2_2$average <- rowMeans(trainpredictions2_2[,1:10])
trainpredictions3_2 <- as.data.frame(trainpredictions3_2)
trainpredictions3_2$average <- rowMeans(trainpredictions3_2[,1:10])
trainpredictions4_2 <- as.data.frame(trainpredictions4_2)
trainpredictions4_2$average <- rowMeans(trainpredictions4_2[,1:10])
# Accuracy comparison by RMSE using model trained on before-scaled variables
# Linear
train_before_scaling$en_pred1 <- trainpredictions1_1$average
test_before_scaling$en_pred1 <- predictions1_1$average
trainrmse1_en1_before_scaling <- sqrt(mse(train_before_scaling$en_pred1,train_before_scaling$star))
rmse1_en1_before_scaling <- sqrt(mse(test_before_scaling$en_pred1,test_before_scaling$star))
# Polynomial
train_before_scaling$en_pred2 <- trainpredictions2_1$average
test_before_scaling$en_pred2 <- predictions2_1$average
trainrmse1_en2_before_scaling <- sqrt(mse(train_before_scaling$en_pred2,train_before_scaling$star))
rmse1_en2_before_scaling <- sqrt(mse(test_before_scaling$en_pred2,test_before_scaling$star))
# Radial
train_before_scaling$en_pred3 <- trainpredictions3_1$average
test_before_scaling$en_pred3 <- predictions3_1$average
trainrmse1_en3_before_scaling <- sqrt(mse(train_before_scaling$en_pred3,train_before_scaling$star))
rmse1_en3_before_scaling <- sqrt(mse(test_before_scaling$en_pred3,test_before_scaling$star))
# Sigmoid
train_before_scaling$en_pred4 <- trainpredictions4_1$average
test_before_scaling$en_pred4 <- predictions4_1$average
trainrmse1_en4_before_scaling <- sqrt(mse(train_before_scaling$en_pred4,train_before_scaling$star))
rmse1_en4_before_scaling <- sqrt(mse(test_before_scaling$en_pred4,test_before_scaling$star))
# Accuracy comparison by RMSE using model trained on after-scaled variables
# Linear
train_after_scaling$en_pred1 <- trainpredictions1_2$average
test_after_scaling$en_pred1 <- predictions1_2$average
trainrmse1_en1_after_scaling <- sqrt(mse(train_after_scaling$en_pred1,train_after_scaling$star))
rmse1_en1_after_scaling <- sqrt(mse(test_after_scaling$en_pred1,test_after_scaling$star))
# Polynomial
train_after_scaling$en_pred2 <- trainpredictions2_2$average
test_after_scaling$en_pred2 <- predictions2_2$average
trainrmse1_en2_after_scaling <- sqrt(mse(train_after_scaling$en_pred2,train_after_scaling$star))
rmse1_en2_after_scaling <- sqrt(mse(test_after_scaling$en_pred2,test_after_scaling$star))
# Radial
train_after_scaling$en_pred3 <- trainpredictions3_2$average
test_after_scaling$en_pred3 <- predictions3_2$average
trainrmse1_en3_after_scaling <- sqrt(mse(train_after_scaling$en_pred3,train_after_scaling$star))
rmse1_en3_after_scaling <- sqrt(mse(test_after_scaling$en_pred3,test_after_scaling$star))
# Sigmoid
train_after_scaling$en_pred4 <- trainpredictions4_2$average
test_after_scaling$en_pred4 <- predictions4_2$average
trainrmse1_en4_after_scaling <- sqrt(mse(train_after_scaling$en_pred4,train_after_scaling$star))
rmse1_en4_after_scaling <- sqrt(mse(test_after_scaling$en_pred4,test_after_scaling$star)) 