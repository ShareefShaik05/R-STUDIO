install.packages("tm", dependencies=TRUE)
install.packages("twitteR" ,dependencies =  TRUE)
install.packages("wordcloud")
install.packages("ROAuth" , dependencies = TRUE)

install.packages("syuzhet")

library(twitteR)
library("tm")
library("wordcloud")
library(syuzhet)
library(ggplot2)
library(dplyr)

options(warn=-1)
testdata <- read.csv(file="C:/Users/share/OneDrive/Documents/Operational Analytics/Dataset/covid.csv")


summary(testdata)

#Data cleaning
#convert all text to lower case
testdata$text <- iconv(testdata$text,"WINDOWS-1252","UTF-8")
testdata_text <- tolower(testdata$text)



# Replace blank space (ârtâ)
testdata_text <- gsub("rt", "", testdata_text)
# Replace @UserName
testdata_text <- gsub("@\\w+", "", testdata_text)

# Remove punctuation
testdata_text <- gsub("[[:punct:]]", "", testdata_text)

# Remove links
testdata_text <- gsub("http\\w+", "", testdata_text)

# Remove tabs
testdata_text <- gsub("[ |\t]{2,}", "", testdata_text)

# Remove blank spaces at the beginning
testdata_text <- gsub("^ ", "", testdata_text)

# Remove blank spaces at the end
testdata_text <- gsub(" $", "", testdata_text)



#Stop word Handling
#corpus build - remove stop words
testdata_text_corpus <- Corpus(VectorSource(testdata_text))
testdata_text_corpus <- tm_map(testdata_text_corpus, function(x)removeWords(x,stopwords()))

#visualize data
wordcloud(testdata_text_corpus,min.freq = 500,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 15000)

#Sentiment Analysis
#sentiment analysis
testdata_text_sent<-get_nrc_sentiment((testdata_text))

#calculationg total score for each sentiment
testdata_text_sent_score<-data.frame(colSums(testdata_text_sent[,]))

names(testdata_text_sent_score)<-"Score"
testdata_text_sent_score<-cbind("sentiment"=rownames(testdata_text_sent_score),testdata_text_sent_score)
rownames(testdata_text_sent_score)<-NULL


#plotting the sentiments with scores
ggplot(data=testdata_text_sent_score,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people ")
  




#remove positive , negative score 
testdata_text_sent<-get_nrc_sentiment((testdata_text))

testdata_text_sent_no_pos_neg<-select(testdata_text_sent,anger,anticipation,disgust,joy,sadness,surprise,trust)

#calculationg total score for each sentiment
testdata_text_sent_no_pos_neg<-data.frame(colSums(testdata_text_sent_no_pos_neg[,]))

names(testdata_text_sent_no_pos_neg)<-"Score"
testdata_text_sent_no_pos_neg<-cbind("sentiment"=rownames(testdata_text_sent_no_pos_neg),testdata_text_sent_no_pos_neg)
rownames(testdata_text_sent_no_pos_neg)<-NULL


#plotting the sentiments with scores
ggplot(data=testdata_text_sent_no_pos_neg,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people ")

head(testdata)


# Get sentiment scores
sentiment <- get_nrc_sentiment(testdata$text)

# Create binary sentiment target
sentiment$sentiment <- NA  
sentiment$sentiment[sentiment$positive > sentiment$negative] <- "positive"
sentiment$sentiment[sentiment$positive < sentiment$negative] <- "negative"
sentiment$sentiment[sentiment$positive == sentiment$negative] <- "neutral"

# Convert to factor 
sentiment$sentiment <- as.factor(sentiment$sentiment)

# Join back to original data
testdata <- cbind(testdata, sentiment)


#View result
sentiment





# Load necessary libraries (if not already loaded)
library(caret)
library(randomForest)

# Set a seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
trainIndex <- createDataPartition(testdata$sentiment, p = 0.8, list = FALSE)
trainData <- testdata[trainIndex, ]
testData <- testdata[-trainIndex, ]


# Train the Logistic Regression model
logistic_model <- glm(sentiment ~ anger + anticipation + disgust + joy + sadness + surprise + trust, 
                      data = trainData, family = "binomial")

# Predict the sentiment on the test data
logistic_predictions <- predict(logistic_model, newdata = testData, type = "response")

# Convert probabilities to sentiment labels
logistic_predictions <- ifelse(logistic_predictions > 0.5, "positive", "negative")











# Logistic Regression

# Make predictions on test set
logistic_preds <- predict(logistic_model, newdata = testData) 

# Evaluate model accuracy
logistic_accuracy <- mean(logistic_preds == testData$sentiment)

# Random Forest

# Train a random forest model
rf_model <- randomForest(sentiment ~ ., data = trainData)

# Make predictions
rf_preds <- predict(rf_model, newdata = testData)

# Evaluate accuracy 
rf_accuracy <- mean(rf_preds == testData$sentiment)

# Compare accuracy
cat("Logistic Regression Accuracy:", logistic_accuracy, "\n") 
cat("Random Forest Accuracy:", rf_accuracy, "\n")






#2


# Get sentiment scores
sentiment <- get_nrc_sentiment(testdata$text)

# Create sentiment labels
sentiment$sentiment[sentiment$positive > sentiment$negative] <- "positive"
sentiment$sentiment[sentiment$positive < sentiment$negative] <- "negative"

# Filter dataset 
testdata <- testdata[sentiment$sentiment %in% c("positive", "negative"),]

# Create train/test split
trainIndex <- createDataPartition(testdata$sentiment, p = 0.8, list = FALSE) 
trainData <- testdata[trainIndex, ]
testData <- testdata[-trainIndex, ]

# Check levels 
table(trainData$sentiment)

# Train models


# Logistic Regression 

# Encode sentiment as factor
trainData$sentiment <- as.factor(trainData$sentiment)
testData$sentiment <- as.factor(testData$sentiment)

# Check class balance 
table(trainData$sentiment)

# Train simpler model
simple_model <- glm(sentiment ~ anger, data = trainData, family = "binomial")
simple_preds <- predict(simple_model, testData, type = "response")
simple_preds <- ifelse(simple_preds > 0.5, "positive", "negative")
mean(simple_preds == testData$sentiment)

# Random Forest

# 10-fold cross validation
rf_model <- train(sentiment ~ ., data = testdata, method = "rf",
                  trControl = trainControl(method = "cv", number = 10))
rf_model$results

# Tune mtry
tunegrid <- expand.grid(.mtry = c(2, 5, 10, 15))
rf_tune <- train(sentiment ~ ., data = testdata, method = "rf",
                 tuneGrid = tunegrid, trControl = trainControl(method = "cv", number = 10))
rf_tune$results

# Variable importance
varImp(rf_tune$finalModel)


levels(logistic_preds)
levels(testData$sentiment)

levels(logistic_preds) <- levels(testData$sentiment)

# Ensure both factors have the same levels
logistic_preds <- factor(logistic_preds, levels = levels(testData$sentiment))

# Now create the confusion matrix
confusionMatrix(logistic_preds, testData$sentiment)



# Confusion matrix for Logistic Regression
confusionMatrix(logistic_preds, testData$sentiment)

# Confusion matrix for Random Forest
confusionMatrix(rf_preds, testData$sentiment)

#3





# Load the pROC package (if not already loaded)
library(pROC)

# Calculate predicted probabilities for binary classification
logistic_probabilities_binary <- predict(logistic_model, newdata = testData, type = "response")

# Create an ROC curve for binary classification
roc_obj_logistic_binary <- roc(response = binary_sentiment, predictor = logistic_probabilities_binary)

# Get AUC (Area Under the Curve) and other summary statistics
roc_summary <- as.numeric(roc_obj_logistic_binary$auc)

# Print the AUC and other summary statistics
cat("AUC (Area Under the Curve):", roc_summary, "\n")

# You can also print other summary statistics
summary(roc_obj_logistic_binary)


#roc for random forest


# Load the necessary packages
library(pROC)
library(randomForest)

# Train a random forest model (if not already trained)
# Replace the formula and data with your actual data and formula
# Example: rf_model <- randomForest(sentiment ~ ., data = trainData)
# Ensure the rf_model is a valid Random Forest model

# Calculate predicted probabilities for binary classification using the Random Forest model
rf_probabilities_binary <- predict(rf_model, newdata = testData, type = "response")
rf_probabilities_binary <- as.numeric(rf_probabilities_binary) # Ensure it's numeric

# Create an ROC curve for binary classification
roc_obj_rf_binary <- roc(response = binary_sentiment, predictor = rf_probabilities_binary)

# Get AUC (Area Under the Curve) and other summary statistics
roc_summary_rf <- as.numeric(roc_obj_rf_binary$auc)

# Print the AUC and other summary statistics
cat("Random Forest AUC (Area Under the Curve):", roc_summary_rf, "\n")

# Print other summary statistics
summary(roc_obj_rf_binary)

# Plot the ROC curve for binary classification with Random Forest
plot(roc_obj_rf_binary, print.auc = TRUE, print.auc.y = 0.2)



# Install and load the PRROC package if you haven't already
install.packages("PRROC")
library(PRROC)

# Precision-recall curve for Logistic Regression
pr_obj_logistic <- pr.curve(scores.class0 = logistic_probabilities, weights.class0 = as.numeric(testData$sentiment == "positive"), curve = TRUE)
plot(pr_obj_logistic)

# Precision-recall curve for Random Forest
pr_obj_rf <- pr.curve(scores.class0 = rf_probabilities_binary, weights.class0 = as.numeric(testData$sentiment == "positive"), curve = TRUE)
plot(pr_obj_rf)


# Obtain precision-recall curve summaries
summary_logistic <- summary(pr_obj_logistic)
summary_rf <- summary(pr_obj_rf)

# Print the summary for the Logistic Regression model
print(summary_logistic)

# Print the summary for the Random Forest model
print(summary_rf)



model_accuracy <- c(Logistic_Regression = logistic_accuracy, Random_Forest = rf_accuracy)
barplot(model_accuracy, col = c("blue", "green"), ylim = c(0, 1),
        main = "Model Accuracy Comparison", names.arg = names(model_accuracy))

# Print the model accuracy summary
print(model_accuracy)


# Assuming you have the predicted values from Logistic Regression in logistic_preds
# Create a vector of ground truth labels
actual_labels <- testData$sentiment

# Calculate accuracy for Logistic Regression
logistic_accuracy <- mean(logistic_preds == actual_labels)

# Print the accuracy
print(logistic_accuracy)


table(trainData$sentiment)
rf_model <- randomForest(sentiment ~ ., data = trainData)



# Train a Random Forest model using the randomForest function
rf_model <- randomForest(sentiment ~ ., data = trainData)

# Create the variable importance plot
varImpPlot(rf_model, main = "Random Forest Feature Importance")







