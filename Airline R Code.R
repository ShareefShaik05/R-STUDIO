rm(list = ls())



# Install the required packages if not installed
install.packages("tidyverse")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("textclean")

# Load the required libraries
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(textclean)

# Set your working directory
setwd("C:/Users/share/Downloads/")

# Read the CSV file
data <- read.csv("Airline-Sentiment-2-w-AA.csv", stringsAsFactors = FALSE)

# Explore the structure of the dataset
str(data)

# Ensure that the 'text' column is character type
data$text <- as.character(data$text)

# Create a Corpus
corpus <- Corpus(VectorSource(data$text))

# Text Cleaning and Transformation
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Sentiment Analysis
sentiment_analysis <- data %>%
  select(airline_sentiment, text) %>%
  mutate(clean_text = sapply(data$text, function(x) tolower(removePunctuation(removeNumbers(stripWhitespace(x))))))

# Word Cloud
wordcloud(words = unlist(strsplit(sentiment_analysis$clean_text, " ")), min.freq = 5, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))

# Display sentiment analysis summary
summary(sentiment_analysis$airline_sentiment)

# Bar plot for sentiment distribution
ggplot(sentiment_analysis, aes(x = airline_sentiment, fill = airline_sentiment)) +
  geom_bar() +
  ggtitle("Sentiment Distribution") +
  xlab("Sentiment") +
  ylab("Count") +
  theme_minimal()

# Data Cleaning and Preprocessing
clean_text <- tolower(removePunctuation(removeNumbers(stripWhitespace(data$text))))

# Create a data frame for analysis
sentiment_data <- data.frame(text = clean_text, sentiment = data$airline_sentiment)

# Binary Sentiment Labeling
sentiment_data$sentiment_binary <- ifelse(sentiment_data$sentiment == "negative", 0, 1)

# Plotting Binary Sentiment Distribution
ggplot(sentiment_data, aes(x = sentiment_binary, fill = factor(sentiment_binary))) +
  geom_bar() +
  ggtitle("Binary Sentiment Distribution") +
  xlab("Sentiment Binary (0: Negative, 1: Not Negative)") +
  ylab("Count") +
  theme_minimal()

# Convert to factor with levels
sentiment_data$sentiment_binary <- factor(sentiment_data$sentiment_binary, levels = c(0, 1))

# Check levels again
levels(sentiment_data$sentiment_binary)












# Install and load the required packages if not installed
install.packages("randomForest")

# Load the randomForest library
library(randomForest)


install.packages("caret")
library(caret)





# Split the data into training and testing sets
set.seed(123)
split_index <- createDataPartition(sentiment_data$sentiment, p = 0.8, list = FALSE)
train_data <- sentiment_data[split_index, ]
test_data <- sentiment_data[-split_index, ]

# Define the predictive model using Random Forest
if (!require("randomForest")) install.packages("randomForest")
library(randomForest)
model <- randomForest(sentiment_binary ~ text, data = train_data, ntree = 100)

# Rest of your code for making predictions and evaluating the model


# Define the predictive model using Random Forest
model <- randomForest(sentiment_binary ~ text, data = train_data, ntree = 100)

# Rest of your code for making predictions and evaluating the model


# Define the predictive model using Random Forest
model <- randomForest(sentiment_binary ~ text, data = train_data, ntree = 100)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "response")

# Evaluate the model performance
confusion_matrix <- table(predictions, test_data$sentiment_binary)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)


# Display the evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")

cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")






if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)
# Create a data frame for model metrics
model_metrics_rf <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy_rf, precision_rf, recall_rf, f1_score_rf)
)

# Plot the model metrics
ggplot(model_metrics_rf, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Random Forest Model Metrics") +
  xlab("Metric") +
  ylab("Value") +
  theme_minimal()



# Assuming you have the following objects available from your previous code:
# accuracy, precision, recall, f1_score

# Create a data frame for model metrics
model_metrics_rf <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score)
)

# Load the ggplot2 library if not loaded
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Plot the model metrics
ggplot(model_metrics_rf, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Random Forest Model Metrics") +
  xlab("Metric") +
  ylab("Value") +
  theme_minimal()
