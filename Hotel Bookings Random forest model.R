library(readr)
hotel_bookings <- read_csv("hotel_bookings.csv")
View(hotel_bookings)
attach(hotel_bookings)
hotel <- hotel_bookings
library(randomForest)
library(caret)
hotel$is_canceled <- as.factor(hotel$is_canceled)
# Removing Missing values
hotel <- na.omit(hotel)
# Split the data into training set and Test set
set.seed(123)
train_index <- sample(1:nrow(hotel), nrow(hotel)*0.7)
train_set <- hotel[train_index, ]
test_set  <- hotel[-train_index, ]
# Build a model using decision trees
rf_model <- randomForest(is_canceled ~ ., data=train_set, ntree=100)
# Predict on the test set
predictions <- predict(rf_model, test_set)
predictions <- factor(predictions, levels=levels(test_set$is_canceled))
# Evaluating the model
confusionMatrix(predictions, test_set$is_canceled)
# Calculate variable importance
importance <- importance(rf_model)
# Print the variable importance
print(importance)
# You can also plot the variable importance for better visualization
varImpPlot(rf_model)
