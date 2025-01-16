#German Credit Risk Classification Analysis

#Required Libraries
library(tidyverse)     #Data Manipulation
library(caret)         #Machine learning workflow
library(rpart)         #Decision Tree Algorithm
library(rpart.plot)    #Decision Tree visualization
library(e1071)         #Additional model evaluation tools
library(randomForest)


#Distribution of target variable
table(credit_data_recode$assessment)
prop.table(table(credit_data_recode$assessment))


#visualize categorical variable
categorical_vars <- c("status", "credit_history","purpose", "savings_account",
      "employment", "personal_status", "housing", "job")

plot_catergorical_distribution <- function(data, var) {
  ggplot(data, aes(x = !!sym(var), fill = assessment)) +
    geom_bar(position = "fill") + 
    theme_minimal() +
    labs(title = paste("Distribution of", var, "by Credit Assessment"),
         y = "Proportion") +
    coord_flip()
}

#Create plots for each categorical variable
categorical_plots <- lapply(categorical_vars, function(var)
  {
  plot_catergorical_distribution(credit_data_recode, var)
})


#2. Feature Engineering
#Handle missing values if any
credit_data_recode <- credit_data_recode %>%
  mutate(across(where(is.character), ~ replace_na(., "unknown")))

#Create age groups
credit_data_recode <- credit_data_recode %>%
  mutate(age_group = cut(age,
                         breaks = c(0, 25, 35, 45, 55, Inf), 
                         labels = c("18-25", "26-35", "36-45", "46-55", "55+")))

#Encoding categorical variables
credit_data_recode <- credit_data_recode %>%
  mutate(
    status_encoded = as.numeric(factor(status)),
    credit_history_encoded = as.numeric(factor
  (credit_history)),
   purpose_encoded = as.numeric(factor(purpose)),
   savings_account_encoded = as.numeric(factor
    (savings_account)),
  employment_encoded = as.numeric(factor(employment))
  )


# 3. Model Building
#Set seed for reproducibility
set.seed(100)

#Shuffle data before splitting
credit_data_recode <- credit_data_recode[sample (nrow(credit_data_recode)), ]

#split data into training and testing sets
index <- createDataPartition(credit_data_recode$assessment, p = 0.7, list = FALSE)
  train_data <- credit_data_recode [index, ]
  test_data <- credit_data_recode [-index, ]


#prepare features and target
features <- c("duration", "credit_amount",
  "installent_rate", "age",
  "num_credits", "status_encoded",
  "credit_history",
  "savings_account_encoded", "employment_encoded")


#Decision Tree Model
dt_model <- rpart(
  formula = as.factor(assessment) ~ .,
  data = train_data[, c(features, "assessment")],
  method = "class",
  control = rpart.control(
    maxdepth = 10,
    minsplit = 20,     #minimum observation in node before splitting
    cp = 0.01,         # complexity parameter
    minbucket = 7      # minimum observation in terminal node
    )
)

#Using Random Forest
rf_model <- randomForest(
  as.factor(assessment) ~ .,
  data = train_data [, c(features, "assessment")],
  ntree = 500,
  mtry = sqrt(length(features))
)

#plot decision tree
rpart.plot(dt_model,
           main = "Credit Risk Decision Tree",
           fallen.leaves = T)

#4. Model Evaluation
# Predictions for Decision Tree Model
predictions <- predict(dt_model, test_data[, features],
          type = "class")

#Predictions for Random Forest Model
predictions_rf <- predict(rf_model, test_data[, features],
                       type = "class")

# Confusion Matrix for decision Tree Model
conf_matrix <- confusionMatrix(predictions, as.factor 
      (test_data$assessment))
print(conf_matrix)

# Confusion Matrix for Random Forest Model
conf_matrix_rf <- confusionMatrix(predictions_rf, as.factor 
                               (test_data$assessment))
print(conf_matrix_rf)

#Additional performance Metrics
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Precision']
recall <- conf_matrix$byClass['Recall']
f1_score <- conf_matrix$byClass['F1']

#Additional Performance Metrics for Random Forest
accuracy_rf <- conf_matrix_rf$overall['Accuracy']
precision_rf <- conf_matrix_rf$byClass['Precision']
recall_rf <- conf_matrix_rf$byClass['Recall']
f1_score_rf <- conf_matrix_rf$byClass['F1']

perf_metrices <- c(accuracy = accuracy,
                   precision = precision,
                   recall = recall,
                   f1_score = f1_score)

#Performance Matrics for RF
perf_metrices_rf <- c(accuracy = accuracy_rf,
                   precision = precision_rf,
                   recall = recall_rf,
                   f1_score = f1_score_rf)

percentage <- function(i){
  x = sprintf("%.2f", i * 100)
  return (x)
}

sapply(perf_metrices, percentage)
sapply(perf_metrices_rf, percentage)

# Performance Summary
performance_summary <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score)
  )

print(performance_summary)
