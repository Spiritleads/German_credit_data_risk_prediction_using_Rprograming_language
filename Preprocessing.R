#Load necessary libraries
library(dplyr) #Data Manipulaion & Preprocessing
library(ggplot2) #Data Visualization
library(readr) #Collection

#Data Collection
# Reading the data as it is; through a .data file format; No column name was given, refer to the data description
credit_data <- read_delim('./german_data/german.data', delim =" ", col_names =FALSE)

#Column names
col_names <- c("status", "duration", "credit_history", "purpose", "credit_amount", "savings_account", "employment", "installent_rate", "personal_status", "guarantors", "residence_since", "property", "age", "installment_plans", "housing", "num_credits", "job", "dependents", "telephone", "foreign_worker", "assessment")

#Attaching column names to dataframe
colnames(credit_data) <- col_names

#Type Conversion
chars_col <- sapply(credit_data, is.character) # find all the character columns

credit_data[, chars_col] <- lapply(credit_data[, chars_col], as.factor)

#Information On Data Set
#structure of the dataset
print(str(credit_data))

##dimension of the dataset
print(dim(credit_data))

#Summary
print(summary(credit_data))

#Check for missing Values
lapply(credit_data, is.null)