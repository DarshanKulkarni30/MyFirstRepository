library(dplyr)
library(ggplot2)
library(DataExplorer)
library(summarytools)
library(reshape2)  

#Data Import
job_data <- read.csv("https://raw.githubusercontent.com/KunaalNaik/R_Basics/main/IPBA%2017/job_change.csv")


##### Check Summary Statistics
summary(job_data)

##### Check Missing Values (Numerical)
sum(is.na(job_data))
missing_values <-colSums(is.na(job_data))
print(missing_values)

##### Check Missing Values (Non Numerical)
missing_categories <- job_data[is.na(job_data)]
print(missing_categories)

# Get column names of non-numeric columns
non_numeric_columns <- names(job_data)[sapply(data, function(x) !is.numeric(x))]

# Initialize a vector to store missing value counts
missing_counts <- numeric(length(non_numeric_columns))

# Loop through each non-numeric column and calculate missing value count
for (i in seq_along(non_numeric_columns)) {
  col_name <- non_numeric_columns[i]
  missing_counts[i] <- sum(is.na(data[[col_name]]))
}

# Print missing value counts for each column
for (i in seq_along(non_numeric_columns)) {
  col_name <- non_numeric_columns[i]
  cat("Missing values in", col_name, ":", missing_counts[i], "\n")
}


#### From tariner - same code
##### Check Missing Values
# Check for missing values in each column - numeric
missing_values <- colSums(is.na(job_data))

# Display the count of missing values for each column
print(missing_values)

# Categorical 
categorical_columns <- c("gender", "relevent_experience", "enrolled_university",
                         "education_level", "major_discipline", "company_size",
                         "company_type", "last_new_job")

missing_values_categorical <- sapply(job_data[categorical_columns], function(col) sum(col == ""))
print(missing_values_categorical)





##### Numerical

# Treat - training_hours

# Check

# Calculate the mean of the column excluding missing values
mean_training_hours <- mean(job_data$training_hours, na.rm = TRUE)

# Replace missing values with the calculated mean
job_data$training_hours[is.na(job_data$training_hours)] <- mean_training_hours

# Print the updated data frame
print(job_data)



# Treat - city_development_index

# Calculate the mean of the column excluding missing values
mean_city_development_index <- mean(job_data$city_development_index, na.rm = TRUE)

# Replace missing values with the calculated mean
job_data$city_development_index[is.na(job_data$city_development_index)] <- mean_city_development_index

# Print the updated data frame
print(job_data)




##### Categorical

# Treat - major_discipline -- Mode


# Calculate the mode of the column
mode_major_discipline <- as.character(stats::mode(job_data$major_discipline, na.rm = TRUE))

# Replace missing values with the mode
job_data$major_discipline[is.na(job_data$major_discipline)] <- mode_major_discipline

# Print the updated data frame
print(job_data)

###### Constant

# Define the constant value to replace missing values
constant_value <- "Missing"

# Replace missing values with the constant value
job_data$major_discipline[job_data$major_discipline)] <- constant_value

# Print the updated data frame
print(job_data)

# Bar Plot to Check


# Treat - education_level