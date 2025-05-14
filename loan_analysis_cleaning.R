# clear the workspace
rm(list = ls())
cat("\014")

# Load and install important libraries
install.packages("tidyverse")
install.packages("dplyr")
install.packages("janitor")
install.packages("tibble")
install.packages("readr")

library(tidyverse)
library(dplyr)
library(janitor)
library(tibble)
library(readr)
library(lubridate)

#read the csv files we want to use for our data (this will take about a minute as it's massive)
accepted_data <- read.csv("/Users/rivermagee/Downloads/archive/accepted_2007_to_2018q4.csv")

#inspect the data for cleaning
glimpse(accepted_data)
summary(accepted_data)
str(accepted_data)

#remove missing columns
accepted_data_clean <- accepted_data %>%
  select(where(~ mean(is.na(.)) < 0.5))  # Keep columns with < 50% NA

#handle missing data
accepted_data_clean <- accepted_data_clean %>%
  filter(!is.na(loan_amnt), !is.na(annual_inc), !is.na(term)) %>% # Required fields
  mutate(
    emp_length = replace_na(emp_length, "Unknown"),
    revol_util = as.numeric(gsub("%", "", revol_util)), # Remove % signs
    revol_util = replace_na(revol_util, median(revol_util, na.rm = TRUE))
  )

#convert to proper data types
accepted_data_clean <- accepted_data_clean %>%
  mutate(
    term = as.numeric(gsub(" months", "", term)),
    issue_d = mdy(issue_d),
    earliest_cr_line = mdy(earliest_cr_line),
    int_rate = as.numeric(gsub("%", "", int_rate))
  )

#create new features and columns to analyze
accepted_data_clean <- accepted_data_clean %>%
  mutate(
    income_to_loan = annual_inc / loan_amnt,
    credit_age_years = as.numeric(difftime(issue_d, earliest_cr_line, units = "days")) / 365
  )

#removing duplicates/irrelvant data
accepted_data_clean <- accepted_data_clean %>%
  distinct() %>%
  filter(loan_status %in% c("Fully Paid", "Charged Off"))

#create a finalized, cleaned csv file for visualization and analysis
write.csv(accepted_data_clean, "cleaned_accepted_data.csv", row.names = FALSE)

#doing the same for rejected data
rejected_data <- read.csv("/Users/rivermagee/Downloads/archive/rejected_2007_to_2018Q4.csv")
glimpse(rejected_data)

#clean these fields
rejected_data <- rejected_data %>%
  mutate(
    Amount.Requested = as.numeric(Amount.Requested),
    Application.Date = ymd(Application.Date)
  )

#create a finalized, cleaned svs file for visualization and analysis
write.csv(rejected_data, "cleaned_rejected_data.csv", row.names = FALSE)

