# clear the workspace
rm(list = ls())
cat("\014")

#install caret
install.packages("caret")

# Load libraries
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(readr)

# Load cleaned datasets
accepted <- read_csv("/Users/rivermagee/Downloads/archive/cleaned_accepted_data.csv")
rejected <- read_csv("/Users/rivermagee/Downloads/archive/cleaned_rejected_data.csv")

#create new column named status
accepted <- accepted %>%
  mutate(status = "accepted")

#change names to match with rejected data
accepted <- accepted %>%
  rename(
    Amount.Requested = loan_amnt,
    Application.Date = issue_d,
    Loan.Title = title,
    Risk_Score = fico_range_low,
    Debt.To.Income.Ratio = dti,
    Zip.Code = zip_code,
    State = addr_state,
    Employment.Length = emp_length)

#create new column named status
rejected <- rejected %>%
  mutate(status = "rejected")

# Downsample rejected to match size of accepted (It causes errors otherwise)
set.seed(42)  # for reproducibility
rejected <- rejected %>%
  sample_n(nrow(accepted))  # match the size of accepted

#additonal cleaning
rejected <- rejected %>%
  mutate(Debt.To.Income.Ratio = as.numeric(gsub("%", "", Debt.To.Income.Ratio)))

# Keep only common columns
common_cols <- intersect(names(accepted), names(rejected))

#combine data sets based on common columns (like join.inner)
combined <- bind_rows(
  accepted[common_cols],
  rejected[common_cols]
) %>%
  mutate(status = factor(status))

#Fixes the outliers issue
combined <- combined %>%
  rename(DTI = `Debt.To.Income.Ratio`)
combined <- combined %>%
  filter(!is.na(DTI), DTI > 0, DTI <= 65) # Assume 65% is a reasonable high end

#cleans employment length values to ensure num
combined <- combined %>%
  mutate(Employment.Length = case_when(
    grepl("^<", Employment.Length) ~ 0.5,  # "< 1 year" becomes 0.5
    grepl("^>", Employment.Length) ~ 11,   # "> 10 years" becomes 11 (or another suitable value)
    grepl("\\+|years", Employment.Length) ~ as.numeric(gsub("\\D", "", Employment.Length)),  # Regular case for "10+ years"
    Employment.Length == "n/a" ~ NA_real_,  # Replace 'n/a' with NA
    TRUE ~ as.numeric(Employment.Length)   # Convert other numeric values to numbers
  ))

#---------------------------------
#visualizations!
#Loan amounts compared
ggplot(combined, aes(x = Amount.Requested, fill = status)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 40) +
  labs(title = "Loan Amounts: Accepted vs. Rejected",
       x = "Loan Amount Requested", y = "Count") +
  scale_fill_manual(values = c("accepted" = "#4CAF50", "rejected" = "#F44336")) +
  theme_minimal()

#FICO distribution
ggplot(combined, aes(x = Risk_Score, fill = status)) +
  geom_density(alpha = 0.5) +
  labs(title = "FICO Risk Score: Accepted vs. Rejected",
       x = "Risk Score (FICO)", y = "Density") +
  scale_fill_manual(values = c("accepted" = "#2196F3", "rejected" = "#FFC107")) +
  theme_minimal()

#Debt to income comparison
ggplot(combined, aes(x = DTI, fill = status)) +
  geom_density(alpha = 0.5) +
  labs(title = "Debt-to-Income Ratio: Accepted vs. Rejected",
       x = "DTI", y = "Density") +
  scale_fill_manual(values = c("accepted" = "#9C27B0", "rejected" = "#FF5722")) +
  theme_minimal()

# Employment length by loan status
ggplot(combined, aes(x = Employment.Length, fill = status)) +
  geom_bar(position = "dodge") +
  labs(title = "Employment Length: Accepted vs. Rejected",
       x = "Employment Length", y = "Count") +
  scale_fill_manual(values = c("accepted" = "#3F51B5", "rejected" = "#CDDC39")) +
  theme_minimal()

#----------------------------------
#Logistic Regression
combined$status <- factor(combined$status, levels = c("rejected", "accepted"))

#Selecting our predictors
model_data <- combined %>%
  select(status, Amount.Requested, DTI, Employment.Length, Risk_Score) %>%
  drop_na()

#Split into train/test sets
set.seed(123)
library(caret)

split <- createDataPartition(model_data$status, p = 0.8, list = FALSE)
train <- model_data[split, ]
test <- model_data[-split, ]

#Building our logistic regression model
model <- glm(status ~ Amount.Requested + DTI + Employment.Length + Risk_Score,
             data = train,
             family = "binomial")
summary(model)

# Get predicted probabilities
test$predicted_prob <- predict(model, test, type = "response")

# Classify as accepted or rejected based on a 0.5 cutoff
test$predicted_class <- ifelse(test$predicted_prob > 0.5, "accepted", "rejected")
test$predicted_class <- as.factor(test$predicted_class)

# Model evaluation
confusionMatrix(test$predicted_class, test$status)

#Which variables most effect the prediction
summary(model)
exp(coef(model))  # Get odds ratios

#visualize model performance
library(pROC)
roc_obj <- roc(test$status, test$predicted_prob)
plot(roc_obj, main = "ROC Curve")
auc(roc_obj)

#--------------------------------------
#group summary data
combined %>%
  group_by(status) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

#Write a new csv file for combined data.
write.csv(combined, "cleaned_combined_data.csv", row.names = FALSE)


