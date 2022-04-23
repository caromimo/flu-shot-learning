# Training model from DrivenData competition Flu Shot Learning
# C. Mimeault
# April 16, 2022

# set-up ----
remove(list=ls())

# load libraries ----
library(tidyverse)
library(here)
library(janitor)
library(stats)

# load data ----

raw_training_labels <-
  read_csv(here("data/raw/training_set_labels.csv"),
           col_types = "fff") %>%
  clean_names()

clean_training_features <- 
  readRDS(here("data/processed/clean_training_set_features.rds"))

clean_test_features <- 
  readRDS(here("data/processed/clean_test_set_features.rds"))

# combine the features and labels of the training dataset

training <- left_join(
  raw_training_labels, 
  clean_training_features, 
  by = "respondent_id")

# creating a vaccine-specific datasets

training_for_seasonal_vaccine <- training %>%
  select(-respondent_id, -h1n1_vaccine)

training_for_h1n1_vaccine <- training %>%
  select(-respondent_id, -seasonal_vaccine)

# create model: seasonal vaccine ----

seasonal_model <- glm(
  data = training_for_seasonal_vaccine, 
  family = binomial, 
  formula = seasonal_vaccine ~ .)
summary(seasonal_model)

# create model: h1n1 vaccine ----

h1n1_model <- glm(
  data = training_for_h1n1_vaccine, 
  family = binomial, 
  formula = h1n1_vaccine ~ .)
summary(h1n1_model)

# make predictions: seasonal vaccination ----

seasonal_vaccine_predictions <- clean_test_features %>%
  mutate(seasonal_vaccine = predict(
    seasonal_model, 
    newdata = clean_test_features, 
    type = "response")
  ) %>%
  select(
    respondent_id,
    seasonal_vaccine
  )

# make predictions: h1n1 vaccination ----

h1n1_vaccine_predictions <- clean_test_features %>%
  mutate(h1n1_vaccine = predict(
    h1n1_model, 
    newdata = clean_test_features, 
    type = "response")
  ) %>%
  select(
    respondent_id,
    h1n1_vaccine
  )

# assemble submission dataset ----

submission <- left_join(
  h1n1_vaccine_predictions, 
  seasonal_vaccine_predictions,
  by = "respondent_id"
)

write_csv(
  submission, 
  here("data/submissions/submission.csv")
       )