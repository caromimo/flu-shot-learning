# Training model from DrivenData competition Flu Shot Learning
# C. Mimeault
# April 24, 2022

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

# MODEL 1: use predicted seasonal vaccine in h1n1 predictions ----

# create training dataset: seasonal vaccine

training_for_seasonal_vaccine <- training %>%
  select(-respondent_id, -h1n1_vaccine)

# create model: seasonal vaccine

seasonal_model <- glm(
  data = training_for_seasonal_vaccine, 
  family = binomial, 
  formula = seasonal_vaccine ~ .)

summary(seasonal_model)

# make predictions: seasonal vaccine

seasonal_vaccine_predictions <- clean_test_features %>%
  mutate(seasonal_vaccine = predict(
    seasonal_model, 
    newdata = clean_test_features, 
    type = "response")
  ) 

clean_test_features_with_seasonal_vaccine <- seasonal_vaccine_predictions %>%
  mutate(
    seasonal_vaccine = as.factor(ifelse(seasonal_vaccine < 0.5, 0, 1))
  )

# create training dataset: h1n1 vaccine

training_for_h1n1_vaccine <- training %>%
  select(-respondent_id)

# create model: h1h1 vaccine

h1n1_model <- glm(
  data = training_for_h1n1_vaccine, 
  family = binomial, 
  formula = h1n1_vaccine ~ .)

summary(h1n1_model)

# make predictions: h1h vaccine

h1n1_vaccine_predictions <- clean_test_features_with_seasonal_vaccine %>%
  mutate(h1n1_vaccine = predict(
    h1n1_model, 
    newdata = clean_test_features_with_seasonal_vaccine, 
    type = "response")
  )

# assemble submission dataset

sub_seasonal_vaccine_prob <- seasonal_vaccine_predictions %>%
  select(respondent_id, seasonal_vaccine)

sub_h1n1_vaccine_prob <- h1n1_vaccine_predictions %>%
  select(respondent_id, h1n1_vaccine)

submission <- left_join(
  sub_h1n1_vaccine_prob, 
  sub_seasonal_vaccine_prob,
  by = "respondent_id"
)

write_csv(
  submission, 
  here("data/submissions/submission-seq-seasonal-h1n1.csv")
       )

# submitted these results - not as good as initial model

# MODEL 2: use predicted h1n1 vaccine in seasonal predictions ----

# create training dataset: h1n1 vaccine

training_for_h1n1_vaccine <- training %>%
  select(-respondent_id, -seasonal_vaccine)

# create model: seasonal vaccine

h1n1_model <- glm(
  data = training_for_h1n1_vaccine, 
  family = binomial, 
  formula = h1n1_vaccine ~ .)

summary(h1n1_model)

# make predictions: seasonal vaccine

seasonal_h1n1_predictions <- clean_test_features %>%
  mutate(h1n1_vaccine = predict(
    h1n1_model, 
    newdata = clean_test_features, 
    type = "response")
  ) 

clean_test_features_with_h1n1_vaccine <- seasonal_h1n1_predictions %>%
  mutate(
    h1n1_vaccine = as.factor(ifelse(h1n1_vaccine < 0.5, 0, 1))
  )

# create training dataset: seasonal vaccine

training_for_seasonal_vaccine <- training %>%
  select(-respondent_id)

# create model: h1h1 vaccine

seasonal_model <- glm(
  data = training_for_seasonal_vaccine, 
  family = binomial, 
  formula = seasonal_vaccine ~ .)

summary(seasonal_model)

# make predictions: seasonal vaccine

seasonal_vaccine_predictions <- clean_test_features_with_h1n1_vaccine %>%
  mutate(seasonal_vaccine = predict(
    seasonal_model, 
    newdata = clean_test_features_with_h1n1_vaccine, 
    type = "response")
  )

# assemble submission dataset

sub_seasonal_vaccine_prob <- seasonal_vaccine_predictions %>%
  select(respondent_id, seasonal_vaccine)

sub_h1n1_vaccine_prob <- h1n1_vaccine_predictions %>%
  select(respondent_id, h1n1_vaccine)

submission <- left_join(
  sub_h1n1_vaccine_prob, 
  sub_seasonal_vaccine_prob,
  by = "respondent_id"
)

write_csv(
  submission, 
  here("data/submissions/submission-seq-h1n1-seasonal.csv")
)

# submitted these results - not as good as initial model