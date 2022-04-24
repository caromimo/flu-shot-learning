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

# create model: seasonal vaccine ----

seasonal_model <- glm(
  data = training,
  family = binomial,
  formula = seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + doctor_recc_seasonal + doctor_recc_h1n1 + chronic_med_condition + child_under_6_months + health_worker + health_insurance + opinion_h1n1_risk + opinion_seas_vacc_effective + opinion_seas_risk + opinion_seas_sick_from_vacc + education + race + rent_or_own + hhs_geo_region + employment_industry
)
summary(seasonal_model)

# highly significant variables in the complete model
# h1n1_knowledge
# behavioral_touch_face
# doctor_recc_seasonal
# doctor_recc_h1n1
# chronic_med_condition
# child_under_6_months
# health_worker
# health_insurance
# opinion_h1n1_risk
# opinion_seas_vacc_effective
# opinion_seas_risk
# opinion_seas_sick_from_vacc
# education
# race
# rent_or_own
# hhs_geo_region
# employment_industry

# create model: h1n1 vaccine ----

h1n1_model <- glm(
  data = training, 
  family = binomial, 
  formula = h1n1_vaccine ~ h1n1_concern + h1n1_knowledge + behavioral_antiviral_meds + doctor_recc_h1n1 + doctor_recc_seasonal + child_under_6_months + health_worker + health_insurance + opinion_h1n1_vacc_effective + opinion_h1n1_risk + opinion_seas_risk + age_group + education + race + employment_industry)
summary(h1n1_model)

# highly significant variables in the complete model
# h1n1_concern
# h1n1_knowledge
# behavioral_antiviral_meds
# doctor_recc_h1n1
# doctor_recc_seasonal
# child_under_6_months
# health_worker
# health_insurance
# opinion_h1n1_vacc_effective
# opinion_h1n1_risk
# opinion_seas_risk
# age_group
# education
# race
# employment_industry

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
  here("data/submissions/submission-selected-features.csv")
       )

# submitted these results - not as good as initial model