# Cleaning data from DrivenData competition Flu Shot Learning
# C. Mimeault
# April 16, 2022

# set-up ----
remove(list=ls())

# load libraries ----
library(tidyverse)
library(here)
library(janitor)

# load data ----
raw_training_features <- read_csv(here("data/raw/training_set_features.csv")) %>%
  clean_names()
raw_training_labels <- read_csv(here("data/raw/training_set_labels.csv")) %>%
  clean_names()
raw_test_features <- read_csv(here("data/raw/test_set_features.csv")) %>%
  clean_names()

# cleaning data: dealing with missing values ----

# The features for the training and test datasets can be cleaned sequentially or
# together. I will label each dataset, combine them, clean the whole dataset, and
# split them back before building the model.

# add labels to datasets in datasets
training_features <- raw_training_features %>%
  mutate(
    dataset = "training_set_features.csv"
  )

test_features <- raw_test_features %>%
  mutate(
    dataset = "test_set_features.csv"
  )

# ensure column names are the same in the two datasets
training_features_col_names <- colnames(training_features)
test_features_col_names <- colnames(test_features)
identical(training_features_col_names, test_features_col_names) # which returned TRUE

# combining datasets

features <-bind_rows(training_features, test_features)

glimpse(features)
# noticing that the class of most variable needs to be specified
# most variables are continuous (dbl) 
# several (most) variables should be categorical (fct)

features <- features %>%
  mutate(
    respondent_id = as.factor(respondent_id), # debatable but definitively not numerical
    h1n1_concern = as.factor(h1n1_concern),
    h1n1_knowledge = as.factor(h1n1_knowledge),
    behavioral_antiviral_meds = as.factor(behavioral_antiviral_meds),
    behavioral_avoidance =  as.factor(behavioral_avoidance),
    behavioral_face_mask = as.factor(behavioral_face_mask),
    behavioral_wash_hands = as.factor(behavioral_wash_hands),
    behavioral_large_gatherings = as.factor(behavioral_large_gatherings),
    behavioral_outside_home = as.factor(behavioral_outside_home),
    behavioral_touch_face = as.factor(behavioral_touch_face),
    doctor_recc_h1n1 = as.factor(doctor_recc_h1n1),
    doctor_recc_seasonal = as.factor(doctor_recc_seasonal),
    chronic_med_condition = as.factor(chronic_med_condition),
    child_under_6_months = as.factor(child_under_6_months),
    health_worker = as.factor(health_worker),
    health_insurance = as.factor(health_insurance),
    opinion_h1n1_vacc_effective = as.factor(opinion_h1n1_vacc_effective),
    opinion_h1n1_risk = as.factor(opinion_h1n1_risk),
    opinion_h1n1_sick_from_vacc = as.factor(opinion_h1n1_sick_from_vacc),
    opinion_seas_vacc_effective = as.factor(opinion_seas_vacc_effective),
    opinion_seas_risk = as.factor(opinion_seas_risk),
    opinion_seas_sick_from_vacc = as.factor(opinion_seas_sick_from_vacc),
    age_group = as.factor(age_group),
    education = as.factor(education),
    race = as.factor(race), 
    sex = as.factor(sex),
    income_poverty = as.factor(income_poverty),
    marital_status = as.factor(marital_status),
    rent_or_own = as.factor(rent_or_own),
    employment_status = as.factor(employment_status),
    hhs_geo_region = as.factor(hhs_geo_region),
    census_msa = as.factor(census_msa),
    employment_industry = as.factor(employment_industry),
    employment_occupation = as.factor(employment_occupation)
    )

glimpse(features)

features %>%
  keep(is.factor) %>%
  summary()

# see that there are a lot of missing data

features %>%
  select(income_poverty) %>%
  table(exclude = NULL) %>% # to include NA values in results
  prop.table

# logistic regression cannot deal with missing values
# assigning a dummy value to the missing values

features <- features %>%
  mutate(income_poverty = as.character(income_poverty)) %>%
  mutate(income_poverty = as.factor(ifelse(is.na(income_poverty), "UNK", income_poverty)))

# applying this to the other categorical features with NA values

features <- features %>%
  mutate(h1n1_concern = as.character(h1n1_concern)) %>%
  mutate(h1n1_concern = as.factor(ifelse(is.na(h1n1_concern), "UNK", h1n1_concern))) %>%
  mutate(h1n1_knowledge = as.character(h1n1_knowledge)) %>%
  mutate(h1n1_knowledge = as.factor(ifelse(is.na(h1n1_knowledge), "UNK", h1n1_knowledge)))

features <- features %>%
  mutate(behavioral_antiviral_meds = as.character(behavioral_antiviral_meds)) %>%
  mutate(behavioral_antiviral_meds = as.factor(ifelse(is.na(behavioral_antiviral_meds), "UNK", behavioral_antiviral_meds))) %>%
  mutate(behavioral_avoidance = as.character(behavioral_avoidance)) %>%
  mutate(behavioral_avoidance = as.factor(ifelse(is.na(behavioral_avoidance), "UNK", behavioral_avoidance))) %>%
  mutate(behavioral_face_mask = as.character(behavioral_face_mask)) %>%
  mutate(behavioral_face_mask = as.factor(ifelse(is.na(behavioral_face_mask), "UNK", behavioral_face_mask))) %>%
  mutate(behavioral_wash_hands = as.character(behavioral_wash_hands)) %>%
  mutate(behavioral_wash_hands = as.factor(ifelse(is.na(behavioral_wash_hands), "UNK", behavioral_wash_hands))) %>%
  mutate(behavioral_large_gatherings = as.character(behavioral_large_gatherings)) %>%
  mutate(behavioral_large_gatherings = as.factor(ifelse(is.na(behavioral_large_gatherings), "UNK", behavioral_large_gatherings))) %>%
  mutate(behavioral_outside_home = as.character(behavioral_outside_home)) %>%
  mutate(behavioral_outside_home = as.factor(ifelse(is.na(behavioral_outside_home), "UNK", behavioral_outside_home))) %>%
  mutate(behavioral_touch_face = as.character(behavioral_touch_face)) %>%
  mutate(behavioral_touch_face = as.factor(ifelse(is.na(behavioral_touch_face), "UNK", behavioral_touch_face)))

features <- features %>%
  mutate(doctor_recc_h1n1 = as.character(doctor_recc_h1n1)) %>%
  mutate(doctor_recc_h1n1 = as.factor(ifelse(is.na(doctor_recc_h1n1), "UNK", doctor_recc_h1n1))) %>%
  mutate(doctor_recc_seasonal = as.character(doctor_recc_seasonal)) %>%
  mutate(doctor_recc_seasonal = as.factor(ifelse(is.na(doctor_recc_seasonal), "UNK", doctor_recc_seasonal))) 

features <- features %>%
  mutate(chronic_med_condition = as.character(chronic_med_condition)) %>%
  mutate(chronic_med_condition = as.factor(ifelse(is.na(chronic_med_condition), "UNK", chronic_med_condition))) %>%
  mutate(child_under_6_months = as.character(child_under_6_months)) %>%
  mutate(child_under_6_months = as.factor(ifelse(is.na(child_under_6_months), "UNK", child_under_6_months))) %>%
  mutate(health_worker = as.character(health_worker)) %>%
  mutate(health_worker = as.factor(ifelse(is.na(health_worker), "UNK", health_worker))) %>%
  mutate(health_insurance = as.character(health_insurance)) %>%
  mutate(health_insurance = as.factor(ifelse(is.na(health_insurance), "UNK", health_insurance)))

features <- features %>%
  mutate(opinion_h1n1_vacc_effective = as.character(opinion_h1n1_vacc_effective)) %>%
  mutate(opinion_h1n1_vacc_effective = as.factor(ifelse(is.na(opinion_h1n1_vacc_effective), "UNK", opinion_h1n1_vacc_effective))) %>%
  mutate(opinion_h1n1_risk = as.character(opinion_h1n1_risk)) %>%
  mutate(opinion_h1n1_risk = as.factor(ifelse(is.na(opinion_h1n1_risk), "UNK", opinion_h1n1_risk))) %>%
  mutate(opinion_h1n1_sick_from_vacc = as.character(opinion_h1n1_sick_from_vacc)) %>%
  mutate(opinion_h1n1_sick_from_vacc = as.factor(ifelse(is.na(opinion_h1n1_sick_from_vacc), "UNK", opinion_h1n1_sick_from_vacc)))
  
features <- features %>% 
  mutate(opinion_seas_vacc_effective = as.character(opinion_seas_vacc_effective)) %>%
  mutate(opinion_seas_vacc_effective = as.factor(ifelse(is.na(opinion_seas_vacc_effective), "UNK", opinion_seas_vacc_effective)))  %>%
  mutate(opinion_seas_risk = as.character(opinion_seas_risk)) %>%
  mutate(opinion_seas_risk = as.factor(ifelse(is.na(opinion_seas_risk), "UNK", opinion_seas_risk))) %>%
  mutate(opinion_seas_sick_from_vacc = as.character(opinion_seas_sick_from_vacc)) %>%
  mutate(opinion_seas_sick_from_vacc = as.factor(ifelse(is.na(opinion_seas_sick_from_vacc), "UNK", opinion_seas_sick_from_vacc)))

features <- features %>% 
  mutate(age_group = as.character(age_group)) %>%
  mutate(age_group = as.factor(ifelse(is.na(age_group), "UNK", age_group)))  %>%
  mutate(education = as.character(education)) %>%
  mutate(education = as.factor(ifelse(is.na(education), "UNK", education))) %>%
  mutate(marital_status = as.character(marital_status)) %>%
  mutate(marital_status = as.factor(ifelse(is.na(marital_status), "UNK", marital_status))) %>%
  mutate(rent_or_own = as.character(rent_or_own)) %>%
  mutate(rent_or_own = as.factor(ifelse(is.na(rent_or_own), "UNK", rent_or_own))) %>%
  mutate(employment_status = as.character(employment_status)) %>%
  mutate(employment_status = as.factor(ifelse(is.na(employment_status), "UNK", employment_status))) %>%
  mutate(employment_industry = as.character(employment_industry)) %>%
  mutate(employment_industry = as.factor(ifelse(is.na(employment_industry), "UNK", employment_industry))) %>%
  mutate(employment_occupation = as.character(employment_occupation)) %>%
  mutate(employment_occupation = as.factor(ifelse(is.na(employment_occupation), "UNK", employment_occupation))) 

features %>%
  keep(is.factor) %>%
  summary()

# we removed all NAs from categorical features

# let's look at the numeric values

features %>%
  keep(is.numeric) %>%
  summary()

# the only numerical data are the number of adults or children in household

features <- features %>%
  mutate(household_adults = ifelse(
    is.na(household_adults),
    median(household_adults, na.rm = TRUE),
    household_adults
  ))

# the above imputation with the median did not significantly change the mean or the median 

features <- features %>%
  mutate(household_children = ifelse(
    is.na(household_children),
    median(household_children, na.rm = TRUE),
    household_children
  ))

features %>%
  keep(is.numeric) %>%
  summary()

# the above imputation with the median did not significantly change the mean or the median 

# cleaning data: dealing with outliers ----

features %>%
  ggplot(
    aes(x = household_adults)) +
  geom_histogram()

features %>%
  ggplot(
    aes(x = household_children)) +
  geom_histogram()

# no outliers in those features

# splitting the data ----

# this was already done for us initially, so simply re-splitting the features
# between training and test

clean_training_features <- features %>%
  filter(dataset == "training_set_features.csv") %>%
  select(-dataset)

write_csv(
  clean_training_features, 
  here("data/processed/clean_training_set_features.csv")
  )

clean_test_features <- features %>%
  filter(dataset == "test_set_features.csv") %>%
  select(-dataset)

write_csv(
  clean_test_features, 
  here("data/processed/clean_test_set_features.csv")
)
# useful references ----
# I followed the steps from Practical machine learning in R from Nwanganga and Chapple
