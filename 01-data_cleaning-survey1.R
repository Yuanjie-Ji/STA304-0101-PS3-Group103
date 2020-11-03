#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from UCLA Democracy Fund Study
# Author: Jia Yuan Liu, Gen Cao, Yuanjie Ji
# Date: 2 November 2020
# Contact: jiayuan.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from UCLA Democracy Fund Study and save the folder that you're 
# interested in to inputs/data 


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/ljy/Desktop/sta304ps3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("/Users/ljy/Desktop/sta304ps3/ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)


#### What else???? ####
# Maybe make some age-groups?
library(data.table)
agebreaks <- c(18,25,30,35,40,45,50,55,60,65,70,75,80,85,500)
agelabels <- c("18-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85+")

setDT(reduced_data)[ , agegroup := cut(age, 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]

# Maybe check the values?
missing_value_percentage <- sum(is.na(reduced_data))/nrow(reduced_data)
missing_value_percentage

reduced_data <- reduced_data%>%drop_na()

# Is vote a binary? If not, what are you going to do?
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0)) %>%
  mutate(vote_bidon = ifelse(vote_2020=="Joe Biden",1,0))

# removing unregistered samples
reduced_data <- reduced_data %>%
  filter(registration == "Registered")

# renaming sex column
reduced_data <- reduced_data%>%
  mutate(sex = ifelse(gender == "Female", "female", "male"))

# rename census_region column
reduced_data$region <- reduced_data$census_region

# reclassifying race
reduced_data$race_ethnicity <- as.vector(reduced_data$race_ethnicity)
reduced_data$race <- reduced_data$race_ethnicity

reduced_data$race[reduced_data$race_ethnicity == "White"] <- "white"
reduced_data$race[reduced_data$race_ethnicity == "Black, or African American"] <- "black/african american/negro"
reduced_data$race[reduced_data$race == "Asian (Asian Indian)" | reduced_data$race == "Asian (Vietnamese)" | 
                    survey_data$race == "Asian (Korean)"| survey_data$race == "Asian (Filipino)"| 
                    survey_data$race == "Asian (Other)"| survey_data$race == "Pacific Islander (Native Hawaiian)"|
                    survey_data$race == "Pacific Islander (Other)"| survey_data$race == "Pacific Islander (Samoan)"| 
                    survey_data$race == "Pacific Islander (Guamanian)"] <- "other asian or pacific islander"
reduced_data$race[reduced_data$race_ethnicity == "Asian (Chinese)"] <- "chinese"
reduced_data$race[reduced_data$race_ethnicity == "Asian (Japanese)"] <- "japanese"
reduced_data$race[reduced_data$race_ethnicity == "American Indian or Alaska Native"] <- "american indian or alaska native"
reduced_data$race[reduced_data$race_ethnicity == "Some other race"] <- "other race, nec"

#reclassifying education
reduced_data$education <- as.vector(reduced_data$education)
reduced_data$educd <- reduced_data$education
reduced_data$educd[reduced_data$educd == "Middle School - Grades 4 - 8" |reduced_data$educd == "Completed some high school" 
                   | reduced_data$educd == "3rd Grade or less" ] <- "Below Highschool"
reduced_data$educd[reduced_data$educd == "College Degree (such as B.A., B.S.)" 
                   | reduced_data$educd == "Completed some graduate, but no degree" ] <- "College Degree (such as B.A., B.S.)"

reduced_data <- reduced_data%>%
  select(agegroup, sex, race, educd, vote_trump, region, vote_bidon, ideo5)
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")

