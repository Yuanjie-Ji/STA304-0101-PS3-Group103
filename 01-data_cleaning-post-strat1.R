#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS USA
# Author: Jia Yuan Liu, Gen Cao, Yuanjie Ji
# Date: 2 November 2020
# Contact: jiayuan.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to ps folder


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/ljy/Desktop/sta304ps3")
raw_data <- read_dta("usa_00001.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(
        region,
         #stateicp,
         sex, 
         age, 
         race, 
         #hispan,
         #marst, 
         #bpl,
         educd,
         #labforce, 
         #inctot
         )
         

#### What's next? ####


## Creating agegroup from age
library(data.table)
reduced_data$age <- as.integer(reduced_data$age)
setDT(reduced_data)[age >18 & age <25, agegroup := "18-24"]
reduced_data[age >24 & age <30, agegroup := "25-29"]
reduced_data[age >29 & age <35, agegroup := "30-34"]
reduced_data[age >34 & age <40, agegroup := "35-39"]
reduced_data[age >39 & age <45, agegroup := "40-44"]
reduced_data[age >44 & age <50, agegroup := "45-49"]
reduced_data[age >49 & age <55, agegroup := "50-54"]
reduced_data[age >54 & age <60, agegroup := "55-59"]
reduced_data[age >59 & age <65, agegroup := "60-64"]
reduced_data[age >64 & age <70, agegroup := "65-69"]
reduced_data[age >69 & age <75, agegroup := "70-74"]
reduced_data[age >74 & age <80, agegroup := "75-79"]
reduced_data[age >79 & age <85, agegroup := "80-84"]
reduced_data[age >84, agegroup := "85+"]

# include individuals > 18 years old
missing_value_percentage <- sum(is.na(reduced_data))/nrow(reduced_data)
missing_value_percentage
reduced_data <- reduced_data%>%drop_na()

#race group merge two/three reace into others
reduced_data$race[reduced_data$race == "two major races" | reduced_data$race == "three or more major races"] <- "other race, nec"

#reclassifying region 
reduced_data$region <- as.vector(reduced_data$region)
reduced_data$region[reduced_data$region == "east south central div" | reduced_data$region == "west south central div" |
                      reduced_data$region == "south atlantic division"] <- "South"
reduced_data$region[reduced_data$region == "west north central div" | reduced_data$region == "east north central div"] <- "Midwest"
reduced_data$region[reduced_data$region == "mountain division" | reduced_data$region == "pacific division"] <- "West"
reduced_data$region[reduced_data$region == "middle atlantic division" | reduced_data$region == "new england division"] <- "Northeast"

#reclassifying education
reduced_data$educd <- as.vector(reduced_data$educd)
reduced_data$educd[reduced_data$educd == "12th grade, no diploma" | reduced_data$educd == "grade 11" |
                     reduced_data$educd == "grade 10" | reduced_data$educd == "grade 9" | reduced_data$educd == "grade 8" |
                     reduced_data$educd == "grade 7" |reduced_data$educd == "grade 6" |reduced_data$educd == "grade 5" |
                     reduced_data$educd == "grade 4" |reduced_data$educd == "grade 3" |reduced_data$educd == "grade 2" |
                     reduced_data$educd == "grade 1" |reduced_data$educd == "nursery school, preschool"|
                     reduced_data$educd == "kindergarten"|reduced_data$educd == "no schooling completed"] <- "Below Highschool"
reduced_data$educd[reduced_data$educd == "regular high school diploma" ] <- "High school graduate"
reduced_data$educd[reduced_data$educd == "some college, but less than 1 year" | reduced_data$educd
                   == "1 or more years of college credit, no degree" ] <- "Completed some college, but no degree"
reduced_data$educd[reduced_data$educd == "bachelor's degree" ] <- "College Degree (such as B.A., B.S.)"
reduced_data$educd[reduced_data$educd == "associate's degree, type not specified" ] <- "Associate Degree"
reduced_data$educd[reduced_data$educd == "ged or alternative credential" | reduced_data$educd == "professional degree beyond a bachelor's degree" ] <- "Other post high school vocational training"
reduced_data$educd[reduced_data$educd == "master's degree" ] <- "Masters degree"
reduced_data$educd[reduced_data$educd == "doctoral degree" ] <- "Doctorate degree"

reduced_data1 <- reduced_data%>%
  select(agegroup,sex,race, region,educd)%>%
  count(agegroup,sex,race, region,educd)%>%
  group_by(agegroup,race, region,educd)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data1, "census_data.csv")



         