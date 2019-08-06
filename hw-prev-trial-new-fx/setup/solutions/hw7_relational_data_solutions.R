#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Solutions: Relational data
#####################################
# Load okR autograder
source("setup/autograder-setup/hw7_relational_data/hw7_relational_data.ok.R")
AutograderInit()

# NOTE: In this homework, in order for the autograder
# to run correctly, you must run the code for each
# problem in order. If you have trouble with the 
# autograder, try running your code again starting 
# from line 12.

# Let's load the dplyr and tidyr packages
library(dplyr)
library(tidyr)

# Load the data for this assignment
load("data/hw7_relational_data.RData")

# This homework will focus on a hypothetical study 
# of children under 10 years of ago. You are the data
# analyst, and your task is to explore the 
# pattern of infection with cryptosporidium, a 
# type of parasite, by age in years and sex. You must also 
# assess whether cryptosporidium infection is 
# associated with diarrhea symptoms. 

# The study collected a baseline survey at enrollment 
# and recorded participant age and sex. At three time points,
# it collected a stool sample from participants and
# tested for cryptosporidium infection. These infections
# can be both symptomatic and asymptomatic, and
# among symptomatic infections, diarrhea is a common
# symptom. The study also conducted follow-up surveys
# at the three time points and recorded diarrhea symptoms
# at the time of the stool sample collection. 

# The baseline dataset contains information on age and sex. 
head(baseline)

# The lab_data dataset contains the cryptosporidium test
# results. In each column starting with "crypto", a zero
# value indicates a negative test results, a 1 value indicates
# a positive result. Some samples were lost or could not
# be analyzed and have missing values. 
head(lab_data)

# The fu_survey dataset contains the results of the follow-up
# surveys. In each column starting with "diarrhea", a zero
# value indicates no diarrhea symptoms, a 1 value indicates
# diarrhea symptoms. If the patient's stool sample was lost
# at that time point, no symptoms were recorded, so there is
# a missing value in those cells. 
head(fu_survey)

#-----------------------------------------------
# Problem 1: Estimate the prevalence of cryptosporidium 
# at each time point using dplyr. Save your results
# in a data frame called prev_time with 1 row and 3 columns.
# The columns should be named "crypto_prev1", "crypto_prev2", 
# "crypto_prev3". You can obtain the prevalence by taking the mean of
# the crypto variables. Be sure to ignore missings when
# taking the mean. 
#-----------------------------------------------
prev_time = lab_data %>%
  summarize(crypto_prev1 = mean(crypto_time1, na.rm=TRUE),
            crypto_prev2 = mean(crypto_time2, na.rm=TRUE),
            crypto_prev3 = mean(crypto_time3, na.rm=TRUE))

# Check your answer
CheckProblem1()

#-----------------------------------------------
# Your next task is to estimate the prevalence of 
# cryptosporidium pooling across all time points. 

# Problem 2: To do this, you will need to convert 
# the lab_data data frame from wide to long format. 
# Create a long format dataset called lab_data_long
# that contains 30 rows (10 participants x 3 time 
# points). The columns will be for id, time point, 
# and crypto test result. Label these columns
# "id", "time", and "crypto". 
#-----------------------------------------------
lab_data_long = gather(lab_data, key = time, value = crypto, crypto_time1:crypto_time3)

# Check your answer
CheckProblem2()

#-----------------------------------------------
# Problem 3: Now use lab_data_long to estimate the
# prevalence of cryptosporidium pooling across all 
# time points. Save results in a data frame called
# prev. It will contain one column and one row. 
# Name the column crypto_prev. 
#-----------------------------------------------
prev = lab_data_long %>%
  summarize(crypto_prev = mean(crypto, na.rm=TRUE))
prev

# Check your answer
CheckProblem3()

#-----------------------------------------------
# Next you are interested in assessing whether
# age is associated with crypto infection. Specifically
# we want to know whether the prevalence is higher
# among children <5 years vs 5 years or older. 

# Problem 4: To assess this, you will need to join 
# the baseline and lab_data_long data frames. 
# Answer this multiple choice question by entering
# "a", "b" etc into p4: 

# a) Full_join is the appropriate type of join
# b) Inner_join is the appropriate type of join
# c) Full_join or inner_join are appropriate
#    and give the same answer
# d) Any of the mutating joins are appropriate 
#    and give the same answer
#-----------------------------------------------
p4 = "d"

# Check your answer
CheckProblem4()

#-----------------------------------------------
# Problem 5: To assess this, you will need to join 
# the baseline and lab_data_long data frames. 
# Use the join of your choice to join them and 
# save the resulting data frame with the name
# baseline_lab_long. It should have columns for id, 
# time, age, female, and crypto, and 30 rows.
#-----------------------------------------------
baseline_lab_long = full_join(baseline, lab_data_long, by="id")

# Check your answer
CheckProblem5()

#-----------------------------------------------
# Problem 6: Now add a column to the data frame 
# baseline_lab_long called age_u5 that contains 
# 1 if the age is <5 and 0 if the age is 5 or older. 
#-----------------------------------------------
baseline_lab_long = baseline_lab_long %>%
  mutate(age_u5 = ifelse(age < 5, 1, 0))

# Check your answer
CheckProblem6()

#-----------------------------------------------
# Problem 7: Next, calculate the prevalence of crypto
# among children <5 years and those 5 or older. 
# Use dplyr and save results in a data frame called
# prev_age. It should contain two rows (one for age <5
# and one for age >=5) and two columns, one for age_u5
# and one for crypto_prev. 
#-----------------------------------------------
prev_age = baseline_lab_long %>%
  group_by(age_u5) %>%
  summarize(crypto_prev = mean(crypto, na.rm=TRUE))
prev_age

# Check your answer
CheckProblem7()

#-----------------------------------------------
# Problem 8: What do you conclude about the 
# association between age and cryptosporidium
# infection? Save your answer choice in p8. 

# a) Crypto infection is higher among children <5
# b) Crypto infection is equivalent among children <5 and >=5 
# c) Crypto infection is higher among children >=5
#-----------------------------------------------
p8 = "a"

# Check your answer
CheckProblem8()

#-----------------------------------------------
# Problem 9: Now adapt your code above to calculate
# the prevalence of cryptosporidium by sex. Save 
# your results in a data frame called prev_sex
# with two rows (one for male, one for female) and
# two columns, one for female and one for crypto_prev.
#-----------------------------------------------
prev_sex = baseline_lab_long %>%
  group_by(female) %>%
  summarize(crypto_prev = mean(crypto, na.rm=TRUE))
prev_sex

# Check your answer
CheckProblem9()

#-----------------------------------------------
# Problem 10: What do you conclude about the 
# association between sex and cryptosporidium
# infection? Save your answer choice in p10. 

# a) Crypto infection is lower among females
# b) Crypto infection is equivalent among females and males
# c) Crypto infection is higher among females
#-----------------------------------------------
p10 = "c"

# Check your answer
CheckProblem10()

#-----------------------------------------------
# OPTIONAL: Complete the remaining challenge
# problems!
#-----------------------------------------------

#-----------------------------------------------
# Next, you will estimate the prevalence of crypto
# among people with and without diarrhea symptoms
# pooling across time points. 

# Problem 11: To do so, you first need to convert
# fu_survey to long format. Create a new data frame
# in long format called fu_survey_long that contains
# 30 rows and the columns id, time, and diarrhea. 
#-----------------------------------------------
fu_survey_long = gather(fu_survey, key="time", value="diarrhea", diarrhea_time1:diarrhea_time3)

# Check your answer
CheckProblem11()

#-----------------------------------------------
# Problem 12: To prepare to merge the fu_survey_long
# and lab_data_long data frames, you need to recode
# the time column to just include values 1, 2, 3
# to indicate each time. This will allow us to include
# time as a key when merging the two data frames. 
# Modify the lab_data_long data frame's time column. For example, 
# replace "crypto_time1" with "1" and so on. You can
# use nested ifelse functions to achieve this. 
#-----------------------------------------------
lab_data_long  = lab_data_long %>%
  mutate(time = ifelse(time=="crypto_time1", "1", 
                       ifelse(time=="crypto_time2", "2", "3")))

# Check your answer
CheckProblem12()

#-----------------------------------------------
# Problem 13: To prepare to merge the fu_survey_long
# and lab_data_long data frames, you need to recode
# the time column to just include values 1, 2, 3
# to indicate each time. This will allow us to include
# time as a key when merging the two data frames. 
# Modify the fu_survey_long data frame's time column. For example, 
# replace "diarrhea_time1" with "1" and so on. You can
# use nested ifelse functions to achieve this. 
#-----------------------------------------------
fu_survey_long  = fu_survey_long %>%
  mutate(time = ifelse(time=="diarrhea_time1", "1", 
                       ifelse(time=="diarrhea_time2", "2", "3")))

# Check your answer
CheckProblem13()

#-----------------------------------------------
# Problem 14: Join together the lab_data_long
# and fu_survey_long data frames. Call the new
# data frame lab_fu_survey. The resulting
# data frame should have 30 rows and 4 columns. 
#-----------------------------------------------
lab_fu_survey = full_join(lab_data_long, fu_survey_long, by=c("id", "time"))

# Check your answer
CheckProblem14()

#-----------------------------------------------
# Problem 15: Estimate crypto prevalence within
# diarrhea symptom categories pooling across time 
# points. Save the results in a new data frame
# called diarrhea_crypto_prev It should contain two
# rows (one for diarrhea presence, one for absence)
# and a column for diarrhea and a column named 
# crypto_prev. 
#-----------------------------------------------
diarrhea_crypto_prev = lab_fu_survey %>%
                    group_by(diarrhea) %>%
                    summarize(crypto_prev = mean(crypto, na.rm=TRUE)) %>%
                    filter(!is.na(diarrhea))
diarrhea_crypto_prev

# Check your answer
CheckProblem15()

#-----------------------------------------------
# Problem 16: What do you conclude about the relationship
# between diarrhea and crypto? Save your answer
# choice in p16. 

# a) Diarrhea is not associated with crypto prevalence 
# b) Crypto prevalence is higher among people with diarrhea
#    symptoms than among people without them. 
# b) Crypto prevalence is higher among people without diarrhea
#    symptoms than among people with them. 
#-----------------------------------------------
p16 = "b"

# Check your answer
CheckProblem16()

# --------------------------------------------
# Check your total score
MyTotalScore()
# --------------------------------------------


