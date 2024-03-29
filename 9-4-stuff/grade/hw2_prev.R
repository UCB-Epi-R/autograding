#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Solutions: Homework 2, Prevalence
#################################################
# Load okR autograder
source('setup/autograder-setup/hw2_prev/hw2_prev.ok.R')
source('setup/autograder-setup/hw2_prev/[new]hw2_prev.ok.R')
AutograderInit()

#################################################
# Read in the data and view the data
#################################################
# Load the dplyr package
library(dplyr)

# This assignment uses data from the WASH Benefits
# Bangladesh trial. The trial assessed whether 
# water, sanitation, handwashing, and nutrition 
# interventions delivered separately or together
# could reduce child diarrhea and/or improve child
# growth. The trials used a cluster- and block-randomized 
# design. Within each geographic block, 8 village clusters
# were randomized to a treatment or conrtol arm. 
# See Luby et al. 2018 for full details 
# (doi: http://dx.doi.org/10.1016/)

# In this problem set we will calculate the prevalence
# of diarrhea in different treatment arms after the 
# interventions were delivered. 

# The data and codebooks are publicly available here: 
# https://osf.io/pqzj5/

# Load the diarrhea dataset:

d = read.csv(paste0(here::here(),"/data/washb-data/washb-bangladesh-diar-public.csv"))

# Load the dataset with treatment variables: 
tr = read.csv(paste0(here::here(),"/data/washb-data/washb-bangladesh-tr-public.csv"))

# Next let's merge the two datasets together.
# This will allow us to calculate the prevalence
# of diarrhea in different treatment arms 
# (e.g., water, sanitation, handwashing, etc.)

d_tr = left_join(d, tr, by=c("block","clusterid"))

# Now let's filter to only keep the rows for diarrhea
# measurements after the interventions were delivered. 
# The svy variable includes values 0, 1, 2. We are going
# to drop the 0 values, which indicate the time period
# before interventions were delivered. 
d_tr = d_tr %>% filter(svy!=0)

# Now we are going to drop children with missing values 
# in the diarrhea variable from the dataset. This 
# assumes that they were missing completely at random - i.e., 
# that there are no characteristics associated with whether
# a child was missing diarrhea measurement. 
d_tr = d_tr %>% filter(!is.na(diar7d))

# Take a look at the merged dataset:
head(d_tr)

#-----------------------------------------------
# Problem 1: Calculate the number of children with
# diarrhea across all children in the dataset 
# (ie., not stratifing by the treatment variable). 
# Use the variable diar7d for diarrhea. 
# Save your result in an object called p1. 
# Label the result inside p1 as n_with_diarrhea
# Hint: Using the code from the tutorial, change
# the part that says "n_with_disease" to say 
# "n_with_diarrhea"
#-----------------------------------------------
p1 = d_tr %>%
     filter(diar7d==0) %>%
    summarise(x=n())
p1

# Check your answer
CheckProblem1()

#-----------------------------------------------
# Problem 2: Calculate the number of children without
# diarrhea across all children in the dataset 
# (ie., not stratifing by the treatment variable). 
# Use the variable diar7d for diarrhea. 
# Save your result in an object called p2.  
# Label the result inside p1 as n_without_diarrhea
#-----------------------------------------------
p2 = d_tr %>%
     filter(diar7d==0) %>%
     summarise(n_without_diarrhea=n())
p2

# Check your answer
CheckProblem2()

#-----------------------------------------------
# Problem 3: Calculate diarrhea prevalence in the 
# whole dataset (ignoring treatment arm) and 
# save it in an object called prevalence.
#-----------------------------------------------
prevalence = d_tr %>% summarise(prevalence=mean(diar7d))
prevalence

# Check your answer
CheckProblem3()

#-----------------------------------------------
# Problem 4: Now let's get counts of whether children
# did or did not have diarrhea in each treatment arm. 
# In the tutorial, this created a 2x2 table for us.
# Here, since the WASH Benefits trial, there were 
# 7 different arms (6 intervention + control) 
# create a data frame with 14 rows (two for each arm). 
# The first column is called "tr" for treatment. 
# The second column is called "diar7d" and includes 0
# for children without diarrhea and 1 for children with
# diarrhea. The third column is called "n" and includes the
# number of children with or without diarrhea in that arm. 

# Hint: the row for Control with no diarrhea should be this: 
# tr              diar7d     n
# Control              0  3782
#-----------------------------------------------
diar_tr_table = d_tr %>%
                group_by(tr,diar7d) %>%
                summarise(n=n())

diar_tr_table

# Check your answer
CheckProblem4()

#-----------------------------------------------
# Problem 5: Calculate the diarrhea prevalence in  
# each treatment arm. You will need to combine
# different commands used in this problem set 
# to calculate this. Save the results in an object
# called prevalence_tr. It should have 7 rows (one
# for each treatment) and two columns. The first 
# column should be for the treatment name and the
# second should be for the prevalence. 
#-----------------------------------------------
prevalence_tr = d_tr %>% 
                group_by(tr) %>%
                summarise(prevalence=mean(diar7d))
prevalence_tr

# Check your answer
CheckProblem5()

#-----------------------------------------------
# Problem 6: Examine the results in prevalence_tr.
# Which arm had the lowest diarrhea prevalence? 
# Save the name of the treatment arm using the
# same spelling as in the treatment label in 
# prevalence_tr to indicate your answer in an
# object called p6. (e.g., p6 = "Control")
#-----------------------------------------------
p6 = "Sanitation"

# Check your answer
CheckProblem6()

#-----------------------------------------------
# Problem 7: Which treatment arm had prevalence
# closest to the prevalence in the control arm? 
# Save the name of the treatment arm using the
# same spelling as in the treatment label in 
# prevalence_tr to indicate your answer in an
# object called p6. (e.g., p7 = "Control")
#-----------------------------------------------
p7 = "Water"

# Check your answer
CheckProblem7()

# --------------------------------------------
# Check your total score
MyTotalScore()
# --------------------------------------------


