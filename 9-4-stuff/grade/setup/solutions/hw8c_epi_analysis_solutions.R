#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Solutions: Epidemiologic data analysis
# homework #3: analysis 
#####################################
# Load okR autograder
source("setup/autograder-setup/hw8c_epi_analysis/hw8c_epi_analysis.ok.R")
AutograderInit()

library(dplyr)

#-----------------------------------------------
# Load data generated in the prior assignment
#-----------------------------------------------
# Load each dataset 
all = read.csv(paste0(here::here(),"/data/washb-data/washb-bangladesh-anthro-combined.csv"))

#-----------------------------------------------
# Problem 1: What measure of association do you 
# plan to estimate to explore this hypothesis? 
# Save your answer choice in an object called p1. 
# For example, you could type p1 = "a".

# a) Mean difference
# b) Risk difference
# c) Relative risk
# d) Odds ratio
#-----------------------------------------------
p1 = "a"
  
# Check your answer
CheckProblem1()

#-----------------------------------------------
# Problem 2: For a bivariable analysis of the 
# association between improved sanitation and 
# length-for-age Z-score, what is the appropriate 
# statistical approach?

# Save your result in an object called p2. 

# a) Correlation test
# b) Chi-square test
# c) T-test
# d) ANOVA
# e) Logistic regression
# f) Log-linear regression
# g) Linear regression
#-----------------------------------------------
p2 = "c"

# Check your answer
CheckProblem2()

#-----------------------------------------------
# Problem 3: Perform the bivariable analysis 
# that you selected in Problem 2 to test whether
# the mean length-for-age Z-score (LAZ) among those
# with an improved latrine is statistically
# different from the mean among those without
# an improved latrine. 

# Your hypothesis does not specify whether one
# mean is larger than the other. 

# Name the t-test output ttest_laz
# Name the t-test test statistic ttest_laz_stat
# Name the t-test p-value ttest_laz_pvalue
#-----------------------------------------------
ttest_laz = t.test(laz ~ implatrine, data =all)
ttest_laz_stat = ttest_laz$statistic
ttest_laz_pvalue = ttest_laz$p.value

# Check your answer
CheckProblem3()

#-----------------------------------------------
# Problem 4: Based on the result of Problem 3,
# which of the following conclusions is correct
# at the alpha  = 0.05 level? 

# a) The difference in mean LAZ among those with
# and without improved latrines is statistically
# significant.

# b) The difference in mean LAZ among those with
# and without improved latrines is not statistically
# significant.

# Save your result in an object called p4.
#-----------------------------------------------
p4 = "a"

# Check your answer
CheckProblem4()

#-----------------------------------------------
# Problem 5: For a multivariable analysis of the 
# association between improved sanitation and 
# length-for-age Z-score, what is the appropriate 
# statistical approach?

# Save your result in an object called p5. 

# a) Correlation test
# b) Chi-square test
# c) T-test
# d) ANOVA
# e) Logistic regression
# f) Log-linear regression
# g) Linear regression
#-----------------------------------------------
p5 = "g"

# Check your answer
CheckProblem5()

#-----------------------------------------------
# Problem 6: Fit a model to estimate the *unadjusted*
# measure of association for length-for-age Z-score  
# and improved latrines. 

# Fit the appropriate model based on 
# your response to Problem 5 to estimate the
# measure of association you selected in Problem 1. 

# For the purpose of this assignment, assume that 
# all assumptions required to correctly obtain 
# the appropriate measure of association have been met. 

# Name the model fit laz_unadj_model
#-----------------------------------------------
laz_unadj_model = glm(laz ~ implatrine, data = all)
summary(laz_unadj_model)

# Check your answer
CheckProblem6()

#-----------------------------------------------
# Problem 7: Using your model fit from Problem 6, 
# obtain the unadjusted measure of association. Save
# it as an object named laz_unadj_moa
#-----------------------------------------------
laz_unadj_moa = laz_unadj_model$coefficients[2]
laz_unadj_moa

# Check your answer
CheckProblem7()

#-----------------------------------------------
# Problem 8: Using your model fit from Problem 7, 
# obtain the 95% confidence interval for the
# measure of association. Save it as a vector of 
# length two named laz_unadj_moa_ci, where the first
# element of the vector is the lower bound, and 
# the second element is the upper bound.
#-----------------------------------------------
laz_unadj_moa_ci = c(confint(laz_unadj_model)[2,1],
                   confint(laz_unadj_model)[2,2])
laz_unadj_moa_ci

# Check your answer
CheckProblem8()

#-----------------------------------------------
# Problem 8b: Interpret this unadjusted measure of 
# association, assessing the magnitude of the unadjusted
# measure of association as well as the 95% CI for it. 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

# The mean LAZ is 0.501 higher among children with 
# an improved latrine compared to the mean among 
# children without an improved latrine. The 
# 95% CI does not span the null (0), so this result
# is statistically significant. This finding
# indicates that there is an association between 
# better linear child growth and household 
# improved latrine ownership.

#-----------------------------------------------
# Problem 9: Fit a model to estimate the *adjusted*
# measure of association for length-for-age Z-score  
# and improved latrines. Adjust for the following
# variables: age in days, sex, month of data collection, 
# ownership of a refrigerator, mother's education level, 
# mother's height. 

# Fit the appropriate model based on 
# your response to Problem 5 to estimate the
# measure of association you selected in Problem 1. 

# Again, assume that all assumptions required to correctly  
# obtain the appropriate measure of association have been met. 

# Name the model fit laz_adj_model
#-----------------------------------------------
laz_adj_model = glm(laz ~ implatrine + aged + sex + month + asset_refrig + momedu + momheight, data = all)

# Check your answer
CheckProblem9()

#-----------------------------------------------
# Problem 10: Using your model fit from Problem 8, 
# obtain the adjusted measure of association. Save 
# it as an object named laz_adj_moa
#-----------------------------------------------
laz_adj_moa = laz_adj_model$coefficients[2]
laz_adj_moa

# Check your answer
CheckProblem10()

#-----------------------------------------------
# Problem 11: Using your model fit from Problem 9, 
# obtain the 95% confidence interval for the *adjusted* 
# measure of association. Save it as a vector of 
# length two named laz_adj_moa_ci, where the first
# element of the vector is the lower bound, and 
# the second element is the upper bound.
#-----------------------------------------------
laz_adj_moa_ci = c(confint(laz_adj_model)[2,1],
                   confint(laz_adj_model)[2,2])
laz_adj_moa_ci

# Check your answer
CheckProblem11()

#-----------------------------------------------
# Problem 11b: Interpret this unadjusted measure of 
# association, assessing the magnitude of the unadjusted
# measure of association as well as the 95% CI for it. 
# Comment on how the adjusted measure compares
# to the unadjusted measure you obtained in 
# Problems 6-8.

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

# The mean LAZ is 0.349 higher among children with 
# an improved latrine compared to the mean among 
# children without an improved latrine when
# adjusting for potential confounders. The 
# 95% CI does not span the null (0), so this result
# is statistically significant. This finding
# indicates that there is an association between 
# better linear child growth and household 
# improved latrine ownership. The adjusted association
# is weaker than the crude association.

#-----------------------------------------------
# Problem 12: Include an interaction term between 
# household refrigerator ownership and improved 
# latrines. Save the model fit as an object called
# laz_unadj_int_model
#-----------------------------------------------
laz_unadj_int_model = glm(laz ~ implatrine*asset_refrig, data = all)


# Check your answer
CheckProblem12()

#-----------------------------------------------
# Problem 13: Using your model fit from Problem 12, 
# obtain the coefficient from the interaction term.
# Save it as an object named laz_interaction
#-----------------------------------------------
laz_interaction = laz_unadj_int_model$coefficients[4]


# Check your answer
CheckProblem13()

#-----------------------------------------------
# Problem 13b: Interpret the coefficient you saved
# in problem 13. 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

# The coefficient on the interaction term is 
# equivalent to the observed mean difference 
# in LAZ among those with both an improved latrine
# and a refrigerator minus the expected mean
# difference for those two exposures. The 
# coefficient is not statistically significant, 
# so we conclude there is no interaction between
# these variables.

#-----------------------------------------------
# Problem 14: For a multivariable analysis of the 
# association between improved sanitation and 
# stunting (LAZ < -2), which of the following 
# approaches can be used to obtain the risk ratio? 

# Save your result in an object called p14. 

# a) Correlation test
# b) Chi-square test
# c) T-test
# d) ANOVA
# e) Logistic regression
# f) Log-linear regression
# g) Linear regression
#-----------------------------------------------
p14 = "f"

# Check your answer
CheckProblem14()

#-----------------------------------------------
# Problem 14b: What are the key assumptions of the 
# method selected in problem 14 that we learned in 
# this class? 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------
# No auto-correlation, no dependency in the data


#-----------------------------------------------
# Problem 15: Fit a model to estimate the *unadjusted*
# measure of association for stunting (LAZ < -2)
# and improved latrines. 

# Fit the appropriate model based on 
# your response to Problem 14.

# Again, assume that all assumptions required to correctly  
# obtain the appropriate measure of association have been met. 

# Name the model fit stunt_unadj_model
#-----------------------------------------------
stunt_unadj_model = glm(lazminus2 ~ implatrine, data = all, family=poisson(link="log"))

# Check your answer
CheckProblem15()

#-----------------------------------------------
# Problem 16: Using your model fit from Problem 15, 
# obtain the unadjusted risk ratio. Save 
# it as an object named stunt_unadj_moa
#-----------------------------------------------
stunt_unadj_moa = exp(summary(stunt_unadj_model)$coefficients[2,"Estimate"])

# Check your answer
CheckProblem16()

#-----------------------------------------------
# Problem 17: Using your model fit from Problem 15, 
# obtain the 95% confidence interval for the *unadjusted* 
# measure of association. Save it as a vector of 
# length two named stunt_unadj_moa_ci, where the first
# element of the vector is the lower bound, and 
# the second element is the upper bound.
#-----------------------------------------------
stunt_unadj_moa_ci = exp(c(confint(stunt_unadj_model)[2,1],
                           confint(stunt_unadj_model)[2,2]))

# Check your answer
CheckProblem17()

#-----------------------------------------------
# Problem 17b: Interpret this unadjusted measure of 
# association, assessing the magnitude of the unadjusted
# measure of association as well as the 95% CI for it. 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

# The prevalence of stunting among those among children 
# with an improved latrine is 0.538 times lower than
# the prevalence among children without an improved latrine. 
# The 95% CI does not span the null (1), so this result
# is statistically significant. This finding
# indicates that there is an association between 
# better linear child growth and household 
# improved latrine ownership. 

#-----------------------------------------------
# Problem 17c: Reflect on the results of the 
# analyses in this assignment. What do you conclude
# about the relationship between improved latrines
# (as defined here) and linear growth (as measured
# by height-for-age Z-score and stunting)?
#-----------------------------------------------

# In the observational analyses conducted in this
# assignment, there is a statistically significant
# association between household improved latrine
# ownership and improved linear child growth.
# This association is present when we do and do
# not adjust for potential confounders. 

#-----------------------------------------------
# Problem 17d: What are the threats to validity 
# in the analyses carried out in this assignment?
#-----------------------------------------------

# There is likely to be residual confounding that
# was not controlled for in the statistical model.
# See the DAG in the solution set to the first
# Epidemiologic analyses assignment. There may 
# also have been measurement error in assessing
# child growth and assessing whether latrines were 
# improved or not. 

#-----------------------------------------------
# Problem 17e: How do the results of this analysis
# compare to what the WASH Benefits Bangladesh
# trial found in the sanitation arm? If results
# are different, what do you think accounts for
# the discrepancy? 
#-----------------------------------------------

# These observational analyses contrast with results
# from the WASH Benefits trial, which found a 
# mean difference in LAZ of -0.02 (-0.14 to 0.09)
# in the sanitation arm at two-year follow-up.
# This discrepancy likely is due to the fact that
# the trial was able to balance measured and 
# unmeasured confounders, while the observational
# study could only account for measured confounders. 
# Any measurement error described above would 
# have applied to both analyses. 

# --------------------------------------------
# Check your total score (excludes short answer
# questions, which will be graded in okpy)
MyTotalScore()
# --------------------------------------------


