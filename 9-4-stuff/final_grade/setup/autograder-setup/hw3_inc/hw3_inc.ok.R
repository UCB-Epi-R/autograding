#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: Homework 3, Incidence
###############################################
source("setup/autograder-setup/autograder_setup.R")

AutograderInit = function() {
  AutograderSetUp(8)
}

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber = 1
  num_tests <<- 3
  tests_failed <<- num_tests
  
  km.correct = readRDS("setup/autograder-setup/hw3_inc/km.correct1.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = is.data.frame(km), 
             correct_message = "Final answer is a data frame",
             error_message = "Did you accidentally overwrite the original data frame km?")
  
  CheckPoint(checkpoint_number = 2,
             test = ("cond_risk" %in% colnames(km) & !is.null(km$cond_risk)), 
             correct_message = "Final answer is a data frame with a cond_risk column",
             error_message = "Did you add a new column named cond_risk?")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(km$cond_risk, km.correct$cond_risk), 
             correct_message = "Final answer has the right values in the cond_risk column",
             error_message = "Did you use the correct formula for cond_risk?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem2 = function() {
  problemNumber = 2
  num_tests <<- 3
  tests_failed <<- num_tests
  
  km.correct = readRDS("setup/autograder-setup/hw3_inc/km.correct2.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = is.data.frame(km), 
             correct_message = "Final answer is a data frame",
             error_message = "Did you accidentally overwrite the original data frame km?")
  
  CheckPoint(checkpoint_number = 2,
             test = ("cond_surv" %in% colnames(km) & !is.null(km$cond_surv)), 
             correct_message = "Final answer is a data frame with a cond_surv column",
             error_message = "Did you add a new column named cond_surv?")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(km$cond_surv, km.correct$cond_surv), 
             correct_message = "Final answer has the right values in the cond_surv column",
             error_message = "Did you use the correct formula for cond_surv?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem3 = function() {
  problemNumber = 3   
  num_tests <<- 1
  tests_failed <<- num_tests
  
  km.correct = readRDS("setup/autograder-setup/hw3_inc/km.correct2.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(km_cum_risk, 1 - prod(km.correct$cond_surv)), 
             correct_message = "Final answer has the right km_cum_risk value",
             error_message = "Did you use the correct formula for km_cum_risk?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem4 = function() {
  problemNumber = 4  
  num_tests <<- 1
  tests_failed <<- num_tests
  
  km.correct = readRDS("setup/autograder-setup/hw3_inc/km.correct2.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(km_cum_surv, 1 - km_cum_risk), 
             correct_message = "Final answer has the right km_cum_surv value",
             error_message = "Did you use the correct formula for km_cum_surv?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem5 = function() {
  problemNumber = 5
  num_tests <<- 3
  tests_failed <<- num_tests
  
  dm.correct = readRDS("setup/autograder-setup/hw3_inc/dm.correct.RDS")
    
  CheckPoint(checkpoint_number = 1,
             test = is.data.frame(dm), 
             correct_message = "Final answer is a data frame",
             error_message = "Did you accidentally overwrite the original data frame dm?")
  
  CheckPoint(checkpoint_number = 2,
             test = !is.null(dm$PT), 
             correct_message = "Final answer is a data frame with a PT column",
             error_message = "Did you add a new column named PT?")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(dm$PT, dm.correct$PT), 
             correct_message = "Final answer has the right values in the PT column",
             error_message = "Did you use the correct formula for PT?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem6 = function() {
  problemNumber = 6
  num_tests <<- 3
  tests_failed <<- num_tests
  
  dm.correct = readRDS("setup/autograder-setup/hw3_inc/dm.correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = is.data.frame(dm), 
             correct_message = "Final answer is a data frame",
             error_message = "Did you accidentally overwrite the original data frame dm?")
  
  CheckPoint(checkpoint_number = 2,
             test = ("ID" %in% colnames(dm) & !is.null(dm$ID)), 
             correct_message = "Final answer is a data frame with an ID column",
             error_message = "Did you add a new column named ID?")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(dm$ID, dm.correct$ID), 
             correct_message = "Final answer has the right values in the ID column",
             error_message = "Did you use the correct formula for ID?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem7 = function() {
  problemNumber = 7
  num_tests <<- 1
  tests_failed <<- num_tests
  
  dm.correct = readRDS("setup/autograder-setup/hw3_inc/dm.correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(dm_cum_risk,  1 - exp(-2*sum(dm.correct$ID))), 
             correct_message = "Final answer has the right dm_cum_risk value",
             error_message = "Did you use the correct formula for dm_cum_risk?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem8 = function() {
  problemNumber = 8
  num_tests <<- 1
  tests_failed <<- num_tests
  
  dm.correct = readRDS("setup/autograder-setup/hw3_inc/dm.correct.RDS")
  dm_cum_risk.correct = 1 - exp(-2*sum(dm.correct$ID))
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(dm_cum_surv,  1 - dm_cum_risk.correct),
             correct_message = "Final answer has the right dm_cum_surv value",
             error_message = "Did you use the correct formula for dm_cum_surv?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}
