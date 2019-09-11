#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: Homework 2, Prevalence
###############################################
source("setup/autograder-setup/autograder_setup.R")

AutograderInit = function() {
  AutograderSetUp(7)
}

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber <<- 1
  num_tests <<- 4
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = is.data.frame(p1), 
             correct_message = "Final answer is a data frame",
             error_message = "Make sure your final answer is a data frame")
  
  CheckPoint(checkpoint_number = 2,
             test = nrow(p1) ==1,
             correct_message = "Final answer is a data frame with only one row",
             error_message = "Did you remember to sum the number of children with diarrhea in the whole dataset? There are too many rows in your result.")
  
  CheckPoint(checkpoint_number = 3,
             test = (p1 != 16727 & p1 != 15966),
             correct_message = "Final answer has been filtered for children with diarrhea",
             error_message = "Did you remember to filter to only show results for children with diarrhea?")
  
  CheckPoint(checkpoint_number = 4,
             test = p1==761,
             correct_message = "Final answer correctly summed the total number of children.",
             error_message = "Did you correctly filter to children with diarrhea and then sum the total number of children?")
  
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
  num_tests <<- 4
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = is.data.frame(p2), 
             correct_message = "Final answer is a data frame",
             error_message = "Make sure your final answer is a data frame")
  
  CheckPoint(checkpoint_number = 2,
             test = nrow(p2) ==1,
             correct_message = "Final answer is a data frame with only one row",
             error_message = "Did you remember to sum the number of children with diarrhea in the whole dataset? There are too many rows in your result.")
  
  CheckPoint(checkpoint_number = 3,
             test = (p2 != 16727 & p2 != 761),
             correct_message = "Final answer has been filtered for children without diarrhea",
             error_message = "Did you remember to filter to only show results for children without diarrhea?")
  
  CheckPoint(checkpoint_number = 4,
             test = p2==15966,
             correct_message = "Final answer correctly summed the total number of children.",
             error_message = "Did you correctly filter to children without diarrhea and then sum the total number of children?")

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
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(prevalence) == 1, 
             correct_message = "Final answer is a data frame with one row",
             error_message = "Did you remember to summarize the dataset to calculate the diarrhea prevalence? There are too many rows in your result.")
  
  CheckPoint(checkpoint_number = 2,
             test = round(prevalence)==round(0.04549531), 
             correct_message = "Correctly calculates the mean of the diar7d variable.",
             error_message = "Did you correctly calculate the mean of the diar7d variable?")
  
  
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
  num_tests <<- 6
  tests_failed <<- num_tests
  
  diar_tr_table_correct = readRDS("setup/autograder-setup/hw2_prev/diar_tr_table_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(diar_tr_table) ==14, 
             correct_message = "Final answer is a data frame with the right number of rows.",
             error_message = "Did you correctly group by the treatment variable and diar7d variable? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = ncol(diar_tr_table) ==3,
             correct_message = "Final answer is a data frame with the right number of column",
             error_message = "Did you correctly group by the treatment variable and diar7d variable? The number of columns in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(colnames(diar_tr_table), colnames(diar_tr_table_correct)),
             correct_message = "Final answer is a data frame with the correct column names",
             error_message =  "Did you correctly group by the treatment variable and diar7d variable? Did you label the count of children with and without diarrhea 'n'? The column names are incorrect.")
  
  CheckPoint(checkpoint_number = 4,
             test = setequal(diar_tr_table$tr, diar_tr_table_correct$tr),
             correct_message = "Final answer is a data frame with the correct treatment column",
             error_message =  "Did you correctly group by the treatment variable and diar7d variable? There is an error in the column with the treatment label.")
  
  CheckPoint(checkpoint_number = 5,
             test = setequal(diar_tr_table$tr, diar_tr_table_correct$tr),
             correct_message = "Final answer is a data frame with the correct diar7d column",
             error_message =  "Did you correctly group by the treatment variable and diar7d variable? There is an error in the column indicating diarrhea vs. no diarrhea.")
  
  CheckPoint(checkpoint_number = 6,
             test = setequal(diar_tr_table$n, diar_tr_table_correct$n),
             correct_message = "Final answer is a data frame with the correct n column",
             error_message =  "Did you correctly group by the treatment variable and diar7d variable and then count the number of children in each category? There is an error in the 'n' column.")
  
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
  num_tests <<- 5
  tests_failed <<- num_tests
  
  prevalence_tr_correct = readRDS("setup/autograder-setup/hw2_prev/prevalence_tr_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(prevalence_tr) ==7, 
             correct_message = "Final answer is a data frame with the right number of rows.",
             error_message = "Did you correctly group by the treatment variable? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = ncol(prevalence_tr) ==2,
             correct_message = "Final answer is a data frame with the right number of column",
             error_message = "Did you correctly group by the treatment variable? The number of columns in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(colnames(prevalence_tr), colnames(prevalence_tr_correct)),
             correct_message = "Final answer is a data frame with the correct column names",
             error_message =  "Did you correctly group by the treatment variable? Did you label the prevalence column 'prevalence'? The column names are incorrect.")
  
  CheckPoint(checkpoint_number = 4,
             test = setequal(prevalence_tr$tr, prevalence_tr_correct$tr),
             correct_message = "Final answer is a data frame with the correct treatment column",
             error_message =  "Did you correctly group by the treatment variable? There is an error in the column with the treatment label.")
  
  CheckPoint(checkpoint_number = 5,
             test = setequal(prevalence_tr$tr, prevalence_tr_correct$tr),
             correct_message = "Final answer is a data frame with the correct prevalence column",
             error_message =  "Did you correctly group by the treatment variable and then take the mean of diar7d in each category? There is an error in the 'prevalence' column.")
  
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
  num_tests <<- 1
  tests_failed <<- num_tests

  CheckPoint(checkpoint_number = 1,
             test = p6=="Sanitation",
             correct_message = "Correct",
             error_message =  "Incorrect. Check your result to Problem 5 and try again.")
  
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
  
  CheckPoint(checkpoint_number = 1,
             test = p7=="Water",
             correct_message = "Correct",
             error_message =  "Incorrect. Check your result to Problem 5 and try again.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


