#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: Setup
###############################################

source("setup/autograder-setup/autograder_setup.R")

AutograderInit = function() {
  AutograderSetUp(2)   # Put total number of questions here
}

CheckProblem1 = function() {
  problemNumber <<- 1
  scores[problemNumber] <<- 0
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = is.numeric(x),
             correct_message = "x is a number",
             error_message = "Make sure that x is set to a numeric value")
  
  CheckPoint(checkpoint_number = 2,
             test = as.numeric(x) == 5,
             correct_message = "x is equal to 5",
             error_message = "x does not equal 5. Check your calculation!")
  
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){scores[problemNumber] <<- 1}
}
