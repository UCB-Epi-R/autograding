#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: R-Quiz
###############################################

source("setup/autograder-setup/autograder_setup.R")

AutograderInit = function() {
  AutograderSetUp(10)   # Put total number of questions here
}

CheckProblem1 = function() {
  problemNumber <<- 1
  scores[problemNumber] <<- 0
  num_tests <<- 3
  tests_failed <<- num_tests

  TestCase((class(q_1) == "character" & length(q_1) == 1), error_message = "Make sure that your answer is a character!")
  TestCase((q_1 %in% c("a", "b", "c", "d")), error_message = "Make sure that your answer is one of the options listed!")
  TestCase((q_1 == "d"), error_message = "Incorrect choice, try again!")
  
  print("Correct!")
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){if(tests_failed == 0){scores[problemNumber] <<- 1}}
}

CheckProblem2 = function() {
  problemNumber <<- 2
  scores[problemNumber] <<- 0
  num_tests <<- 3
  tests_failed <<- num_tests
  
  TestCase((class(q_2) == "character" & length(q_2) == 1), error_message = "Make sure that your answer is a character!")
  TestCase((q_2 %in% c("a", "b", "c", "d")), error_message = "Make sure that your answer is one of the options listed!")
  TestCase((q_2 == "b"), error_message = "Incorrect choice, try again!")
  
  print("Correct!")
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){scores[problemNumber] <<- 1}
}

CheckProblem3 = function() {
  problemNumber <<- 3
  scores[problemNumber] <<- 0
  num_tests <<- 3
  tests_failed <<- num_tests
  
  TestCase((class(q_3) == "character" & length(q_3) == 1), error_message = "Make sure that your answer is a character!")
  TestCase((q_3 %in% c("a", "b", "c", "d")), error_message = "Make sure that your answer is one of the options listed!")
  TestCase((q_3 == "c"), error_message = "Incorrect choice, try again!")
  
  print("Correct!")
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){scores[problemNumber] <<- 1}
}

CheckProblem4 = function() {
  problemNumber <<- 4
  scores[problemNumber] <<- 0
  num_tests <<- 5
  tests_failed <<- num_tests
  q_4_correct = readRDS("setup/autograder-setup/hw1_r_quiz/q_4_correct.RDS")
  
  TestCase((length(q_4) == 4), error_message = "Make sure your vector contains 4 items")
  TestCase((q_4[1] == q_4_correct[1]), error_message = "Check the first item of your vector")
  TestCase((q_4[2] == q_4_correct[2]), error_message = "Check the second item of your vector")
  TestCase((q_4[3] == q_4_correct[3]), error_message = "Check the third item of your vector")
  TestCase((q_4[4] == q_4_correct[4]), error_message = "Check the fourth item of your vector")
  
  print("Correct!")
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){scores[problemNumber] <<- 1}
}

CheckProblem5 = function() {
  problemNumber <<- 5
  scores[problemNumber] <<- 0
  num_tests <<- 4
  tests_failed <<- num_tests
  q_5_correct = readRDS("setup/autograder-setup/hw1_r_quiz/q_5_correct.RDS")
  
  TestCase((length(q_5) == 3), error_message = "Make sure your vector contains 3 items")
  TestCase((q_5[1] == q_5_correct[1]), error_message = "Check the first item of your vector")
  TestCase((q_5[2] == q_5_correct[2]), error_message = "Check the second item of your vector")
  TestCase((q_5[3] == q_5_correct[3]), error_message = "Check the third item of your vector")

  print("Correct!")
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){scores[problemNumber] <<- 1}
}

CheckProblem6 = function() {
  problemNumber <<- 6
  scores[problemNumber] <<- 0
  num_tests <<- 3
  tests_failed <<- num_tests

  TestCase((length(q_6)==2), "Make sure that q_6 contains two column names")
  TestCase((q_6[1] == colnames(df)[1]), "Check the first column name, make sure you're not manually typing it in")
  TestCase((q_6[2] == colnames(df)[2]), "Check the second column name, make sure you're not manually typing it in")
  
  
  print("Correct!")
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){scores[problemNumber] <<- 1}
}

CheckProblem7 = function() {
  problemNumber <<- 7
  scores[problemNumber] <<- 0
  num_tests <<- 3
  tests_failed <<- num_tests
  q_7_correct = readRDS("setup/autograder-setup/hw1_r_quiz/q_7_correct.RDS")
  
  TestCase((nrow(q_7_correct)==nrow(q_7)), "Make sure that q_6 contains the same number of rows as the original table")
  TestCase((q_7$total_cost[1] == q_7_correct$total_cost[1]), "Check your calculations, make sure that you're multiplying each amount_remaining by the right price_per_item")
  TestCase((sum(q_7$total_cost) == sum(q_7_correct$total_cost)), "Check your calculations, make sure that you're multiplying each amount_remaining by the right price_per_item")
  
  
  print("Correct!")
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){scores[problemNumber] <<- 1}
}

CheckProblem8 = function() {
  problemNumber <<- 8
  scores[problemNumber] <<- 0
  num_tests <<- 3
  tests_failed <<- num_tests
  
  TestCase((class(q_8) == "character" & length(q_8) == 1), error_message = "Make sure that your answer is a character!")
  TestCase((q_8 %in% c("a", "b", "c", "d")), error_message = "Make sure that your answer is one of the options listed!")
  TestCase((q_8 == "a"), error_message = "Incorrect choice, try again!")
  
  print("Correct!")
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){scores[problemNumber] <<- 1}
}

CheckProblem9 = function() {
  problemNumber <<- 9
  scores[problemNumber] <<- 0
  num_tests <<- 4
  tests_failed <<- num_tests

  TestCase((nrow(q_9) != 0), "Your table is empty. Check the conditions that you're filtering by")
  TestCase((unique(q_9$major) == "Public Health"), "Make sure to filter the student_info table down to only Public Health majors")
  TestCase((ncol(q_9) == 4), "Make sure to keep all of the columns from the original student_info table")
  TestCase(("Alice" %in% q_9$name & !("Bob" %in% q_9$name)), "Make sure to keep students matched to their correct majors after filtering the table.")
  
  print("Correct!")
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){scores[problemNumber] <<- 1}
}

CheckProblem10 = function() {
  problemNumber <<- 10
  scores[problemNumber] <<- 0
  num_tests <<- 3
  tests_failed <<- num_tests
  age_order = readRDS("setup/autograder-setup/hw1_r_quiz/q_10_correct.RDS")
  
  TestCase((nrow(q_10) == 5), "Make sure to keep all of the rows from the original student_info table")
  TestCase((ncol(q_10) == 4), "Make sure to keep all of the columns from the original student_info table")
  TestCase((all((q_10$age - age_order) == 0)), "Make sure you sort by descending order")

  
  print("Correct!")
  ReturnScore(problemNumber, num_tests, tests_failed)
  if(tests_failed == 0){scores[problemNumber] <<- 1}
}



