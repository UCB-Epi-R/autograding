#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Autograder: Homework 2, Prevalence
###############################################
source("setup/autograder-setup/autograder_setup.R")

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber <<- 1
  scores[problemNumber] <<- 0
  num_tests <<- 4
  tests_failed <<- num_tests
  
  CheckPoint(is.data.frame(p1), 
             correct_message = "Checkpoint 1 Passed: answer is a data frame",
             error_message = "Checkpoint 1 Error: Make sure your final answer is a data frame")
  
  CheckPoint(nrow(p1) ==1,
             correct_message = "Checkpoint 2 Passed: answer is a data frame with only one row",
             error_message = "Checkpoint 2 Error: Did you remember to sum the number of children with diarrhea in the whole dataset? There are too many rows in your result.")
  
  CheckPoint((p1 != 16727 & p1 != 15966),
             correct_message = "Checkpoint 3 Passed: data frame has been filtered for children with diarrhea",
             error_message = "Checkpoint 3 Error: Did you remember to filter to only show results for children with diarrhea?")
  
  CheckPoint(p1==761,
             correct_message = "Checkpoint 4 Passed: correctly summed the total number of children.",
             error_message = "Checkpoint 4 Error: Did you correctly filter to children with diarrhea and then sum the total number of children?")
  
  scores[problemNumber] <<- 1
}


# --------------------------------------------
CheckProblem2 = function() {
  problemNumber = 2
  
  eval <- tryCatch(
    {
      assert_that(nrow(p2) ==1, 
                  msg="Did you remember to sum the number of children without diarrhea in the whole dataset? There are too many rows in your result.")
      assert_that(p2 != 761, 
                  msg="Did you remember to filter to only show results for children without diarrhea?")
      assert_that(p2 != 16727, 
                  msg="Did you remember to filter to only show results for children without diarrhea?")
      assert_that(p2==15966, 
                  msg="Did you correctly filter to children without diarrhea and then sum the total number of children?")
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}



# --------------------------------------------
CheckProblem3 = function() {
  problemNumber = 3
  
  eval <- tryCatch(
    {
      assert_that(nrow(prevalence) ==1, 
                  msg="Did you remember to summarize the dataset to calculate the diarrhea prevalence? There are too many rows in your result.")
     assert_that(round(prevalence)==round(0.04549531), 
                  msg="Did you correctly calculate the mean of the diar7d variable?")
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}



# --------------------------------------------
CheckProblem4 = function() {
  problemNumber = 4
  
  diar_tr_table_correct = readRDS("setup/autograder-setup/hw2_prev/diar_tr_table_correct.RDS")

  eval <- tryCatch(
    {
      assert_that(nrow(diar_tr_table) ==14, 
                  msg="Did you correctly group by the treatment variable and diar7d variable? The number of rows in your result is incorrect.")
      assert_that(ncol(diar_tr_table) ==3,
                  msg="Did you correctly group by the treatment variable and diar7d variable? The number of columns in your result is incorrect.")
      assert_that(setequal(colnames(diar_tr_table), c("tr","diar7d","n")),
                  msg="Did you correctly group by the treatment variable and diar7d variable? Did you label the count of children with and without diarrhea 'n'? The column names are in correct.")
      assert_that(setequal(diar_tr_table$tr, diar_tr_table_correct$tr),
                  msg="Did you correctly group by the treatment variable and diar7d variable? There is an error in the column with the treatment label.")
      assert_that(setequal(diar_tr_table$diar7d, diar_tr_table_correct$diar7d),
                  msg="Did you correctly group by the treatment variable and diar7d variable? There is an error in the column indicating diarrhea vs. no diarrhea.")
      assert_that(setequal(diar_tr_table$n, diar_tr_table_correct$n),
                  msg="Did you correctly group by the treatment variable and diar7d variable and then count the number of children in each category? There is an error in the 'n' column.")
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem5 = function() {
  problemNumber = 5
  
  prevalence_tr_correct = readRDS("setup/autograder-setup/hw2_prev/prevalence_tr_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(nrow(prevalence_tr) ==7, 
                  msg="Did you correctly group by the treatment variable? The number of rows in your result is incorrect.")
      assert_that(ncol(prevalence_tr) ==2,
                  msg="Did you correctly group by the treatment variable? The number of columns in your result is incorrect.")
      assert_that(setequal(colnames(prevalence_tr), c("tr","prevalence")),
                  msg="Did you correctly group by the treatment variable? Did you label the prevalence column 'prevalence'? The column names are incorrect.")
      assert_that(setequal(prevalence_tr$tr, prevalence_tr_correct$tr),
                  msg="Did you correctly group by the treatment variable? There is an error in the column with the treatment label.")
      assert_that(setequal(prevalence_tr$prevalence, prevalence_tr_correct$prevalence),
                  msg="Did you correctly group by the treatment variable and diar7d variable? There is an error in the column indicating diarrhea vs. no diarrhea.")
      assert_that(setequal(prevalence_tr$prevalence, prevalence_tr_correct$prevalence),
                  msg="Did you correctly group by the treatment variable and then take the mean of diar7d in each category? There is an error in the 'prevalence' column.")
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem6 = function() {
  problemNumber = 6
  
  eval <- tryCatch(
    {
      assert_that(p6=="Sanitation",
                  msg="Incorrect. Check your result to Problem 5 and try again.")
    }
  ) 
    
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem7 = function() {
  problemNumber = 7
  
  eval <- tryCatch(
    {
      assert_that(p7=="Water",
                  msg="Incorrect. Check your result to Problem 5 and try again.")
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


###############################################
# Renders scores from each problem and returns as a dataframe.
MyTotalScore = function() {
  scoresList = c()
  problemNumber = 1
  problemTitles = c()
  totalCorrect = 0
  
  while (problemNumber <= length(scores)) {
    
    score = scores[problemNumber]
    if (score == TRUE) {
      scoresList = c(scoresList, "1/1")
      totalCorrect = totalCorrect + 1
    } else {
      scoresList = c(scoresList, "0/1")
    }
    
    problemTitle = sprintf("Problem %d:", problemNumber)
    problemTitles = c(problemTitles, problemTitle)
    
    problemNumber = problemNumber + 1
  }
  
  problemTitles = c(problemTitles, "Total Score:")
  scoresList = c(scoresList, sprintf("%d/%d", totalCorrect, length(scores)))
  
  renderedScores = data.frame(
    Score = scoresList
  )
  rownames(renderedScores) = problemTitles
  colnames(renderedScores) = ""
  
  return(renderedScores)
}
