#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Autograder: Homework 4, Functions
###############################################
library(here)
library(jsonlite)
library(rlist)
library(checkr)         # CheckR is generally more useful for open-ended, broader checks
library(assertthat)     # Assertthat is generally more useful for more specific, assertion-based checks
library(dplyr)

AutograderInit = function() {
  scores <<- vector(mode="character", length=7)   # Put total number of questions here
}

ReturnScore = function(problemNumber, eval) {
  if (eval == TRUE) {
    cat(sprintf("Problem %d: 1/1\n", problemNumber))
  }
  else {
    cat(sprintf("Problem %d: 0/1\nError: %s\n", problemNumber, eval))
  }
}

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber = 1
  
  eval <- tryCatch(
    {
      assert_that(exists("estimate_rr"),
                  msg="Did you create a function called estimate_rr?")
      assert_that(is.function(estimate_rr),
                  msg="Are you sure you defined estimate_rr as a function?")
      assert_that(has_args(estimate_rr, c("a","b","c","d"), exact=TRUE),
                  msg="Did you enter the correct arguments to the function (a, b, c, d)?")
    }
  ) 
  
  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem2 = function() {
  problemNumber = 2
  
  eval <- assert_that(p2 == 0.5,
                      msg="Check that your estimate_rr function uses the correct formula and that
                       your function call entered the correct arguments.")
  
  if(eval==TRUE){
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
      assert_that(exists("estimate_rr_ci"),
                  msg="Did you create a function called estimate_rr_ci?")
      assert_that(is.function(estimate_rr_ci),
                  msg="Are you sure you defined estimate_rr_ci as a function?")
      assert_that(has_args(estimate_rr_ci, c("a","b","c","d"), exact=TRUE),
                  msg="Did you enter the correct arguments to the function (a, b, c, d)?")
      
    }
  ) 
  
  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem4 = function() {
  problemNumber = 4
  
  estimate_rr_ci_correct=readRDS("setup/autograder-setup/hw4_functions/estimate_rr_ci_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(length(p4_lb) == 1,
                  msg="p4_lb should only contain one number.")
      assert_that(length(p4_ub) == 1,
                  msg="p4_ub should only contain one number.")
      assert_that(p4_lb != estimate_rr_ci_correct(5, 10, 20, 10)[2],
                  msg="You accidentally saved the upper bound in p4_lb.")
      assert_that(p4_ub != estimate_rr_ci_correct(5, 10, 20, 10)[1],
                  msg="You accidentally saved the lower bound in p4_ub.")
     
      assert_that(round(p4_lb,3) == round(estimate_rr_ci_correct(5, 10, 20, 10)[1],3),
                  msg="Check that your function uses the correct formula and that
                  your function call entered the correct arguments.")
      assert_that(round(p4_ub,3) == round(estimate_rr_ci_correct(5, 10, 20, 10)[2],3),
                  msg="Check that your function uses the correct formula and that
                  your function call entered the correct arguments.")

    }
  ) 
  
  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem5 = function() {
  problemNumber = 5

  eval <- tryCatch(
    {
      assert_that(exists("estimate_or"),
                  msg="Did you create a function called estimate_or?")
      assert_that(is.function(estimate_or),
                  msg="Are you sure you defined estimate_or as a function?")
      assert_that(has_args(estimate_or, c("a","b","c","d"), exact=TRUE),
                  msg="Did you enter the correct arguments to the function (a, b, c, d)?")
    }
  ) 
  
  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem6 = function() {
  problemNumber = 6
  
  eval <- assert_that(p6 == 0.25,
                      msg="Check that your estimate_or function uses the correct formula and that
                       your function call entered the correct arguments.")
  
  if(eval==TRUE){
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
      assert_that(exists("estimate_or_ci"),
                  msg="Did you create a function called estimate_or_ci?")
      assert_that(is.function(estimate_or_ci),
                  msg="Are you sure you defined estimate_or_ci as a function?")
      assert_that(has_args(estimate_or_ci, c("a","b","c","d"), exact=TRUE),
                  msg="Did you enter the correct arguments to the function (a, b, c, d)?")
    }
  ) 
  
  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem8 = function() {
  problemNumber = 8
  
  estimate_or_ci_correct=readRDS("setup/autograder-setup/hw4_functions/estimate_or_ci_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(length(p8_lb) == 1,
                  msg="p8_lb should only contain one number.")
      assert_that(length(p8_ub) == 1,
                  msg="p8_ub should only contain one number.")
      assert_that(p8_lb != estimate_or_ci_correct(5, 10, 20, 10)[2],
                  msg="You accidentally saved the upper bound in p8_lb.")
      assert_that(p8_ub != estimate_or_ci_correct(5, 10, 20, 10)[1],
                  msg="You accidentally saved the lower bound in p8_ub.")
      
      assert_that(round(p8_lb,3) == round(estimate_or_ci_correct(5, 10, 20, 10)[1],3),
                  msg="Check that your function uses the correct formula and that
                  your function call entered the correct arguments.")
      assert_that(round(p8_ub,3) == round(estimate_or_ci_correct(5, 10, 20, 10)[2],3),
                  msg="Check that your function uses the correct formula and that
                  your function call entered the correct arguments.")
      
    }
  ) 
  
  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem9 = function() {
  problemNumber = 9

  eval <- tryCatch(
    {
      assert_that(exists("estimate_par"),
                  msg="Did you create a function called estimate_par?")
      assert_that(is.function(estimate_par),
                  msg="Are you sure you defined estimate_par as a function?")
      assert_that(has_args(estimate_par, c("a","b","c","d"), exact=TRUE),
                  msg="Did you enter the correct arguments to the function (a, b, c, d)?")
    }
  ) 
  
  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem10 = function() {
  problemNumber = 10
  
  estimate_par_correct = readRDS("setup/autograder-setup/hw4_functions/estimate_par_correct.RDS")
  
  eval <- assert_that(round(p10,3) == round(estimate_par_correct(5, 10, 20, 10),3),
                      msg="Check that your estimate_par function uses the correct formula and that
                       your function call entered the correct arguments.")
  
  if(eval==TRUE){
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
