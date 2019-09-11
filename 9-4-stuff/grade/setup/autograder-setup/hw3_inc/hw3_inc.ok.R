#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Autograder: Homework 3, Incidence
###############################################
library(here)
library(jsonlite)
library(rlist)
library(checkr)         # CheckR is generally more useful for open-ended, broader checks
library(assertthat)     # Assertthat is generally more useful for more specific, assertion-based checks
library(dplyr)

AutograderInit = function() {
  scores <<- vector(mode="character", length=8)   # Put total number of questions here
}

ReturnScore = function(problemNumber, eval) {
  if (eval == TRUE) {
    cat(sprintf("Problem %d: 1/1\n", problemNumber))
  }
  else {
    cat(sprintf("Problem %d: 0/1\nError: %s\n", problemNumber, eval))
  }
}

km.correct=data.frame(T=c(2,4,10,14,18,20),
              N=c(10,8,6,5,3,2),
              I=rep(1,6))

dm.correct=data.frame(T=seq(2,24,by=2),
              N=c(10,8,7,7,7,5,5,4,3,2,1,1),
              I=c(1,1,0,0,1,0,1,0,1,1,0,0))

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber = 1
  
  km.correct = readRDS("setup/autograder-setup/hw3_inc/km.correct1.RDS")
  
  eval <- tryCatch(
    {
      assert_that(is.data.frame(km),
                  msg="Did you accidentally overwrite the original data frame km?")
      assert_that("cond_risk" %in% colnames(km),
                  msg="Did you add a new column named cond_risk?")
      assert_that(!is.null(km$cond_risk), 
                  msg="Did you add a new column named cond_risk?")
      assert_that(setequal(km$cond_risk, km.correct$cond_risk), 
                  msg="Did you use the correct formula for cond_risk?")
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem2 = function() {
  problemNumber = 2

  km.correct = readRDS("setup/autograder-setup/hw3_inc/km.correct2.RDS")

  eval <- tryCatch(
    {
      assert_that(is.data.frame(km),
                  msg="Did you accidentally overwrite the original data frame km?")
      assert_that("cond_surv" %in% colnames(km),
                  msg="Did you add a new column named cond_surv?")
      assert_that(!is.null(km$cond_surv), 
                  msg="Did you add a new column named cond_surv?")
      assert_that(setequal(km$cond_surv, km.correct$cond_surv), 
                  msg="Did you use the correct formula for cond_surv?")
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
  
  km.correct = readRDS("setup/autograder-setup/hw3_inc/km.correct2.RDS")
  
  eval <- assert_that(setequal(km_cum_risk, 1 - prod(km.correct$cond_surv)),
                  msg="Did you use the correct formula for km_cum_risk?")

  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem4 = function() {
  problemNumber = 4   
  
  km.correct = readRDS("setup/autograder-setup/hw3_inc/km.correct2.RDS")
  
  eval <- assert_that(setequal(km_cum_surv, 1 - km_cum_risk),
                      msg="Did you use the correct formula for km_cum_surv?")
  
  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem5 = function() {
  problemNumber = 5
  
  dm.correct = readRDS("setup/autograder-setup/hw3_inc/dm.correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(is.data.frame(dm),
                  msg="Did you accidentally overwrite the original data frame dm?")
      assert_that(!is.null(dm$PT), 
                  msg="Did you add a new column named PT?")
      assert_that(setequal(dm$PT, dm.correct$PT), 
                  msg="Did you use the correct formula for PT?")
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
  
  dm.correct = readRDS("setup/autograder-setup/hw3_inc/dm.correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(is.data.frame(dm),
                  msg="Did you accidentally overwrite the original data frame dm?")
      assert_that("ID" %in% colnames(dm),
                  msg="Did you add a new column named ID?")
      assert_that(!is.null(dm$ID), 
                  msg="Did you add a new column named ID?")
      assert_that(setequal(dm$ID, dm.correct$ID), 
                  msg="Did you use the correct formula for ID?")
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
  
  dm.correct = readRDS("setup/autograder-setup/hw3_inc/dm.correct.RDS")
  
  eval <- assert_that(setequal(dm_cum_risk,  1 - exp(-2*sum(dm.correct$ID))),
                      msg="Did you use the correct formula for dm_cum_risk?")
  
  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem8 = function() {
  problemNumber = 8
  
  eval <- assert_that(setequal(dm_cum_surv,  1 - dm_cum_risk),
                      msg="Did you use the correct formula for dm_cum_surv?")
  
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
