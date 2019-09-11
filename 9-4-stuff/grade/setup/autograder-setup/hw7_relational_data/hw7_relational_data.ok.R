#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Autograder: Relational data
###############################################
library(here)
library(jsonlite)
library(rlist)
library(checkr)         # CheckR is generally more useful for open-ended, broader checks
library(assertthat)     # Assertthat is generally more useful for more specific, assertion-based checks
library(dplyr)

AutograderInit = function() {
  scores <<- vector(mode="character", length=16)   # Put total number of questions here
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
  
  prev_time_correct = readRDS("setup/autograder-setup/hw7_relational_data/prev_time_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(nrow(prev_time) ==1, 
                  msg="Did you correctly summarize the data? The number of rows in your result is incorrect.")
      assert_that(ncol(prev_time) ==3, 
                  msg="Did you correctly summarize the data? The number of columns in your result is incorrect.")
      assert_that(setequal(prev_time$crypto_prev1, prev_time_correct$crypto_prev1),
                  msg="There is an error in the 'crypto_prev1' column.")
      assert_that(setequal(prev_time$crypto_prev2, prev_time_correct$crypto_prev2),
                  msg="There is an error in the 'crypto_prev2' column.")
      assert_that(setequal(prev_time$crypto_prev3, prev_time_correct$crypto_prev3),
                  msg="There is an error in the 'crypto_prev3' column.")
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
  
  lab_data_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/lab_data_long_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(nrow(lab_data_long)==30,
                  msg="Did you correctly convert to long format? The number of rows is incorrect.")
      assert_that(ncol(lab_data_long)==3,
                  msg="Did you correctly convert to long format? The number of columns is incorrect.")
      assert_that(setequal(lab_data_long$crypto, lab_data_long_correct$crypto),
                  msg="Did you correctly convert to long format? There is an error in the crypto column.")
      assert_that(setequal(lab_data_long$time, lab_data_long_correct$time),
                  msg="Did you correctly convert to long format? There is an error in the time column.")
    }
  ) 
  
  if(eval==TRUE){
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem3 = function() {
  problemNumber = 3
  
  prev_correct = readRDS("setup/autograder-setup/hw7_relational_data/prev_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(nrow(prev) ==1, 
                  msg="Did you correctly summarize the data? The number of rows in your result is incorrect.")
      assert_that(ncol(prev) ==1, 
                  msg="Did you correctly summarize the data? The number of columns in your result is incorrect.")
      assert_that(!is.na(prev$crypto_prev),
                  msg="Did you remember to remove missings mean calculating prevalence? The prevalence result is incorrect.")
      assert_that(setequal(prev$crypto_prev, prev_correct$crypto_prev),
                  msg="Did you correctly summarize the data? The prevalence result is incorrect.")

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

  eval <- tryCatch(
    {
      assert_that(p4 =="d", 
                  msg="Incorrect. Try again.")
      
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
  
  lab_data_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/lab_data_long_correct.RDS")
  
  baseline_lab_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/baseline_lab_long_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(nrow(baseline_lab_long)==30,
                  msg="Did you correctly convert to long format? The number of rows is incorrect.")
      assert_that(setequal(baseline_lab_long$crypto, baseline_lab_long_correct$crypto),
                  msg="Did you correctly convert to long format? There is an error in the crypto column.")
      assert_that(setequal(baseline_lab_long$time, lab_data_long_correct$time),
                  msg="Did you correctly convert to long format? There is an error in the time column.")
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
  
  baseline_lab_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/baseline_lab_long_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that("age_u5" %in% colnames(baseline_lab_long),
                  msg="Did you add a new column named age_u5?")
      assert_that(setequal(baseline_lab_long$age_u5, baseline_lab_long_correct$age_u5),
                  msg="There is an error in the age_u5 column. Try again.")

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
  
  prev_age_correct = readRDS("setup/autograder-setup/hw7_relational_data/prev_age_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(nrow(prev_age) ==2, 
                  msg="Did you correctly summarize the data? The number of rows in your result is incorrect.")
      assert_that(ncol(prev_age) ==2, 
                  msg="Did you correctly summarize the data? The number of columns in your result is incorrect.")
      assert_that("age_u5" %in% colnames(prev_age),
                  msg="Did you calculate prevalence within each level of age_u5?")
      assert_that(all(!is.na(prev_age$crypto_prev)),
                  msg="Did you remember to remove missings mean calculating prevalence? The prevalence result is incorrect.")
      assert_that(setequal(prev_age$crypto_prev, prev_age_correct$crypto_prev),
                  msg="Did you correctly summarize the data? The prevalence result is incorrect.")
      
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem8 = function() {
  problemNumber = 8
  
  eval <- tryCatch(
    {
      assert_that(p8 =="a", 
                  msg="Incorrect. Try again.")
      
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem9 = function() {
  problemNumber = 9
  
  prev_sex_correct = readRDS("setup/autograder-setup/hw7_relational_data/prev_sex_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(nrow(prev_sex) ==2, 
                  msg="Did you correctly summarize the data? The number of rows in your result is incorrect.")
      assert_that(ncol(prev_sex) ==2, 
                  msg="Did you correctly summarize the data? The number of columns in your result is incorrect.")
      assert_that("female" %in% colnames(prev_sex),
                  msg="Did you calculate prevalence within each level of female?")
      assert_that(all(!is.na(prev_sex$crypto_prev)),
                  msg="Did you remember to remove missings mean calculating prevalence? The prevalence result is incorrect.")
      assert_that(setequal(prev_sex$crypto_prev, prev_sex_correct$crypto_prev),
                  msg="Did you correctly summarize the data? The prevalence result is incorrect.")
      
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem10 = function() {
  problemNumber = 10
  
  eval <- tryCatch(
    {
      assert_that(p10 =="c", 
                  msg="Incorrect. Try again.")
      
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem11 = function() {
  problemNumber = 11
  
  fu_survey_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/fu_survey_long_correct.RDS")

  
  eval <- tryCatch(
    {
      assert_that(nrow(fu_survey_long)==30,
                  msg="Did you correctly convert to long format? The number of rows is incorrect.")
      assert_that(setequal(fu_survey_long$diarrhea, fu_survey_long_correct$diarrhea),
                  msg="Did you correctly convert to long format? There is an error in the diarrhea column.")
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem12 = function() {
  problemNumber = 12
  
  fu_survey_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/fu_survey_long_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(setequal(fu_survey_long_correct$time, fu_survey_long_correct$time),
                  msg="There is an error in the time column. Try again.")
      
    }
  ) 
  
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem13 = function() {
  problemNumber = 13
  
  fu_survey_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/fu_survey_long_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(setequal(fu_survey_long_correct$time, fu_survey_long_correct$time),
                  msg="There is an error in the time column. Try again.")
      
    }
  ) 
  
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem14 = function() {
  problemNumber = 14
  
  lab_data_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/lab_data_long_correct.RDS")
  
  fu_survey_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/fu_survey_long_correct.RDS")
  
  lab_fu_survey_correct = readRDS("setup/autograder-setup/hw7_relational_data/lab_fu_survey_correct.RDS")
  
  eval <- tryCatch(
    {
      assert_that(nrow(fu_survey_long)==30,
                  msg="Did you correctly convert to long format? The number of rows is incorrect.")
      assert_that(setequal(fu_survey_long$diarrhea, fu_survey_long_correct$diarrhea),
                  msg="Did you correctly convert to long format? There is an error in the diarrhea column.")
      assert_that(setequal(fu_survey_long$time, fu_survey_long_correct$time),
                  msg="Did you correctly convert to long format? There is an error in the time column.")
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem15 = function() {
  problemNumber = 15
  
  lab_data_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/lab_data_long_correct.RDS")
  
  fu_survey_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/fu_survey_long_correct.RDS")
  
  lab_fu_survey_correct = readRDS("setup/autograder-setup/hw7_relational_data/lab_fu_survey_correct.RDS")
  
  diarrhea_crypto_prev_correct = readRDS("setup/autograder-setup/hw7_relational_data/diarrhea_crypto_prev_correct.RDS")

  eval <- tryCatch(
    {
      assert_that(nrow(diarrhea_crypto_prev) ==2, 
                  msg="Did you correctly summarize the data? The number of rows in your result is incorrect.")
      assert_that(ncol(diarrhea_crypto_prev) ==2, 
                  msg="Did you correctly summarize the data? The number of columns in your result is incorrect.")
      assert_that("diarrhea" %in% colnames(diarrhea_crypto_prev),
                  msg="Did you calculate prevalence within each level of diarrhea?")
      assert_that(all(!is.na(diarrhea_crypto_prev$crypto_prev)),
                  msg="Did you remember to remove missings mean calculating prevalence? The prevalence result is incorrect.")
      assert_that(setequal(diarrhea_crypto_prev$crypto_prev, diarrhea_crypto_prev$crypto_prev),
                  msg="Did you correctly summarize the data? The prevalence result is incorrect.")
      
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem16 = function() {
  problemNumber = 16
  
  eval <- tryCatch(
    {
      assert_that(p16 =="b", 
                  msg="Incorrect. Try again.")
      
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
