#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Autograder: Epidemiologic data analysis
# homework #3: analysis 
###############################################
library(here)
library(jsonlite)
library(rlist)
library(checkr)         # CheckR is generally more useful for open-ended, broader checks
library(assertthat)     # Assertthat is generally more useful for more specific, assertion-based checks
library(dplyr)

AutograderInit = function() {
  scores <<- vector(mode="character", length=17)   # Put total number of questions here
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
  
 pkg = (.packages())
 
  eval <- tryCatch(
    {
      assert_that(p1 == "a",
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
CheckProblem2 = function() {
  problemNumber = 2

  eval <- tryCatch(
    {
      assert_that(p2 == "c",
                  msg="Incorrect. Try again.")
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
  
  eval <- tryCatch(
    {
      assert_that(setequal(class(ttest_laz), "htest"),
                  msg="Is ttest_laz the result of a t-test?")
      assert_that(setequal(round(ttest_laz[1]$statistic, 3), -6.328 ),
                  msg="Did you correctly set up the t-test?")
      assert_that(setequal(round(ttest_laz_stat,3), -6.328 ),
                  msg="Did you save the t statistic in ttest_laz_stat?")
      assert_that(setequal(round(ttest_laz_pvalue,3), 0 ),
                  msg="Did you save the p-value in ttest_laz_stat?")
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
  
  eval <- tryCatch(
    {
      assert_that(p4=="a",
                  msg="Incorrect. Try again.")
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
      assert_that(p5=="g",
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
CheckProblem6 = function() {
  problemNumber = 6
  
  eval <- tryCatch(
    {
      assert_that(setequal(class(laz_unadj_model)[1], "glm"),
                  msg="Is laz_unadj_model the output of a glm model?")
      assert_that(setequal(round(laz_unadj_model$coefficients[1], 3), -1.875),
                  msg="Does your model have the correct exposure, outcome, and data?")
      assert_that(setequal(round(laz_unadj_model$coefficients[2], 3), 0.501),
                  msg="Does your model have the correct exposure, outcome, and data?")
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
  
  names_imp = c("Does not have improved latrine", "Has improved latrine")
  values_imp = c(857, 204)

  eval <- tryCatch(
    {
      assert_that(round(laz_unadj_moa, 3) != -1.875,
                  msg="Did you save the correct coefficient?")
      assert_that(setequal(round(laz_unadj_moa, 3), 0.501),
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
CheckProblem8 = function() {
  problemNumber = 8
  
  eval <- tryCatch(
    {
      assert_that(setequal(round(laz_unadj_moa_ci[1], 3), 0.348),
                  msg="Incorrect. Try again.")
      assert_that(setequal(round(laz_unadj_moa_ci[2], 3), 0.655),
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
  
  eval <- tryCatch(
    {
      assert_that(setequal(class(laz_adj_model)[1], "glm"),
                  msg="Is laz_adj_model the output of a glm model?")
      assert_that(setequal(round(laz_adj_model$coefficients[1], 3), -12.524),
                  msg="Does your model have the correct exposure, outcome, and data?")
      assert_that(setequal(round(laz_adj_model$coefficients[2], 3), 0.349),
                  msg="Does your model have the correct exposure, outcome, and data?")
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
      assert_that(round(laz_adj_model$coefficients[1], 3) != -1.252,
                  msg="Did you save the correct coefficient?")
      assert_that(setequal(round(laz_adj_moa, 3), 0.349),
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

  eval <- tryCatch(
    {
      assert_that(setequal(round(laz_adj_moa_ci[1], 3), 0.198),
                  msg="Incorrect. Try again.")
      assert_that(setequal(round(laz_adj_moa_ci[2], 3), 0.500),
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
CheckProblem12 = function() {
  problemNumber = 12
  
  eval <- tryCatch(
    {
      assert_that(setequal(class(laz_unadj_int_model)[1], "glm"),
                  msg="Is laz_unadj_int_model the output of a glm model?")
      assert_that(setequal(round(laz_unadj_int_model$coefficients[1], 3),  -1.898),
                  msg="Does your model have the correct exposure, outcome, and data?")
      assert_that(setequal(round(laz_unadj_int_model$coefficients[2], 3), 0.382),
                  msg="Does your model have the correct exposure, outcome, and data?")
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
  
  eval <- tryCatch(
    {
      assert_that(setequal(round(laz_interaction, 3), -0.035),
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
CheckProblem14 = function() {
  problemNumber = 14
  
  eval <- tryCatch(
    {
      assert_that(p14 == "f",
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
CheckProblem15 = function() {
  problemNumber = 15
  
  eval <- tryCatch(
    {
      assert_that(setequal(class(stunt_unadj_model)[1], "glm"),
                  msg="Is stunt_unadj_model the output of a glm model?")
      assert_that(setequal(round(stunt_unadj_model$coefficients[1], 3),  -0.802),
                  msg="Does your model have the correct exposure, outcome, and data?")
      assert_that(setequal(round(stunt_unadj_model$coefficients[2], 3), -0.620),
                  msg="Does your model have the correct exposure, outcome, and data?")
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
      assert_that(setequal(round(stunt_unadj_moa, 3), 0.538),
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
CheckProblem17 = function() {
  problemNumber = 17
  
  eval <- tryCatch(
    {
      assert_that(setequal(round(stunt_unadj_moa_ci[1], 3), 0.393),
                  msg="Incorrect. Try again.")
      assert_that(setequal(round(stunt_unadj_moa_ci[2], 3), 0.718),
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
