library(here)
library(jsonlite)
library(rlist)
library(checkr)         # CheckR is generally more useful for open-ended, broader checks
library(assertthat)     # Assertthat is generally more useful for more specific, assertion-based checks
library(dplyr)

AutograderSetUp = function(num_questions) {
  scores <<- vector(mode="character", length=num_questions)   # Put total number of questions here
}

ReturnScore = function(problemNumber, num_tests, num_failed) {
  num_passed = num_tests - num_failed
  cat(sprintf("Problem %d\nTests Passed: %d\nTests Failed: %d\n%g%% passed\n", 
              problemNumber, num_passed, num_failed, round(num_passed/num_tests * 100, digits = 2)))
}

TestCase = function(test, error_message){
  if (test) {
    tests_failed <<- tests_failed - 1
  } else {
    ReturnScore(problemNumber, num_tests, tests_failed)
    assert_that(test, msg =  error_message)
  }
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
    if (score == 1) {
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
