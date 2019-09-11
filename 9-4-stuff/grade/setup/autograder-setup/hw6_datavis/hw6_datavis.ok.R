#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Autograder: Data visualization 
###############################################
library(here)
library(jsonlite)
library(rlist)
library(checkr)         # CheckR is generally more useful for open-ended, broader checks
library(assertthat)     # Assertthat is generally more useful for more specific, assertion-based checks
library(dplyr)
library(ggplot2)

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
      assert_that("ggplot" %in% class(p1),
                  msg="You did not define a ggplot.")
      assert_that(length(grep("meanfcolif3d", p1$layers[[1]]$mapping))==1,
                  msg="Did you define the correct outcome variable?")
      assert_that("GeomBar" %in% class(p1$layers[[1]]$geom), 
                  msg="Did you define a histogram in ggplot?")
      assert_that(p1$labels$x=="Mean log 10 fecal coliform concentration in the past 3 days",
                  msg="Did you add the correct x-axis label?")
      assert_that(p1$labels$y=="Number of observations", 
                  msg="Did you add the correct y-axis label?")
      assert_that(p1$layers[[1]]$stat_params$bins==50, 
                  msg="Did you set the number of bins to 50?")
      if(length(p1$layers[[1]]$aes_params)==0) stop("Did you set the bar fill and outline colors?")
      
        assert_that(p1$layers[[1]]$aes_params$colour=="black",
                  msg="Did you set the color of the bar outline to black?")
      assert_that(p1$layers[[1]]$aes_params$fill=="gray",
                  msg="Did you set the color of the bar fill to gray?")
    }
    
  ) 
  if(eval==TRUE) print("Correct!")
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}


# --------------------------------------------
CheckProblem2 = function() {
  problemNumber = 2  
  eval = validate_that(p2=="a", msg="Wrong!")
  
  if(eval==TRUE) print("Correct!")
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------


CheckProblem3 = function() {
  problemNumber = 3   
  
  p3.correct = readRDS("setup/autograder-setup/hw6_datavis/p3.correct.RDS")
  
  student_y_limits=as.list(ggplot_build(p3)$plot$scales$scales[[1]])$limits
  correct_y_limits=as.list(ggplot_build(p3.correct)$plot$scales$scales[[1]])$limits
  
  student_x_limits=as.list(ggplot_build(p3)$plot$scales$scales[[2]])$limits
  correct_x_limits=as.list(ggplot_build(p3.correct)$plot$scales$scales[[2]])$limits
  
  eval <- tryCatch(
    {
      assert_that("ggplot" %in% class(p3),
                  msg="You did not define a ggplot.")
      assert_that("GeomPoint" %in% class(p3$layers[[1]]$geom), 
                  msg="Did you define a histogram in ggplot?")
      assert_that(p3$labels$x=="Precipitation in the past 3 days (inches)",
                  msg="Did you add the correct x-axis label?")
      assert_that(p3$labels$y=="Mean log 10 fecal coliform concentration in the past 3 days", 
                  msg="Did you add the correct y-axis label?")
      assert_that(p3$layers[[1]]$aes_params$alpha==0.3, 
                  msg="Did you set alpha to 0.3?")
      assert_that(setequal(student_y_limits, correct_y_limits),
                  msg="Did you set the y axis limits to 0 and 4.5?")
      assert_that(setequal(student_x_limits, correct_x_limits),
                  msg="Did you set the y axis limits to 0 and 2.7?")
      # not sure how to do this with environments 
    }
    # https://stackoverflow.com/questions/31118785/r-extract-scale-name-from-ggplot-object
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
  eval = validate_that(p4=="c", msg="Wrong!")
  
  if(eval==TRUE) {
    print("Correct!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem5 = function() {
  problemNumber = 5 
  eval = validate_that(p5=="b", msg="Wrong!")
  
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
    { # NOT DONE
      assert_that("ggplot" %in% class(p6),
                  msg="You did not define a ggplot.")
      assert_that("GeomBoxplot" %in% class(p6$layers[[1]]$geom), 
                  msg="Did you define a boxplot in ggplot?")
      assert_that(p6$labels$x=="Beach",
                  msg="Did you add the correct x-axis label?")
      assert_that(p6$labels$y=="Mean log 10 fecal coliform concentration in the past 3 days", 
                  msg= "Did you add the correct y-axis label?")
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
  # eval = validate_that(p7=="a" | q4=="a", msg="Wrong!")
  eval <- tryCatch(
    { # NOT DONE
      if(exists("q4")){
        assert_that(q4=="a",
                    msg="Wrong!")
      }
      assert_that(p7=="a",
                  msg="Wrong!")
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
