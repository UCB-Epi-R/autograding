#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Autograder: Epidemiologic data analysis
# homework #2: analysis preparation
###############################################
library(here)
library(jsonlite)
library(rlist)
library(checkr)         # CheckR is generally more useful for open-ended, broader checks
library(assertthat)     # Assertthat is generally more useful for more specific, assertion-based checks
library(dplyr)

AutograderInit = function() {
  scores <<- vector(mode="character", length=12)   # Put total number of questions here
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
      assert_that("dplyr" %in% pkg, 
                  msg="You are missing one of the required packages.")
      assert_that("ggplot2" %in% pkg, 
                  msg="You are missing one of the required packages.")
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
  
  treatment_cols = c("block", "clusterid", "tr")
  enroll_cols = c("block", "asset_tv", "momedu")
  anthro_cols = c("lazminus2", "tchild", "waz")
  
  eval <- tryCatch(
    {
      assert_that(exists("treatment"),
                  msg="Did you correctly load the treatment dataset?")
      assert_that(exists("enroll"),
                  msg="Did you correctly load the enroll dataset?")
      assert_that(exists("anthro"),
                  msg="Did you correctly load the anthro dataset?")
      assert_that(setequal(colnames(treatment),treatment_cols),
                  msg="Did you name the correct dataset treatment? The column names are incorrect.")
      assert_that(sum(enroll_cols %in% colnames(enroll))==3,
                  msg="Did you name the correct dataset treatment? The column names are incorrect.")
      assert_that(sum(anthro_cols %in% colnames(anthro))==3,
                  msg="Did you name the correct dataset treatment? The column names are incorrect.")

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
  
  enroll_tr_cols = c("block", "tr", "hfiacat")
  
  eval <- tryCatch(
    {
      assert_that(exists("enroll_tr"),
                  msg="Did you create a merged dataset called enroll_tr?")
      assert_that(sum(enroll_tr_cols %in% colnames(enroll_tr))==3,
                  msg="Did you name the correct dataset enroll_tr? The column names are incorrect.")
      
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

  all_cols = c("block", "tr", "hfiacat", "laz")
  
  eval <- tryCatch(
    {
      assert_that(exists("all"),
                  msg="Did you create a merged dataset called all?")
      assert_that(sum(all_cols %in% colnames(all))==4,
                  msg="Did you name the correct dataset all? The column names are incorrect.")
      
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

  eval <- tryCatch(
    {
      assert_that(setequal(names(table(all$tr)), "Control"),
                  msg="Did you correctly subset the data? The tr column contains arms other than the control arm.")
      assert_that(setequal(nrow(all), 2252),
                  msg="Did you correctly subset the data? The number of rows is incorrect.")
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
      assert_that(setequal(names(table(all$svy)), "2"),
                  msg="Did you correctly subset the data? The tr column contains arms other than the control arm.")
      assert_that(setequal(nrow(all), 1122),
                  msg="Did you correctly subset the data? The number of rows is incorrect.")
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
  
  all = readRDS("setup/autograder-setup/hw8b_epi_analysis/all_correct.RDS")

  eval <- tryCatch(
    {
      assert_that("implatrine" %in% colnames(all), 
                  msg="Did you add a column called implatrine to the dataset called all?")
      assert_that(setequal(names(table(all$implatrine_correct)), names(table(all$implatrine))), 
                  msg="Did you correctly label the values of implatrine?")
      assert_that(is.factor(all$implatrine), 
                  msg="Did you define implatrine as a factor?")
      assert_that(setequal(as.data.frame(table(all$implatrine))$Freq, as.data.frame(table(all$implatrine_correct))$Freq),
                  msg="Did you correctly assign values to the variable implatrine?")
      
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
      assert_that(!"TRUE" %in% names(table(is.na(all$laz))),
                  msg="Did you drop missing values from the laz column of the all data frame?")
      assert_that(!"TRUE" %in% names(table(is.na(all$implatrine))),
                  msg="Did you drop missing values from the implatrine column of the all data frame?")
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
      assert_that("ggplot" %in% class(laz_histogram),
                  msg="laz_histogram is not a ggplot.")
      assert_that(sum(c("GeomBar", "GeomRect") %in% class(laz_histogram$layers[[1]]$geom))==2, 
                  msg="Did you define a histogram in ggplot?")
      assert_that(laz_histogram$labels$x=="Length-for-age Z-score",
                  msg="Did you add the correct x-axis label?")
      assert_that(laz_histogram$layers[[1]]$aes_params$colour=="black", 
                  msg="Did you set the bar outline to be black?")
      assert_that(laz_histogram$layers[[1]]$aes_params$fill=="gray", 
                  msg="Did you set the bar full to be gray?")
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
      assert_that("ggplot" %in% class(laz_lat_boxplot),
                  msg="laz_lat_boxplot is not a ggplot.")
      assert_that("GeomBoxplot" %in% class(laz_lat_boxplot$layers[[1]]$geom), 
                  msg="Did you define a histogram in ggplot?")
      assert_that(laz_lat_boxplot$labels$x=="Type of latrine",
                  msg="Did you add the correct x-axis label?")
      assert_that(laz_lat_boxplot$labels$y=="Length-for-age Z-score",
                  msg="Did you add the correct x-axis label?")
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
      assert_that(round(diff_mean_laz_implat, 3)==0.504,
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
      assert_that(round(diff_mean_laz_implat_refrig0, 3)==0.385,
                  msg="Incorrect. Try again.")
      assert_that(round(diff_mean_laz_implat_refrig1, 3)==0.315,
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
