###############################################
# Epidemiologic Methods II 
# PHW250F, PHW250G, PH250B

# Autograder: Iteration
###############################################
library(here)
library(jsonlite)
library(rlist)
library(checkr)         # CheckR is generally more useful for open-ended, broader checks
library(assertthat)     # Assertthat is generally more useful for more specific, assertion-based checks
library(dplyr)
library(reticulate)

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
  
  calculate_inc_rare_correct=function(prev,d){
    id = prev / d
    return(id)
  }
  
  prevalence_tr_correct = d_tr %>% 
    group_by(tr) %>%
    summarise(prevalence=mean(diar7d)) %>%
    mutate(incidence = calculate_inc_rare_correct(prev = prevalence, d = 5))
  
  eval <- tryCatch(
    {
      assert_that(nrow(prevalence_tr) ==7, 
                  msg="Did you correctly group by the treatment variable? The number of rows in your result is incorrect.")
      assert_that(setequal(prevalence_tr$tr, prevalence_tr_correct$tr),
                  msg="Did you correctly group by the treatment variable? There is an error in the column with the treatment label.")
      assert_that(setequal(prevalence_tr$prevalence, prevalence_tr_correct$prevalence),
                  msg="Did you correctly group by the treatment variable and then take the mean of diar7d in each category? There is an error in the 'prevalence' column.")
    }
  ) 
  
  if(eval==TRUE) {
    print("Correct!!")
  }
  
  scores[problemNumber] <<- eval
  ReturnScore(problemNumber, eval)
}

# --------------------------------------------
CheckProblem2 = function() {
  problemNumber = 2
  
  calculate_inc_rare_correct=function(prev,d){
    id = prev / d
    return(id)
  }
  
  
  eval <- tryCatch(
    {
      assert_that(exists("calculate_inc_rare"),
                  msg="Did you create a function called calculate_inc_rare?")
      assert_that(is.function(calculate_inc_rare),
                  msg="Are you sure you defined calculate_inc_rare as a function?")
      assert_that(setequal(list(args(calculate_inc_rare)), list(args(calculate_inc_rare_correct))),
                  msg="Did you enter the correct arguments to the function (id and d)?")
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
  
  prevalence_tr_correct = d_tr %>% 
    group_by(tr) %>%
    summarise(prevalence=mean(diar7d)) %>%
    mutate(incidence = calculate_inc_rare(prev = prevalence, d = 5))
  
  eval <- tryCatch(
    {
      assert_that(nrow(prevalence_tr) ==7, 
                  msg="Did you correctly group by the treatment variable? The number of rows in your result is incorrect.")
      assert_that(setequal(prevalence_tr$tr, prevalence_tr_correct$tr),
                  msg="Did you correctly group by the treatment variable? There is an error in the column with the treatment label.")
      assert_that(setequal(prevalence_tr$incidence, prevalence_tr_correct$incidence),
                  msg="There is an error in the incidence column. Check the formula in the function you used to calculate incidence.")

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
  
  control_inc_correct = prevalence_tr %>% 
    filter(tr=="Control") %>%
    select(incidence)
  control_inc_correct = as.vector(control_inc_correct$incidence)
  
  # calculate the incidence difference in each arm 
  prevalence_tr_correct = d_tr %>% 
    group_by(tr) %>%
    summarise(prevalence=mean(diar7d)) %>%
    mutate(incidence = calculate_inc_rare(prev = prevalence, d = 5))
  
  prevalence_tr_correct = prevalence_tr_correct %>%
    mutate(inc_diff = incidence - control_inc_correct)

  
  eval <- tryCatch(
    {
      assert_that(nrow(prevalence_tr) ==7, 
                  msg="Did you correctly group by the treatment variable? The number of rows in your result is incorrect.")
      assert_that(setequal(prevalence_tr$tr, prevalence_tr_correct$tr),
                  msg="Did you correctly group by the treatment variable? There is an error in the column with the treatment label.")
      assert_that(setequal(prevalence_tr$inc_diff, prevalence_tr_correct$inc_diff),
                  msg="There is an error in the inc_diff column. Check the formula in the function you used to calculate the incidence difference.")
      
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
  
  inc_diff_loop_correct = vector(length=6)
  for(i in 1:length(inc_diff_loop_correct)){
    inc_diff_loop_correct[i] = prevalence_tr$incidence[i+1] - control_inc
  }
  
  eval <- tryCatch(
    {
      assert_that(is.vector(inc_diff_loop),
                  msg="You did not define inc_diff_loop as a vector. Try again.")
      assert_that(length(inc_diff_loop)!=7,
                  msg="The length of inc_diff_loop is incorrect. Did you accidentally include the control arm?")
      assert_that(length(inc_diff_loop)==6,
                  msg="The length of inc_diff_loop is incorrect. Try again.")
      assert_that(setequal(inc_diff_loop, inc_diff_loop_correct),
                  msg="There is an error in the way you calculated the inc_diff_loop vector.")
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
  
  symptoms_df_correct = d_tr %>%
    select(d3plus2d:tooth7d)
  
  symptoms_correct = vector(length = ncol(symptoms_df_correct))
  for(i in 1:length(symptoms)){
    symptoms_correct[i] = mean(symptoms_df_correct[,i], na.rm=TRUE)
  }
  
  eval <- tryCatch(
    {
      assert_that(is.vector(symptoms),
                  msg="You did not define symptoms as a vector. Try again.")
      assert_that(setequal(length(symptoms), length(symptoms_correct)),
                  msg="The length of symptoms is incorrect. Try again.")
      assert_that(setequal(symptoms, symptoms_correct),
                  msg="There is an error in the way you calculated the symptoms vector.")
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
      assert_that(setequal(p7, "d3plus7d"),
                  msg="Try again.")
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
