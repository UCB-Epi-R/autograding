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