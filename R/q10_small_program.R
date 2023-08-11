##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 10 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q10_small_program <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(small_program) |> 
    
    # sum ----
  group_by(small_program) |> 
    count() |> 
    ungroup() 
  
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
    # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(small_program = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
  # reorder factors ----
  mutate(small_program = fct_relevel(small_program,
                                     c("1 (strongly disagree)", "2", 
                                     "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
  # create col with xvar name for plotting consistency ----
  mutate(xvar = small_program)
  
  # return final wrangled df
  return(df2)
  
}

