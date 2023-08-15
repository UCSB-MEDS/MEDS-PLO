
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 4 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q4_prog_lang <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Never", "Less than once per year", "Several times per year", "Monthly", "Weekly", "Daily")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
  # select necessary cols ----
  select(program_lang) |> 
    
  # sum ----
  group_by(program_lang) |> 
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
      new_row <- data.frame(program_lang = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
  # reorder factors ----
  mutate(program_lang = fct_relevel(program_lang,
                                    c("Never", "Less than once per year", "Several times per year",
                                      "Monthly", "Weekly", "Daily"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |>
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = program_lang)
  
  # return final wrangled df
  return(df2)
  
}

