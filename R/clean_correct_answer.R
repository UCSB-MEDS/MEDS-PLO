# NOT WORKING YET

clean_correct_answer <- function(all_PLO_data, col_name, categories){
  
  # ........................to iterate over.........................
  options <- categories
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    select(!!sym(col_name), timepoint) |> 
    group_by(timepoint, !!sym(col_name)) |>
    count() |>
    ungroup()
  
  #................separate pre- and post-MEDS data................
  pre_meds <- df |>
    filter(timepoint == "Pre-MEDS")
  post_meds <- df |>
    filter(timepoint == "Post-MEDS")
  
  
  #................add 0s where missing (pre-MEDS).................
  for (i in 1:length(options)){
    cat_name <- options[i]
    
    # ---- Check for this category in the Pre-Meds df ----
    # if category already exists in df, skip to next one ----
    if (cat_name %in% pull(pre_meds[,2])) {
      message(cat_name, " already exists. Moving to next option.")
      pre_meds <- pre_meds
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(timepoint = "Pre-MEDS", !!col_name := cat_name, n = 0)
      pre_meds <- rbind(pre_meds, new_row)
    }
    
    message("----------------------")
    
  } 
    # ---- Check for this category in the Post-Meds df ----
    # if category already exists in df, skip to next one ----
    if (cat_name %in% pull(post_meds[,2])) {
      message(cat_name, " already exists in Post-Meds. Moving to next option.")
      post_meds <- post_meds
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot ----  
    } else {
      message(cat_name, " does not exist in Post-Meds. Adding now.")
      new_row <- tibble(timepoint = "Post-MEDS", !!col_name := cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
    }
  
    message("----------------------")
  
    #.................calculate pre-MEDS percentages.................
    pre_meds <- pre_meds |>
      mutate(total_respondents = sum(n),
             percentage = round((n/total_respondents)*100, 1),
             perc_label = paste0(percentage, "%")) |>
      mutate(xvar = !!sym(col_name))
    
    #................calculate post-MEDS percentages.................
    post_meds <- post_meds |>
      mutate(total_respondents = sum(n),
             percentage = round((n/total_respondents)*100, 1),
             perc_label = paste0(percentage, "%")) |>
      mutate(xvar = !!sym(col_name))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_answers <- rbind(pre_meds, post_meds) #|> 
    
    # filter only for correct answer ---- MOVE THIS TO SCRIPT
    #filter(what_lang_is_this == "Python") 

  return(all_answers)
  
}
