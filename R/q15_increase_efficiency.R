
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- CLEAN QUESTION 15 DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# clean_q15_increase_efficiency_bothPP <- function(PLO_data_clean){
#   
#   #........................to iterate over.........................
#   options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
#   
#   #........................initial wrangling.......................
#   df <- PLO_data_clean |> 
#     
#     # select necessary cols ----
#   select(increase_efficiency, timepoint) |> 
#     
#     # sum ----
#   group_by(timepoint, increase_efficiency) |> 
#     count() |> 
#     ungroup() 
#   
#   ##~~~~~~~~~~~~~~~~~~
#   ##  ~ pre-MEDS  ----
#   ##~~~~~~~~~~~~~~~~~~
#   
#   #.........separate pre-MEDS (to add 0s for missing cats).........
#   pre_meds <- df |> 
#     filter(timepoint == "Pre-MEDS") 
#   
#   #................add 0s where missing (pre-MEDS).................
#   for (i in 1:length(options)){
#     
#     cat_name <- options[i]
#     
#     # if category already exists in df, skip to next one
#     if (cat_name %in% pull(pre_meds[,2])) {
#       
#       message(cat_name, " already exists. Moving to next option.")
#       pre_meds <- pre_meds
#       
#       # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
#     } else {
#       
#       message(cat_name, " does not exist. Adding now.")
#       new_row <- data.frame(timepoint = "Pre-MEDS", increase_efficiency = cat_name, n = 0)
#       pre_meds <- rbind(pre_meds, new_row)
#       
#     }
#     
#     message("----------------------")
#     
#   } 
#   
#   #.................calculate pre-MEDS percentages.................
#   pre_meds <- pre_meds |> 
#     mutate(total_respondents = sum(n),
#            percentage = round((n/total_respondents)*100, 1),
#            perc_label = paste0(percentage, "%")) |>
#     mutate(xvar = increase_efficiency)
#   
#   ##~~~~~~~~~~~~~~~~~~~
#   ##  ~ post-MEDS  ----
#   ##~~~~~~~~~~~~~~~~~~~
#   
#   #........separate post-MEDS (to add 0s for missing cats).........
#   post_meds <- df |> 
#     filter(timepoint == "Post-MEDS") 
#   
#   #................add 0s where missing (post-MEDS)................
#   for (i in 1:length(options)){
#     
#     cat_name <- options[i]
#     
#     # if category already exists in df, skip to next one ----
#     if (cat_name %in% pull(post_meds[,2])) {
#       
#       message(cat_name, " already exists. Moving to next option.")
#       post_meds <- post_meds
#       
#       # if category doesn't already exist, add it with n = 0 so that it still shows up on plot ----
#     } else {
#       
#       message(cat_name, " does not exist. Adding now.")
#       new_row <- data.frame(timepoint = "Post-MEDS", increase_efficiency = cat_name, n = 0)
#       post_meds <- rbind(post_meds, new_row)
#       
#     }
#     
#     message("----------------------")
#     
#   } 
#   
#   #................calculate post-MEDS percentages.................
#   post_meds <- post_meds |> 
#     mutate(total_respondents = sum(n),
#            percentage = round((n/total_respondents)*100, 1),
#            perc_label = paste0(percentage, "%")) |>
#     mutate(xvar = increase_efficiency)
#   
#   ##~~~~~~~~~~~~~~~~~~~~~~~
#   ##  ~ recombine dfs  ----
#   ##~~~~~~~~~~~~~~~~~~~~~~~
#   
#   all_q14_increase_efficiency <- rbind(pre_meds, post_meds) |> 
#     mutate(increase_efficiency = fct_relevel(increase_efficiency,
#                                          c("1 (strongly disagree)", "2", 
#                                            "3 (neutral)", "4", "5 (strongly agree)"))) 
#   
#   return(all_q14_increase_efficiency)
#   
# }

  