
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- CLEAN QUESTION 3 DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# clean_q3_gui_bothPP <- function(PLO_data_clean){
#   
#   #........................to iterate over.........................
#   options <- c("Never", "Less than once per year", "Several times per year", "Monthly", "Weekly", "Daily")
#   
#   #........................initial wrangling.......................
#   df <- PLO_data_clean |> 
#     
#     # select necessary cols ----
#     select(point_and_click_gui, timepoint) |> 
#     
#     # sum ----
#     group_by(timepoint, point_and_click_gui) |> 
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
#     # if category already exists in df, skip to next one ----
#     if (cat_name %in% pull(pre_meds[,2])) {
#       
#       message(cat_name, " already exists. Moving to next option.")
#       pre_meds <- pre_meds
#       
#       # if category doesn't already exist, add it with n = 0 so that it still shows up on plot ----
#     } else {
#       
#       message(cat_name, " does not exist. Adding now.")
#       new_row <- data.frame(timepoint = "Pre-MEDS", point_and_click_gui = cat_name, n = 0)
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
#     mutate(point_and_click_gui = fct_relevel(point_and_click_gui,
#                                            c("Never", 
#                                              "Less than once per year", 
#                                              "Several times per year",
#                                              "Monthly", "Weekly", "Daily"))) |>
#     mutate(total_respondents = sum(n),
#            percentage = round((n/total_respondents)*100, 1),
#            perc_label = paste0(percentage, "%")) |>
#     mutate(xvar = point_and_click_gui)
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
#       new_row <- data.frame(timepoint = "Post-MEDS", point_and_click_gui = cat_name, n = 0)
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
#     mutate(point_and_click_gui = fct_relevel(point_and_click_gui,
#                                              c("Never", 
#                                                "Less than once per year", 
#                                                "Several times per year",
#                                                "Monthly", "Weekly", "Daily"))) |>
#     mutate(total_respondents = sum(n),
#            percentage = round((n/total_respondents)*100, 1),
#            perc_label = paste0(percentage, "%")) |>
#     mutate(xvar = point_and_click_gui)
#   
#   ##~~~~~~~~~~~~~~~~~~~~~~~
#   ##  ~ recombine dfs  ----
#   ##~~~~~~~~~~~~~~~~~~~~~~~
#   
#   all_q3_data <- rbind(pre_meds, post_meds)
#   
#   return(all_q3_data)
#   
# }
