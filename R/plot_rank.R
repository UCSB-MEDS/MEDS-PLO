##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ clean part 4 (rank agreement 1-5)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# =================================
# NOT WORKING ---------------------
# =================================
# clean_rank_agreement <- function(data, colname){
#   
#   # to iterate over ----
#   options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
#   
#   # select var of interest ----
#   df1 <- data |> 
#     
#     # select necessary cols ----
#   select({{ colname }}) |> 
#     
#     # sum ----
#   group_by({{ colname }}) |> 
#     count() |> 
#     ungroup() 
#   
#   for (i in 1:length(options)){
#     
#     cat_name <- options[i]
#     
#     # if category already exists in df, skip to next one
#     if (cat_name %in% pull(df1[,1])) {
#       
#       message(cat_name, " already exists. Moving to next option.")
#       df1 <- df1
#       
#       # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
#     } else {
#       
#       message(cat_name, " does not exist. Adding now.")
#       new_row <- data.frame({{ colname }} := cat_name, n = 0)
#       df1 <- rbind(df1, new_row)
#       
#     }
#     
#     message("----------------------")
#     
#   } 
#   
#   # finish wrangling ----
#   df2 <- df1 |> 
#     
#     # reorder factors ----
#   mutate({{ colname }} := fct_relevel({ {colname }},
#                                       c("1 (strongly disagree)", "2", 
#                                         "3 (neutral)", "4", "5 (strongly agree)"))) |>
#     
#     # add col for percentages ----
#   mutate(percentage = round((n/(sum(n)))*100, 1),
#          perc_label = paste0(percentage, "%")) |> 
#     
#     # create col with xvar name for plotting consistency ----
#   mutate(xvar := {{ colname }})
#   
#   # return final wrangled df
#   return(df2)
#   
# }


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ plot part 4 (rank agreement 1-5)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_rank_agreement <- function(data, title, caption){
  
  ggplot(data, aes(x = xvar, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Agreement level",
         title = title,
         caption = caption) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ compare pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_rank <- function(data, title, caption){
  
  ggplot(data, aes(x = xvar, y = n, label = perc_label)) +
    geom_col(aes(fill = timepoint), position = position_dodge(preserve = "total")) +
    coord_flip() +
    geom_text( # see: https://cedricscherer.com/2023/10/26/yet-another-how-to-on-labelling-bar-graphs-in-ggplot2/
      aes(label = paste0("  ", sprintf("%2.1f", percentage), "%  "), 
          hjust = percentage > 3),
      position = position_dodge2(width = 0.9),
      size = 3, color = "white", family = "nunito"
    ) +
    scale_fill_manual(values = meds_pal) +
    labs(y = "Number of MEDS students", x = "Agreement level",
         title = title,
         caption = caption) +
    facet_wrap(~timepoint) +
    meds_theme() +
    theme(
      legend.position = "blank",
      axis.title.y = element_blank()
    )
  
}
