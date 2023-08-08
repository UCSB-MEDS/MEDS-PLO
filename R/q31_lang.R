##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 31 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q31_lang <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(what_lang_is_this) |> 
    
  # sum ----
  group_by(what_lang_is_this) |>
    count() |>
    ungroup() |> 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(what_lang_is_this = "SQL", n = 0) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 31 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q31_lang <- function(data){
  
  ggplot(data, aes(x = what_lang_is_this, y = n, label = perc_label, fill = what_lang_is_this)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Language",
         title = "What programming language is the above code written in?",
         caption = "Question 31") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme
  
}

