##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 24a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q24a_familiarity_rs <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
  # select necessary cols ----
  select(reflec_spec) |>
    
  # sum ----
  group_by(reflec_spec) |>
    count() |>
    ungroup() |>
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(reflec_spec = "5 (very familiar)", n = 0) |>
    
  # reorder factors ----
  mutate(reflec_spec = fct_relevel(reflec_spec,
                                   c("1 (never heard of it)", "2",
                                     "3 (vague sense of what it means)", "4", "5 (very familiar)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 24a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q24a_familiarity_rs <- function(data){
  
  ggplot(data, aes(x = reflec_spec, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the term reflectance spectra?",
         caption = "Question 24a (choosing '1 (never heard of it)' skips respondent to question 25)") +
    meds_theme
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 24b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q24b_veg_wave <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(veg_wavelength) |> 
    
  # sum ----
  group_by(veg_wavelength) |>
    count() |>
    ungroup() |>
    
  # replace NA (from respondents who selected 'No' for previous question) ----
  replace_na(list(veg_wavelength = "No response")) |> 
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 24b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q24b_veg_wave <- function(data){
  
  ggplot(data, aes(x = fct_reorder(veg_wavelength, desc(n)), y = n, label = perc_label, fill = veg_wavelength)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Of the following wavelengths, which one does vegetation reflect the\nmost?",
         caption = "Question 24b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme
  
}

