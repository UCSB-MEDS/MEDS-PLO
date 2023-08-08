
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 4 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q4_prog_lang <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(program_lang) |>
    
  # sum ----
  group_by(program_lang) |>
    count() |>
    ungroup() |> 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(program_lang = "Monthly", n = 0) |> 
    add_row(program_lang = "Several times per year", n = 0) |> 
    add_row(program_lang = "Less than once per year", n = 0) |> 
    add_row(program_lang = "Never", n = 0) |>
    
  # reorder factors ----
  mutate(program_lang = fct_relevel(program_lang, c("Never", "Less than once per year", "Several times per year",
                                                    "Monthly", "Weekly", "Daily"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
  # create col with xvar name for plotting consistency ----
  mutate(xvar = program_lang)
  
}