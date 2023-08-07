##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 18a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18a_familiar_prob_dist <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(prob_dist) |> 
    
    # sum ----
  group_by(prob_dist) |>
    count() |>
    ungroup() |> 
    
    # ONLY ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(prob_dist = "2", n = 0) |>
    add_row(prob_dist = "1 (never heard of it)", n = 0) |>
    
    # reorder factors ----
  mutate(prob_dist = fct_relevel(prob_dist, 
                                 c("1 (never heard of it)", "2", 
                                   "3 (vague sense of what it means)", "4", "5 (very familiar)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 18a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q18a_familiar_prob_dist <- function(data){
  
  ggplot(data, aes(x = prob_dist, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the term probability distribution?",
         caption = "Question 18a (choosing '1 (never heard of it)' skips respondent to question 18)") +
    meds_theme
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 18b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18b_prob_dist_terms <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(prob_dist_terms) |> 
    
    # split strings by `,` delim ----
  separate_longer_delim(prob_dist_terms, delim = ",") |> 
    
    # sum ----
  group_by(prob_dist_terms) |>
    count() |>
    ungroup() |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/q18b_num_answers)*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 18b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q18b_prob_dist_terms <- function(data){
  
  ggplot(data, aes(x = fct_reorder(prob_dist_terms, desc(n)), y = n, label = perc_label, fill = prob_dist_terms)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Which of the following terms are used to describe probability\ndistributions (select all that apply)?",
         caption = "Question 18b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      clean Question 18b FULLY CORRECT                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18b_FULLY_CORRECT <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(prob_dist_terms) |> 
    
    # sum ----
  group_by(prob_dist_terms) |>
    count() |>
    ungroup() |> 
    
    # add correct or incorrect label ----
  mutate(correctness = case_when(
    prob_dist_terms == "normal" ~ "no",
    prob_dist_terms == "normal,bimodal,symmetric" ~ "no",
    prob_dist_terms == "normal,uniform" ~ "no",
    prob_dist_terms == "normal,uniform,bimodal" ~ "no",
    prob_dist_terms == "normal,uniform,bimodal,symmetric" ~ "yes",
    prob_dist_terms == "normal,uniform,bimodal,variable,symmetric" ~ "no"
  )) |> 
    
    # coerce data types ----
  mutate(correctness = as_factor(correctness)) |> 
    
    # sum ----
  group_by(correctness) |> 
    summarize(total = sum(n)) |> 
    ungroup() |> 
    
    # add col for percentages ----
  mutate(percentage = round((total/(sum(total)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      plot Question 18b FULLY CORRECT                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q18b_FULLY_CORRECT <- function(data){
  
  ggplot(data, aes(x = fct_reorder(correctness, desc(total)), y = total, label = perc_label, fill = correctness)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Did they get the question fully correct?",
         caption = "Question 18b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme
  
}
