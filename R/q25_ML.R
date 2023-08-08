##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 25a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q25a_familiar_ml <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
  # select necessary cols ----
  select(sup_vs_unsup_learn) |>
    
  # sum ----
  group_by(sup_vs_unsup_learn) |>
    count() |>
    ungroup() |>
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(sup_vs_unsup_learn = "1 (never heard of either of these terms)", n = 0) |>
    
  # reorder factors ----
  mutate(sup_vs_unsup_learn = fct_relevel(sup_vs_unsup_learn,
                                          c("1 (never heard of either of these terms)", "2",
                                            "3 (vague sense of these terms, but not why they are distinct from one another)", "4", "5 (very familiar with both concepts and how they differ)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 25a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q25a_familiar_ml <- function(data){
  
  ggplot(data, aes(x = sup_vs_unsup_learn, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the difference between supervised and\nunsupervised learning?",
         caption = "Question 25a (choosing '1 (never heard of it)' skips respondent to question 26)") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 25b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q25b_unsup_alg <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
  # select necessary cols ----
  select(implemented_algo) |>
    
  # sum ----
  group_by(implemented_algo) |>
    count() |>
    ungroup() |>
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(implemented_algo = "1 (definitely not)", n = 0) |>
    
  # reorder factors ----
  mutate(implemented_algo = fct_relevel(implemented_algo,
                                        c("1 (definitely not)", "2",
                                          "3 (maybe, but I'm not sure)", "4", "5 (yes)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 25b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q25b_unsup_alg <- function(data){
  
  ggplot(data, aes(x = implemented_algo, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Have you ever implemented an unsupervised learning algorithm?",
         caption = "Question 25b (choosing '1 (definitely not)' skips respondent to question 26)") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 25c data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q25c_kmeans <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
  # select necessary cols ----
  select(kmeans) |>
    
  # sum ----
  group_by(kmeans) |>
    count() |>
    ungroup() |>
    
  # coerce to factor ----
  mutate(kmeans = as_factor(kmeans)) |> 
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 25c data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q25c_kmeans <- function(data){
  
  ggplot(data, aes(x = fct_reorder(kmeans, desc(n)), y = n, label = perc_label, fill = kmeans)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "K-means clustering is an example of a(n) ___ learning approach\nbecause it ___ (fill in the blanks).",
         caption = "Question 25c") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme
  
}
