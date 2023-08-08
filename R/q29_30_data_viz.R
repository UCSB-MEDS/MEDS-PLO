##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 29 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q29_create_viz <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(data_viz_programming) |> 
    
  # sum ----
  group_by(data_viz_programming) |>
    count() |>
    ungroup() |> 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(data_viz_programming = "1 (never done it)", n = 0) |>
    
  # reorder factors ----
  mutate(data_viz_programming = fct_relevel(data_viz_programming, 
                                            c("1 (never done it)", "2", 
                                              "3", "4", "5 (very comfortable, do it often)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 29 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q29_create_viz <- function(data){

  ggplot(data, aes(x = data_viz_programming, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Comfort level",
         title = "How comfortable are you with creating a data visualization using a\nprogramming language?",
         caption = "Question 29") +
    meds_theme
    
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 30 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q30_improve_dv <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
  # select necessary cols ----
  select(improve_data_viz) |> 
    
  # split strings by `,` delim ----
  separate_longer_delim(improve_data_viz, " ")  |> 
    separate_longer_delim(improve_data_viz, ",") |> 
    separate_longer_delim(improve_data_viz, ".") |>  
    separate_longer_delim(improve_data_viz, ")") |>  
    separate_longer_delim(improve_data_viz, "(") |> 
    separate_longer_delim(improve_data_viz, ";") |> 
    filter(improve_data_viz != "") |> 
    filter(!improve_data_viz %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "-")) |> 
    mutate(improve_data_viz = str_to_lower(improve_data_viz)) |> 
    group_by(improve_data_viz) |> 
    count() |> 
    arrange(desc(n)) |> 
    rename(word = improve_data_viz) |> 
    anti_join(y = stop_words)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 30 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q30_improve_dv <- function(data){
  
  wordcloud(data$word, freq = data$n,
            min.freq = 1, max.words = 85, 
            random.order = FALSE, rot.per = 0.35,
            scale = c(4, .5), colors = brewer.pal(8, "Dark2")) 
  
}
