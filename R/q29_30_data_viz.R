

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- PLOT QUESTION 29 DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q29_create_viz <- function(data){

  ggplot(data, aes(x = data_viz_programming, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Comfort level",
         title = "How comfortable are you with creating a data visualization using a\nprogramming language?",
         caption = "Question 29") +
    meds_theme()
    
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- CLEAN QUESTION 30 DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
##                                                                            --
##--------------------------- PLOT QUESTION 30 DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q30_improve_dv <- function(data){
  
  wordcloud(data$word, freq = data$n,
            min.freq = 1, max.words = 85, 
            random.order = FALSE, rot.per = 0.35,
            scale = c(4, .5), colors = brewer.pal(8, "Dark2")) 
  
}
