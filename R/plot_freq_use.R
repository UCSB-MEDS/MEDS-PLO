##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_frequency_use <- function(data, survey, title, caption){
  
  if (survey == "Pre") {
    
    color <- "#047C91"
    
  } else if (survey == "Post") {
    
    color <- "#003660"
    
  }
  
  ggplot(data, aes(x = xvar, y = n, label = perc_label)) +
    geom_col(fill = color) +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Frequency of use",
         title = title,
         caption = caption) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_frequency_use_bothPP <- function(data, title, caption){
  
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
    labs(y = "Number of MEDS students", x = "Frequency of use",
         title = title,
         caption = caption) +
    facet_wrap(~timepoint) +
    meds_theme() +
    theme(
      legend.position = "blank",
      axis.title.y = element_blank()
    )
  
}
