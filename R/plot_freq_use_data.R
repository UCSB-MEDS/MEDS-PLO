#' Plot data for Questions 3 - 7, which asks respondents to select how frequently they use a tool or workflow
#'
#' @param data data processed using `clean_freq_use_data()`
#' @param title plot title
#' @param caption plot caption (which includes Question number)
#'
#' @return
#' @export
#'
#' @examples
plot_freq_use_data <- function(data, title, caption){
  
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
