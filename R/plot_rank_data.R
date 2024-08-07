#' Plot data for Questions 9 - 15, which asks respondents to rank their level of agreement
#'
#' @param data data generated from `clean_rank_data()`
#' @param title plot title
#' @param caption plot caption
#'
#' @return
#' @export
#'
#' @examples
plot_rank_data <- function(data, title, caption){
  
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
