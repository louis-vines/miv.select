#' @importFrom stringr str_replace
cut_vector <- function(x, breaks, closed_on_right = TRUE){
  breaks <- unique(c(-Inf, breaks, Inf))
  x_cut <- cut(x, breaks, right = closed_on_right)
  cut_levels <- levels(x_cut) %>% str_replace("(-| )Inf", "")

  x_cut %>%
    str_replace("(-| )Inf", "") %>%
    factor(levels = cut_levels)
}
