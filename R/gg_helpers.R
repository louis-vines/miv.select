#' @export
remove_x_axis <- function(){
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank())
}

#' @export
gg_value_labels <- function(dframe, input_var_name, format_fn=identity, ...){
  input_var <- as.symbol(input_var_name)
  input_values <- dframe[[input_var_name]]
  value_range <- range(input_values) %>% {.[2] - .[1]}

  outside_lim <- value_range / 8
  value_nudge <- value_range / 30

  label_df <- dframe %>%
    mutate(neg_inside = (!!input_var) %>% replace(. > - outside_lim, NA)) %>%
    mutate(neg_outside = (!!input_var) %>% replace(. <= - outside_lim | . >= 0, NA)) %>%
    mutate(pos_outside = (!!input_var) %>% replace(. < 0 | . >= outside_lim, NA)) %>%
    mutate(pos_inside = (!!input_var) %>% replace(.  < outside_lim, NA)) %>%
    mutate(!!input_var_name := format_fn(!!input_var))

  numeric_label <- function(col_name, nudge_y, ...){
    args <- list(mapping = aes_string(y = col_name, label = input_var_name),
                 data = label_df,
                 nudge_y = nudge_y,
                 na.rm = TRUE,
                 ...)

    do.call(geom_text, args)
  }

  list(
    numeric_label("neg_inside", value_nudge, colour = "white", ...),
    numeric_label("neg_outside", -value_nudge, ...),
    numeric_label("pos_outside", value_nudge, ...),
    numeric_label("pos_inside", -value_nudge, colour = "white", ...)
  )
}
