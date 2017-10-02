bin_factor <- function(dframe, x, y = "gb12", supervised = FALSE){
  feature_is_ordered <- "ordered" %in% class(dframe[[x]])

  feature_type <- if(feature_is_ordered) "ordered_factor" else "factor"

  if(supervised){
    binned_data <- supervised_factor_grouping(dframe, x, y)
    dframe <- binned_data$data
  } else {
    dframe <- dframe %>% rename(group = !!x)
  }

  iv_table <- build_iv_table(dframe, y = y)

  if(!feature_is_ordered){
    iv_table <- iv_table %>% arrange(bad_rate)
  }

  bin_levels <- as.character(iv_table$group)
  total_iv <- sum(iv_table$iv %>% .[!is.infinite(.)])

  binned_feature <- list(feature = x,
                         feature_type = feature_type,
                         levels = bin_levels,
                         node_groups = if(supervised) binned_data$node_groups else NULL,
                         tree = if(supervised) binned_data$tree else NULL,
                         iv = total_iv,
                         iv_table = iv_table)

  binned_feature <- purrr::discard(binned_feature, ~ is.null(.x))

  attr(binned_feature, "class") <- c("binned_factor", "binned")

  binned_feature
}

#' @importFrom purrr map
supervised_factor_grouping <- function(dframe, x, y){
  if(!is.factor(dframe[[x]])) dframe[[x]] <- factor(dframe[[x]])
  if(!is.factor(dframe[[y]])) dframe[[y]] <- factor(dframe[[y]])

  tree_obj <- ctree(formula(paste(y, "~", x)), data = dframe, na.action = na.pass)

  tree_len <- length(tree_obj)

  cuts <- map(seq_len(tree_len), ~ tree_obj[.x]$node$split$breaks) %>%
    discard(is.null) %>%
    flatten_dbl %>%
    sort %>%
    {levels(dframe[[x]])[.]}

  wrapr::let(list(.x. = x), {
    dframe <- dframe %>%
      mutate(node = predict(tree_obj, newdata = ., type = 'node')) %>%
      mutate(node = if_else(is.na(.x.), NA_integer_, node))

    node_groups <- dframe %>%
      filter(!is.na(node)) %>%
      group_by(node) %>%
      summarise(group = .x. %>% unique %>% sort %>% stringr::str_c(collapse = "; "))
  })

  binned_data <- dframe %>%
    left_join(node_groups, by = "node") %>%
    mutate(group = group %>% forcats::fct_explicit_na(na_level = "missing"))

  list(data = binned_data, node_groups = node_groups, tree = tree_obj)
}
