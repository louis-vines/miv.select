bin_factor <- function(dframe, x, y = "gb12", supervised = FALSE){
  feature_is_ordered <- "ordered" %in% class(dframe[[x]])

  FEATURE_TYPE <- if(feature_is_ordered) "ordered_factor" else "factor"

  print(supervised)

  if(supervised){
    dframe <- supervised_factor_grouping(dframe, x, y)

  } else {
    dframe <- dframe %>% rename(group = !!x)
  }

  iv_table <- build_iv_table(dframe %>% rename(group = !!x), y = y)

  if(!feature_is_ordered){
    iv_table <- iv_table %>% arrange(bad_rate)
  }

  bin_levels <- as.character(iv_table$group)
  total_iv <- sum(iv_table$iv %>% .[!is.infinite(.)])

  binned_feature <- list(feature = x,
                         feature_type = FEATURE_TYPE,
                         levels = bin_levels,
                         iv = total_iv,
                         iv_table = iv_table)

  attr(binned_feature, "class") <- c("binned_factor", "binned")

  binned_feature
}

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

  let(list(.x. = x), {
    dframe <- dframe %>%
      mutate(node = predict(tree_obj, newdata = ., type = 'node')) %>%
      mutate(node = if_else(is.na(.x.), NA_integer_, node))

    node_groups <- dframe %>%
      filter(!is.na(node)) %>%
      group_by(node) %>%
      summarise(group = .x. %>% unique %>% sort %>% str_c(collapse = "; "))
  })

  dframe %>%
    left_join(node_groups, by = "node") %>%
    mutate(group = group %>% fct_explicit_na(na_level = "missing"))
}
