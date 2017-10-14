#' @importFrom patykit ctree_control
bin_factor <- function(dframe, x, y = "gb12", supervised = FALSE, tree_control = ctree_control()){
  feature_is_ordered <- "ordered" %in% class(dframe[[x]])

  feature_type <- if(feature_is_ordered) "ordered_factor" else "factor"

  if(supervised){
    binned_data <- supervised_factor_grouping(dframe, x, y, tree_control = tree_control)
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
#' @importFrom partykit ctree
supervised_factor_grouping <- function(dframe, x, y, tree_control){
  if(!is.factor(dframe[[x]])) dframe[[x]] <- factor(dframe[[x]])
  if(!is.factor(dframe[[y]])) dframe[[y]] <- factor(dframe[[y]])

  tree_obj <- ctree(formula(paste(y, "~", x)), data = dframe,
                    na.action = na.pass, control = tree_control)

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

predict.binned_factor <- function(binned_feature, dframe){
  binned_feature_was_supervised <- "tree" %in% names(binned_feature)

  if(!binned_feature_was_supervised) return(dframe)

  feature <- binned_feature$feature

  dframe[[feature]] <- factor(dframe[[feature]])

  dframe$node <- predict(binned_feature$tree, newdata = dframe[,"property"], type = 'node')
  dframe$node <- if_else(is.na(dframe[[feature]]), NA_integer_, dframe$node)

  dframe %<>%
    left_join(binned_feature$node_groups, by = 'node') %>%
    mutate(group = forcats::fct_explicit_na(group))

  dframe[[feature]] <- dframe$group


  dframe %>% select(-node, -group)
}

plot.binned_factor <- function(binned_feature, old_frame, y = 'gb12'){
  woe_plot <- binned_feature$iv_table %>%
    ggplot(aes(group, woe)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = 0, alpha = 0.5) +
    ggtitle('Weight of Evidence')

  bad_rate_plot <- binned_feature$iv_table %>%
    ggplot(aes(group, bad_rate)) +
    geom_bar(stat = 'identity') +
    ggtitle('Bad Rate')

  freq_plot <- binned_feature$iv_table %>%
    ggplot(aes(group, freq)) +
    geom_bar(stat = 'identity') +
    ggtitle('Frequency')

  if(missing(old_frame)){
    return(
      cowplot::plot_grid(woe_plot + remove_x_axis(),
                         bad_rate_plot + remove_x_axis(),
                         freq_plot,
                         nrow = 3)
    )
  }

  feature_name <- binned_feature$feature
  old_frame[[y]] <- factor(old_frame[[y]])

  distribution_plot <- old_frame %>%
    group_by_(y, feature_name) %>%
    summarise(freq = n()) %>%
    mutate(density = freq / sum(freq)) %>%
    ggplot(aes_string(feature_name, y = "density", fill = y)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    ggtitle('Prebinned') +
    scale_colour_discrete(labels = c('good', 'bad')) +
    #scale_fill_discrete(labels = c('good', 'bad')) +
    theme(legend.title = element_blank(), legend.position = c(0.85, 0.85))

  cowplot::plot_grid(distribution_plot ,
                     woe_plot,
                     freq_plot,
                     bad_rate_plot,
                     nrow = 2)
}