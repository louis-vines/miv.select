#' Numeric Factor Binning

#' @export

#' @importFrom partykit ctree
#' @importFrom partykit ctree_control
#' @importFrom purrr map
#' @importFrom purrr discard
#' @importFrom purrr flatten_dbl
bin_numeric <- function(dframe, x, y,
                        tree_control = ctree_control(), bins = NULL){
  FEATURE_TYPE <- 'numeric'

  if(is.null(bins)){
    binned_data <- create_numeric_supervised_bins(dframe, x, y = y,
                                                  tree_control = tree_control)

    if(!binned_data$significant_splits_found){
      output <- list(feature = x,
                     feature_type = FEATURE_TYPE,
                     significant_splits_found = binned_data$significant_splits_found)

      return(output)
    }

  } else {
    binned_data <- create_numeric_frequency_bins(dframe, x, y, bins)
  }

  iv_table <- build_iv_table(binned_data$data, y)
  bin_levels <- levels(iv_table$group)
  total_iv <- sum(iv_table$iv %>% .[!is.infinite(.)])

  binned_feature <- list(feature = x,
                         feature_type = FEATURE_TYPE,
                         significant_splits_found = binned_data$significant_splits_found,
                         cuts = binned_data$cuts,
                         levels = bin_levels,
                         tree = binned_data$tree,
                         iv = total_iv,
                         iv_table = iv_table)

  attr(binned_feature, "class") <- c('binned_numeric', 'binned')

  binned_feature
}

#' @export

predict.binned_numeric <- function(binned_feature, dframe, largest_level_first = TRUE){
  feature <- binned_feature$feature
  feature_sym <- as.symbol(feature)
  cuts <- binned_feature$cuts

  dframe <- dframe %>%
    mutate(!!feature_sym := cut_vector(!!feature_sym, breaks = cuts)) %>%
    mutate(!!feature_sym := forcats::fct_explicit_na(!!feature_sym))

  if(!largest_level_first) return(dframe)

  largest_factor_level <- binned_feature$iv_table %>%
    filter(freq == max(freq)) %>%
    purrr::pluck("group") %>%
    first() %>%
    as.character()

    dframe %>%
      mutate(!!feature_sym := forcats::fct_relevel(!!feature_sym, largest_factor_level))
}

#' @export

plot.binned_numeric <- function(binned_feature, old_frame, y, xmin=-Inf, xmax=Inf){
  woe_plot <- binned_feature$iv_table %>%
    ggplot(aes(group, woe)) +
    geom_bar(stat = 'identity') +
    ggtitle('Weight of Evidence') +
    theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.5)) +
    xlab("")

  bad_rate_plot <- binned_feature$iv_table %>%
    ggplot(aes(group, bad_rate)) +
    geom_bar(stat = 'identity') +
    ggtitle('Bad Rate') +
    theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.5)) +
    xlab("")

  freq_plot <- binned_feature$iv_table %>%
    ggplot(aes(group, freq)) +
    geom_bar(stat = 'identity') +
    ggtitle('Frequency') +
    theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.5)) +
    xlab("")

  if(missing(old_frame)){
    return(
      cowplot::plot_grid(woe_plot + remove_x_axis(),
                bad_rate_plot + remove_x_axis(),
                freq_plot,
                nrow = 3)
    )
  }

  old_frame[[y]] <- factor(old_frame[[y]])
  feature_name <- as.symbol(binned_feature$feature)

  distribution_plot <- old_frame %>%
    filter((!!feature_name) >= xmin, (!!feature_name) <= xmax) %>%
    ggplot(aes_string(feature_name, colour = y, fill = y)) +
    geom_density(alpha = 0.3) +
    ggtitle('Distribution') +
    scale_colour_discrete(labels = c('good', 'bad')) +
    scale_fill_discrete(labels = c('good', 'bad')) +
    theme(legend.title = element_blank(), legend.position = c(0.85, 0.85))

    cowplot::plot_grid(distribution_plot ,
                       woe_plot,
                       freq_plot,
                       bad_rate_plot,
                       nrow = 2)
}

create_numeric_supervised_bins <- function(dframe, x, y, tree_control = ctree_control()){
  x_sym <- as.symbol(x)
  y_sym <- as.symbol(y)

  dframe[[y]] <- factor(dframe[[y]])

  tree_obj <- ctree(formula(paste(y, "~", x)), data = dframe, na.action = na.exclude,
                    control = tree_control)

  nbins <- partykit::width(tree_obj)
  if(nbins < 2){
    return(list(significant_splits_found = FALSE))
  }

  tree_len <- length(tree_obj)

  cuts <- map(seq_len(tree_len), ~ tree_obj[.x]$node$split$breaks) %>%
    discard(is.null) %>%
    flatten_dbl %>%
    sort

  binned_data <- dframe %>%
    mutate(group = cut_vector(!!x_sym, breaks = cuts)) %>%
    select(group, !!y_sym)

  list(data = binned_data, significant_splits_found = TRUE, cuts = cuts, tree = tree_obj)
}

create_numeric_frequency_bins <- function(dframe, x, y, bins=10){
  if(bins %% 1 != 0){
    stop("in create_numeric_frequency_bins bins argument must be integer >=2")
  }

  if(bins < 2){
    stop("in create_numeric_frequency_bins bins argument must be integer >=2")
  }

  x_sym <- as.symbol(x)
  y_sym <- as.symbol(y)


  if(length(unique(dframe[[x]])) <= bins){
    cuts <- unique(dframe[[x]]) %>%
      sort %>%
      {.[-length(.)]}
  } else {
    quantile_cuts <- (1:(bins - 1)) / bins
    cuts <- as.vector(quantile(dframe[[x]], quantile_cuts, na.rm = T))
  }

  binned_data <- dframe %>%
    mutate(group = cut_vector(!!x_sym, cuts)) %>%
    select(group, !!y_sym)

  list(data = binned_data, cuts = cuts)
}
