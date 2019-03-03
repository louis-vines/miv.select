#' Bin All Features in a Dataframe
#'
#' @export
bin_all <- function(dframe, y, vars_to_exclude, vars_to_include, bins = NULL, tree_control = ctree_control(), verbose = FALSE){
  features_to_bin <- setdiff(names(dframe), y)

  if(!missing(vars_to_exclude)){
    features_to_bin <- setdiff(features_to_bin, vars_to_exclude)
  }

  if(!missing(vars_to_include)){
    features_to_bin <- intersect(features_to_bin, vars_to_include)
  }

  binned_features <- map(features_to_bin, function(feature_name){
    if(verbose) cat('binning:', feature_name, "...\n")

    check_type_and_bin(dframe, feature_name, y, bins, tree_control)
  }) %>%
    setNames(features_to_bin)

  iv_summary <- extract_iv_summary(binned_features)

  attr(binned_features, "class") <- "binned_features"
  attr(binned_features, "iv_summary") <- iv_summary

  binned_features
}

#' @importFrom purrr map
#' @importFrom partykit ctree_control
check_type_and_bin <- function(dframe, feature_name, y, bins, tree_control = ctree_control()){
  feature <- dframe[[feature_name]]

  if(is.factor(feature) | is.character(feature)){
    supervised <- is.null(bins)
    return(bin_factor(dframe, feature_name, y, supervised, tree_control))
  }

  if(is.numeric(feature)){
    return(bin_numeric(dframe, feature_name, y, tree_control = tree_control, bins = bins))
  }
}

#' @importFrom purrr pluck
extract_iv_summary <- function(binned_features){
  binned_features %>%
    map_dbl(~ pluck(.x, 'iv', .default = NA_real_)) %>%
    sort(decreasing = TRUE) %>%
    {
      data_frame(feature = names(.), iv = as.vector(.))
    }
}

#' @export
print.binned_features <- function(binned_features, ...){
  extra_args <- list(...)
  iv_summary <- attr(binned_features, "iv_summary")
  all_args_for_call <- c(list(iv_summary), extra_args)

  do.call(tibble:::print.tbl_df, all_args_for_call)
}

#' @export
plot.binned_features <- function(binned_features, train_data, y){
  iv_summary <- attr(binned_features, "iv_summary")

  for (feature in iv_summary$feature) {
    cat("==", feature, "==\n")

    iv <- binned_features[[feature]]$iv
    if (is.null(iv) || is.na(iv)) {
      cat("feature has no IV value\n")
      next()
    }

    cat("iv: ", iv , "\n")
    print(plot(binned_features[[feature]], train_data, y))
    input_val <- readline("Input q to quit or hit Enter to continue:")
    if (stringr::str_to_lower(input_val) == 'q') {
      break()
    }
  }
}

#' @export
predict.binned_features <- function(binned_features, train_data, y, iv_cutoff){
  iv_summary <- attr(binned_features, 'iv_summary') %>% filter(!is.na(iv))

  if(!missing(iv_cutoff)){
    iv_summary <- iv_summary %>% filter(iv > iv_cutoff)
  }
  
  features_to_keep <- iv_summary$feature

  reduce(binned_features, ~ predict(.y, .x), .init = train) %>%
    select(!!!quos(features_to_keep), !!as.symbol(y))
}
