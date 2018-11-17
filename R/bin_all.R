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

  map(features_to_bin, function(feature_name){
    if(verbose) cat('binning:', feature_name, "...\n")

    check_type_and_bin(dframe, feature_name, y, bins, tree_control)
  }) %>%
    setNames(features_to_bin)
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
