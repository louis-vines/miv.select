#' @importFrom purrr map_dbl
#'
#' @export
calculate_all_mivs <- function(dframe, y, predictions, vars_to_exclude,
                               vars_to_include, as_dframe = TRUE){
  predictions <- as.vector(predictions)
  features_to_bin <- setdiff(names(dframe), y)

  if(!missing(vars_to_exclude)){
    features_to_bin <- setdiff(features_to_bin, vars_to_exclude)
  }

  if(!missing(vars_to_include)){
    features_to_bin <- intersect(features_to_bin, vars_to_include)
  }

  mivs <- map(features_to_bin, function(feature) calculate_miv(dframe, feature, y, predictions)) %>%
    setNames(features_to_bin)

  if(!as_dframe){
    return(mivs)
  }

  data_frame(feature_name = names(mivs),
             iv = map_dbl(mivs, "iv"),
             miv = map_dbl(mivs, "miv")) %>%
  arrange(desc(miv))
}

calculate_miv <- function(dframe, binned_x, y, predictions){
  feature_is_categorical <- dframe[[binned_x]] %>% {is.character(.) | is.factor(.)}

  if(!feature_is_categorical){
    stop("cannot calculate miv for a non-categorical variable")
  }

  miv_tables <- dframe %>%
    rename_(group = binned_x) %>%
    build_miv_tables(y, predictions)

  c(miv_tables, iv = sum(miv_tables$miv_table$iv), miv = sum(miv_tables$miv_table$miv))
}
