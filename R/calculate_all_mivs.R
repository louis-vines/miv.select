#' @importFrom purrr map_dbl
calculate_all_mivs <- function(dframe, y = "gb12", pd = "pd", as_dframe = TRUE){
  features_to_bin <- setdiff(names(dframe), c(y, pd))

  mivs <- map(features_to_bin, function(name) calculate_miv(dframe, name, y, pd)) %>%
    setNames(features_to_bin)

  if(!as_dframe){
    return(mivs)
  }

  data_frame(feature_name = names(mivs),
             iv = map_dbl(mivs, "iv"),
             miv = map_dbl(mivs, "miv")) %>%
  arrange(desc(miv))
}

calculate_miv <- function(dframe, binned_x, y = "gb12", pd = "pd"){
  feature_is_categorical <- dframe[[binned_x]] %>% {is.character(.) | is.factor(.)}

  if(!feature_is_categorical){
    stop("cannot calculate miv for a non-categorical variable")
  }

  miv_tables <- dframe %>%
    rename_(group = binned_x) %>%
    build_miv_tables(y, pd)

  c(miv_tables, iv = sum(miv_tables$miv_table$iv), miv = sum(miv_tables$miv_table$miv))
}
