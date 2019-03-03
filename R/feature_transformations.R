#' @export
replace_na <- function(x, replacement){
  replace(x,is.na(x), replacement)
}

#' @export
safe_log <- function(x, adjuster = 1){
  log(x + adjuster)
}
