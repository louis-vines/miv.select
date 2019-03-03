#load users global R profile first if it exists
.rprofile_exists <- length(grep("\\.Rprofile", dir("~", all.files=TRUE))) > 0

if(.rprofile_exists){
  source("~/.Rprofile")
}

#check testthat version and set old printing style if major version >= 2
if(utils::packageVersion("testthat")$major >= 2){
  options(testthat.default_reporter = "summary")
}
