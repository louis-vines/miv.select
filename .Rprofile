.rprofile_exists <- length(grep("\\.Rprofile", dir("~", all.files=TRUE))) > 0

if(.rprofile_exists){
  source("~/.Rprofile")
}

if(interactive()){
  devtools::load_all()
}
