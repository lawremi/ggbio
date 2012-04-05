args <- commandArgs(TRUE)
library(knitr)
.back <- getwd()
base.url <- tools::file_path_as_absolute("/home/tengfei/Codes/gitrepos/ggbio/gh-pages/ggbio/")
dirs <- setdiff(list.dirs(".", full.names = FALSE), ".")
if(length(args)){
if(!all(args %in% dirs))
  stop(args, "must in ", dirs)
else
  dirs <- args
}
  
for(d in dirs){
  dir.nm <- basename(d)
  setwd(.back)
  dir.den <- file.path(base.url, dir.nm)
  dir.cur <- tools::file_path_as_absolute(d)
  fls <- grep("\\_knit_.md$",list.files(dir.cur, full.names = TRUE),value = TRUE)
  for(fl.cur in fls){
    fl.cur <- tools::file_path_as_absolute(fl.cur)
    setwd(dir.den)
    tryCatch(knit(fl.cur), error = function(e){
      print(paste("error produced by kniting", fl.cur))
      print("keep going ...")
    })
  }
}
