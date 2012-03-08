library(knitr)
.back <- getwd()
base.url <- tools::file_path_as_absolute("../../../../gh-pages/ggbio/")

dirs <- setdiff(list.dirs(".", full.names = FALSE), ".")
for(d in dirs){
  dir.nm <- basename(d)
  setwd(.back)
  dir.den <- file.path(base.url, dir.nm)
  dir.cur <- tools::file_path_as_absolute(d)
  fl.cur <- tools::file_path_as_absolute(grep("\\_knit_.md$",list.files(dir.cur, full.names = TRUE),value = TRUE))
  setwd(dir.den)
  knit(fl.cur)
}
