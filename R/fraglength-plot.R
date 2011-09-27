plotFragLength <- function(file, model){
    if((missing(model)))
      stop("Fragment length require a specified model(GRanges)
    or txdb(TranscriptDb) object")
    if(!is(model, "GRanges"))
      stop("model must be a GRangs object")
    ## mds <- model
    which <- reduce(model)
    ## model <- model
    ga <- readBamGappedAlignments(file,
                                  param = ScanBamParam(which = which),           
                                  use.name = TRUE)
    dt <- biovizBase:::fetch(ga)
    gr <- getFragLength(dt, model)
    p <- qplot(gr, y = .fragLength, geom = "point") + ylab("Fragment Length")
      opts(title = "Fragment Length Estimation")
    p
}


plotFragWithGC <- function(file, model){
  ## Michael's sample case with tracks about
  ## coverage, GC content, and fragment length.
  
  
}
