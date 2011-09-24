plotManhattan <- function(obj, y, title,
                          scales = c("free", "fixed"),
                          space = c("free", "fixed"),
                          color.type = c("identity", "seqnames", "twocolor"),
                          two.color = c("#0080FF", "#4CC4FF"),
                          cutoff = NULL,
                          cutoff.color = "red",
                          cutoff.size = 1,
                          legend = FALSE,
                          xlab = "Chromosome",
                          ylab = expression(-log[10](italic(p))),
                          theme_bw = TRUE){

  scales <- match.arg(scales)
  space <- match.arg(space)
  color.type <- match.arg(color.type)
  if(missing(y))
    stop("need to provide y, which is -log10(p-value), make sure you")
  message("make sure y is already -log10(p-value)")
  if(color.type %in% c("seqnames", "twocolor")){
    args <- list(data = obj, geom = "point", y = substitute(y), scales = scales,
                 space = space, color = substitute(seqnames))
    p <- do.call(qplot, args)
  }else{
    args <- list(data = obj, geom = "point", y = substitute(y), scales = scales,
                 space = space)
    p <- do.call(qplot, args)
  }
  if(theme_bw)
    p <- p + theme_bw()
  if(!legend)
    p <- p + opts(legend.position = "none")

  p <- p + ylab(ylab) +  xlab(xlab)+
    opts(axis.text.x = theme_blank(),
         axis.ticks = theme_blank())
  if(!is.null(cutoff))
    p <-  p + geom_hline(yintercept = cutoff, color = cutoff.color,
                         size = cutoff.size)
  
  if(color.type == "twocolor"){
    chrs <- unique(as.character(seqnames(gr.snp)))
    N <- length(chrs)
    id1 <- seq(from = 1, by = 2, to = N)
    id2 <- seq(from = 2, by = 2, to = N)
    color1 <- two.color[1]
    color2 <- two.color[2]
    cols <- c(rep(color1, length(id1)), rep(color2, length(id2)))
    names(cols) <- c(chrs[id1], chrs[id2])
    p <- p + scale_color_manual(values = cols)
  }
  if(!missing(title))
    p <- p + opts(title = title)
  p
}
