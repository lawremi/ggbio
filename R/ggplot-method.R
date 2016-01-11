ggbio_ggplot <- function(data, mapping = aes(), ...,
                         environment = parent.frame()) {
    gg <- ggplot(mapping = mapping, ..., environment=environment)
    GGbio(gg, data = data)
}

ggbio_ggplot_mold <- function(data, mapping = aes(), ...,
                              environment = parent.frame()) {
    gg <- ggplot(mold(data), mapping, ..., environment=environment)
    GGbio(gg, data = data)
}

ggplot.Vector <- ggbio_ggplot_mold
ggplot.Seqinfo <- ggbio_ggplot_mold
ggplot.matrix <- ggbio_ggplot_mold # highly questionable
ggplot.ExpressionSet <- ggbio_ggplot_mold
ggplot.RsamtoolsFile <- ggbio_ggplot
ggplot.character <- ggbio_ggplot # highly questionable
ggplot.TxDbOREnsDb <- ggbio_ggplot
ggplot.BSgenome <- ggbio_ggplot
ggplot.GAlignments <- ggbio_ggplot
ggplot.VCF <- ggbio_ggplot

ggplot.SummarizedExperiment <- function(data, mapping = aes(), assay.id = 1L,
                                        ..., environment = parent.frame()) {
    df <- mold(data, assay.id=assay.id)
    g <- ggplot(df, mapping, ..., environment=environment)
    g <- GGbio(g, data = data)
    g
}
