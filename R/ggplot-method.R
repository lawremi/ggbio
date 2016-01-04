.ggbio_ggplot <- function(ggdata, data, mapping = aes(), ...,
                          environment = parent.frame()) {
    g <- ggplot(ggdata, mapping, ..., environment=environment)
    g <- GGbio(g, data = data)
    g
}

ggbio_ggplot <- function(data, mapping = aes(), ...,
                         environment = parent.frame()) {
    .ggbio_ggplot(data, data, mapping, ..., environment=environment)
}

ggbio_ggplot_mold <- function(data, mapping = aes(), ...,
                              environment = parent.frame()) {
    .ggbio_ggplot(mold(data), data, mapping, ..., environment=environment)
}

ggplot.Vector <- ggbio_ggplot_mold
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
