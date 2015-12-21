ggbio_ggplot <- function(data, mapping = aes(), ...,
                         environment = parent.frame()) {
    df <- mold(data)
    g <- ggplot(df, mapping, ..., environment=environment)
    g <- GGbio(g, data = data)
    g
}

ggplot.Vector <- ggbio_ggplot
ggplot.matrix <- ggbio_ggplot # highly questionable
ggplot.ExpressionSet <- ggbio_ggplot
ggplot.RsamtoolsFile <- ggbio_ggplot
ggplot.character <- ggbio_ggplot # highly questionable
ggplot.TxDb <- ggbio_ggplot
ggplot.BSgenome <- ggbio_ggplot

ggplot.SummarizedExperiment <- function(data, mapping = aes(), assay.id = 1L,
                                        ..., environment = parent.frame()) {
    df <- mold(data, assay.id=assay.id)
    g <- ggplot(df, mapping, ..., environment=environment)
    g <- GGbio(g, data = data)
    g
}
