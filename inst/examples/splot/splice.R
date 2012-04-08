library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
library(ggbio)

## try a gene structure view

extdatadir <- system.file("extdata", 
                          package = "isoformExprTutorial")
## your bam file path
files <- tools::list_files_with_exts(extdatadir, "bam")
## need to name your bamfiles
names(files) <- tools::file_path_sans_ext(basename(files))
## 226 is your gene_id in txdb, this will give you a plot of tracks
res <- ggbio:::splicefun(files[1], txdb, id = "226")
res <- splicefun(files, txdb, id = "226")

## other ways to just plot gene stucture
data(genesymbol, package = "biovizBase")
## which is a GRanges object to specify a viewd range
autoplot(txdb, which = genesymbol["RBM17"], geom = "gene")
autoplot(txdb, which = genesymbol["RBM17"], geom = "reduced_gene")

library(devtools)
load_all("~/Codes/gitrepos/ggbio")
extdatadir <- system.file("extdata", 
                          package = "isoformExprTutorial")
files <- tools::list_files_with_exts(extdatadir, "bam")
## need to name your bamfiles
names(files) <- tools::file_path_sans_ext(basename(files))
res <- splicefun(files, txdb, id = aldoa_eg)
library(ggbio)
read_track <- autoplot(res$uniq_splices, geom = "arch",
                       aes(size = score, 
                           height = width / 200),
                       ## max.height = 200, 
                       color = "deepskyblue3",
                       ylab = "coverage") +
  stat_coverage(res$both_uniq) +
  geom_arch(res$uniq_novel_splices,   aes(size = score, 
                                          height = width / 300,
                                          color = novel)) 
read_track
res$tx <- split(stack(res$tx)[,1],values(stack(res$tx)[,1])$sample)
tx_track <- autoplot(res$tx, geom = "alignment", ylab = "")
tx_track <- autoplot(res$tx, ylab = "")

novel_track <- autoplot(res$uniq_novel_splices, geom = "arch",
                        aes(size = score, 
                            height = width / 5,
                            color = novel), 
                        ylab = "coverage") +
          stat_coverage(res$both_uniq)
tracks(novel_track, tx_track, heights = c(4, 1))

## Michael's code

