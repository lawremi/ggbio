library(VariantAnnotation)
library(ggbio)
vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
vcf <- readVcf(vcffile, "hg19")

p1 <- autoplot(vcf, type = "geno")
p2 <- autoplot(vcf, type = "info")
rescale(autoplot(vcf, type = "fixed"), xlim = c(51200000, 51200000 + 1000))
unlist(values(alt(vcf)[start(alt(vcf)) < 51200300 & start(alt(vcf)) > 51200000])$ALT)
autoplot(vcf, stat = "identity", geom = "text", aes(label = ALT), type = "fixed")
png("~/Desktop/vcf.png", 800, 600)
tracks(p2, p1, heights = c(1, 4))
dev.off()

