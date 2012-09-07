data(volcano)
autoplot(volcano)
autoplot(volcano, geom = "line")
autoplot(volcano, geom = "line", facet = FALSE)
autoplot(volcano, geom = "line", facet = FALSE, alpha = 0.1) 

rownames(volcano) <- paste("sample", seq_len(nrow(volcano)), sep = "")
colnames(volcano) <- as.character(seq(from = 1000, length.out = ncol(volcano)))
p1 <- autoplot(volcano - 150)
p2 <- qplot(data = mtcars, x = mpg, y = wt)
p1
tracks(p1, p2)
p2
data(volcano)
autoplot(volcano - 150)


## for lenold
library(ggbio)

set.seed(1)
x <- 1:100
N <- 100
y <- c()
for(i in 1:N){
  y <- c(y, dnorm(x, mean = 50+rnorm(1, mean = 5), sd=  20))
}
mx <- matrix(y, byrow = TRUE, nrow = N)

y <- c()
for(i in 1:N){
  y <- c(y, dnorm(x, mean = 50+rnorm(1, mean = 20), sd=  5))
}
mx2 <- matrix(y, byrow = TRUE, nrow = N)

## rename the column names to 'genomic position' or position you want to label
colnames(mx) <- -50:49
colnames(mx2) <- -50:49
autoplot(mx) + scale_fill_fold_change()
autoplot(mx, geom = "line", alpha = 0.1)
autoplot(mx[1:10,], geom = "line", facet = TRUE)
autoplot(mx[1:10,], geom = "line", facet = TRUE, strip.bg = TRUE)
autoplot(mx[1:10,], geom = "line", facet = TRUE, strip.text.y = FALSE)

p1 <- autoplot(mx) + scale_fill_fold_change()
p2 <- autoplot(mx2) + scale_fill_fold_change()
png("~/Desktop/heatmap.png", 700, 700)
tracks(human = p1, mouse = p2)
dev.off()

p4 <- autoplot(mx, geom = "line", alpha = 0.1)
p5 <- autoplot(mx2, geom = "line", alpha = 0.1)
png("~/Desktop/line.png", 700, 700)
tracks(human = p4, mouse = p5)
dev.off()

p6 <- autoplot(mx[1:10, ], geom = "line", facet = TRUE)
p7 <- autoplot(mx2[1:10, ], geom = "line", facet = TRUE)
png("~/Desktop/line_sep.png", 700, 700)
tracks(human = p6, mouse = p7)
dev.off()

