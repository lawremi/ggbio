data(volcano)
autoplot(volcano)
rownames(volcano) <- paste("sample", seq_len(nrow(volcano)), sep = "")
colnames(volcano) <- as.character(seq(from = 1000, length.out = ncol(volcano)))
p1 <- autoplot(volcano - 150)
p2 <- qplot(data = mtcars, x = mpg, y = wt)
p1
tracks(p1, p2)
p2
data(volcano)
autoplot(volcano - 150)



