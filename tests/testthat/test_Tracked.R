context("Tracked")

## Tracked
td <- ggbio:::Tracked()

# bgColor
test_that("Test bgColor() of Tracked class", {
    expect_identical(bgColor(td), "white")
})

test_that("Test bgColor()<- of Tracked class", {
    bgColor(td) <- "blue"
    expect_identical(bgColor(td), "blue")
})

# fixed
test_that("Test fixed() of Tracked class", {
    expect_identical(fixed(td), FALSE)
})

test_that("Test fixed()<- of Tracked class", {
    fixed(td) <- TRUE
    expect_identical(fixed(td), TRUE)
})

# labeled
test_that("Test labeled() of Tracked class", {
    expect_identical(labeled(td), TRUE)
})

test_that("Test labeled()<- of Tracked class", {
    labeled(td) <- FALSE
    expect_identical(labeled(td), FALSE)
})

# mutable
test_that("Test mutable() of Tracked class", {
    expect_identical(mutable(td), TRUE)
})

test_that("Test mutable()<- of Tracked class", {
    mutable(td) <- FALSE
    expect_identical(mutable(td), FALSE)
})

# hasAxis
test_that("Test hasAxis() of Tracked class", {
    expect_identical(hasAxis(td), FALSE)
})

test_that("Test hasAxis()<- of Tracked class", {
    hasAxis(td) <- TRUE
    expect_identical(hasAxis(td), TRUE)
})

# height
test_that("Test height() of Tracked class", {
    expect_identical(height(td), unit(1, "null"))
})

test_that("Test height()<- of Tracked class", {
    height(td) <- unit(2, "null")
    expect_identical(height(td), unit(2, "null"))
})


## gg
gg <- ggplot()

# bgColor
test_that("Test bgColor() of gg class", {
    expect_identical(bgColor(gg), "white")
})

test_that("Test bgColor()<- of gg class", {
    bgColor(gg) <- "blue"
    expect_identical(bgColor(gg), "blue")
})

# fixed
test_that("Test fixed() of gg class", {
    expect_identical(fixed(gg), FALSE)
})

test_that("Test fixed()<- of gg class", {
    fixed(gg) <- TRUE
    expect_identical(fixed(gg), TRUE)
})

# labeled
test_that("Test labeled() of gg class", {
    expect_identical(labeled(gg), TRUE)
})

test_that("Test labeled()<- of gg class", {
    labeled(gg) <- FALSE
    expect_identical(labeled(gg), FALSE)
})

# mutable
test_that("Test mutable() of gg class", {
    expect_identical(mutable(gg), TRUE)
})

test_that("Test mutable()<- of gg class", {
    mutable(gg) <- FALSE
    expect_identical(mutable(gg), FALSE)
})

# hasAxis
test_that("Test hasAxis() of gg class", {
    expect_identical(hasAxis(gg), FALSE)
})

test_that("Test hasAxis()<- of gg class", {
    hasAxis(gg) <- TRUE
    expect_identical(hasAxis(gg), TRUE)
})

# height
test_that("Test height() of gg class", {
    expect_identical(height(gg), unit(1, "null"))
})

test_that("Test height()<- of gg class", {
    height(gg) <- unit(2, "null")
    expect_identical(height(gg), unit(2, "null"))
})


## GGbio
ggbio <- ggbio::ggbio()

# bgColor
test_that("Test bgColor() of GGbio class", {
    expect_identical(bgColor(ggbio), "white")
})

test_that("Test bgColor()<- of GGbio class", {
    bgColor(ggbio) <- "blue"
    expect_identical(bgColor(ggbio), "blue")
})

# fixed
test_that("Test fixed() of GGbio class", {
    expect_identical(fixed(ggbio), FALSE)
})

test_that("Test fixed()<- of GGbio class", {
    fixed(ggbio) <- TRUE
    expect_identical(fixed(ggbio), TRUE)
})

# labeled
test_that("Test labeled() of GGbio class", {
    expect_identical(labeled(ggbio), TRUE)
})

test_that("Test labeled()<- of GGbio class", {
    labeled(ggbio) <- FALSE
    expect_identical(labeled(ggbio), FALSE)
})

# mutable
test_that("Test mutable() of GGbio class", {
    expect_identical(mutable(ggbio), TRUE)
})

test_that("Test mutable()<- of GGbio class", {
    mutable(ggbio) <- FALSE
    expect_identical(mutable(ggbio), FALSE)
})

# hasAxis
test_that("Test hasAxis() of GGbio class", {
    expect_identical(hasAxis(ggbio), FALSE)
})

test_that("Test hasAxis()<- of GGbio class", {
    hasAxis(ggbio) <- TRUE
    expect_identical(hasAxis(ggbio), TRUE)
})

# height
test_that("Test height() of GGbio class", {
    expect_identical(height(ggbio), unit(1, "null"))
})

test_that("Test height()<- of GGbio class", {
    height(ggbio) <- unit(2, "null")
    expect_identical(height(ggbio), unit(2, "null"))
})


## gtable
gtable <- gtable::gtable()
# bgColor
test_that("Test bgColor() of gtable class", {
    expect_identical(bgColor(gtable), "white")
})

test_that("Test bgColor()<- of gtable class", {
    bgColor(gtable) <- "blue"
    expect_identical(bgColor(gtable), "blue")
})

# labeled
test_that("Test labeled() of gtable class", {
    expect_identical(labeled(gtable), TRUE)
})

test_that("Test labeled()<- of gtable class", {
    labeled(gtable) <- FALSE
    expect_identical(labeled(gtable), FALSE)
})


## text
text <- grid::textGrob("test")
# labeled
test_that("Test labeled() of text class", {
    expect_identical(labeled(text), TRUE)
})


## gTree
gTree <- grid::gTree()
# labeled
test_that("Test labeled() of gTree class", {
    expect_identical(labeled(gTree), TRUE)
})
