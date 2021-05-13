context("theme")

test_that("Test theme_null()", {
    test <- theme_null()
    expected <- theme_bw() +
                theme(axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.background = element_rect(fill = "white", colour = NA),
                      legend.key = element_rect(colour = "white"),
                      panel.background = element_blank(),
                      panel.border = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.background = element_blank(),
                      strip.background = element_blank(),
                      strip.text.y = element_blank(),
                      strip.text.x = element_blank())
    expect_equal(test, expected)
})

test_that("Test theme_alignment()", {
    test <- theme_alignment()
    base_family <- ""
    base_size <- 12
    theme <- theme_gray() +
             theme(axis.line = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_text(family = base_family, size = base_size,
                                               vjust = 1),
                   axis.title.y = element_text(family = base_family, size = base_size,
                                               angle = 90, vjust = 0.5, colour = "white"),
                   axis.ticks.length = unit(0.3, "lines"),
                   axis.text = element_text(margin=unit(0.5, "lines")),
                   panel.background = element_blank(),
                   panel.border = element_rect(fill = NA, colour = "grey50"),
                   panel.grid.major = element_line(colour = "grey90", size = 0.2),
                   panel.grid.minor = element_blank(),
                   panel.margin = unit(0.25, "lines"),
                   strip.background = element_rect(fill = "grey80", colour = "grey50"),
                   strip.text.x = element_text(family = base_family, size = base_size * 0.8),
                   strip.text.y = element_text(family = base_family, size = base_size * 0.8,
                                               angle = -90),
                   plot.background = element_rect(colour = NA),
                   plot.title = element_text(family = base_family, size = base_size * 1.2),
                                             plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"))
    expected <- list(theme, list(scale_y_continuous(breaks = NULL)))
    expect_equal(test, expected)
})

test_that("Test theme_pack_panels()", {
     test <- theme_pack_panels()
     expected <- theme(panel.background = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.y = element_blank(),
                       strip.background = element_blank(),
                       strip.text.y = element_text(angle = 0),
                       panel.margin = grid::unit(0, "lines"))
     expected <- c(list(expected), list(scale_y_continuous(breaks = NULL)))
     expect_equal(test, expected)
})

test_that("Test theme_noexpand()", {
    test <- theme_noexpand()
    expected <- c(list(scale_x_continuous(expand = c(0, 0))),
                  list(scale_y_continuous(expand = c(0, 0))))
    expect_equal(test, expected)
})

test_that("Test theme_clear()", {
    test <- theme_clear()
    expected <- theme_gray() +
                theme(panel.background = element_rect(fill = NA, color = NA),
                      panel.border = element_rect(fill = NA, color = NA)) +
                theme(panel.grid.major.y = element_blank(),
                      panel.grid.minor.y = element_blank()) +
                theme(panel.grid.minor.x = element_blank()) +
                theme(panel.grid.major.x = element_blank()) +
                theme(axis.ticks.x = element_blank()) +
                theme(axis.ticks.x = element_line(colour = "grey50")) +
                theme(axis.line = element_line(color = "gray80"))
    expect_equal(test, expected)
})

test_that("Test theme_tracks_sunset()", {
    test <- theme_tracks_sunset()
    bg <- "#fffedb"
    expected <- theme_clear(grid.x.major = FALSE)
    attr(expected, "track.plot.color") <- vapply(bg, scales::alpha, 1, FUN.VALUE = character(1L))
    attr(expected, "track.bg.color") <- bg
    attr(expected, "label.text.color") <- "white"
    attr(expected, "label.bg.fill") <- "#a52a2a"
    expect_equal(test, expected)
})

test_that("Test theme_tracks_fancy()", {
    test <- ggbio:::theme_tracks_fancy()
    bg <- c("white", "#F2C545")
    expected <- theme_clear(grid.x.major = FALSE)
    attr(expected, "track.plot.color") <- vapply(bg, scales::alpha, 0.3, FUN.VALUE = character(1L))
    attr(expected, "label.bg.fill") <- c("gray80", "darkblue")
    attr(expected, "label.text.color") <- "white"
    expect_equal(test, expected)
})
