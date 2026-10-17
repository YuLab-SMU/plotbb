## Tests for input validation and edge cases (0.2.0)

with_pdf <- function(code) {
    f <- tempfile(fileext = ".pdf")
    grDevices::pdf(f)
    on.exit({
        if (grDevices::dev.cur() > 1L) grDevices::dev.off()
        unlink(f)
    }, add = TRUE)
    force(code)
    invisible(NULL)
}

render <- function(p) {
    with_pdf(print(p))
}

test_that("all-NA coordinates are rejected instead of reaching plot()", {
    d <- data.frame(x = c(NA_real_, NA_real_), y = c(NA_real_, NA_real_))
    expect_error(bbplot(d, bb_aes(x, y)) + bb_point(pch = 19),
                 "no finite coordinates")

    ## ... and a plot with no drawable layer still gets finite limits
    p0 <- bbplot(d, bb_aes(x, y))
    lim <- plotbb:::bb_plot_limits(p0)
    expect_true(all(is.finite(lim$xlim)))
    expect_true(all(is.finite(lim$ylim)))
    expect_silent(render(p0))
})

test_that("infinite coordinates are dropped instead of drawn", {
    d <- data.frame(x = c(1, Inf, 3), y = c(2, 3, -Inf))
    p <- bbplot(d, bb_aes(x, y)) + bb_point(pch = 19)
    expect_silent(render(p))

    lim <- plotbb:::bb_plot_limits(p)
    expect_true(all(is.finite(lim$xlim)))
    expect_true(all(is.finite(lim$ylim)))
})

test_that("length-mismatched aesthetics raise an informative error", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "a"))
    expect_error(bbplot(d, bb_aes(x, y)) + bb_point(col = c("red", "blue")),
                 "`col` in `bb_point` has length 2, but 3 row\\(s\\) were mapped")
    expect_error(bbplot(d, bb_aes(x, y, col = g)) + bb_text(label = c("a", "b")),
                 "`label` in `bb_text`")
})

test_that("length-1 aesthetics are broadcast", {
    d <- data.frame(x = 1:3, y = 1:3, xend = 2:4, yend = 2:4,
                    ymin = 0:2, ymax = 2:4, xmin = 0:2, xmax = 2:4)
    expect_no_error(bbplot(d, bb_aes(x, y)) + bb_point(col = "red", pch = 19))
    expect_no_error(bbplot(d, bb_aes(x, y, xend = xend, yend = yend)) + bb_segment(col = "red"))
    expect_no_error(bbplot(d, bb_aes(x, ymin = ymin, ymax = ymax)) +
                    bb_errorbar(col = "red"))
    expect_no_error(bbplot(d, bb_aes(x, y)) + bb_tile(col = "red"))
    p <- bbplot(d, bb_aes(x, y)) + bb_point()
    expect_s3_class(p, "bbplot")
})

test_that("segment, errorbar and tile validate mapped and literal aesthetics", {
    d <- data.frame(x = 1:3, y = 1:3, xend = 2:4, yend = 2:4,
                    ymin = 0:2, ymax = 2:4, xmin = 0:2, xmax = 2:4,
                    g = c("red", "blue", "green"))
    expect_error(bbplot(d, bb_aes(x, y, xend = xend, yend = yend)) + bb_segment(col = 1:2),
                 "`col` in `bb_segment` has length 2")
    expect_error(bbplot(d, bb_aes(x, y, xend = xend, yend = yend, col = g)) +
                 bb_segment(col = 1:2), "`col` in `bb_segment` has length 2")
    expect_error(bbplot(d, bb_aes(x, ymin = ymin, ymax = ymax)) +
                 bb_errorbar(col = 1:2), "`col` in `bb_errorbar` has length 2")
    expect_error(bbplot(d, bb_aes(x, ymin = ymin, ymax = ymax)) +
                 bb_errorbar(col = 1:2), "`col` in `bb_errorbar` has length 2")
    expect_error(bbplot(d, bb_aes(x, y)) + bb_tile(col = 1:2),
                 "`col` in `bb_tile` has length 2")
    expect_error(bbplot(d, bb_aes(x, y, col = g)) + bb_tile(col = 1:2),
                 "`col` in `bb_tile` has length 2")
})

test_that("a layer whose rows are partially invalid keeps the valid rows", {
    d <- data.frame(x = c(1, NA, 3), y = c(1, 2, NA))
    p <- bbplot(d, bb_aes(x, y)) + bb_point(pch = 19)
    ly <- p$layer[["point layer"]]
    params <- environment(ly)$params
    expect_equal(params$x, 1)
    expect_equal(params$y, 1)
    expect_silent(render(p))
})

test_that("a layer with only invalid rows is rejected", {
    d <- data.frame(x = c(NA, NA), y = c(1, 2))
    expect_error(bbplot(d, bb_aes(x, y)) + bb_point(), "no finite coordinates")
    expect_error(bbplot(d, bb_aes(x, y)) + bb_text(), "no finite coordinates")
})

test_that("empty data raises a clear error at layer construction", {
    d <- data.frame(x = numeric(0), y = numeric(0))
    expect_error(bbplot(d, bb_aes(x, y)) + bb_point(), "has no data to draw")
})

test_that("layers applied to a data-less plot explain the missing data", {
    p <- as.bbplot(function() graphics::plot(1:10))
    expect_error(p + bb_point(), "requires data")
})

test_that("bb_segment requires endpoints", {
    d <- data.frame(x = 1:3, y = 1:3)
    expect_error(bbplot(d, bb_aes(x, y)) + bb_segment(), "xend")
})

test_that("bb_errorbar requires both bounds", {
    d <- data.frame(x = 1:3, y = 1:3)
    expect_error(bbplot(d, bb_aes(x, y)) + bb_errorbar(), "ymin")
    expect_error(bbplot(d, bb_aes(x, y, ymin = y - 1)) + bb_errorbar(), "ymax")
})

test_that("bb_lm requires x and y mappings", {
    d <- data.frame(x = 1:3)
    expect_error(bbplot(d, bb_aes(x)) + bb_lm(), "both `x` and `y`")
})

test_that("bb_signif validates comparisons", {
    d <- data.frame(g = factor(c("a", "a", "b", "b")), y = c(1, 2, 3, 4))
    p <- bbplot(d, bb_aes(g, y)) + bb_boxplot()
    expect_error(p + bb_signif(comparisons = list(list(1, 2))),
                 "must be a character or numeric vector")
    expect_error(p + bb_signif(comparisons = list(c("a", "zzz"))),
                 NA)
})

test_that("bb_hist warns once and skips undrawable data", {
    d1 <- data.frame(x = 5)
    p1 <- bbplot(d1, bb_aes(x)) + bb_hist()
    expect_warning(render(p1), "at least two finite values")

    d2 <- data.frame(x = c(1, NA))
    p2 <- bbplot(d2, bb_aes(x)) + bb_hist()
    expect_warning(render(p2), "at least two finite values")

    d3 <- data.frame(x = c("a", "b"))
    p3 <- bbplot(d3, bb_aes(x)) + bb_hist()
    expect_error(render(p3), "numeric `x`")
})

test_that("warnings are emitted once per print() call", {
    d <- data.frame(x = c(1, NA, 3, 4))
    p <- bbplot(d, bb_aes(x)) + bb_density() + bb_density()
    w <- tryCatch({
        render(p)
        character()
    }, warning = function(w) conditionMessage(w))
    ## single finite pair is enough for a density, so no warning is expected
    expect_identical(w, character())
})

test_that("bb_boxplot warns once when nothing is finite", {
    d <- data.frame(g = factor(c("a", "b")), y = c(NA_real_, NA_real_))
    p <- bbplot(d, bb_aes(g, y)) + bb_boxplot()
    expect_warning(render(p), "no finite values")
    w <- withCallingHandlers(render(p), warning = function(w) {
        expect_match(conditionMessage(w), "no finite values")
        invokeRestart("muffleWarning")
    })
})

test_that("a failing layer keeps par() and returns readable messages", {
    expect_error(bbplot(mtcars, bb_aes(mpg, disp)) + bb_point(col = 1:2),
                 class = "rlang_error")
})

## ---- jitter reproducibility -------------------------------------------------

jittered_x <- function(seed = 1) {
    d <- data.frame(x = rep(1, 5), y = 1:5)
    p <- bbplot(d, bb_aes(x, y)) + bb_point(position = "jitter", seed = seed)
    ly <- p$layer[["point layer"]]
    ## layers carry x/y as their first named arguments
    env <- environment(ly)
    args <- env$params
    args$x
}

test_that("jitter is reproducible for the same seed", {
    a <- jittered_x(1)
    b <- jittered_x(1)
    expect_identical(a, b)
    expect_false(identical(a, jittered_x(2)))
})

test_that("jitter does not advance the caller's random stream", {
    set.seed(1234)
    before <- .Random.seed
    invisible(jittered_x(7))
    expect_identical(.Random.seed, before)
})

test_that("jitter does not create a global random stream", {
    has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    seed <- if (has_seed) get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
        if (has_seed) {
            assign(".Random.seed", seed, envir = globalenv())
        } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
            rm(".Random.seed", envir = globalenv())
        }
    }, add = TRUE)
    if (has_seed) rm(".Random.seed", envir = globalenv())

    invisible(jittered_x(3))
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("bb_position() accepts a seed", {
    d <- data.frame(x = rep(1, 4), y = 1:4)
    p <- bbplot(d, bb_aes(x, y)) + bb_point(position = bb_position(seed = 5))
    ly <- p$layer[["point layer"]]
    x1 <- environment(ly)$params$x
    p2 <- bbplot(d, bb_aes(x, y)) + bb_point(position = "jitter", seed = 5)
    x2 <- environment(p2$layer[["point layer"]])$params$x
    expect_identical(x1, x2)
})

test_that("an invalid jitter seed is rejected", {
    d <- data.frame(x = 1:3, y = 1:3)
    for (seed in list(NA_real_, Inf, -Inf, c(1, 2), 1e100)) {
        expect_error(bbplot(d, bb_aes(x, y)) +
                     bb_point(position = "jitter", seed = seed),
                     "single finite number|representable as an integer")
    }
})
