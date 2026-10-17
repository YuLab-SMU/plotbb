## 0.7.0 violin layer tests.

test_that("bb_violin draws one polygon per group", {
    set.seed(1)
    d <- data.frame(g = factor(rep(c("a", "b", "c"), c(20, 15, 10))),
                    y = c(rnorm(20), rnorm(15, 2), rnorm(10, 4)))
    rec <- new.env()
    rec$calls <- list()
    options(plotbb.test.rec = rec)
    trace("polygon", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$calls[[length(.rec$calls) + 1L]] <- list(col = col)
    }), print = FALSE)
    on.exit({
        untrace("polygon", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x = g, y = y)) + bb_violin(fill = "steelblue")
    with_test_pdf(print(p))
    expect_length(rec$calls, 3)
    expect_equal(unique(vapply(rec$calls, `[[`, "", "col")),
                 scales::alpha("steelblue", 1))
})

test_that("bb_violin skips constant and short groups with a warning", {
    d <- data.frame(g = c(rep("a", 10), rep("b", 10), rep("c", 4)),
                    y = c(rnorm(10), rep(1, 10), c(1, 2, 3, 4)))
    p <- bbplot(d, bb_aes(x = g, y = y)) + bb_violin()
    with_test_pdf(expect_warning(print(p), "skipped group 'b'"))
})

test_that("bb_violin drops non-finite y and honours a numeric x", {
    d <- data.frame(x = rep(c(1, 2), each = 11),
                    y = c(rnorm(10), rnorm(10, 3), NA, NA))
    p <- bbplot(d, bb_aes(x = x, y = y)) + bb_violin()
    with_test_pdf(expect_silent(print(p)))
    built <- bb_build(p)
    expect_true(built$limits$xlim[[1]] < 1)
    expect_true(built$limits$xlim[[2]] > 2)
})

test_that("bb_violin fill mapping and legend agree", {
    set.seed(2)
    d <- data.frame(g = rep(c("a", "b"), each = 15),
                    y = c(rnorm(15), rnorm(15, 2)))
    p <- bbplot(d, bb_aes(x = g, y = y, fill = g)) +
        bb_violin(alpha = 0.6) +
        bb_legend(aesthetic = "fill", position = "topright")
    with_test_pdf(expect_silent(print(p)))
})

test_that("bb_violin works in facets", {
    set.seed(3)
    d <- data.frame(g = rep(c("a", "b"), 20),
                    f = rep(c("f1", "f2"), each = 20),
                    y = c(rnorm(20), rnorm(20, 3)))
    p <- bbplot(d, bb_aes(x = g, y = y)) + bb_violin() +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")
    with_test_pdf(expect_silent(print(p)))
})

test_that("bb_smooth draws a curve and a confidence band per group", {
    set.seed(5)
    d <- data.frame(x = 1:40, y = sin((1:40) / 6) + rnorm(40, sd = 0.2),
                    g = rep(c("a", "b"), each = 20))
    rec <- new.env(); rec$polygons <- 0L; rec$lines <- 0L
    options(plotbb.test.rec = rec)
    trace("polygon", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$polygons <- .rec$polygons + 1L
    }), print = FALSE)
    trace("lines", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$lines <- .rec$lines + 1L
    }), print = FALSE)
    on.exit({
        untrace("polygon", where = asNamespace("graphics"))
        untrace("lines", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x = x, y = y, col = g)) +
        bb_point() + bb_smooth(method = "loess", se = TRUE)
    with_test_pdf(print(p))
    expect_equal(rec$polygons, 2)
    expect_equal(rec$lines, 2)

    rec$polygons <- 0L; rec$lines <- 0L
    with_test_pdf(print(bbplot(d, bb_aes(x = x, y = y, col = g)) +
                            bb_smooth(method = "loess", se = FALSE)))
    expect_equal(rec$polygons, 0)
    expect_equal(rec$lines, 2)
})

test_that("bb_smooth falls back for small groups and skips single points", {
    d <- data.frame(x = c(1, 2, 3, 4, 1:3),
                    y = c(1, 2, 4, 3, 2, 3, 4),
                    g = c(rep("a", 4), rep("b", 3)))
    p <- NULL
    expect_warning(
        p <- bbplot(d, bb_aes(x = x, y = y, group = g)) +
            bb_smooth(method = "loess", se = FALSE),
        "too few"
    )
    with_test_pdf(expect_silent(print(p)))

    d2 <- data.frame(x = c(1, 1:10),
                     y = c(1, rnorm(10)), g = c("solo", rep("ok", 10)))
    p2 <- NULL
    expect_warning(
        p2 <- bbplot(d2, bb_aes(x = x, y = y, group = g)) +
            bb_smooth(method = "lm", se = FALSE),
        "needs at least two points"
    )
    with_test_pdf(expect_silent(print(p2)))
})

test_that("bb_smooth supports a formula, facets and a mapped fill", {
    set.seed(6)
    d <- data.frame(x = 1:60, y = sin((1:60) / 8) + rnorm(60, sd = 0.2),
                    f = rep(c("f1", "f2"), each = 30))
    p <- bbplot(d, bb_aes(x = x, y = y)) +
        bb_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE,
                  fill = "orange") +
        bb_facet_wrap(~f, ncol = 2)
    with_test_pdf(expect_silent(print(p)))
})

test_that("bb_smooth validates its parameters", {
    d <- data.frame(x = 1:10, y = rnorm(10))
    p <- bbplot(d, bb_aes(x, y))
    expect_error(with_test_pdf(print(p + bb_smooth(level = 1))),
                 "between 0 and 1")
    expect_error(with_test_pdf(print(p + bb_smooth(se = NA))),
                 "TRUE or FALSE")
    expect_error(with_test_pdf(print(p + bb_smooth(n = 1))),
                 "at least 2")
    expect_error(with_test_pdf(print(p + bb_smooth(formula = y ~ z))),
                 "may only use")
})

test_that("bb_violin validates its parameters", {
    d <- data.frame(x = rep(c(1, 2), each = 5), y = rnorm(10))
    p <- bbplot(d, bb_aes(x = x, y = y)) + bb_point()
    expect_error(with_test_pdf(print(p + bb_violin(width = 0))),
                 "positive number")
    expect_error(with_test_pdf(print(p + bb_violin(alpha = 2))),
                 "between 0 and 1")
    expect_error(with_test_pdf(print(p + bb_violin(trim = NA))),
                 "TRUE or FALSE")
})
