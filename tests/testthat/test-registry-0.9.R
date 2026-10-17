## 0.9.0 unified grouping and statistical-layer registry.

test_that("bb_group_values has one documented precedence", {
    d <- data.frame(x = rep(c("a", "b"), each = 3),
                    y = 1:6,
                    g = rep(c("u", "v", "w"), 2),
                    f = rep(c("p", "q"), 3))
    mapping <- bb_aes(x, y, group = g, col = f)
    expect_equal(bb_group_values(mapping, d, d$x), d$g)
    ## a discrete x beats col/fill
    mapping <- bb_aes(x, y, col = f, fill = g)
    expect_equal(bb_group_values(mapping, d, d$x), d$x)
    ## without a discrete x, col wins over fill
    mapping <- bb_aes(x, y, col = f, fill = g)
    expect_equal(bb_group_values(mapping, d), d$f)
    mapping <- bb_aes(x, y, fill = g)
    expect_equal(bb_group_values(mapping, d), d$g)
})

test_that("violin groups a numeric x by fill instead of every unique x", {
    d <- data.frame(x = rep(1:3, each = 8),
                    y = c(rnorm(8), rnorm(8, 1), rnorm(8, 2)),
                    g = factor(rep(c("u", "v"), 12)))
    rec <- new.env(); rec$n <- 0L
    options(plotbb.test.rec = rec)
    trace("polygon", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$n <- .rec$n + 1L
    }), print = FALSE)
    on.exit({
        untrace("polygon", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x = x, y = y, fill = g)) + bb_violin()
    with_test_pdf(print(p))
    expect_equal(rec$n, 2)
})

test_that("a custom statistical layer can declare fill, summary and ranges", {
    d <- data.frame(x = 1:10, y = 1:10)
    draw <- function(x, y, ...) {
        graphics::rect(0, -5, 10, 5, col = "grey90", border = NA)
        graphics::points(x, y, ...)
    }
    layer <- bb_layer(
        draw, aesthetics = "fill", summary = TRUE,
        range = function(plot, data, mapping) list(x = c(0, 10), y = c(-5, 5))
    )
    p <- bbplot(d, bb_aes(x, y)) + layer
    built <- bb_build(p)
    expect_lte(built$limits$xlim[[1]], 0)
    expect_gte(built$limits$xlim[[2]], 10)
    expect_lte(built$limits$ylim[[1]], -5)
    expect_gte(built$limits$ylim[[2]], 5)
    with_test_pdf(expect_silent(print(p)))
})

test_that("a custom layer's range must be a list of x/y", {
    d <- data.frame(x = 1:4, y = 1:4)
    p <- bbplot(d, bb_aes(x, y)) +
        bb_layer(function(x, y, ...) graphics::points(x, y),
                 summary = TRUE, range = function(...) c(0, 1))
    expect_error(with_test_pdf(print(p)), "must return a list")
    expect_error(bb_layer(function(...) NULL, range = 1),
                 "must be a function")

    bad_range <- bbplot(d, bb_aes(x, y)) +
        bb_layer(function(x, y, ...) graphics::points(x, y),
                 summary = TRUE,
                 range = function(...) stop("range boom"))
    expect_error(with_test_pdf(print(bad_range)), "range boom")
})
