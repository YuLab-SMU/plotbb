## 0.4.0 layer contract tests: bb_ribbon(), bb_hline(), bb_vline().

## ---- ribbon ---------------------------------------------------------------

test_that("ribbon draws one sorted polygon per group", {
    d <- data.frame(x = c(3, 1, 2), ymin = c(3, 1, 2), ymax = c(6, 4, 5))
    rec <- new.env()
    rec$calls <- list()
    options(plotbb.test.rec = rec)
    trace("polygon", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$calls[[length(.rec$calls) + 1L]] <- list(x = x, y = y, col = col)
    }), print = FALSE)
    on.exit({
        untrace("polygon", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x = x, ymin = ymin, ymax = ymax)) +
        bb_ribbon(fill = "steelblue", alpha = 0.3)
    with_test_pdf(print(p))
    calls <- rec$calls

    expect_length(calls, 1L)
    expect_equal(calls[[1]]$x, c(1, 2, 3, 3, 2, 1))
    expect_equal(calls[[1]]$y, c(4, 5, 6, 3, 2, 1))
    expect_equal(calls[[1]]$col, scales::alpha("steelblue", 0.3))
})

test_that("ribbon drops incomplete rows and needs two points per group", {
    d <- data.frame(
        x = 1:6,
        ymin = c(0, NA, 2, 3, NA, 5),
        ymax = c(1, 2, 3, Inf, 5, 6),
        g = c("a", "a", "a", "b", "b", "b")
    )
    rec <- new.env()
    rec$calls <- list()
    options(plotbb.test.rec = rec)
    trace("polygon", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$calls[[length(.rec$calls) + 1L]] <- list(x = x, y = y)
    }), print = FALSE)
    on.exit({
        untrace("polygon", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x = x, ymin = ymin, ymax = ymax, fill = g)) + bb_ribbon()
    with_test_pdf(expect_warning(print(p), "fewer than two"))
    calls <- rec$calls
    expect_true(length(calls) >= 1L)
    for (call in calls) {
        expect_true(all(is.finite(call$x)))
        expect_true(all(is.finite(call$y)))
    }
})

test_that("ribbon contributes its coordinates to the ranges", {
    d <- data.frame(x = 1:3, y = 1:3)
    ribbon <- data.frame(x = c(10, 20), ymin = c(50, 60), ymax = c(70, 80))
    p <- bbplot(d, bb_aes(x, y)) +
        bb_ribbon(bb_aes(x = x, ymin = ymin, ymax = ymax), data = ribbon)
    lim <- plotbb:::bb_plot_limits(p)
    expect_true(lim$xlim[[2]] > 20)
    expect_true(lim$ylim[[2]] > 80)
})

test_that("ribbon maps fill through the fill scale", {
    d <- data.frame(x = 1:4, ymin = c(0, 1, 0, 1),
                    ymax = c(2, 3, 2, 3), g = factor(c("a", "b", "a", "b")))
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

    p <- bbplot(d, bb_aes(x = x, ymin = ymin, ymax = ymax, fill = g)) +
        bb_ribbon(alpha = 0.5) +
        bb_scale_fill_manual(c(a = "red", b = "blue"))
    with_test_pdf(print(p))
    calls <- rec$calls
    expect_true(length(calls) == 2L)
    expect_setequal(unlist(lapply(calls, `[[`, "col")),
                    c(scales::alpha("red", 0.5), scales::alpha("blue", 0.5)))
})

test_that("a length-n constant fill follows the surviving ribbon rows", {
    d <- data.frame(x = 1:3, ymin = 0, ymax = c(NA, 1, 1))
    rec <- new.env()
    rec$calls <- list()
    options(plotbb.test.rec = rec)
    trace("polygon", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$calls[[length(.rec$calls) + 1L]] <- col
    }), print = FALSE)
    on.exit({
        untrace("polygon", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x = x, ymin = ymin, ymax = ymax)) +
        bb_ribbon(fill = c("red", "green", "blue"))
    with_test_pdf(print(p))
    ## row 1 is dropped, so the first surviving row carries "green"
    expect_equal(rec$calls[[1]], scales::alpha("green", 0.3))
})

test_that("ribbon accepts reversed bounds, duplicate x and missing groups", {
    d_rev <- data.frame(x = 1:4, ymin = c(3, 2, 4, 3), ymax = c(1, 1, 2, 1))
    with_test_pdf(expect_silent(
        print(bbplot(d_rev, bb_aes(x = x, ymin = ymin, ymax = ymax)) + bb_ribbon())
    ))

    d_dup <- data.frame(x = c(1, 2, 2, 3), ymin = c(0, 0, 1, 1),
                        ymax = c(1, 2, 1, 2))
    with_test_pdf(expect_silent(
        print(bbplot(d_dup, bb_aes(x = x, ymin = ymin, ymax = ymax)) + bb_ribbon())
    ))

    d_na <- data.frame(x = 1:4, ymin = 0, ymax = 1,
                       g = c("a", NA, "a", "b"))
    with_test_pdf(
        expect_warning(print(bbplot(d_na, bb_aes(x = x, ymin = ymin,
                                                 ymax = ymax, group = g)) +
                                 bb_ribbon()),
                       "missing group")
    )
})

test_that("ribbon style arguments are layer-wide", {
    d <- data.frame(x = 1:3, ymin = 0, ymax = 1)
    p <- bbplot(d, bb_aes(x = x, ymin = ymin, ymax = ymax))
    expect_error(with_test_pdf(print(p + bb_ribbon(alpha = c(0.1, 0.2)))),
                 "single number")
    expect_error(with_test_pdf(print(p + bb_ribbon(alpha = 2))),
                 "between 0 and 1")
    expect_error(with_test_pdf(print(p + bb_ribbon(border = c("red", "blue")))),
                 "single colour")
    expect_error(with_test_pdf(print(p + bb_ribbon(lty = c(1, 2)))),
                 "length 2|single value")
})

test_that("a layer with its own data must carry inherited mappings or opt out", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    band <- data.frame(x = 1:2, lo = 0, hi = 1)
    p <- bbplot(d, bb_aes(x, y, col = g)) +
        bb_ribbon(bb_aes(x = x, ymin = lo, ymax = hi), data = band)
    with_test_pdf(expect_error(print(p), "not found"))

    ok <- bbplot(d, bb_aes(x, y, col = g)) +
        bb_ribbon(bb_aes(x = x, ymin = lo, ymax = hi), data = band,
                  inherit = FALSE)
    with_test_pdf(expect_silent(print(ok)))
})

test_that("reference lines with their own data can opt out of inherited mappings", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    thresholds <- data.frame(threshold = c(1, 2))
    p <- bbplot(d, bb_aes(x, y, col = g)) +
        bb_hline(mapping = bb_aes(yintercept = threshold), data = thresholds)
    with_test_pdf(expect_error(print(p), "not found"))

    ok <- bbplot(d, bb_aes(x, y, col = g)) +
        bb_hline(mapping = bb_aes(yintercept = threshold), data = thresholds,
                 inherit = FALSE)
    with_test_pdf(expect_silent(print(ok)))
})

test_that("ribbon requires a numeric band", {
    d <- data.frame(x = factor(c("a", "b")), lo = 1:2, hi = 2:3)
    p <- bbplot(d, bb_aes(x = x, ymin = lo, ymax = hi))
    expect_error(with_test_pdf(print(p + bb_ribbon())),
                 "requires numeric")
})

test_that("a constant border applies to every ribbon group", {
    d <- data.frame(x = rep(1:2, 2), lo = c(0, 0, 1, 1), hi = c(1, 1, 2, 2),
                    g = rep(c("a", "b"), each = 2))
    rec <- new.env()
    rec$calls <- list()
    options(plotbb.test.rec = rec)
    trace("polygon", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$calls[[length(.rec$calls) + 1L]] <- border
    }), print = FALSE)
    on.exit({
        untrace("polygon", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x = x, ymin = lo, ymax = hi, group = g)) +
        bb_ribbon(border = "black", alpha = 0.2)
    with_test_pdf(print(p))
    expect_length(rec$calls, 2)
    expect_equal(unlist(rec$calls), c("black", "black"))
})

test_that("ribbon renders under facets with free scales", {
    d <- data.frame(x = rep(1:4, 2), lo = c(1, 2, 3, 4, 10, 12, 14, 16),
                    hi = c(2, 3, 4, 5, 12, 14, 16, 18),
                    f = rep(c("a", "b"), each = 4))
    p <- bbplot(d, bb_aes(x = x, ymin = lo, ymax = hi)) +
        bb_ribbon(fill = "steelblue", alpha = 0.2) +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")
    with_test_pdf(expect_silent(print(p)))
})

test_that("ribbon reports missing mappings", {
    d <- data.frame(x = 1:3, y = 1:3)
    expect_error(
        with_test_pdf(print(bbplot(d, bb_aes(x, y)) + bb_ribbon())),
        "requires `x`, `ymin` and `ymax`"
    )
})

test_that("ribbon keeps the order of the `+` chain", {
    d <- data.frame(x = 1:4, ymin = rep(0, 4), ymax = rep(2, 4))
    p <- bbplot(d, bb_aes(x)) +
        bb_ribbon(bb_aes(ymin = ymin, ymax = ymax)) +
        bb_point(bb_aes(y = ymin))
    expect_identical(names(p$layer), c("ribbon layer", "point layer"))
    with_test_pdf(expect_silent(print(p)))
})

## ---- reference lines ------------------------------------------------------

test_that("hline and vline do not change the ranges by default", {
    d <- data.frame(x = 1:3, y = 1:3)
    base <- bbplot(d, bb_aes(x, y))
    plain <- plotbb:::bb_plot_limits(base + bb_hline(100))
    expanded <- plotbb:::bb_plot_limits(base + bb_hline(100, expand_limits = TRUE))
    expect_true(plain$ylim[[2]] < 100)
    expect_true(expanded$ylim[[2]] >= 100)

    plain_x <- plotbb:::bb_plot_limits(base + bb_vline(100))
    expanded_x <- plotbb:::bb_plot_limits(base + bb_vline(100, expand_limits = TRUE))
    expect_true(plain_x$xlim[[2]] < 100)
    expect_true(expanded_x$xlim[[2]] >= 100)
})

test_that("reference lines draw at the declared positions", {
    rec <- new.env()
    rec$calls <- list()
    options(plotbb.test.rec = rec)
    trace("abline", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$calls[[length(.rec$calls) + 1L]] <- list(h = h, v = v, dots = list(...))
    }), print = FALSE)
    on.exit({
        untrace("abline", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y)) +
        bb_hline(c(0, 1), col = c("red", "blue"), lty = c(2, 3))
    with_test_pdf(print(p))
    calls <- rec$calls
    expect_length(calls, 1L)
    expect_equal(calls[[1]]$h, c(0, 1))
    expect_equal(calls[[1]]$dots$col, c("red", "blue"))
    expect_equal(calls[[1]]$dots$lty, c(2, 3))
})

test_that("reference lines take a mapped intercept", {
    rec <- new.env()
    rec$calls <- list()
    options(plotbb.test.rec = rec)
    trace("abline", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$calls[[length(.rec$calls) + 1L]] <- list(h = h)
    }), print = FALSE)
    on.exit({
        untrace("abline", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    d <- data.frame(x = 1:3, y = 1:3, threshold = c(0.5, 1.5, 2.5))
    p <- bbplot(d, bb_aes(x, y, yintercept = threshold)) +
        bb_point() + bb_hline()
    with_test_pdf(print(p))
    calls <- rec$calls
    expect_equal(calls[[1]]$h, c(0.5, 1.5, 2.5))
})

test_that("a mapped reference line requires data", {
    p <- as.bbplot(function() graphics::plot(1:3))
    expect_error(p + bb_hline(mapping = bb_aes(yintercept = threshold)),
                 "requires data")
})

test_that("a mapped intercept carries per-line colour and line type", {
    rec <- new.env()
    rec$calls <- list()
    options(plotbb.test.rec = rec)
    trace("abline", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$calls[[length(.rec$calls) + 1L]] <- list(h = h, dots = list(...))
    }), print = FALSE)
    on.exit({
        untrace("abline", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    d <- data.frame(x = 1:2, y = 1:2, threshold = c(0.5, 1.5),
                    colour = c("red", "blue"), styles = c(2, 3))
    p <- bbplot(d, bb_aes(x, y, yintercept = threshold,
                          col = colour, lty = styles)) +
        bb_hline() +
        bb_scale_col_manual(c(red = "red", blue = "blue"))
    with_test_pdf(print(p))
    expect_equal(rec$calls[[1]]$dots$col, c("red", "blue"))
    expect_equal(rec$calls[[1]]$dots$lty, c(2, 3))
})

test_that("invalid reference intercepts are reported", {
    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y))
    with_test_pdf(expect_error(print(p + bb_hline("a")), "must be numeric"))
    with_test_pdf(expect_error(print(p + bb_hline(NA_real_)),
                               "no finite intercept"))
    expect_error(p + bb_hline(0, expand_limits = c(TRUE, FALSE)),
                 "TRUE or FALSE")
    expect_error(p + bb_vline(0, expand_limits = NA),
                 "TRUE or FALSE")
})

test_that("a fixed reference line works on a canvas without data", {
    p <- as.bbplot(function() graphics::plot(1:3))
    with_test_pdf(expect_silent(print(p + bb_hline(2, col = "red"))))
})

test_that("reference lines render per facet panel", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_hline(0, col = "red") + bb_vline(3, col = "blue") +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")
    with_test_pdf(expect_silent(print(p)))
})

test_that("fill is rejected for reference lines", {
    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y))
    expect_error(p + bb_hline(0, fill = "red"), "not an aesthetic")
    expect_error(p + bb_vline(0, fill = "red"), "not an aesthetic")
})

## ---- facet boundaries -----------------------------------------------------

test_that("a layout larger than the number of panels is allowed", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_wrap(~f, nrow = 3, ncol = 2)
    with_test_pdf(expect_silent(print(p)))
})

test_that("facet layout rejects invalid nrow/ncol", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    p <- bbplot(d, bb_aes(x, y)) + bb_point()

    with_test_pdf(
        expect_error(print(p + bb_facet_wrap(~f, nrow = 0, ncol = 1)),
                     "positive integer")
    )
    with_test_pdf(
        expect_error(print(p + bb_facet_wrap(~f, nrow = 1, ncol = 1)),
                     "smaller than the number of panels")
    )
    with_test_pdf(
        expect_error(print(p + bb_facet_wrap(~f, ncol = 1.5)),
                     "positive integer")
    )
})

test_that("a summary layer does not inherit the plot-level canvas range", {
    d <- data.frame(x = c("a", "b", "a"))
    p <- bbplot(mtcars, bb_aes(mpg, disp)) +
        bb_bar(bb_aes(x), data = d, stat = "count")
    built <- bb_build(p)
    expect_lt(max(built$limits$xlim), 10)
    expect_lt(max(built$limits$ylim), 10)
})
