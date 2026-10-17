## 0.3.0 scale and legend contract tests.

train <- function(p) plotbb:::bb_train_scales(p)
scales_of <- function(p) plotbb:::bb_get_scales(p)
map_of <- function(p, aesthetic, values) {
    plotbb:::bb_map_aesthetic(p, aesthetic, values)
}

## ---- training -------------------------------------------------------------

test_that("one aesthetic trains from the plot and from every layer", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    extra <- data.frame(x = 4:5, y = 5:6, g = c("c", "d"))
    p <- bbplot(d, bb_aes(x, y, col = g)) +
        bb_point() +
        bb_point(data = extra, pch = 17) +
        bb_scale_col_manual(c(a = "red", b = "green", c = "blue", d = "yellow"))
    p <- train(p)

    expect_equal(scales_of(p)$col$domain, c("a", "b", "c", "d"))
    expect_equal(map_of(p, "col", c("a", "d")), c("red", "yellow"))
})

test_that("the same value gets the same colour in different layers", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    extra <- data.frame(x = 4, y = 4, g = "c")
    p <- bbplot(d, bb_aes(x, y, col = g)) +
        bb_point() +
        bb_point(data = extra, pch = 17) +
        bb_scale_col_manual(c(a = "red", b = "green", c = "blue"))
    p <- train(p)
    expect_identical(map_of(p, "col", "c"), "blue")
})

test_that("factor levels keep their order and drop is explicit", {
    d <- data.frame(x = 1:3, y = 1:3,
                    f = factor(c("b", "a", "b"), levels = c("c", "a", "b")))
    p <- bbplot(d, bb_aes(x, y, col = f)) + bb_point() +
        bb_scale_col_manual(c(c = "red", a = "green", b = "blue"))
    expect_equal(scales_of(train(p))$col$domain, c("a", "b"))

    p2 <- bbplot(d, bb_aes(x, y, col = f)) + bb_point() +
        bb_scale_col_manual(c(c = "red", a = "green", b = "blue"), drop = FALSE)
    expect_equal(scales_of(train(p2))$col$domain, c("c", "a", "b"))
})

test_that("character levels keep first-appearance order", {
    d <- data.frame(x = 1:4, y = 1:4, g = c("z", "a", "z", "m"))
    p <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_scale_col_manual(c(z = "red", a = "green", m = "blue"))
    expect_equal(scales_of(train(p))$col$domain, c("z", "a", "m"))
})

test_that("constants never train a scale", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    ## the plot maps no colour; the only colour is a layer constant
    p <- bbplot(d, bb_aes(x, y)) +
        bb_point(col = "tomato") +
        bb_scale_col_manual(c(a = "red"))
    p <- train(p)
    expect_length(scales_of(p)$col$domain, 0)
})

## ---- discrete mapping -----------------------------------------------------

test_that("named manual values match by name, unmatched levels are na.value", {
    p <- bbplot(data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c")),
                bb_aes(x, y, col = g)) +
        bb_point() +
        bb_scale_col_manual(c(a = "red"), na.value = "grey80")
    p <- train(p)
    expect_equal(map_of(p, "col", c("a", "b")), c("red", "grey80"))
    ## a value outside the trained domain is reported, not silently recycled
    expect_warning(
        expect_identical(map_of(p, "col", "z"), "grey80"),
        "na.value"
    )
})

test_that("unnamed manual values map in level order", {
    p <- bbplot(data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c")),
                bb_aes(x, y, col = g)) +
        bb_point() +
        bb_scale_col_manual(c("red", "green", "blue"))
    p <- train(p)
    expect_equal(map_of(p, "col", c("a", "b", "c")),
                 c("red", "green", "blue"))
})

test_that("discrete limits define the shown levels", {
    p <- bbplot(data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c")),
                bb_aes(x, y, col = g)) +
        bb_point() +
        bb_scale_col_manual(c(a = "red", b = "green", c = "blue"),
                            limits = c("a", "b"))
    p <- train(p)
    expect_equal(scales_of(p)$col$domain, c("a", "b"))
    expect_warning(
        expect_identical(map_of(p, "col", "c"), "grey50"),
        "na.value"
    )
})

## ---- continuous mapping ---------------------------------------------------

test_that("a continuous scale uses one domain for layers and legend", {
    d <- data.frame(x = 1:3, y = 1:3, v = c(0, 5, 10))
    p <- bbplot(d, bb_aes(x, y, col = v)) + bb_point() +
        bb_scale_col_gradient(low = "white", high = "black",
                              limits = c(0, 5))
    p <- train(p)
    expect_equal(scales_of(p)$col$domain, c(0, 5))

    expected <- scales::col_numeric(c("white", "black"), domain = c(0, 5))(5)
    expect_identical(map_of(p, "col", 5), expected)
})

test_that("oob squish clamps and censor maps to na.value", {
    d <- data.frame(x = 1:3, y = 1:3, v = c(0, 5, 10))
    squish <- train(bbplot(d, bb_aes(x, y, col = v)) + bb_point() +
        bb_scale_col_gradient(limits = c(0, 5)))
    censor <- train(bbplot(d, bb_aes(x, y, col = v)) + bb_point() +
        bb_scale_col_gradient(limits = c(0, 5), oob = "censor"))

    top <- scales::col_numeric(c("#132B43", "#56B1F7"), c(0, 5))(5)
    expect_identical(map_of(squish, "col", 10), top)
    expect_identical(map_of(censor, "col", 10), "grey50")
})

test_that("zero-width and all-missing domains are defined", {
    d <- data.frame(x = 1:3, y = 1:3, v = c(2, 2, 2))
    p <- train(bbplot(d, bb_aes(x, y, col = v)) + bb_point() +
        bb_scale_col_gradient())
    expect_equal(scales_of(p)$col$domain, c(1.5, 2.5))

    d2 <- data.frame(x = 1:3, y = 1:3, v = c(NA, NA, NA))
    p2 <- train(bbplot(d2, bb_aes(x, y, col = v)) + bb_point() +
        bb_scale_col_gradient())
    expect_null(scales_of(p2)$col$domain)
})

test_that("a continuous colour scale needs a numeric mapping", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    p <- train(bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_scale_col_gradient())
    expect_error(map_of(p, "col", c("a", "b")), "numeric")
})

test_that("default mappings survive missing and constant values", {
    d <- data.frame(x = 1:3, y = 1:3,
                    v = c(NA, NA, NA), w = c(5, 5, 5),
                    g = factor(c(NA, NA, NA)))
    p1 <- bbplot(d, bb_aes(x, y, col = v)) + bb_point() + bb_legend()
    p2 <- bbplot(d, bb_aes(x, y, col = w)) + bb_point() + bb_legend()
    p3 <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() + bb_legend()
    for (p in list(p1, p2, p3)) {
        with_test_pdf(expect_silent(print(p)))
    }
    ## the default mapping never produces Inf/NaN colours
    expect_identical(plotbb:::bb_map_aesthetic(p1, "col", c(NA, NA)),
                     c("grey50", "grey50"))
})

## ---- col and fill are independent -----------------------------------------

test_that("fill has its own scale and is not borrowed from col", {
    d <- data.frame(x = 1:4, y = 1:4,
                    g = c("a", "b", "a", "b"), v = 1:4)
    p <- bbplot(d, bb_aes(x, y, fill = g)) + bb_tile() +
        bb_scale_fill_manual(c(a = "red", b = "blue")) +
        bb_scale_col_manual(c(a = "orange", b = "purple"))
    p <- train(p)

    expect_equal(map_of(p, "fill", c("a", "b")), c("red", "blue"))
    ## the col scale was trained on no col mapping: its values are not covered
    suppressWarnings(
        expect_equal(map_of(p, "col", c("a", "b")), c("grey50", "grey50"))
    )
})

test_that("fill scales reach density, tile and bar", {
    d <- data.frame(x = c(rnorm(20), rnorm(20, 3)),
                    y = c(1:20, 1:20),
                    g = factor(rep(c("a", "b"), each = 20)))
    for (layer in list(
        bbplot(d, bb_aes(x, fill = g)) + bb_density(alpha = 0.4) +
            bb_scale_fill_manual(c(a = "red", b = "blue")),
        bbplot(d, bb_aes(y, y, fill = g)) + bb_tile() +
            bb_scale_fill_manual(c(a = "red", b = "blue")),
        bbplot(d, bb_aes(g, y, fill = g)) + bb_bar(stat = "identity") +
            bb_scale_fill_manual(c(a = "red", b = "blue"))
    )) {
        with_test_pdf(expect_silent(print(layer)))
    }
})

test_that("a constant fill removes the fill mapping for summary layers", {
    d <- data.frame(x = factor(c("a", "a", "b", "b")), y = c(1, 2, 3, 4),
                    g = factor(c("u", "v", "u", "v")))
    for (layer in list(bb_boxplot(fill = "grey90"),
                       bb_bar(stat = "count", fill = "grey90"),
                       bb_hist(fill = "grey90"))) {
        p <- bbplot(d, bb_aes(x, y, fill = g)) + layer
        obj <- Filter(function(o) inherits(o, "bb_layer"), p$adds)[[1]]
        mapping <- plotbb:::bb_layer_aesthetics(p, obj$mapping, obj$inherit,
                                                obj$params)
        expect_null(mapping$fill)
        with_test_pdf(expect_silent(print(p)))
    }
})

## ---- symbol scales --------------------------------------------------------

test_that("manual symbol scales train and map by level", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    p <- bbplot(d, bb_aes(x, y, pch = g, cex = g)) + bb_point() +
        bb_scale_pch_manual(c(a = 1, b = 2, c = 3)) +
        bb_scale_cex_manual(c(a = 1, b = 1.5, c = 2))
    p <- train(p)
    expect_equal(map_of(p, "pch", c("a", "c")), c(1, 3))
    expect_equal(map_of(p, "cex", c("a", "c")), c(1, 2))
    with_test_pdf(expect_silent(print(p + bb_legend(aesthetic = "pch"))))
})

test_that("mapped lty is applied by line-like layers", {
    d <- data.frame(x = 1:6, y = c(1, 2, 2, 3, 4, 4),
                    g = rep(c("a", "b"), each = 3))

    rec <- new.env()
    options(plotbb.test.rec = rec)
    trace("segments", where = asNamespace("graphics"),
          tracer = quote(assign("lty", list(lty = lty),
                                envir = getOption("plotbb.test.rec"))),
          print = FALSE)
    on.exit({
        untrace("segments", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x, y, lty = g)) + bb_point() +
        bb_segment(bb_aes(xend = x + 0.3, yend = y + 0.3)) +
        bb_scale_lty_manual(c(a = 2, b = 3))
    with_test_pdf(print(p))
    expect_setequal(unique(rec$lty$lty), c(2, 3))
})

test_that("boxplot applies lty per box in box order", {
    rec <- new.env()
    options(plotbb.test.rec = rec)
    trace("boxplot", where = asNamespace("graphics"),
          tracer = quote(assign("dots", list(...),
                                envir = getOption("plotbb.test.rec"))),
          print = FALSE)
    on.exit({
        untrace("boxplot", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(mtcars, bb_aes(factor(cyl), mpg, lty = factor(cyl))) +
        bb_boxplot() +
        bb_scale_lty_manual(c(`4` = 1, `6` = 2, `8` = 3))
    with_test_pdf(print(p))
    expect_equal(rec$dots$lty, c(1, 2, 3))
})

test_that("lm draws one line type per group", {
    rec <- new.env()
    options(plotbb.test.rec = rec)
    trace("segments", where = asNamespace("graphics"),
          tracer = quote({
              r <- getOption("plotbb.test.rec")
              if (!exists("vals", envir = r)) assign("vals", numeric(), envir = r)
              assign("vals", c(get("vals", envir = r), lty), envir = r)
          }), print = FALSE)
    on.exit({
        untrace("segments", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(mtcars, bb_aes(mpg, disp, group = gear,
                               lty = factor(gear))) +
        bb_lm(se = FALSE) +
        bb_scale_lty_manual(c(`3` = 1, `4` = 2, `5` = 3)) +
        bb_legend(aesthetic = "lty")
    with_test_pdf(print(p))
    expect_setequal(unique(rec$vals), c(1, 2, 3))
})

test_that("continuous cex maps into a range and clamps", {
    d <- data.frame(x = 1:3, y = 1:3, v = c(0, 5, 10))
    p <- train(bbplot(d, bb_aes(x, y, cex = v)) + bb_point() +
        bb_scale_cex_continuous(range = c(1, 2), limits = c(0, 5)))
    expect_equal(map_of(p, "cex", c(0, 5, 10)), c(1, 2, 2))
})

## ---- legends --------------------------------------------------------------

test_that("discrete legends and colour bars render inside and outside", {
    p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl))) + bb_point() +
        bb_scale_col_manual(c(`4` = "red", `6` = "green", `8` = "blue"))

    with_test_pdf(expect_silent(print(p + bb_legend())))
    with_test_pdf(expect_silent(print(p + bb_legend(outside = TRUE))))

    grad <- bbplot(mtcars, bb_aes(mpg, disp, col = hp)) + bb_point() +
        bb_scale_col_gradient()
    with_test_pdf(expect_silent(print(grad + bb_legend())))
    with_test_pdf(expect_silent(print(grad + bb_legend(orientation = "horizontal"))))
    with_test_pdf(expect_silent(print(grad + bb_legend(outside = TRUE))))
})

test_that("continuous colour bar honours breaks and labels", {
    p <- bbplot(mtcars, bb_aes(mpg, disp, col = hp)) + bb_point() +
        bb_scale_col_gradient(breaks = c(100, 200, 300),
                              labels = c("low", "mid", "high"))
    with_test_pdf(expect_silent(print(p + bb_legend())))
})

test_that("fill legend uses the fill scale", {
    d <- data.frame(x = c(rnorm(20), rnorm(20, 3)),
                    g = factor(rep(c("a", "b"), each = 20)))
    p <- bbplot(d, bb_aes(x, fill = g)) + bb_density(alpha = 0.4) +
        bb_scale_fill_manual(c(a = "red", b = "blue")) +
        bb_legend(aesthetic = "fill")
    with_test_pdf(expect_silent(print(p)))
})

test_that("unsupported legend positions are a clear error", {
    p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl))) + bb_point() +
        bb_scale_col_manual(c(`4` = "red", `6` = "green", `8` = "blue"))
    with_test_pdf(
        expect_error(print(p + bb_legend(position = "middle")),
                     "unsupported legend position")
    )

    grad <- bbplot(mtcars, bb_aes(mpg, disp, col = hp)) + bb_point() +
        bb_scale_col_gradient()
    with_test_pdf(
        expect_error(print(grad + bb_legend(position = "center")),
                     "unsupported legend position")
    )
})

test_that("outside legends survive narrow and wide devices", {
    p <- bbplot(mtcars, bb_aes(mpg, disp, col = hp)) + bb_point() +
        bb_scale_col_gradient() + bb_legend(outside = TRUE)
    for (width in c(4, 12)) {
        with_test_pdf(expect_silent(print(p)), width = width)
    }
})

test_that("multiple outside legends stack in the legend column", {
    p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl),
                               pch = factor(gear))) +
        bb_point() +
        bb_scale_col_manual(c(`4` = "red", `6` = "green", `8` = "blue")) +
        bb_scale_pch_manual(c(`3` = 1, `4` = 2, `5` = 3)) +
        bb_legend(aesthetic = "col", outside = TRUE) +
        bb_legend(aesthetic = "pch", outside = TRUE)
    with_test_pdf(expect_silent(print(p)), width = 6)
})

test_that("the old colour scale signatures still work", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    p <- bbplot(d, bb_aes(x, y, col = g)) + bb_point()
    with_test_pdf(expect_silent(
        print(p + bb_scale_col_manual(c("red", "green", "blue")) + bb_legend())
    ))
    with_test_pdf(expect_silent(
        print(p + bb_scale_col_palette("Set1") + bb_legend())
    ))
    with_test_pdf(expect_silent(
        print(bbplot(mtcars, bb_aes(mpg, disp, col = hp)) + bb_point() +
                  bb_scale_col_gradient() + bb_legend())
    ))
})

test_that("a faceted plot with an outside legend draws the legend once", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3),
                    g = rep(c("u", "v"), 3))
    p <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_scale_col_manual(c(u = "red", v = "blue")) +
        bb_legend(outside = TRUE) + bb_facet_wrap(~f, ncol = 2)
    with_test_pdf(expect_silent(print(p)))
})

test_that("all-missing variables are skipped, not type errors", {
    d <- data.frame(x = c(NA, NA, NA), y = 1:3)
    expect_warning(with_test_pdf(print(bbplot(d, bb_aes(x)) + bb_hist())),
                   "at least two")
    expect_warning(with_test_pdf(print(bbplot(d, bb_aes(x)) + bb_density())),
                   "at least two")

    d2 <- data.frame(x = 1:3, y = 1:3, v = c(NA, NA, NA))
    with_test_pdf(expect_silent(
        print(bbplot(d2, bb_aes(x, y, col = v)) + bb_point() +
                  bb_scale_col_gradient())
    ))
})

test_that("legend colours come from the trained scale", {
    d <- data.frame(x = 1:4, y = 1:4, g = factor(c("a", "b", "a", "b")))
    rec <- new.env()
    options(plotbb.test.rec = rec)
    trace("legend", where = asNamespace("graphics"),
          tracer = quote(assign("legend",
                                list(labels = legend, fill = fill, col = col),
                                envir = getOption("plotbb.test.rec"))),
          print = FALSE)
    on.exit({
        untrace("legend", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x, y, fill = g)) + bb_tile() +
        bb_scale_fill_manual(c(a = "red", b = "blue")) +
        bb_legend(aesthetic = "fill")
    with_test_pdf(print(p))

    expected <- plotbb:::bb_map_aesthetic(plotbb:::bb_train_scales(p), "fill",
                                          c("a", "b"))
    expect_equal(rec$legend$labels, c("a", "b"))
    expect_equal(unname(rec$legend$fill), unname(expected))

    rec$legend <- NULL
    p2 <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_scale_col_manual(c(a = "red", b = "blue")) + bb_legend()
    with_test_pdf(print(p2))
    expected_col <- plotbb:::bb_map_aesthetic(plotbb:::bb_train_scales(p2),
                                              "col", c("a", "b"))
    expect_equal(unname(rec$legend$col), unname(expected_col))
})

test_that("a continuous cex legend uses a few breaks, not every value", {
    d <- data.frame(x = 1:20, y = 1:20, v = seq(0, 100, length.out = 20))
    rec <- new.env()
    options(plotbb.test.rec = rec)
    trace("legend", where = asNamespace("graphics"),
          tracer = quote(assign("legend", list(labels = legend,
                                               pt.cex = pt.cex),
                                envir = getOption("plotbb.test.rec"))),
          print = FALSE)
    on.exit({
        untrace("legend", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x, y, cex = v)) + bb_point() +
        bb_scale_cex_continuous(range = c(0.8, 2)) +
        bb_legend(aesthetic = "cex")
    with_test_pdf(print(p))
    expect_equal(rec$legend$labels,
                 format(pretty(c(0, 100), n = 5), trim = TRUE))
    expect_true(all(diff(rec$legend$pt.cex) > 0))
})

## ---- facets ---------------------------------------------------------------

test_that("facets share one trained domain", {
    d <- data.frame(x = 1:6, y = 1:6,
                    f = rep(c("a", "b"), each = 3),
                    v = c(1, 2, 3, 100, 200, 300))
    p <- bbplot(d, bb_aes(x, y, col = v)) + bb_point() +
        bb_scale_col_gradient(low = "white", high = "black") +
        bb_facet_wrap(~f, ncol = 2)
    p <- train(p)
    expect_equal(scales_of(p)$col$domain, c(1, 300))
    with_test_pdf(expect_silent(print(p + bb_legend())))
})
