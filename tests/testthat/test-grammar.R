## 0.2.5 grammar contract tests.

## ---- data and mapping inheritance ----------------------------------------

test_that("a layer mapping overrides the global mapping aesthetic by aesthetic", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    p <- bbplot(d, bb_aes(x, y, col = g))
    q <- p + bb_point(bb_aes(col = factor(g)))

    m <- layer_mapping_of(q)
    expect_named(m, sort(c("x", "y", "col")), ignore.order = TRUE)
    expect_equal(rlang::as_label(m$col), "factor(g)")
    expect_equal(rlang::as_label(m$x), "x")
})

test_that("inherit = FALSE drops the global mapping instead of silently using it", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    p <- bbplot(d, bb_aes(x, y))

    q <- p + bb_point(bb_aes(x, y, col = g), inherit = FALSE)
    expect_named(layer_mapping_of(q), sort(c("x", "y", "col")), ignore.order = TRUE)

    expect_error(p + bb_point(bb_aes(col = g), inherit = FALSE),
                 "maps to an empty vector")
})

test_that("a layer without its own mapping inherits everything", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    p <- bbplot(d, bb_aes(x, y, col = g))
    q <- p + bb_point()
    expect_equal(rlang::as_label(layer_mapping_of(q)$col), "g")
})

test_that("layer data is a first-class input for the plot ranges", {
    d <- data.frame(x = 1:3, y = 1:3)
    extra <- data.frame(x = c(10, 20), y = c(50, 60))
    p <- bbplot(d, bb_aes(x, y)) + bb_point(data = extra, pch = 19)
    lim <- plotbb:::bb_plot_limits(p)
    expect_true(lim$xlim[2] > 20)
    expect_true(lim$ylim[2] > 60)
})

test_that("layer data participates in facet panels", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    extra <- d
    extra$y <- d$y + 100
    p <- bbplot(d, bb_aes(x, y)) +
        bb_point(data = extra, pch = 19) +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")
    with_test_pdf(expect_silent(print(p)))
})

test_that("layer data is faceted by its own facet column, not by row position", {
    seen <- list()
    local_mocked_bindings(
        points = function(x, y, ...) {
            seen[[length(seen) + 1L]] <<- x
            invisible(NULL)
        },
        .package = "plotbb"
    )
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    extra <- data.frame(x = c(1, 5, 9), y = c(10, 20, 30),
                        f = c("a", "b", "a"))
    p <- bbplot(d, bb_aes(x, y)) +
        bb_point(data = extra, pch = 19) +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")
    with_test_pdf(print(p))
    expect_equal(seen[[1]], c(1, 9))
    expect_equal(seen[[2]], 5)
})

test_that("multi-variable facets subset layer data by the facet expression", {
    seen <- list()
    local_mocked_bindings(
        points = function(x, y, ...) {
            seen[[length(seen) + 1L]] <<- x
            invisible(NULL)
        },
        .package = "plotbb"
    )
    d <- data.frame(x = 1:8, y = 1:8,
                    f = rep(c("a", "b"), each = 4),
                    g = rep(c("u", "v"), 4))
    extra <- data.frame(x = c(1, 5, 9), y = c(10, 20, 30),
                        f = c("a", "b", "a"), g = c("u", "v", "v"))
    p <- bbplot(d, bb_aes(x, y)) +
        bb_point(data = extra, pch = 19) +
        bb_facet_wrap(~paste(f, g, sep = "/"), ncol = 2, scales = "free_y")
    with_test_pdf(print(p))
    ## panels in sorted key order: a/u -> 1, a/v -> 9, b/v -> 5; b/u has no rows
    expect_equal(seen[[1]], 1)
    expect_equal(seen[[2]], 9)
    expect_equal(seen[[3]], 5)
    expect_length(seen, 3)
})

test_that("an unsplittable layer data set is reported, not silently drawn", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    foreign <- data.frame(x = c(10, 20), y = c(30, 40))
    p <- bbplot(d, bb_aes(x, y)) +
        bb_point(data = foreign, pch = 19) +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")
    with_test_pdf(
        expect_warning(print(p), "does not carry the facet variable")
    )
})

test_that("a layer is absent from a panel where its data has no rows", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    extra <- data.frame(x = c(1, 2), y = c(10, 20), f = c("a", "a"))
    p <- bbplot(d, bb_aes(x, y)) +
        bb_point(data = extra, pch = 19) +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")
    with_test_pdf(expect_silent(print(p)))
})

test_that("layer data that cannot become a data frame is rejected", {
    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y))
    expect_error(p + bb_point(data = new.env()),
                 "must be a data frame")
})

## ---- constants versus mappings -------------------------------------------

test_that("discrete x plus numeric y supports grouped histogram and density", {
    d <- data.frame(group = factor(rep(c("a", "b"), each = 6)),
                    value = c(1:6, 2:7),
                    colour = factor(rep(c("red", "blue"), each = 6)))
    d$value[c(2, 8)] <- NA_real_
    h <- bbplot(d, bb_aes(group, value)) + bb_hist()
    den <- bbplot(d, bb_aes(group, value)) + bb_density()
    by_colour <- bbplot(d, bb_aes(group, value, col = colour)) + bb_hist()
    with_test_pdf({
        expect_silent(print(h))
        expect_silent(print(den))
        expect_silent(print(by_colour))
    })
})

test_that("grouped histogram keeps data and colour alignment after filtering", {
    d <- data.frame(group = factor(rep(c("a", "b"), each = 5)),
                    value = c(1, NA, Inf, 4, 5, 2, 3, NA, 6, 7),
                    colour = factor(rep(c("low", "high"), each = 5)))
    p <- bbplot(d, bb_aes(group, value, col = colour)) + bb_hist()
    with_test_pdf(expect_silent(print(p)))

    bad_col <- bbplot(d, bb_aes(group, value)) + bb_hist(col = c("red", "blue", "green"))
    expect_error(with_test_pdf(print(bad_col)), "must have length 1 or the number of groups")
})

test_that("a constant removes the mapping of that aesthetic for the layer", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    p <- bbplot(d, bb_aes(x, y, col = g))

    merged <- plotbb:::bb_layer_aesthetics(p, bb_aes(col = factor(g)), TRUE,
                                           list(col = "tomato"))
    expect_named(merged, c("x", "y"))
    expect_null(merged$col)

    ## a constant on a *different* aesthetic does not drop the colour mapping
    kept <- plotbb:::bb_layer_aesthetics(p, NULL, TRUE, list(pch = 19))
    expect_equal(rlang::as_label(kept$col), "g")

    ## inherit = FALSE still drops the global mapping entirely
    none <- plotbb:::bb_layer_aesthetics(p, bb_aes(col = factor(g)), FALSE,
                                         list(col = "tomato"))
    expect_length(none, 0L)
})

test_that("length-n constants follow the rows that survive validation", {
    seen <- NULL
    local_mocked_bindings(
        points = function(x, y, col = NULL, pch = NULL, ...) {
            seen <<- list(x = x, col = col, pch = pch)
            invisible(NULL)
        },
        .package = "plotbb"
    )
    d <- data.frame(x = c(1, NA, 3), y = c(1, 2, 3))
    p <- bbplot(d, bb_aes(x, y)) +
        bb_point(col = c("red", "green", "blue"), pch = c(1, 2, 3))
    with_test_pdf(print(p))
    expect_equal(seen$x, c(1, 3))
    expect_equal(seen$col, c("red", "blue"))
    expect_equal(seen$pch, c(1, 3))
})

test_that("a constant fill colours a density layer", {
    d <- data.frame(x = c(stats::rnorm(20), stats::rnorm(20, 3)),
                    g = factor(rep(c("a", "b"), each = 20)))
    mapping <- plotbb:::bb_as_mapping_list(bb_aes(x, col = g))
    plot <- bbplot(d, bb_aes(x, col = g))

    expect_identical(
        plotbb:::bb_density_colour(list(fill = "steelblue"), mapping, d, plot),
        "steelblue"
    )
    expect_identical(
        plotbb:::bb_density_colour(list(col = "red"), mapping, d, plot),
        "red"
    )
    expect_length(
        plotbb:::bb_density_colour(list(), mapping, d, plot),
        nrow(d)
    )
    with_test_pdf(expect_silent(
        print(plot + bb_density(fill = "steelblue"))
    ))
})

test_that("fill is accepted only by layers that have an interior", {
    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y))

    expect_error(p + bb_point(fill = "red"), "not an aesthetic")
    expect_error(p + bb_line(fill = "red"), "not an aesthetic")

    with_test_pdf(expect_silent(print(p + bb_boxplot(fill = "grey90"))))
    with_test_pdf(expect_silent(print(p + bb_hist(fill = "grey90"))))
    with_test_pdf(expect_silent(print(p + bb_bar(fill = "grey90"))))
    with_test_pdf(expect_silent(print(p + bb_tile(fill = "grey90"))))
    with_test_pdf(expect_silent(print(p + bb_density(fill = "steelblue"))))

    ## a custom layer forwards fill to its own drawing function
    seen <- NULL
    custom <- p + bb_layer(function(x, y, fill = NULL, ...) {
        seen <<- fill
        invisible(NULL)
    }, fill = "tomato")
    with_test_pdf(print(custom))
    expect_identical(seen, "tomato")
})

test_that("statistical layers share one grouping precedence", {
    d <- data.frame(x = 1:4, g = factor(c("a", "b", "a", "b")))
    mapping <- plotbb:::bb_as_mapping_list(bb_aes(x, group = g, col = g))
    values <- list(x = d$x, group = NULL)

    expect_equal(plotbb:::bb_stat_group(values, mapping, d), d$g)

    ## a discrete x groups by itself, and wins over the group mapping
    discrete <- list(x = d$x, group = d$g)
    expect_equal(plotbb:::bb_stat_group(discrete, list(), d), d$g)

    ## grouping by a mapped colour when no group is given
    colour_only <- plotbb:::bb_as_mapping_list(bb_aes(x, col = g))
    expect_equal(plotbb:::bb_stat_group(values, colour_only, d), d$g)
})

test_that("every exported layer accepts inherit", {
    layers <- c("bb_point", "bb_line", "bb_text", "bb_segment",
                "bb_errorbar", "bb_bar", "bb_boxplot", "bb_hist",
                "bb_density", "bb_tile", "bb_lm", "bb_signif")
    for (layer in layers) {
        fun <- get(layer, envir = asNamespace("plotbb"))
        expect_true("inherit" %in% names(formals(fun)),
                    info = layer)
    }
    expect_true("inherit" %in% names(formals(plotbb::bb_layer)))
})

test_that("a constant aesthetic is not repainted by a later colour scale", {
    seen <- NULL
    local_mocked_bindings(
        points = function(x, y, col = NULL, ...) {
            seen <<- col
            invisible(NULL)
        },
        .package = "plotbb"
    )
    p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl))) +
        bb_point(pch = 19, col = "tomato") +
        bb_scale_col_manual(c(`4` = "red", `6` = "green", `8` = "blue"))
    with_test_pdf(print(p))
    expect_identical(unique(seen), "tomato")
})

test_that("a mapped aesthetic follows the last scale in the `+` chain", {
    seen <- NULL
    local_mocked_bindings(
        points = function(x, y, col = NULL, ...) {
            seen <<- col
            invisible(NULL)
        },
        .package = "plotbb"
    )
    p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl))) +
        bb_scale_col_manual(c(`4` = "red", `6` = "green", `8` = "blue")) +
        bb_point(pch = 19) +
        bb_scale_col_manual(c(`4` = "black", `6` = "black", `8` = "black"))
    with_test_pdf(print(p))
    expect_identical(unique(seen), "black")
})

test_that("a mapped aesthetic still follows the colour scale", {
    p <- bbplot(mtcars, bb_aes(mpg, disp)) +
        bb_point(bb_aes(col = factor(cyl)), data = mtcars) +
        bb_scale_col_manual(c(`4` = "red", `6` = "green", `8` = "blue"))
    with_test_pdf(expect_silent(print(p)))
})

test_that("constants are not sent through the scale and keep their length", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    p <- bbplot(d, bb_aes(x, y, col = g))
    q <- p + bb_point(col = "tomato", cex = 2)
    with_test_pdf(expect_silent(print(q)))

    expect_error(bbplot(d, bb_aes(x, y, col = g)) + bb_point(col = c("a", "b")),
                 "`col` in `bb_point` has length 2")
})

test_that("a constant label wins over a mapped one", {
    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y, label = x)) + bb_text(label = "fixed")
    with_test_pdf(expect_silent(print(p)))
})

## ---- draw order -----------------------------------------------------------

test_that("layers are drawn in the order they were added", {
    calls <- character(0)
    d <- data.frame(x = 1:3, y = 1:3)
    mark <- function(tag) {
        function(x, y, ...) {
            calls <<- c(calls, tag)
            invisible(NULL)
        }
    }
    p <- bbplot(d, bb_aes(x, y)) +
        bb_layer(mark("first"), name = "first") +
        bb_layer(mark("second"), name = "second") +
        bb_layer(mark("third"), name = "third")
    with_test_pdf(print(p))
    expect_identical(calls, c("first", "second", "third"))
    expect_identical(names(p$layer), c("first", "second", "third"))
})

test_that("an error in a layer propagates and keeps the graphics state", {
    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y)) +
        bb_layer(function(x, y, ...) stop("boom from a custom layer"))
    with_test_pdf({
        before <- graphics::par(no.readonly = TRUE)
        expect_error(print(p), "boom from a custom layer")
        after <- graphics::par(no.readonly = TRUE)
        expect_identical(after[c("mar", "mgp", "las", "mfrow", "new")],
                         before[c("mar", "mgp", "las", "mfrow", "new")])
    })
})

## ---- custom layer entry point --------------------------------------------

test_that("bb_layer draws from public API only", {
    d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
    drawn <- NULL
    p <- bbplot(d, bb_aes(x, y, col = g)) +
        bb_layer(function(x, y, col, ...) {
            drawn <<- list(x = x, y = y, col = col)
            graphics::points(x, y, col = col, pch = 19)
        })
    with_test_pdf(print(p))
    expect_equal(drawn$x, 1:3)
    expect_length(drawn$col, 3)
})

test_that("bb_layer validates its input", {
    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y))
    expect_error(bb_layer("not a function"), "`draw` must be a function")
    expect_silent(with_test_pdf(print(p + bb_layer(function(x, y, ...) NULL))))
})

test_that("bb_layer requires a data frame it can use", {
    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y))
    p2 <- p + bb_point(pch = 19)
    expect_silent(p2 + bb_layer(function(x, y, ...) graphics::points(x, y)))
})

test_that("bb_layer limits extend the canvas for coordinates it computes itself", {
    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y)) +
        bb_layer(function(x, y, ...) graphics::abline(h = min(y) - 5),
                 limits = list(y = c(-10, 0)))
    lim <- plotbb:::bb_plot_limits(p)
    expect_true(lim$ylim[1] <= -10)
    with_test_pdf(expect_silent(print(p)))
})

test_that("bb_layer limits are optional per axis and validated", {
    draw <- function(x, y, ...) NULL
    expect_silent(bb_layer(draw, limits = list(x = c(10, 0))))
    layer <- bb_layer(draw, limits = list(x = c(10, 0)))
    expect_equal(layer$limits$x, c(0, 10))

    expect_error(bb_layer(draw, limits = c(0, 1)), "must be a list")
    expect_error(bb_layer(draw, limits = list(z = c(0, 1))), "may only contain")
    expect_error(bb_layer(draw, limits = list(x = 1)), "two finite numbers")
    expect_error(bb_layer(draw, limits = list(x = c(0, NA))), "two finite numbers")
    expect_error(bb_layer(draw, limits = list()), "at least one of")
})

test_that("declared limits are not passed to the drawing function", {
    seen <- NULL
    d <- data.frame(x = 1:3, y = 1:3)
    p <- bbplot(d, bb_aes(x, y)) +
        bb_layer(function(x, y, ...) {
            seen <<- names(list(...))
            invisible(NULL)
        }, limits = list(x = c(0, 10), y = c(0, 10)))
    with_test_pdf(print(p))
    expect_false("limits" %in% seen)
})

## ---- `+` boundary ---------------------------------------------------------

test_that("`+` refuses to combine two complete plots", {
    p <- bbplot(mtcars, bb_aes(mpg, disp))
    expect_error(p + p, "does not combine two complete plots")
})

test_that("`+` with NULL returns the plot unchanged", {
    p <- bbplot(mtcars, bb_aes(mpg, disp))
    expect_identical(p + NULL, p)
})

## ---- escape hatch ---------------------------------------------------------

test_that("functions, formulas and expressions remain valid layers", {
    p <- bbplot(mtcars, bb_aes(mpg, disp))
    with_test_pdf({
        expect_silent(print(p + (~points(20, 200, pch = 19))))
        expect_silent(print(p + function() graphics::points(20, 200)))
        expect_silent(print(p + quote(graphics::points(20, 200))))
    })
})
