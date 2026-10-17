## 0.5.0 build and save contract tests.

## ---- bb_build -------------------------------------------------------------

test_that("bb_build inspects a plot without touching graphics state", {
    p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl))) +
        bb_point() +
        bb_scale_col_manual(c(`4` = "red", `6` = "green", `8` = "blue"))
    set.seed(42)
    seed <- .Random.seed

    probe_file <- tempfile(fileext = ".pdf")
    grDevices::pdf(probe_file)
    on.exit({
        if (grDevices::dev.cur() > 1L) grDevices::dev.off()
        unlink(probe_file)
    }, add = TRUE)
    device <- grDevices::dev.cur()
    par_before <- graphics::par(c("mar", "mfrow", "new"))

    built <- bb_build(p)

    expect_identical(as.integer(grDevices::dev.cur()), as.integer(device))
    expect_identical(graphics::par(c("mar", "mfrow", "new")), par_before)
    expect_identical(.Random.seed, seed)

    expect_equal(built$mapping,
                 c(x = "mpg", y = "disp", col = "factor(cyl)"))
    expect_equal(built$limits, plotbb:::bb_prepare_plot(p)$limits)
    expect_false(is.null(built$scales$col))
    expect_equal(built$scales$col$domain, c("4", "6", "8"))
    expect_match(built$layers[[1]]$name, "point layer")
    expect_equal(built$layers[[1]]$data_source, "plot")
    expect_true("col" %in% names(built$layers[[1]]$mapping))
})

test_that("repeated builds are equivalent", {
    p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl))) +
        bb_point() + bb_lm(se = FALSE)
    a <- bb_build(p)
    b <- bb_build(p)
    expect_identical(a$limits, b$limits)
    expect_identical(a$mapping, b$mapping)
    expect_identical(a$scales$col$domain, b$scales$col$domain)
    expect_identical(a$layers[[2]]$name, b$layers[[2]]$name)
})

test_that("bb_build reports facet panels and their limits", {
    d <- data.frame(x = 1:6, y = c(1, 2, 3, 100, 200, 300),
                    f = rep(c("a", "b"), each = 3))
    fixed <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_wrap(~f, ncol = 2)
    free <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")

    b_fixed <- bb_build(fixed)
    b_free <- bb_build(free)

    expect_equal(vapply(b_fixed$panels, `[[`, "", "key"), c("a", "b"))
    expect_equal(b_fixed$panels[[1]]$n, 3)
    expect_identical(b_fixed$panels[[1]]$ylim, b_fixed$limits$ylim)
    expect_identical(b_fixed$panels[[2]]$ylim, b_fixed$limits$ylim)

    expect_lt(b_free$panels[[1]]$ylim[[2]], 10)
    expect_gt(b_free$panels[[2]]$ylim[[2]], 200)
    expect_identical(b_free$panels[[1]]$xlim, b_free$limits$xlim)

    expect_equal(b_free$facet$vars, "f")
    expect_equal(b_free$facet$scales, "free_y")
    rds <- tempfile(fileext = ".rds")
    on.exit(unlink(rds), add = TRUE)
    expect_silent(saveRDS(b_free, rds))
    expect_true(file.exists(rds))
})

test_that("bb_build collects the warnings it muffles", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    foreign <- data.frame(x = c(10, 20), y = c(30, 40))
    p <- bbplot(d, bb_aes(x, y)) +
        bb_point(data = foreign, pch = 19) +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")

    built <- NULL
    expect_silent(built <- bb_build(p))
    expect_match(paste(built$warnings, collapse = " "),
                 "does not carry the facet variable")
})

test_that("bb_build works on a canvas without data", {
    p <- as.bbplot(function() graphics::plot(1:3)) +
        bb_hline(2, expand_limits = TRUE)
    built <- bb_build(p)
    expect_null(built$data)
    expect_true(built$limits$ylim[[2]] >= 2)
})

test_that("built facet limits are the limits that are drawn", {
    d <- data.frame(x = 1:6, y = c(1, 2, 3, 100, 200, 300),
                    f = rep(c("a", "b"), each = 3))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")
    built <- bb_build(p)

    rec <- new.env()
    rec$lims <- list()
    options(plotbb.test.rec = rec)
    trace("plot", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .dots <- list(...)
        if (!is.null(.dots$xlim) && !is.null(.dots$ylim)) {
            .rec$lims[[length(.rec$lims) + 1L]] <-
                list(xlim = .dots$xlim, ylim = .dots$ylim)
        }
    }), print = FALSE)
    on.exit({
        untrace("plot", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    with_test_pdf(print(p))
    expect_length(rec$lims, length(built$panels))
    for (i in seq_along(rec$lims)) {
        expect_equal(rec$lims[[i]]$xlim, built$panels[[i]]$xlim)
        expect_equal(rec$lims[[i]]$ylim, built$panels[[i]]$ylim)
    }
})

test_that("a third-party component is re-applied in every facet panel", {
    bb_note <- function(label) {
        structure(list(label = label), class = "bb_note")
    }
    method <- function(object, plot) {
        plot + bb_layer(function(x, y, ...) {
            graphics::text(mean(range(x)), mean(range(y)), object$label)
        })
    }
    registerS3method("bbplot_add", "bb_note", method,
                     envir = asNamespace("plotbb"))

    rec <- new.env()
    rec$labels <- character()
    options(plotbb.test.rec = rec)
    trace("text.default", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$labels <- c(.rec$labels, labels)
    }), print = FALSE)
    on.exit({
        untrace("text.default", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() + bb_note("note") +
        bb_facet_wrap(~f, ncol = 2)
    with_test_pdf(expect_silent(print(p)))
    expect_equal(sum(rec$labels == "note"), 2)

    built <- bb_build(p)
    expect_length(built$layers, 2)
    expect_match(built$layers[[2]]$name, "custom layer")
})

test_that("bb_build validates facet layouts like print", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    bad_nrow <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_wrap(~f, nrow = 0)
    expect_error(bb_build(bad_nrow), "positive integer")

    too_small <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_wrap(~f, nrow = 1, ncol = 1)
    expect_error(bb_build(too_small), "smaller than")

    no_data <- as.bbplot(function() graphics::plot(1:3)) +
        bb_facet_wrap(~f, ncol = 2)
    expect_error(bb_build(no_data), "requires plot data")
})

test_that("bb_build describes escape-hatch layers too", {
    p <- bbplot(mtcars, bb_aes(mpg, disp)) +
        bb_point() +
        function() graphics::abline(h = 300)
    built <- bb_build(p)
    expect_length(built$layers, 2)
    expect_equal(built$layers[[1]]$kind, "layer")
    expect_equal(built$layers[[2]]$kind, "function")
    expect_null(built$layers[[2]]$data)
})

test_that("bb_build rejects non-plots", {
    expect_error(bb_build(1:3), "requires a `bbplot`")
})

## ---- bb_save --------------------------------------------------------------

test_that("bb_save writes pdf and png files", {
    p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point(pch = 19)

    f_pdf <- tempfile(fileext = ".pdf")
    on.exit(unlink(f_pdf), add = TRUE)
    expect_invisible(bb_save(p, f_pdf))
    expect_true(file.exists(f_pdf))
    expect_gt(file.info(f_pdf)$size, 0)

    skip_if_not(capabilities("png"))
    f_png <- tempfile(fileext = ".png")
    on.exit(unlink(f_png), add = TRUE)
    bb_save(p, f_png, width = 6, height = 4, units = "in", res = 96)
    expect_true(file.exists(f_png))
    expect_gt(file.info(f_png)$size, 0)
})

test_that("bb_save follows the extension, device and overwrite rules", {
    p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point()
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)

    bb_save(p, f)
    expect_error(bb_save(p, f), "already exists")
    expect_silent(bb_save(p, f, overwrite = TRUE))

    png_name <- tempfile(fileext = ".png")
    on.exit(unlink(png_name), add = TRUE)
    expect_error(bb_save(p, png_name, device = "pdf"),
                 "conflicts with")

    weird <- tempfile(fileext = ".plotbb")
    on.exit(unlink(weird), add = TRUE)
    expect_error(bb_save(p, weird), "cannot infer a device")
    expect_silent(bb_save(p, weird, device = grDevices::pdf))
    expect_true(file.exists(weird))
})

test_that("bb_save validates its arguments", {
    p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point()
    f <- tempfile(fileext = ".pdf")

    expect_error(bb_save(p, file.path(tempdir(), "no-such-dir", "x.pdf")),
                 "does not exist")
    expect_error(bb_save(p, f, width = -1), "positive number")
    expect_error(bb_save(p, f, units = "inch"), "should be one of")
    expect_error(bb_save(p, f, units = "px", res = 0),
                 "positive `res`")
    expect_error(bb_save(1:3, f), "requires a `bbplot`")
})

test_that("bb_save restores the caller's device with several devices open", {
    files <- c(tempfile(fileext = ".pdf"), tempfile(fileext = ".pdf"))
    grDevices::pdf(files[[1]])
    grDevices::pdf(files[[2]])
    on.exit({
        while (grDevices::dev.cur() > 1L) grDevices::dev.off()
        unlink(files)
    }, add = TRUE)
    caller_id <- grDevices::dev.cur()

    p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point()
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)
    bb_save(p, f)
    expect_identical(as.integer(grDevices::dev.cur()), as.integer(caller_id))
})

test_that("a failed overwrite leaves the existing file untouched", {
    p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point()
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)
    bb_save(p, f)
    before <- readBin(f, "raw", file.info(f)$size)

    bad <- bbplot(data.frame(x = 1:3, y = 1:3), bb_aes(x, y)) +
        bb_layer(function(x, y, ...) stop("boom"))
    expect_error(bb_save(bad, f, overwrite = TRUE), "boom")

    expect_true(file.exists(f))
    after <- readBin(f, "raw", file.info(f)$size)
    expect_identical(before, after)
    leftovers <- list.files(dirname(f), pattern = paste0("^", basename(f), "-"))
    expect_length(leftovers, 0)
})

test_that("units are converted for vector devices", {
    p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point()
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)

    rec <- new.env()
    options(plotbb.test.rec = rec)
    trace("pdf", where = asNamespace("grDevices"), tracer = quote({
        assign("args", list(width = width, height = height),
               envir = getOption("plotbb.test.rec"))
    }), print = FALSE)
    on.exit({
        untrace("pdf", where = asNamespace("grDevices"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    bb_save(p, f, width = 10, height = 5, units = "cm")
    expect_equal(rec$args$width, 10 / 2.54, tolerance = 1e-6)
    expect_equal(rec$args$height, 5 / 2.54, tolerance = 1e-6)
})

test_that("bb_save does not close the caller's device", {
    caller_file <- tempfile(fileext = ".pdf")
    grDevices::pdf(caller_file)
    on.exit({
        while (grDevices::dev.cur() > 1L) grDevices::dev.off()
        unlink(caller_file)
    }, add = TRUE)
    caller_id <- grDevices::dev.cur()

    p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point()
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)
    bb_save(p, f)
    expect_identical(as.integer(grDevices::dev.cur()), as.integer(caller_id))
    expect_true(file.exists(f))
})

test_that("bb_save removes its partial file when drawing fails", {
    caller_file <- tempfile(fileext = ".pdf")
    grDevices::pdf(caller_file)
    on.exit({
        while (grDevices::dev.cur() > 1L) grDevices::dev.off()
        unlink(caller_file)
    }, add = TRUE)
    caller_id <- grDevices::dev.cur()

    p <- bbplot(data.frame(x = 1:3, y = 1:3), bb_aes(x, y)) +
        bb_layer(function(x, y, ...) stop("boom"))
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)

    expect_error(bb_save(p, f), "boom")
    expect_false(file.exists(f))
    expect_identical(as.integer(grDevices::dev.cur()), as.integer(caller_id))
})
