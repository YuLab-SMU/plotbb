## Tests for graphics state management (0.2.0)

## parameters bbplot promises to restore exactly
manual_par <- c("bg", "mar", "oma", "mgp", "tck", "las", "mfrow", "new")

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

par_snapshot <- function(names) {
    launch <- suppressWarnings(graphics::par(names))
    launch
}

test_that("print() restores par() even when no theme is set", {
    with_pdf({
        p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point(pch = 19)
        p$canvas <- function(xlim = NULL, ylim = NULL) {
            graphics::par(mar = c(1, 1, 1, 1), mgp = c(1, 0.1, 0), las = 3)
            graphics::plot(NA, NA, type = "n", xlim = xlim, ylim = ylim)
        }
        before <- par_snapshot(manual_par)
        print(p)
        after <- par_snapshot(manual_par)
        expect_identical(after, before)
    })
})

test_that("print() restores par() after drawing a themed plot", {
    with_pdf({
        p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl))) +
            bb_point(pch = 19) +
            bb_theme(mar = c(3, 3, 2, 1), bg = "grey90", las = 2)
        before <- par_snapshot(manual_par)
        print(p)
        expect_identical(par_snapshot(manual_par), before)
    })
})

test_that("print() restores par() when a layer throws", {
    with_pdf({
        p <- bbplot(mtcars, bb_aes(mpg, disp)) +
            bb_point(pch = 19) +
            bb_theme(mar = c(3, 3, 2, 1), bg = "grey90") +
            (~ stop("boom from a layer"))
        before <- par_snapshot(manual_par)
        expect_error(print(p), "boom from a layer")
        expect_identical(par_snapshot(manual_par), before)
    })
})

test_that("print() restores par() after a faceted plot", {
    with_pdf({
        p <- bbplot(iris, bb_aes(Petal.Length, Sepal.Length, col = Species)) +
            bb_point(pch = 19) +
            bb_facet_wrap(~Species, nrow = 1) +
            bb_title("faceted") +
            bb_theme(bg = "grey95")
        before <- par_snapshot(manual_par)
        print(p)
        expect_identical(par_snapshot(manual_par), before)
    })
})

test_that("print() never leaves par(new = TRUE) behind", {
    with_pdf({
        p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point(pch = 19)
        print(p)
        expect_false(par("new"))
        print(p + bb_theme(col.axis = "grey30"))
        expect_false(par("new"))
    })
})

test_that("print() restores par() when the device has no plot yet", {
    with_pdf({
        p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point(pch = 19) +
            bb_theme(bg = "white")
        before <- par_snapshot(manual_par)
        expect_silent(print(p))
        expect_identical(par_snapshot(manual_par), before)
    })
})

test_that("unset_bb_theme() clears the saved state and is idempotent", {
    with_pdf({
        before <- par_snapshot(c("bg", "fg", "col.axis", "col.lab", "cex.axis"))
        old_opt <- getOption("bb_old_par")
        on.exit(options(bb_old_par = old_opt), add = TRUE)

        set_bb_theme(bb_theme_deepblue)
        expect_identical(suppressWarnings(par("bg")), "#002E49")
        expect_false(is.null(getOption("bb_old_par")))

        unset_bb_theme()
        expect_null(getOption("bb_old_par"))
        expect_identical(par_snapshot(c("bg", "fg", "col.axis", "col.lab", "cex.axis")),
                         before)

        ## a second call without a theme must be a no-op
        expect_silent(unset_bb_theme())
        expect_null(getOption("bb_old_par"))
    })
})

test_that("set_bb_theme() can replace an active theme without losing the original", {
    with_pdf({
        before <- par_snapshot(c("bg", "fg", "col.axis", "col.lab", "cex.axis"))
        old_opt <- getOption("bb_old_par")
        on.exit(options(bb_old_par = old_opt), add = TRUE)

        set_bb_theme(bb_theme_deepblue)
        set_bb_theme(bb_theme_grey)
        expect_identical(suppressWarnings(par("bg")), "gray85")

        unset_bb_theme()
        expect_identical(par_snapshot(c("bg", "fg", "col.axis", "col.lab", "cex.axis")),
                         before)
    })
})

test_that("with_bb_theme() applies a theme and restores the previous state", {
    with_pdf({
        before <- par_snapshot(c("bg", "fg", "col.axis", "col.lab", "cex.axis"))
        old_opt <- getOption("bb_old_par")
        on.exit(options(bb_old_par = old_opt), add = TRUE)

        value <- with_bb_theme(bb_theme_deepblue(), {
            expect_identical(suppressWarnings(par("bg")), "#002E49")
            "returned"
        })
        expect_identical(value, "returned")
        expect_identical(par_snapshot(c("bg", "fg", "col.axis", "col.lab", "cex.axis")),
                         before)
        ## scoped themes do not touch the option used by set_bb_theme()
        expect_null(getOption("bb_old_par"))
    })
})

test_that("with_bb_theme() restores the state when the code errors", {
    with_pdf({
        before <- par_snapshot(c("bg", "fg", "col.axis", "col.lab", "cex.axis"))
        expect_error(with_bb_theme(bb_theme_deepblue(), stop("inner failure")),
                     "inner failure")
        expect_identical(par_snapshot(c("bg", "fg", "col.axis", "col.lab", "cex.axis")),
                         before)
    })
})

test_that("with_bb_theme() can be nested", {
    with_pdf({
        before <- par_snapshot(c("bg", "fg", "col.axis", "col.lab", "cex.axis"))
        with_bb_theme(bb_theme_deepblue(), {
            with_bb_theme(bb_theme_grey(), {
                expect_identical(suppressWarnings(par("bg")), "gray85")
            })
            expect_identical(suppressWarnings(par("bg")), "#002E49")
        })
        expect_identical(par_snapshot(c("bg", "fg", "col.axis", "col.lab", "cex.axis")),
                         before)
    })
})

test_that("with_bb_theme() evaluates code in the caller's environment", {
    with_pdf({
        x <- 41
        expect_identical(with_bb_theme(bb_theme(bg = "white"), x + 1), 42)
        expect_error(with_bb_theme(bb_theme(bg = "white"), nonexistent_object_xyz),
                     "nonexistent_object_xyz")
    })
})

test_that("with_bb_theme() rejects non-theme input", {
    expect_error(with_bb_theme(list(a = 1), "x"), class = "simpleError")
})
