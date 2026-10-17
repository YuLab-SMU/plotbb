## 0.6.0 facet tail and facet grid tests.

test_that("facet_wrap can keep unused factor levels as empty panels", {
    d <- data.frame(x = 1:6, y = 1:6,
                    f = factor(rep(c("a", "b"), each = 3),
                               levels = c("a", "b", "c")))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_wrap(~f, ncol = 3, drop = FALSE)
    built <- bb_build(p)
    expect_length(built$panels, 3)
    expect_equal(vapply(built$panels, `[[`, 0L, "n"), c(3L, 3L, 0L))
    expect_equal(built$panels[[3]]$label, "c")
    with_test_pdf(expect_silent(print(p)))
})

test_that("facet_wrap labeller and strip_width change the strip text", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_wrap(~f, ncol = 2, strip_width = 8,
                      labeller = function(key) paste("panel", key))
    built <- bb_build(p)
    expect_equal(vapply(built$panels, `[[`, "", "label"),
                 c("panel a", "panel b"))
    with_test_pdf(expect_silent(print(p)))

    wrapped <- bb_build(bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_wrap(~f, ncol = 2, strip_width = 5,
                      labeller = function(key) paste("group", key)))
    expect_match(wrapped$panels[[1]]$label, "\n")
})

test_that("facet_wrap validates labeller, drop and strip_width", {
    d <- data.frame(x = 1:4, y = 1:4, f = c("a", "a", "b", "b"))
    p <- bbplot(d, bb_aes(x, y)) + bb_point()
    expect_error(p + bb_facet_wrap(~f, labeller = "x"), "function")
    expect_error(p + bb_facet_wrap(~f, drop = NA), "TRUE or FALSE")
    expect_error(p + bb_facet_wrap(~f, strip_width = 0), "positive integer")
})

test_that("facet_grid lays out rows and columns with shared scales", {
    d <- data.frame(x = 1:12, y = c(1:6, 10 * (1:6)),
                    r = rep(c("r1", "r2"), each = 6),
                    cc = rep(rep(c("c1", "c2"), each = 3), 2))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_grid(rows = ~r, cols = ~cc, scales = "free_y")
    built <- bb_build(p)
    expect_length(built$panels, 4)
    expect_equal(built$facet$kind, "grid")
    expect_equal(built$facet$vars, c("r", "cc"))
    expect_equal(vapply(built$panels, `[[`, "", "key"),
                 c("r1|c1", "r1|c2", "r2|c1", "r2|c2"))

    ## free_y is shared within a row, not within a column
    expect_equal(built$panels[[1]]$ylim, built$panels[[2]]$ylim)
    expect_equal(built$panels[[3]]$ylim, built$panels[[4]]$ylim)
    expect_false(isTRUE(all.equal(built$panels[[1]]$ylim,
                                  built$panels[[3]]$ylim)))
    expect_equal(built$panels[[1]]$xlim, built$panels[[4]]$xlim)
    with_test_pdf(expect_silent(print(p)))
})

test_that("facet_grid supports one direction and empty combinations", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3))
    only_rows <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_grid(rows = ~f)
    expect_length(bb_build(only_rows)$panels, 2)

    only_cols <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_grid(cols = ~f)
    expect_length(bb_build(only_cols)$panels, 2)

    d$r <- factor(rep(c("r1", "r2"), each = 3), levels = c("r1", "r2", "r3"))
    empty_combo <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_grid(rows = ~r, cols = ~f, drop = FALSE)
    built <- bb_build(empty_combo)
    expect_length(built$panels, 6)
    expect_true(any(vapply(built$panels, `[[`, 0L, "n") == 0L))
    with_test_pdf(expect_silent(print(empty_combo)))
})

test_that("facet_grid validates its arguments", {
    d <- data.frame(x = 1:4, y = 1:4, f = c("a", "a", "b", "b"))
    p <- bbplot(d, bb_aes(x, y)) + bb_point()
    expect_error(p + bb_facet_grid(), "at least one")
    expect_error(p + bb_facet_grid(rows = "f"), "formula")
    expect_error(p + bb_facet_grid(rows = ~f, drop = NA), "TRUE or FALSE")
    expect_error(p + bb_facet_grid(rows = ~f, strip_width = 0),
                 "positive integer")
})

test_that("facet_grid draws the same limits that bb_build reports", {
    d <- data.frame(x = 1:12, y = c(1:6, 10 * (1:6)),
                    r = rep(c("r1", "r2"), each = 6),
                    cc = rep(rep(c("c1", "c2"), each = 3), 2))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_grid(rows = ~r, cols = ~cc, scales = "free_x")
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

test_that("facet_grid works with an outside legend", {
    d <- data.frame(x = 1:12, y = 1:12, r = rep(c("r1", "r2"), each = 6),
                    cc = rep(c("c1", "c2"), each = 6),
                    g = rep(c("a", "b"), 6))
    p <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_facet_grid(rows = ~r, cols = ~cc) +
        bb_legend(position = "right", outside = TRUE)
    with_test_pdf(expect_silent(print(p)))
})
