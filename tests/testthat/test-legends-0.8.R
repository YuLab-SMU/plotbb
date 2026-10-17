## 0.8.0 per-layer legend, free colour scales and colour-bar polish.

test_that("a legend can describe one layer's mapping", {
    d <- data.frame(x = 1:6, y = 1:6,
                    h = rep(c("a", "b"), 3),
                    g = rep(c("u", "v", "w"), each = 2))
    rec <- new.env(); rec$titles <- character()
    options(plotbb.test.rec = rec)
    trace("legend", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$titles <- c(.rec$titles, title)
    }), print = FALSE)
    on.exit({
        untrace("legend", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x, y, col = h)) +
        bb_point() +
        bb_line(bb_aes(group = g, col = g)) +
        bb_legend(layer = 2, aesthetic = "col")
    with_test_pdf(print(p))
    expect_true(any(rec$titles == "g"))
})

test_that("a per-layer legend reports invalid layers", {
    d <- data.frame(x = 1:4, y = 1:4, g = c("a", "a", "b", "b"))
    p <- bbplot(d, bb_aes(x, y)) + bb_point()
    expect_error(with_test_pdf(print(p + bb_legend(layer = 2))),
                 "existing layer")
    expect_error(with_test_pdf(print(p + bb_legend(layer = "missing"))),
                 "existing layer")
    expect_error(with_test_pdf(print(p + bb_legend(layer = 1, aesthetic = "col"))),
                 "does not map")
})

test_that("a bar style and a wrapped title render", {
    d <- data.frame(x = 1:6, y = 1:6, g = rep(c("a", "b", "c"), each = 2))
    rec <- new.env(); rec$titles <- character()
    options(plotbb.test.rec = rec)
    trace("legend", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$titles <- c(.rec$titles, title)
    }), print = FALSE)
    on.exit({
        untrace("legend", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_legend(style = "bar", title = "a very long legend title",
                  title_width = 8)
    with_test_pdf(print(p))
    expect_true(grepl("\n", rec$titles[[1]], fixed = TRUE))
})

test_that("colour bars draw minor ticks", {
    d <- data.frame(x = 1:10, y = 1:10, z = 1:10)
    rec <- new.env(); rec$segments <- 0L
    options(plotbb.test.rec = rec)
    trace("segments", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$segments <- .rec$segments + 1L
    }), print = FALSE)
    on.exit({
        untrace("segments", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x, y, col = z)) + bb_point() +
        bb_scale_col_gradient(minor_breaks = c(2, 5, 8)) +
        bb_legend(position = "right")
    with_test_pdf(print(p))
    expect_gt(rec$segments, 0L)
})

test_that("free colour scales give each panel its own colour domain", {
    d <- data.frame(x = 1:6, y = 1:6, f = rep(c("a", "b"), each = 3),
                    g = factor(c("u", "v", "u", "v", "w", "w")))
    shared <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_scale_col_manual(c(u = "red", v = "blue", w = "green")) +
        bb_facet_wrap(~f, ncol = 2)
    free <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_scale_col_manual(c(u = "red", v = "blue", w = "green")) +
        bb_facet_wrap(~f, ncol = 2, free_colour = TRUE)

    b_shared <- bb_build(shared)
    b_free <- bb_build(free)
    expect_equal(b_shared$panels[[1]]$col_domain,
                 b_shared$panels[[2]]$col_domain)
    expect_false(identical(b_free$panels[[1]]$col_domain,
                           b_free$panels[[2]]$col_domain))
    expect_equal(b_free$panels[[1]]$col_domain, c("u", "v"))
    expect_equal(b_free$panels[[2]]$col_domain, c("v", "w"))

    with_test_pdf(expect_silent(print(free)))
    outside <- free + bb_legend(position = "right", outside = TRUE)
    expect_error(with_test_pdf(print(outside)), "outside legend")
})

test_that("free colour scales handle empty panels", {
    d <- data.frame(x = 1:6, y = 1:6,
                    f = factor(rep(c("a", "b"), each = 3),
                               levels = c("a", "b", "c")),
                    g = factor(rep(c("u", "v"), 3)))
    p <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_scale_col_manual(c(u = "red", v = "blue")) +
        bb_facet_wrap(~f, ncol = 3, drop = FALSE, free_colour = TRUE)
    with_test_pdf(expect_silent(print(p)))
    built <- bb_build(p)
    expect_equal(vapply(built$panels, `[[`, 0L, "n"), c(3L, 3L, 0L))
})

test_that("grid free colour supports inside legends and rejects outside ones", {
    d <- data.frame(x = 1:8, y = 1:8,
                    r = rep(c("r1", "r2"), each = 4),
                    cc = rep(c("c1", "c2"), 4),
                    g = factor(rep(c("u", "v"), 4)))
    p <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_scale_col_manual(c(u = "red", v = "blue")) +
        bb_facet_grid(rows = ~r, cols = ~cc, free_colour = TRUE) +
        bb_legend(position = "topright")
    with_test_pdf(expect_silent(print(p)))
    outside <- p + bb_legend(position = "right", outside = TRUE)
    expect_error(with_test_pdf(print(outside)), "outside legend")
})

test_that("grid row labels account for long wrapped labels", {
    d <- data.frame(x = 1:12, y = 1:12,
                    r = rep(c("a very long row label one",
                              "a very long row label two"), each = 6),
                    cc = rep(c("c1", "c2"), 6))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_grid(rows = ~r, cols = ~cc, strip_width = 10)
    with_test_pdf(expect_silent(print(p)))
})

test_that("free colour scales work in a facet grid", {
    d <- data.frame(x = 1:8, y = 1:8,
                    r = rep(c("r1", "r2"), each = 4),
                    cc = rep(c("c1", "c2"), 4),
                    g = factor(rep(c("u", "v"), 4)))
    p <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_scale_col_manual(c(u = "red", v = "blue")) +
        bb_facet_grid(rows = ~r, cols = ~cc, free_colour = TRUE)
    built <- bb_build(p)
    expect_equal(built$facet$free_colour, TRUE)
    expect_length(built$panels, 4)
    expect_true(all(vapply(built$panels, function(z) !is.null(z$col_domain),
                           logical(1))))
    with_test_pdf(expect_silent(print(p)))
})
