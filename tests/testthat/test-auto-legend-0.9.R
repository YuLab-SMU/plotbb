## 0.9.0 automatic legends.

test_that("mapped aesthetics get an outside legend by default", {
    d <- data.frame(x = 1:6, y = 1:6,
                    g = factor(rep(c("a", "b"), 3)),
                    s = factor(rep(c("u", "v", "w"), each = 2)))
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

    p <- bbplot(d, bb_aes(x, y, col = g, pch = s)) + bb_point()
    with_test_pdf(print(p))
    expect_true(any(rec$titles == "g"))
    expect_true(any(rec$titles == "s"))
})

test_that("auto_legend = FALSE disables the automatic legends", {
    d <- data.frame(x = 1:4, y = 1:4, g = c("a", "a", "b", "b"))
    rec <- new.env(); rec$n <- 0L
    options(plotbb.test.rec = rec)
    trace("legend", where = asNamespace("graphics"), tracer = quote({
        .rec <- getOption("plotbb.test.rec")
        .rec$n <- .rec$n + 1L
    }), print = FALSE)
    on.exit({
        untrace("legend", where = asNamespace("graphics"))
        options(plotbb.test.rec = NULL)
    }, add = TRUE)

    p <- bbplot(d, bb_aes(x, y, col = g), auto_legend = FALSE) + bb_point()
    with_test_pdf(print(p))
    expect_equal(rec$n, 0L)
})

test_that("explicit legends replace the automatic ones for that aesthetic", {
    d <- data.frame(x = 1:4, y = 1:4, g = c("a", "a", "b", "b"))
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
        bb_legend(position = "topright", title = "explicit")
    with_test_pdf(print(p))
    expect_true(any(rec$titles == "explicit"))
    expect_false("g" %in% rec$titles)
    expect_false("col" %in% rec$titles)
})

test_that("a layer-local mapping gets a per-layer automatic legend", {
    d <- data.frame(x = 1:4, y = 1:4, g = c("a", "a", "b", "b"))
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

    p <- bbplot(d, bb_aes(x, y)) + bb_point(bb_aes(col = g))
    with_test_pdf(print(p))
    expect_true(any(rec$titles == "g"))
})

test_that("free colour facets skip automatic outside legends", {
    d <- data.frame(x = 1:6, y = 1:6,
                    f = rep(c("a", "b"), each = 3),
                    g = factor(c("u", "v", "u", "v", "w", "w")))
    p <- bbplot(d, bb_aes(x, y, col = g)) + bb_point() +
        bb_scale_col_manual(c(u = "red", v = "blue", w = "green")) +
        bb_facet_wrap(~f, ncol = 2, free_colour = TRUE)
    with_test_pdf(expect_silent(print(p)))
})
