## 0.9.0 positional scales.

test_that("positional limits, breaks and labels are used", {
    d <- data.frame(x = 1:20, y = seq(1, 100, length.out = 20))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_scale_x_continuous(limits = c(5, 15),
                              breaks = c(5, 10, 15),
                              labels = c("lo", "mid", "hi"))
    built <- bb_build(p)
    expect_equal(built$limits$xlim, c(5, 15))
    expect_equal(built$position$x$breaks, c(5, 10, 15))
    expect_equal(built$position$x$labels, c("lo", "mid", "hi"))
    with_test_pdf(expect_silent(print(p)))
})

test_that("a log scale sets positive limits and renders", {
    d <- data.frame(x = 1:20, y = seq(1, 100, length.out = 20))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_scale_y_log10(limits = c(1, 100))
    built <- bb_build(p)
    expect_equal(built$limits$ylim, c(1, 100))
    with_test_pdf(expect_silent(print(p)))
})

test_that("a date scale reports Date limits and renders", {
    d <- data.frame(date = as.Date("2024-01-01") + 0:20, value = 1:21)
    lim <- as.Date(c("2024-01-05", "2024-01-15"))
    p <- bbplot(d, bb_aes(date, value)) + bb_point() +
        bb_scale_x_date(limits = lim, format = "%b %d")
    built <- bb_build(p)
    expect_s3_class(built$limits$xlim, "Date")
    expect_equal(built$limits$xlim, lim)
    with_test_pdf(expect_silent(print(p)))
})

test_that("facets honour positional limits", {
    d <- data.frame(x = 1:20, y = 1:20,
                    f = rep(c("a", "b"), each = 10))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_scale_x_continuous(limits = c(5, 15)) +
        bb_facet_wrap(~f, ncol = 2, scales = "free_y")
    built <- bb_build(p)
    expect_true(all(vapply(built$panels, function(panel) {
        identical(panel$xlim, c(5, 15))
    }, logical(1))))
    with_test_pdf(expect_silent(print(p)))
})

test_that("positional scales validate their arguments", {
    d <- data.frame(x = 1:4, y = 1:4)
    p <- bbplot(d, bb_aes(x, y)) + bb_point()
    expect_error(p + bb_scale_x_continuous(labels = c("a", "b")),
                 "requires `breaks`")
    expect_error(p + bb_scale_x_continuous(limits = c(1, 2, 3)),
                 "two finite")
    expect_error(p + bb_scale_y_log10(limits = c(0, 10)),
                 "positive")
    expect_error(p + bb_scale_x_continuous(breaks = 1:2,
                                            labels = c("a", "b", "c")),
                 "same length")
})
