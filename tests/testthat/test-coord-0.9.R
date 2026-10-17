## 0.9.0 coordinate flip (limited to coordinate-symmetric layers).

test_that("coord_flip swaps axes and positional scales", {
    d <- data.frame(x = 1:10, y = 10 * (1:10))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_hline(50) +
        bb_scale_x_continuous(limits = c(2, 8)) +
        bb_coord_flip()
    built <- bb_build(p)
    expect_equal(built$limits$ylim, c(2, 8))
    expect_null(built$position$x)
    expect_equal(built$position$y$limits, c(2, 8))
    with_test_pdf(expect_silent(print(p)))
})

test_that("coord_flip flips violin and boxplot", {
    d <- data.frame(x = 1:10, y = 1:10, g = rep(c("a", "b"), 5))
    p <- bbplot(d, bb_aes(g, y)) + bb_violin() + bb_coord_flip()
    with_test_pdf(expect_silent(print(p)))
})

test_that("coord_flip refuses layers without an orientation", {
    d <- data.frame(x = 1:10, y = 1:10)
    p <- bbplot(d, bb_aes(x, y)) + bb_density() + bb_coord_flip()
    expect_error(with_test_pdf(print(p)), "does not support")
})

test_that("coord_flip keeps facets and labels consistent", {
    d <- data.frame(x = 1:12, y = 1:12, f = rep(c("a", "b"), each = 6))
    p <- bbplot(d, bb_aes(x, y)) + bb_point() +
        bb_facet_wrap(~f, ncol = 2) + bb_coord_flip()
    with_test_pdf(expect_silent(print(p)))
    built <- bb_build(p)
    expect_equal(length(built$panels), 2)
})

test_that("coord_flip toggles errorbar orientation", {
    d <- data.frame(x = 1:4, y = 1:4, lo = 0:3, hi = 2:5)
    p <- bbplot(d, bb_aes(x, y, ymin = lo, ymax = hi)) +
        bb_point() + bb_errorbar() + bb_coord_flip()
    with_test_pdf(expect_silent(print(p)))
})
