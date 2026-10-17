## Horizontal violin and boxplot orientation.

test_that("horizontal violin puts values on x and groups on y", {
    set.seed(1)
    d <- data.frame(g = factor(rep(c("a", "b"), each = 15)),
                    v = c(rnorm(15), rnorm(15, 2)))
    p <- bbplot(d, bb_aes(v, g)) +
        bb_violin(orientation = "horizontal")
    built <- bb_build(p)
    expect_true(built$limits$xlim[[1]] < -1)
    expect_true(built$limits$xlim[[2]] > 3)
    expect_lte(built$limits$ylim[[1]], 0.55)
    expect_gte(built$limits$ylim[[2]], 2.45)
    with_test_pdf(expect_silent(print(p)))
})

test_that("horizontal boxplot puts values on x and groups on y", {
    set.seed(2)
    d <- data.frame(g = factor(rep(c("a", "b"), each = 15)),
                    v = c(rnorm(15), rnorm(15, 2)))
    p <- bbplot(d, bb_aes(v, g)) +
        bb_boxplot(orientation = "horizontal")
    built <- bb_build(p)
    expect_true(built$limits$xlim[[1]] < -1)
    expect_true(built$limits$xlim[[2]] > 3)
    expect_lte(built$limits$ylim[[1]], 0.6)
    expect_gte(built$limits$ylim[[2]], 2.4)
    with_test_pdf(expect_silent(print(p)))
})

test_that("orientation is validated", {
    d <- data.frame(g = rep(c("a", "b"), each = 5), v = rnorm(10))
    p <- bbplot(d, bb_aes(g, v))
    expect_error(with_test_pdf(print(p + bb_violin(orientation = "diagonal"))),
                 "arg")
    expect_error(with_test_pdf(print(p + bb_boxplot(orientation = "diagonal"))),
                 "arg")
})

test_that("horizontal bars put counts on x and categories on y", {
    d <- data.frame(g = factor(c("a", "b", "a", "c", "b", "a")))
    p <- bbplot(d, bb_aes(g)) +
        bb_bar(stat = "count", orientation = "horizontal")
    built <- bb_build(p)
    expect_lte(built$limits$xlim[[1]], 0)
    expect_gte(built$limits$xlim[[2]], 3)
    expect_lte(built$limits$ylim[[1]], 0.55)
    expect_gte(built$limits$ylim[[2]], 3.45)
    with_test_pdf(expect_silent(print(p)))
})

test_that("horizontal identity bars take values on x", {
    d <- data.frame(g = factor(c("a", "b", "c")), v = c(3, 5, 2))
    p <- bbplot(d, bb_aes(v, g)) +
        bb_bar(orientation = "horizontal")
    built <- bb_build(p)
    expect_lte(built$limits$xlim[[1]], 0)
    expect_gte(built$limits$xlim[[2]], 5)
    expect_lte(built$limits$ylim[[1]], 0.55)
    expect_gte(built$limits$ylim[[2]], 3.45)
    with_test_pdf(expect_silent(print(p)))
})

test_that("coord_flip turns vertical bars horizontal and back", {
    d <- data.frame(g = factor(c("a", "b", "a", "c", "b", "a")),
                    v = c(3, 5, 2, 4, 6, 1))
    count <- bbplot(d, bb_aes(g)) + bb_bar(stat = "count") +
        bb_coord_flip()
    with_test_pdf(expect_silent(print(count)))
    identity <- bbplot(d, bb_aes(g, v)) + bb_bar() + bb_coord_flip()
    with_test_pdf(expect_silent(print(identity)))
})

test_that("bar orientation and legend width are validated", {
    d <- data.frame(g = factor(c("a", "b")), v = c(1, 2))
    p <- bbplot(d, bb_aes(g, v))
    expect_error(with_test_pdf(print(p + bb_bar(stat = "count",
                                                orientation = "diagonal"))),
                 "arg")
    expect_error(p + bb_legend(width = 0), "positive number")
})

test_that("horizontal ribbon puts bounds on x and the centre on y", {
    d <- data.frame(y = 1:5, lo = c(0, 1, 2, 3, 4), hi = c(2, 3, 4, 5, 6))
    p <- bbplot(d, bb_aes(y = y, xmin = lo, xmax = hi)) +
        bb_ribbon(orientation = "horizontal", fill = "steelblue")
    built <- bb_build(p)
    expect_lte(built$limits$xlim[[1]], 0)
    expect_gte(built$limits$xlim[[2]], 6)
    expect_lte(built$limits$ylim[[1]], 1)
    expect_gte(built$limits$ylim[[2]], 5)
    with_test_pdf(expect_silent(print(p)))
})

test_that("coord_flip turns a vertical ribbon horizontal", {
    d <- data.frame(x = 1:5, lo = c(0, 1, 2, 3, 4), hi = c(2, 3, 4, 5, 6))
    p <- bbplot(d, bb_aes(x = x, ymin = lo, ymax = hi)) +
        bb_ribbon(fill = "steelblue") + bb_coord_flip()
    built <- bb_build(p)
    expect_lte(built$limits$xlim[[1]], 0)
    expect_gte(built$limits$xlim[[2]], 6)
    expect_lte(built$limits$ylim[[1]], 1)
    expect_gte(built$limits$ylim[[2]], 5)
    with_test_pdf(expect_silent(print(p)))
})

test_that("horizontal density and histogram put statistics on x", {
    set.seed(3)
    d <- data.frame(v = rnorm(40), g = factor(rep(c("a", "b"), 20)))
    dens <- bbplot(d, bb_aes(y = v)) +
        bb_density(orientation = "horizontal")
    built <- bb_build(dens)
    expect_lte(built$limits$xlim[[1]], 0)
    expect_gte(built$limits$xlim[[2]], 0.1)
    expect_equal(built$labs$xlab, "density")
    with_test_pdf(expect_silent(print(dens)))

    hist <- bbplot(d, bb_aes(y = v)) +
        bb_hist(orientation = "horizontal")
    built <- bb_build(hist)
    expect_lte(built$limits$xlim[[1]], 0)
    expect_gte(built$limits$xlim[[2]], 1)
    expect_equal(built$labs$xlab, "count")
    with_test_pdf(expect_silent(print(hist)))
})

test_that("coord_flip flips density and hist", {
    set.seed(4)
    d <- data.frame(v = rnorm(30))
    with_test_pdf(expect_silent(print(
        bbplot(d, bb_aes(x = v)) + bb_density() + bb_coord_flip()
    )))
    with_test_pdf(expect_silent(print(
        bbplot(d, bb_aes(x = v)) + bb_hist() + bb_coord_flip()
    )))
})

test_that("horizontal smooth fits the value against the other axis", {
    set.seed(5)
    d <- data.frame(t = 1:30, v = sin(1:30 / 5) + rnorm(30, sd = 0.1))
    p <- bbplot(d, bb_aes(v, t)) +
        bb_smooth(orientation = "horizontal", se = TRUE)
    built <- bb_build(p)
    expect_lte(built$limits$xlim[[1]], -0.9)
    expect_gte(built$limits$xlim[[2]], 0.9)
    expect_lte(built$limits$ylim[[1]], 1)
    expect_gte(built$limits$ylim[[2]], 30)
    with_test_pdf(expect_silent(print(p)))
})

test_that("coord_flip flips smooth", {
    set.seed(6)
    d <- data.frame(t = 1:30, v = sin(1:30 / 5) + rnorm(30, sd = 0.1))
    p <- bbplot(d, bb_aes(t, v)) + bb_smooth() + bb_coord_flip()
    with_test_pdf(expect_silent(print(p)))
})
