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
