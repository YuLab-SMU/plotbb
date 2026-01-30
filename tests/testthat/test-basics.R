test_that("bbplot basic execution", {
  p <- bbplot(mtcars, bb_aes(mpg, disp))
  expect_s3_class(p, "bbplot")
})

test_that("bbplot with layers", {
  p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point()
  expect_true(length(p$layer) == 1)
})

test_that("bb_boxplot execution", {
  p <- bbplot(mtcars, bb_aes(factor(cyl), mpg)) + bb_boxplot()
  expect_s3_class(p, "bbplot")
  expect_true("boxplot layer" %in% names(p$layer))
})

test_that("bb_boxplot defaults to add=TRUE and axes=FALSE", {
  p <- bbplot(mtcars, bb_aes(factor(cyl), mpg)) + bb_boxplot()
  ly <- p$layer[["boxplot layer"]]
  expect_true(is.function(ly))
  env <- environment(ly)
  expect_true(is.list(env$params))
  expect_identical(env$params$add, TRUE)
  expect_identical(env$params$axes, FALSE)
})

test_that("bb_hist execution", {
  p <- bbplot(mtcars, bb_aes(x = mpg)) + bb_hist()
  expect_s3_class(p, "bbplot")
  expect_true("histogram layer" %in% names(p$layer))
})

test_that("bb_hist contributes to plot limits", {
  p <- bbplot(mtcars, bb_aes(mpg)) + bb_hist()
  lim <- plotbb:::bb_plot_limits(p)
  expect_true(is.numeric(lim$xlim) && length(lim$xlim) == 2)
  expect_true(is.numeric(lim$ylim) && length(lim$ylim) == 2)
  expect_true(is.finite(lim$xlim[[1]]) && is.finite(lim$xlim[[2]]))
  expect_true(lim$ylim[[2]] > 0)
})

test_that("bb_signif comparisons execution", {
  p <- bbplot(mtcars, bb_aes(factor(cyl), mpg)) +
    bb_boxplot() +
    bb_signif(comparisons = list(c("4", "6")))
  expect_s3_class(p, "bbplot")
  expect_true("signif layer" %in% names(p$layer))

  lim <- plotbb:::bb_plot_limits(p)
  expect_true(lim$ylim[[2]] > max(mtcars$mpg, na.rm = TRUE))
})

test_that("bb_signif wilcox ties warning is suppressed by default", {
  df <- data.frame(
    g = factor(rep(c("A", "B"), each = 10)),
    y = c(rep(1, 5), rep(2, 5), rep(1, 5), rep(2, 5))
  )
  p <- bbplot(df, bb_aes(g, y)) +
    bb_boxplot() +
    bb_signif(comparisons = list(c("A", "B")))

  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_warning(print(p), NA)
})

test_that("bb_bar canvas respects global limits", {
  df <- data.frame(g = factor(rep(c("A", "B"), each = 5)), y = c(1:5, 2:6))
  p <- bbplot(df, bb_aes(g, y)) +
    bb_bar() +
    bb_signif(comparisons = list(c("A", "B")))

  fml <- formals(p$canvas)
  expect_true(all(c("xlim", "ylim") %in% names(fml)))

  lim <- plotbb:::bb_plot_limits(p)
  expect_true(lim$ylim[[2]] > max(df$y, na.rm = TRUE))
})
