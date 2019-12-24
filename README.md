<!-- README.md is generated from README.Rmd. Please edit that file -->

# Grammar of Graphics for base plot

## Aesthetic mapping

`bb_aes()` for aesthetic mapping, that equivalents to `ggplot2::aes()`.

``` r
library(bbplot)

p <- bbplot(mtcars, bb_aes(mpg, disp, col=factor(cyl)))
p + bb_point()
```

![](README_files/figure-gfm/aes-1.png)<!-- -->

## Geometric layer

``` r
p2 <- p + bb_point() + bb_lm(bb_aes(group=cyl), lwd=2)
p3 <- p2 + bb_lm(col="red", lwd=3, lty='dotted')
par(mfrow=c(1,2))
p2; p3
```

![](README_files/figure-gfm/layer-1.png)<!-- -->

### TODO

  - [x] bb\_point
  - [x] bb\_lm
  - more layers need to be added

## Setting labels

``` r
p2 + bb_labs(title = "hello", sub = "just for demo",
              xlab="this is xlab", ylab = "this is ylab")
```

![](README_files/figure-gfm/labs-1.png)<!-- -->

## Theme

``` r
g <- p2 +
     bb_theme(col.main="red", cex.main=2,
             mar = c(4, 4, 3, 1)) +
     bb_title("applying graphics::par")
par(mfrow=c(1,2))
print(g)
p2 + bb_title("theme has no side effect")
```

![](README_files/figure-gfm/theme-1.png)<!-- -->

`bb_theme` has no side effect and will only apply to the `bbplot` that
it added to. This is very important for developing pre-defined themes.

### TODO

  - develop pre-defined themes

## Scale

Not yet implemented

## Legend

Not yet implemented
