<!-- README.md is generated from README.Rmd. Please edit that file -->

# Grammar of Graphics for base plot

`amp()` for **a**esthetic **m**a**p**ping, that equivalents to
`ggplot2::aes()`.

``` r
library(bbplot)

p <- bbplot(mtcars, amp(mpg, disp, col=factor(cyl)))
p1 <- p %>% ly_point
p2 <- p %>% ly_point %>% ly_lm(amp(group=cyl), lwd=2)
p3 <- p %>% ly_point %>% ly_lm(amp(group=cyl), lwd=2, col='red')
par(mfrow=c(2,2))
p; p1; p2; p3
```

![](README_files/figure-gfm/fig1-1.png)<!-- -->
