# plotbb: Grammar of Graphics on ‘base’ Graphics


<!-- README.md is generated from README.qmd. Please edit that file -->

Implements a grammar of graphics on top of the ‘base’ graphics system. A
‘bbplot’ object is built incrementally with a ‘+’ operator from data,
aesthetic mappings and layers, then rendered with base graphics, so
plotbb layers can be mixed with ordinary base plotting calls. The
package provides layers for points, lines, bars, histograms, densities,
boxplots, violins, ribbons, smooths, error bars and tiles, together with
colour and other aesthetic scales, automatic legends, faceting, themes,
coordinate flipping and significance annotations.

## :writing_hand: Author

Guangchuang YU

School of Basic Medical Sciences, Southern Medical University

<https://yulab-smu.top>

## :arrow_double_down: Installation

Get the released version from CRAN:

``` r
install.packages("plotbb")
```

Or the development version from github:

``` r
remotes::install_github("YuLab-SMU/plotbb")
```

## :sparkling_heart: Contributing

We welcome any contributions! By participating in this project you agree
to abide by the terms outlined in the [Contributor Code of
Conduct](CONDUCT.md).
