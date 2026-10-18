# plotbb 1.0.0

+ `bb_density()`, `bb_hist()` and `bb_smooth()` gain `orientation = "horizontal"`, with the statistic on x and the values or groups on y (2026-10-18, Sun)
+ `bb_coord_flip()` flips density, histogram and smooth layers; a supplied smooth formula is swapped with the mapping
+ horizontal summary labels put `density`/`count` on the x axis
+ tests cover horizontal statistics, flipped formulas and their ranges

# plotbb 0.9.0

+ `bb_violin()` and `bb_boxplot()` gain horizontal orientation and are flipped by `bb_coord_flip()` (2026-10-18, Sun)
+ `bb_bar(orientation = "horizontal")` draws count bars with categories on y and identity bars with values on x
+ `bb_errorbar()` and `bb_ribbon()` gain horizontal orientation and coord-flip support
+ `bb_coord_flip()` now also flips violin, boxplot, bar, errorbar and ribbon
+ multiple outside legends stack by their drawn height; `bb_legend(width = )` sets the legend column width
+ tests cover horizontal layers and their coord-flip forms

# plotbb 0.8.5

+ positional scales for continuous, log10 and Date x/y axes, with limits, breaks and labels; limits are final and clip values outside them (2026-10-17, Sat)
+ `bb_coord_flip()` swaps axes, labels and positional scales for coordinate-symmetric layers
+ mapped `col`, `fill`, `pch`, `lty` and `cex` get outside legends by default; `bb_legend()` can take over an aesthetic and `auto_legend = FALSE` disables the automatic ones
+ one grouping rule everywhere: explicit `group`, then a discrete `x`, then `col`, then `fill`
+ `bb_layer()` exposes `aesthetics`, `summary` and `range` for custom statistical layers
+ errors inside a custom `range` function propagate instead of being ignored

# plotbb 0.8.1

+ `bb_smooth()` includes its confidence band in the range and caches the band range (2026-10-17, Sat)
+ `bb_violin(trim = FALSE)` includes the estimated density support in the range
+ `bb_smooth()` rejects a response other than `y`; `bb_violin()` validates `kernel`, `bw` and `n`
+ long grid row labels are drawn horizontally with a dynamic right margin
+ the per-layer legend example and known limits are corrected

# plotbb 0.8.0

+ `bb_legend(layer = )` describes one layer's mapping; `style = "bar"` draws adjacent colour blocks and `title_width` wraps long titles (2026-10-17, Sat)
+ `free_colour = TRUE` in wrap and grid facets trains the col/fill domain per panel; outside legends are an error with free colour
+ `bb_build()` reports per-panel `col_domain`/`fill_domain` and the facet records `free_colour`
+ colour bars gain `minor_breaks`
+ tests and the vignette cover per-layer legends, free colour facets and minor breaks

# plotbb 0.7.0

+ `bb_violin()` draws one density polygon per group, scaled to `width`, with `adjust`, `bw`, `kernel`, `trim` and `n` (2026-10-17, Sat)
+ `bb_violin()` skips groups with too few finite values or zero spread and supports constant or mapped `fill`/`col`
+ `bb_smooth()` supports `"loess"` and `"lm"`, formula, `method.args`, `se` and `level`, and caches the fit
+ smooth groups come from `group`, `col` or `fill`; each group gets its own curve and confidence band
+ tests and the vignette cover both statistical layers

# plotbb 0.6.0

+ `bb_facet_wrap()` gains `labeller`, `drop` and `strip_width` (2026-10-17, Sat)
+ `bb_facet_grid()` places one variable on rows and another on columns, with labels on top and right
+ facet free scales follow the grid layout; `fixed` uses the global limits
+ `bb_build()` reports grid panels with row/column keys, labels and limits
+ tests and the vignette cover empty levels, labellers, grid layouts and outside legends

# plotbb 0.5.0

+ `bb_build()` returns data, mapping, layers, scales, limits, panels, labels, facet and collected warnings without opening a device or changing `par()` (2026-10-17, Sat)
+ `bb_save()` writes pdf, png, jpeg, tiff and svg through the device implied by the extension, with unit-aware sizes and safe overwrite
+ drawing goes through a temporary file; failed writes preserve the caller's device and any existing target
+ components that add layers are re-run per facet panel without double drawing
+ `bbplot_add()` is documented as the public component contract
+ new build/save and extension tests, plus the "Extending from a package" vignette section

# plotbb 0.4.0

+ `bb_ribbon()` draws a band between `ymin` and `ymax` along `x`, one polygon per group, with `alpha`, `border` and mapped `fill`/`col` (2026-10-17, Sat)
+ `bb_hline()` and `bb_vline()` accept constant or mapped intercepts with per-line `col`/`lty`; `expand_limits = TRUE` includes them in the range
+ layer data that cannot supply an inherited mapping contributes no range instead of failing
+ `bb_facet_wrap()` validates the layout size against the number of panels
+ new layer and facet tests plus the "Bands and reference lines" vignette section

# plotbb 0.3.0

+ one trained scale per aesthetic, used by both layers and legends, with separate `col` and `fill` scales (2026-10-17, Sat)
+ discrete rules keep factor order and first-appearance order; named palettes match by name and uncovered values use `na.value`
+ continuous rules support `limits`, `oob = "squish"`/`"censor"`, zero-width domains and all-missing domains
+ new fill palette, manual and gradient scales; mapped `lty` is applied by line-like layers
+ legends use the trained scale; continuous colour bars support horizontal orientation
+ `outside = TRUE` places legends in a dedicated layout column
+ histograms and densities compute their summary coordinates on the correct axis
+ new scale tests and a rewritten scale/legend vignette section

# plotbb 0.2.5

+ every layer accepts `inherit`; a local mapping overrides the global mapping aesthetic by aesthetic (2026-10-17, Sat)
+ layer `data` contributes to ranges and to free facet panels, including row-aligned data
+ constants are handled in one place and remove the matching mapping; length-n constants follow surviving rows
+ `fill` is accepted only where the primitive has an interior; otherwise the error names `col`
+ `+` refuses to combine two complete plots and points to `aplot::plot_list()`
+ `bb_layer()` is the supported custom-layer entry point with inheritance, constants, ranges and facets
+ the vignette was rewritten around the grammar, with a contract recap and an error-message guide

# plotbb 0.2.0

+ `print.bbplot()` snapshots and restores `par()` on normal return and on error; themes and facets restore their layout state (2026-10-17, Sat)
+ `with_bb_theme()` applies a theme for the duration of a block; `unset_bb_theme()` clears the saved pre-theme state
+ invalid coordinates are dropped row by row, all-invalid layers error, and aesthetic lengths are validated
+ `bb_hist()` and `bb_density()` warn and skip too-small data; `bb_plot_limits()` always returns finite limits
+ `bb_point(position = "jitter")` gains a reproducible `seed` and does not disturb `.Random.seed`
+ layer arguments are passed by name; layers no longer rely on `par(new = TRUE)`
+ new state and input tests

# plotbb 0.1.2

+ `bb_density()` with grouped polygons, `fill` mapping and alpha blending (2026-05-21, Thu)
+ `bb_lm(se = TRUE)` supports confidence intervals for grouped fits and tiny groups
+ `bb_legend()` supports discrete fill legends and outside legends for faceted plots
+ `bb_facet_wrap(nrow = 1)` layout and global title fixes
+ regression tests for faceted titles and grouped `bb_lm()` intervals

# plotbb 0.1.1

+ `bb_boxplot()` and `bb_hist()` (2026-01-30, Fri)
+ `bb_signif()` supports `comparisons`
+ `bb_theme_bw()` and `bb_theme_minimal()`
+ unit tests for bar, boxplot, histogram and significance layers

# plotbb 0.1.0

+ `bb_line()`, `bb_segment()`, `bb_errorbar()` and `bb_bar()` (2026-01-15, Thu)
+ `bb_bar(stat = "count")` respects factor levels
+ aesthetic mappings use `rlang::eval_tidy()`
+ discrete and continuous colour legends
+ manual and gradient scales for colour, `pch`, `lty` and `cex`
+ `bb_facet_wrap(scales = "free")` and Quarto vignette support

# plotbb 0.0.5

+ expanded the Description text and documented return values of `bb_aes()`, `set_bb_theme()` and `unset_bb_theme()` (2021-08-27, Fri)

# plotbb 0.0.4

+ added examples and prepared the package for CRAN submission (2021-08-22, Sun)

# plotbb 0.0.3

+ `set_bb_theme()` and `unset_bb_theme()` (2020-02-11, Tue)

# plotbb 0.0.2

+ `bb_scale_col_palette()` (2020-01-06, Mon)
+ `bb_tile()` for heatmaps
+ `bb_col()` supports character and numeric variables

# plotbb 0.0.1

+ initial grammar: `bbplot()`, `bb_aes()`, `+` and `as.bbplot()` (2019-12-28, Sat)
+ `bb_point()`, `bb_lm()` and `bb_text()`
+ labels, titles and themes
+ `bb_grid()` and the first theme presets
