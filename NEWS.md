# plotbb 0.2.0

## Graphics state

+ `print.bbplot()` now snapshots and restores graphics parameters for **every** plot, not only when a theme is set. Parameters changed by a plot, a theme, or a custom base-graphics layer are restored on normal return **and when a layer raises an error**; the original condition still propagates unchanged
+ `print.bbplot()` no longer leaves `par(new = TRUE)` behind, and faceted plots restore `mfrow`/`oma`/`mar`
+ `unset_bb_theme()` now clears the saved pre-theme state, so it is safe to call repeatedly and `set_bb_theme()` always restores the state from before the first call
+ new `with_bb_theme(theme, code)` applies a theme to the device for the duration of `code` and restores it afterwards, also on error; it is scoped (no options involved) and can be nested
+ `set_bb_theme()` returns the applied theme invisibly

## Input validation

+ coordinates that cannot be drawn are handled predictably: rows with `NA`, `NaN` or infinite values are dropped, and a layer whose rows are *all* invalid raises an error instead of reaching `plot()` with `Inf` limits
+ aesthetic mappings are validated when the layer is built: a mapped or literal aesthetic (`col`, `fill`, `pch`, `lty`, `cex`, `label`, `ymin`, `ymax`, `xend`, `yend`, ...) must have length 1 or one value per row. A mismatch is an error naming the aesthetic and the layer, instead of silent vector recycling
+ errors now tell the user which layer needs data (e.g. layers added to `as.bbplot()` objects) and which mapping is missing (`bb_segment()` endpoints, `bb_errorbar()` bounds, `bb_lm()` x/y)
+ `bb_signif()` validates `comparisons` and `test` when the layer is built rather than during drawing
+ `bb_hist()` and `bb_density()` warn once and skip data with too few finite values, instead of failing
+ `bb_plot_limits()` always returns finite limits: a plot without drawable data falls back to `[0, 1]` instead of passing `Inf` to `plot()`

## Reproducible jitter

+ `bb_point(position = "jitter")` gained a `seed` argument (default `1`), so jittered positions are reproducible. `seed = NULL` jitters from the current random state
+ jittering never advances the caller's random number stream and never leaves a `.Random.seed` behind when there was none
+ new `bb_position()` builds a jitter position object with its own `width`, `height` and `seed`

## Fixes

+ layer arguments are now passed by name (`rlang::exec()` instead of `do.call()`): previously a list-valued argument such as `col = c("red", "blue")` replaced the layer function's formal argument, so the value was silently dropped
+ layers are no longer stacked on top of the previous plot via `par(new = TRUE)`; overlapping plots would produce a garbled figure. Use `aplot::plot_list()` to combine plots
+ `bb_bar(stat = "count")` computes fill colours once instead of inside every redraw

## Tests

+ new `test-state.R` covers `par()` restoration on normal return, on error, for faceted plots and for data-less devices, plus `with_bb_theme()` scoping and nesting
+ new `test-inputs.R` covers invalid coordinates, length mismatches, missing data and reproducible jitter

# plotbb 0.1.2

+ `bb_density` for density plots, including grouped density polygons with `fill` mapping and alpha blending (2026-05-21, Thu)
+ `bb_lm(se = TRUE)` supports confidence intervals for grouped linear fits, and skips CI prediction for tiny groups without residual degrees of freedom
+ `bb_legend` supports discrete `fill` legends and placing legends outside faceted plots
+ `bb_facet_wrap(nrow = 1)` layout is fixed, with panel strip titles showing facet labels and `bb_title` used as the overall plot title
+ add regression tests for faceting with global titles and grouped `bb_lm` CI behavior

# plotbb 0.1.1

+ `bb_boxplot` and `bb_hist` (2026-01-30, Fri)
+ `bb_signif` supports `comparisons` for significance annotations
+ `bb_theme_bw` and `bb_theme_minimal`
+ add unit tests for `bb_bar`, `bb_boxplot`, `bb_hist` and `bb_signif`

# plotbb 0.1.0

+ `bb_line`, `bb_segment`, `bb_errorbar`, and `bb_bar` (2026-01-15, Thu)
+ fix `bb_bar(stat = "count")` order to respect factor levels
+ evaluate aesthetic mappings with `rlang::eval_tidy` for better expression support
+ `bb_legend` supports both discrete and continuous color mapping
+ `bb_scale_col_manual` and `bb_scale_col_gradient`
+ `bb_scale_pch_manual`, `bb_scale_lty_manual`, `bb_scale_cex_manual` and `bb_scale_cex_continuous`
+ `bb_facet_wrap` supports `scales = "free"` (`free_x`/`free_y`)
+ migrate vignette to Quarto and build with `VignetteBuilder: quarto`
+ fix roxygen exports for S3 methods and public functions

# plotbb 0.0.5

+ add more details in Description text and document return values of `bb_aes`, `set_bb_theme` and `unset_bb_theme` (2021-08-27, Fri)

# plotbb 0.0.4

+ add examples and prepare for submitting to cran (2021-08-22, Sun)

# plotbb 0.0.3

+ `set_bb_theme` and `unset_bb_theme` (2020-02-11, Tue)

# plotbb 0.0.2

+ `bb_scale_col_palette` (2020-01-06, Mon)
+ `bb_tile` for plotting heatmap 
+ `bb_col` supports both character and numeric variable

# plotbb 0.0.1

+ formula, expression and function can be added to `bbplot` object (2019-12-28, Sat)
+ `bb_text`
+ `as.bbplot` to convert function as `bbplot` object
+ `bb_grid` to add grid line (background grid line by default)
+ `bb_theme_expand`, `bb_theme_grey` and `bb_theme_deepblue`
+ `+` operator supported, e.g. `bbplot() + bb_point()`
+ `bb_theme` for setting graphic parameters
+ `bb_point` and `bb_lm` geometric layers
+ `bb_labs`, `bb_title`, `bb_sub`, `bb_xlab` and `bb_ylab` for setting labels
+ `bb_aes` for aesthetic mapping
+ `bbplot` function that produces `bbplot` object
