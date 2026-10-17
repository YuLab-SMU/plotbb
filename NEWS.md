# plotbb 0.2.5

This release fixes the grammar rather than adding layers: it states the rules a
user can rely on when composing a plot, and rewrites the vignette around them.

## Inheritance

+ every layer now accepts `inherit`. A local mapping overrides the global
  mapping aesthetic by aesthetic; `inherit = FALSE` makes the layer use its own
  mapping only. Previously `inherit` did not exist and a layer always inherited
  everything
+ layer `data` is a first-class input: it contributes to the axis ranges and to
  `bb_facet_wrap(scales = "free_*")` ranges. Each panel takes the rows of the
  layer data that carry the panel's facet value, using the same facet expression
  as the plot data (so `~f`, `~paste(f, g)` and transformed keys all work);
  row-aligned layer data with the same number of rows as the plot data uses the
  panel row positions instead, and any other layout is left untouched rather
  than silently mis-sliced
+ a layer `data` that cannot be turned into a data frame is reported with a
  message naming the layer, instead of failing later with "no data to draw"
+ `bb_facet_wrap()` warns when a layer's own data can neither be split by the
  facet expression nor be row-aligned with the plot data, instead of silently
  drawing the whole layer in every panel

## Constants versus mappings

+ enforced in one place instead of per layer: a constant aesthetic removes the
  mapping of that aesthetic for the layer (`bb_layer_aesthetics()`), so it is
  never sent through a scale, is not repainted by a later scale, and does not
  group a statistical layer. Previously the rule was re-implemented by every
  layer and broke in `bb_hist()` (mapped `col` overwrote `col =`) and
  `bb_density()` (constant `col`/`fill` was ignored entirely); `bb_boxplot()`
  let a mapped `col` take over a constant one
+ a length-n constant is row-aligned: it follows the rows that survive
  validation, the same way mapped values do, so dropping `NA`/infinite rows no
  longer shifts a colour, `pch` or `cex` vector onto the wrong points
+ `bb_hist()` now groups like `bb_density()`: discrete `x`, else `group`, then
  `col`, then `fill` (`bb_stat_group()`), instead of silently ignoring a
  `group` mapping
+ `fill` is accepted only where the drawing primitive has an interior separate
  from its border (`bb_density()`, `bb_tile()`, a custom `bb_layer()`); passing
  it to another layer is an error naming `col`, instead of being silently
  dropped by the base function
+ mapped aesthetics are resolved when the plot is drawn, so the scale in effect
  is the last one in the `+` chain. `bb_point()` and `bb_layer()` no longer
  freeze a mapped colour at the moment the layer is added

## Boundaries

+ `+` now refuses to combine two complete `bbplot` objects with a message that
  points to `aplot::plot_list()`; previously it failed with an internal
  `UseMethod("bbplot_add")` error
+ `+` accepts bare calls (e.g. `quote(points(...))`) like formulas and
  expressions
+ new `bb_layer(draw, mapping, data, inherit, ..., name, limits)` is the
  supported custom-layer entry point: it inherits data and mapping, draws in `+`
  order, honours constants, and propagates errors unchanged. Its mapped `x`/`y`
  values contribute to the ranges and its `data` follows the facets; ranges that
  the drawing function computes itself (a reference line, a band) are declared
  with `limits = list(x = ..., y = ...)` and then take part in the canvas. The
  scales and the legend are not extended

## Documentation

+ the vignette was rewritten from scratch around the grammar (plot object and
  components, data and mapping, inheritance, constants, order, scales/legends,
  facets, statistical layers, custom layers and the base graphics escape hatch)
  instead of being extended section by section, with each rule explained in
  prose as well as demonstrated, plus a one-page contract recap and an error
  message guide
+ new `notes/0.2.5-inventory.md` records the inventory of delivered features and
  their support status, which the vignette is checked against
+ all layer help pages document `inherit`; `bbplot()` documents the `+` and
  inheritance rules; the vignette lists which aesthetics each layer maps
+ known limit, stated rather than hidden: a mapped `lty` is read by the legend
  but not yet applied by the line-based layers (0.3.0 scale work)

## Tests

+ new `test-grammar.R` covers mapping override, `inherit = FALSE`, layer data in
  ranges and facets (including facet-column subsetting), constants versus the
  last scale, grouped histogram/density behaviour, draw order, graphics-state
  restoration after a layer error, `bb_layer()` and the boundary of `+`

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
