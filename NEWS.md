# plotbb 0.13.1

Audit fix for horizontal smooth.

+ `bb_coord_flip()` now swaps a supplied `bb_smooth()` formula along with the
  mapping, so a vertical `y ~ x` fit becomes the horizontal `x ~ y` fit instead
  of failing the response check
+ a regression test covers the flipped formula

# plotbb 0.13.0

Horizontal density, histogram and smooth layers complete the coord-flip story.

## Horizontal statistical layers

+ `bb_density(orientation = "horizontal")` draws the density on x and the
  estimated values on y; `bb_hist(orientation = "horizontal")` puts counts on x
+ `bb_smooth(orientation = "horizontal")` fits `x` against `y`, draws the curve
  and confidence band sideways, and converts a supplied formula written in the
  mapped variables
+ summary labels follow the orientation (density/count on x for horizontal
  layers)
+ `bb_coord_flip()` supports density, histogram and smooth by swapping the
  mapping and toggling their orientation

## Tests and documentation

+ test-orientation-0.9.R covers horizontal density/histogram/smooth and their
  coord-flip forms
+ the vignette statistical section documents horizontal statistics; known limits
  now list only `bb_lm()`/`bb_signif()` as non-flippable

# plotbb 0.12.0

Horizontal ribbons.

+ `bb_ribbon(orientation = "horizontal")` maps `y` to the centre and `xmin`/`xmax`
  to the band; ranges and the facet panels follow the orientation
+ `bb_coord_flip()` turns a vertical ribbon horizontal by swapping
  `x`/`y`, `ymin`/`xmin` and `ymax`/`xmax` and toggling the orientation

# plotbb 0.11.0

Horizontal bars, errorbar flipping and outside-legend width control.

## Bars and error bars

+ `bb_bar(orientation = "horizontal")` draws count bars with categories on y and
  counts on x, and identity bars with numeric values on x and categories on y
+ `bb_coord_flip()` supports `bb_bar()` (count and identity) and
  `bb_errorbar()` by toggling their orientation; count bars keep the category
  mapping because counts are computed

## Outside legends

+ `bb_legend(width = )` sets the relative width of the outside legend column;
  the plot layout uses the widest requested legend
+ multiple outside legends continue to stack by their actual drawn height

## Tests and documentation

+ test-orientation-0.9.R covers horizontal count and identity bars and their
  ranges; test-coord-0.9.R covers bar and errorbar flipping
+ the layer reference and known limits describe the horizontal bar mapping

# plotbb 0.10.0

Horizontal statistical layers and a wider coord flip.

## Orientation

+ `bb_violin(orientation = "horizontal")` puts the numeric values on x and the
  groups on y (map `x` to values and `y` to groups); the ranges, density support
  and `trim = FALSE` behaviour follow the orientation
+ `bb_boxplot(orientation = "horizontal")` follows the same mapping convention
+ `bb_coord_flip()` now flips violin and boxplot by swapping the mapping and
  toggling their orientation, in addition to point, line, text, segment, tile,
  custom layers and hline/vline

## Outside legend stacking

+ multiple outside legends are stacked using the height `graphics::legend()`
  reports, so automatic legends no longer clip each other at the bottom
+ `bb_coord_flip()`'s error lists the layers that need an orientation model;
  `bb_bar()` remains vertical-only

## Tests and documentation

+ test-orientation-0.9.R covers horizontal violin/boxplot and validation;
  test-coord-0.9.R now checks violin/boxplot flip and an unsupported layer
+ the vignette documents the horizontal mapping and the updated coord-flip scope

# plotbb 0.9.1

Small fixes after the 0.9.0 release.

+ an error inside a custom layer's `range` function now propagates instead of
  being silently ignored while computing the canvas
+ the grouping-rule paragraph in the vignette matches the unified rule
  (`group` > discrete `x` > `col` > `fill`)

# plotbb 0.9.0

This release addresses the three structural gaps: positional scales, automatic
legends, and the grouping/statistical-layer extension contract.

## Positional scales and coord flip

+ `bb_scale_x_continuous()`, `bb_scale_y_continuous()`, `bb_scale_x_log10()`,
  `bb_scale_y_log10()`, `bb_scale_x_date()` and `bb_scale_y_date()` control
  axis limits, breaks, labels, log10 transforms and Date axes
+ limits are the final axis range, so values outside them are clipped instead
  of stretching the canvas
+ `bb_coord_flip()` swaps the axes, labels and positional scales for
  coordinate-symmetric layers (point, line, text, segment, tile, custom and
  hline/vline); layers with their own orientation raise a clear error

## Automatic legends

+ mapped `col`, `fill`, `pch`, `lty` and `cex` get outside legends by default;
  `bbplot(auto_legend = FALSE)` disables them and an explicit `bb_legend()` for
  an aesthetic takes over that aesthetic
+ a layer-local mapping gets a per-layer automatic legend, so its title comes
  from the layer mapping
+ a free-colour facet skips automatic outside legends (one legend cannot
  describe several colour domains)

## Unified grouping and layer registry

+ `bb_group_values()` is the one grouping rule: explicit `group`, then a
  discrete `x`, then mapped `col`, then mapped `fill`; density/histogram,
  violin, ribbon and smooth all use it
+ `bb_layer()` gains `aesthetics`, `summary` and `range` so a custom
  statistical layer can declare fill support, computed coordinates and its
  ranges without editing package internals
+ `bb_plot_summarises()` and `bb_layer_is_summary()` honour the declared
  `summary`; `bb_layer_limits()` calls the declared `range`

## Tests and documentation

+ test-auto-legend-0.9.R, test-position-0.9.R, test-coord-0.9.R and
  test-registry-0.9.R cover the new contracts
+ the vignette documents positional scales, coord flip, automatic legends and
  the custom statistical-layer registry

# plotbb 0.8.1

Bug fixes found while auditing the 0.6.0-0.8.0 features.

## Ranges

+ `bb_smooth()` now includes its confidence band in the y range; the band range
  is cached when the layer is added, so drawing and `bb_build()` do not refit
  the model
+ `bb_violin(trim = FALSE)` includes the estimated density support in the y
  range instead of clipping the tails

## Validation

+ `bb_smooth()` rejects a formula whose response is not exactly `y` (for
  example `log(y) ~ x`), which would otherwise draw the band on the wrong scale
+ `bb_violin()` validates `kernel`, `bw` and `n` instead of silently skipping
  groups when the density cannot be estimated

## Layout

+ long row labels in `bb_facet_grid()` are drawn horizontally and the right
  margin now follows the longest wrapped label instead of clipping it

## Documentation

+ the per-layer legend example groups the line so its colours actually match the
  legend
+ known limits document that violin/boxplot use numeric group positions and that
  a mapped `col`/`lty` on `bb_line()` applies to the whole polyline

# plotbb 0.8.0

This release closes the legend and colour-scale backlog.

## Per-layer legends

+ `bb_legend(layer = )` describes one layer's mapping (1-based index or layer
  name) and uses its mapping label as the default title; the colours still come
  from the shared trained scale, so this selects the explained mapping, not a
  second domain
+ `style = "bar"` draws a discrete col/fill legend as adjacent colour blocks
+ `title_width` wraps a long legend title

## Free colour scales by facet

+ `bb_facet_wrap(..., free_colour = TRUE)` and
  `bb_facet_grid(..., free_colour = TRUE)` train the col/fill domain per panel
  from that panel's rows, while every other aesthetic keeps the shared scale
+ this requires an explicit `bb_scale_col_*()`/`bb_scale_fill_*()` scale; the
  default colour mapping is value-based and shared
+ an outside legend combined with `free_colour = TRUE` is an error, because one
  legend cannot describe several domains; inside legends show the panel domain
+ `bb_build()` reports each panel's `col_domain`/`fill_domain` and the facet
  summary records `free_colour`

## Colour-bar polish

+ `minor_breaks` on the colour scales adds small ticks between the labelled
  breaks of the colour bar; `NULL` uses the major-break midpoints

## Tests and documentation

+ test-legends-0.8.R covers per-layer titles, invalid layer selection, bar
  style, wrapped titles, minor ticks, free colour domains in wrap and grid, and
  the outside-legend error
+ the vignette documents per-layer legends, free colour facets and minor breaks
+ known limits now state the explicit-scale requirement and the shared-scale
  nature of a per-layer legend

# plotbb 0.7.0

This release adds the two remaining statistical layers: violin and smooth.

## bb_violin()

+ one density polygon per group, estimated with `stats::density()` and scaled to
  `width` in x units; `adjust`/`bw` control the bandwidth, `kernel`, `trim` and
  `n` are passed through
+ `x` can be a factor, character or numeric group position; `y` must be numeric;
  the layer contributes the group positions and the values to the ranges
+ groups with fewer than two finite values or zero spread are skipped with a
  warning; only vertical violins are drawn
+ `fill`/`col` can be constant or mapped and the layer takes part in the fill
  scale and legend

## bb_smooth()

+ explicit fit method (`"loess"` or `"lm"`), formula (default `y ~ x`),
  `method.args`, `se` and `level`; the fit is computed when the layer is added,
  so repeated draws do not re-fit
+ groups come from `group`, then a mapped `col`, then `fill`; each group gets its
  own curve and confidence band
+ groups with too few points or distinct x values fall back to a linear fit with
  a warning; a group with fewer than two points is skipped
+ the band uses `fill` (or the curve colour at low alpha), and the curve uses
  `col`/`lty`/`lwd`; the band is drawn before the line and contributes to the
  ranges

## Tests and documentation

+ test-layers-0.7.R covers violin polygons, skipped groups, numeric positions,
  fill legends and facets; smooth curves/bands, fallback warnings, formulas,
  facets and parameter validation
+ the vignette statistical section documents both layers with runnable examples
+ the known-limits list now leaves only the per-layer legend and colour-bar
  polish as planned work

# plotbb 0.6.0

This release finishes the facet layout and adds a row/column facet grid.

## Facets

+ `bb_facet_wrap()` gains `labeller`, `drop` and `strip_width`: strip labels can
  be relabelled and wrapped, and `drop = FALSE` keeps unused factor levels as
  empty panels (they fall back to the global limits, also on free scales)
+ `bb_facet_grid()` places one variable on rows and another on columns, with
  column labels on top and row labels on the right; a combination without rows
  is drawn empty
+ grid free scales follow the layout: x is shared within a column and y within a
  row, while `fixed` uses the global limits
+ `bb_build()` reports grid panels with their row/column keys, labels, row
  counts and the limits that are drawn; the facet summary records `kind = "grid"`

## Tests and documentation

+ test-facets-0.6.R covers empty levels, labellers/wrapping, grid layout,
  every-direction grids, empty combinations, outside legends and build/draw
  limit equality
+ the vignette facet section documents `drop`, `labeller`, `strip_width` and
  `bb_facet_grid()`
+ the layout boundary is recorded: complete plots are combined with
  `aplot::plot_list()` (aplot has no `plot_grid()`), while facets stay in plotbb

# plotbb 0.5.0

This release adds the build and save workflow: a plot can be inspected without
drawing it and written to a file without disturbing the caller's graphics state.

## bb_build()

+ `bb_build()` performs the computation `print()` performs — trains the scales,
  resolves the mappings, collects layer data and computes the limits — and
  returns a plain list with `data`, `mapping` (labels), `layers` (name, data,
  mapping labels, inherit, parameter names), `scales`, `limits`, `panels`,
  `labs`, `facet` and `warnings`
+ it does not open a device, change `par()` or touch `.Random.seed`; repeated
  builds of the same object return the same structure and limits
+ faceted plots report one entry per panel with its key, row count and the
  limits the panel would use (global for shared scales, per-panel for free
  scales)
+ warnings raised while building are collected in `warnings` and muffled; print
  and build share `bb_prepare_plot()`, so the built limits are the drawn limits

## bb_save()

+ `bb_save()` opens the device implied by the extension (`pdf`, `png`,
  `jpeg`/`jpg`, `tiff`/`tif`, `svg`) or an explicit device name/function,
  draws the plot and closes only the device it opened
+ `width`/`height` with `units` (`in`, `cm`, `mm`, `px`) and `res` for raster
  devices; the caller's active device is restored even with several devices open
+ an existing file requires `overwrite = TRUE`; a missing directory, a device
+ conflicting with the extension and invalid sizes are clear errors
+ drawing goes to a temporary file next to the target and the file is
  moved into place only after success; on failure the temporary file is
  removed, the original error is kept, the caller's device is restored
  and an existing target is left untouched even with `overwrite = TRUE`

## Extension API

+ a component that adds layers now has those layers marked, so a faceted plot
  re-runs the component per panel without drawing the generated layers twice
+ the raw function/formula/expression escape hatch is reported by `bb_build()`
  as `kind = "function"` (no mapping, data inheritance, scale or range)
+ `bb_build()` validates `nrow`/`ncol` and the layout capacity like the printer,
  and requires plot data for a faceted build
+ `bbplot_add()` is documented as the public contract for a new component class:
  a method returns the modified plot, adds through public constructors such as
  `bb_layer()`, and must not draw directly or touch internal helpers; packages
  register the method with `S3method()`
+ the vignette has an "Extending from a package" section comparing the escape
  hatch, `bb_layer()` and `bbplot_add()`, with an example that uses only public
  API and is covered by the facet/order/state rules

## Decisions recorded

+ `notes/0.5.0-decisions.md` records the decisions on a per-layer legend, free
+ colour scales by facet, `bb_facet_grid()` and the remaining colour-bar polish;
+ those features are not implemented in this release

## Tests and documentation

+ new `test-build-save.R` covers build fields, side-effect freedom, repeated
  builds, facet panel limits, collected warnings, pdf/png output, overwrite and
  directory rules, device conflicts, device restoration and failure cleanup
+ the vignette has an "Inspecting and saving a plot" section

# plotbb 0.4.0

This release adds the high-frequency scientific layers and keeps them inside
the grammar contract: data/mapping inheritance, constant aesthetics, range
contribution, draw order and facets behave as everywhere else.

## New layers

+ `bb_ribbon()` draws a band between `ymin` and `ymax` along `x`, one polygon
  per group, sorted by `x`; rows with a missing or non-finite coordinate are
  dropped (a missing group warns), a group needs two finite points and is
  skipped with a warning otherwise, `alpha` (0-1) controls transparency (0.3),
  `border`/`lty`/`lwd` are layer-wide, and `fill`/`col` can be mapped or
  constant
+ `bb_hline()` and `bb_vline()` draw reference lines at constant or mapped
  `yintercept`/`xintercept` positions; a mapped position can carry `col` and
  `lty` per line
+ reference lines do not change the axis ranges by default; set
  `expand_limits = TRUE` when the intercept must be inside the visible range

## Facets

+ `bb_facet_wrap()` validates `nrow`/`ncol` (positive integers) and reports a
  layout that is too small for the number of panels instead of silently
  overwriting panels

## Layer contract

+ an inherited mapping that cannot be evaluated on a layer's own data no
  longer raises "object not found" while computing ranges; that layer simply
  contributes no range for it, which keeps `bb_ribbon(data = ...)` and similar
  partial-data layers usable

## Tests and documentation

+ new `test-layers-0.4.R` covers ribbon sorting/grouping/ranges/fill, reference
  line positions, mapped intercepts, `expand_limits`, facet rendering and
  layout validation
+ the vignette has a "Bands and reference lines" section with a confidence
  band workflow and threshold lines
+ `notes/0.4.0-layers.md` records the layer decisions

# plotbb 0.3.0

This release unifies the scale and legend semantics: one trained scale per
aesthetic, used by both the layers and the legend, with `col` and `fill` as
separate aesthetics.

## Scales

+ scales are trained at draw time from the plot-level mapping and from every
  layer's effective mapping (including layer data), so a scale works wherever it
  sits in the `+` chain and constants never train a level
+ discrete rules are explicit: factor levels keep their order, character values
  keep first-appearance order, `drop = FALSE` keeps unobserved factor levels,
  named manual palettes match by name and an uncovered value maps to
  `na.value` (with a warning) instead of being recycled
+ continuous rules are explicit: `limits` fixes the domain, `oob = "squish"`
  (default) clamps and `oob = "censor"` maps to `na.value`, a zero-width domain
  is widened, and an all-missing domain maps everything to `na.value`
+ `breaks` and `labels` control the colour bar and are shared with the layer
  colours
+ new `bb_scale_fill_palette()`, `bb_scale_fill_manual()` and
  `bb_scale_fill_gradient()`; `col` and `fill` no longer share a scale, so a
  fill mapping uses the fill scale or the default fill mapping
+ facets train once on the full plot and share one colour domain
+ mapped `lty` is now applied by the line-like layers (`bb_line()`,
  `bb_segment()`, `bb_errorbar()`, `bb_lm()`, `bb_boxplot()`), so the line
  types in the legend match the drawing

## Legends

+ legends are drawn from the same trained scale as the layers: a fill legend
  uses filled boxes, a col legend the mapped colours and shapes
+ continuous colour bars use the scale palette, domain, breaks and labels, and
  support `orientation = "horizontal"`
+ `outside = TRUE` places discrete legends and colour bars in a dedicated
  layout column for both faceted and single plots, instead of a clipped inset

## Fixes that follow from the unified coordinates

+ histograms and densities no longer mix their summary coordinates with the
  mapped x/y when computing the canvas; a grouped histogram is scaled to the
  tallest single group and a density over a discrete x puts the numeric
  variable on the x axis
+ automatic axis labels follow the summary: counts for a histogram, density for
  a density, and the analysed variable on x when x is discrete

## Tests and documentation

+ new `test-scales.R` covers training across layers, factor/character order,
  named and unnamed palettes, limits, unmatched levels, squish/censor, zero
  width, missing domains, col/fill separation, symbol scales, legends inside and
  outside at two device sizes, colour-bar breaks and shared facet domains
+ the vignette scale/legend section is rewritten around training, discrete and
  continuous rules, fill scales and legend placement
+ `notes/0.3.0-scale-design.md` records the API decisions

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
  but not yet applied by the line-based layers (a later scale revision)

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
