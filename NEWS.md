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
