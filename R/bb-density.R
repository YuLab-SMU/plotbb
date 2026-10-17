##' density layer
##'
##' @title bb_density
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param orientation `"vertical"` (default; density on y) or `"horizontal"`
##'   (density on x). A horizontal density estimates `y`, or `x` when `y` is a
##'   discrete group.
##' @param ... addition parameter for the layer, such as \code{adjust}, \code{kernel}.
##' @return A modified bbplot object
##' @importFrom stats density
##' @export
##' @author Guangchuang Yu
bb_density <- function(mapping = NULL, data = NULL, inherit = TRUE,
                       orientation = c("vertical", "horizontal"), ...) {
    orientation <- match.arg(orientation)
    build_layer(mapping, data, inherit = inherit, orientation = orientation,
                ..., layer = ly_density)
}

## The value whose density is estimated. Vertical uses x (or y when x is a
## discrete group); horizontal uses y (or x when y is a discrete group).
bb_density_values <- function(mapping, data, orientation) {
    x <- eval_mapping(mapping, "x", data)
    y <- eval_mapping(mapping, "y", data)
    if (identical(orientation, "horizontal")) {
        if (is.numeric(y)) {
            group <- if (!is.null(x) && !is.numeric(x)) x else NULL
            return(list(x = y, group = group))
        }
        if (is.numeric(x)) return(list(x = x, group = y))
        bb_abort("a horizontal density needs a numeric `y`, or a numeric `x` ",
                 "with a discrete `y`.")
    }
    bb_stat_values(x, y)
}

## Fill colour of a polygon layer.
##
## A constant `fill`/`col` applies to the whole layer: it is not a mapping and is
## never sent through a colour scale. Without a constant, the mapped `col` (or
## `fill`) decides.
bb_density_colour <- function(params, mapping, data, plot) {
    constant <- params$fill %||% params$col
    if (!is.null(constant)) return(constant)
    if (!is.null(mapping$fill)) return(bb_fill(mapping, data, plot = plot))
    bb_col(mapping, data, plot = plot)
}

ly_density <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)
    orientation <- params$orientation %||% "vertical"
    params$orientation <- NULL
    if (!orientation %in% c("vertical", "horizontal")) {
        bb_abort("`orientation` in `bb_density()` must be 'vertical' or ",
                 "'horizontal'.")
    }

    ly <- function() {
        if (is.null(mapping$x) && is.null(mapping$y)) return(invisible(NULL))

        values <- bb_density_values(mapping, data, orientation)
        x_raw <- values$x
        if (is.null(x_raw)) {
            bb_abort("`bb_density()` requires a numeric variable to estimate.")
        }

        grp <- bb_stat_group(values, mapping, data)

        args <- params
        alpha <- args$alpha %||% 0.5

        if (is.null(grp)) {
            x_raw <- x_raw[is.finite(x_raw)]
            if (length(x_raw) < 2) {
                bb_warn("`bb_density()` needs at least two finite values; skipped.")
                return(invisible(NULL))
            }
            d <- stats::density(x_raw, na.rm = TRUE,
                                adjust = args$adjust %||% 1,
                                kernel = args$kernel %||% "gaussian")
            col <- bb_density_colour(params, mapping, data, plot) %||% "black"
            if (length(col) > 0) col <- col[1]

            if (identical(orientation, "horizontal")) {
                graphics::polygon(c(0, d$y, 0), c(min(d$x), d$x, max(d$x)),
                                  col = scales::alpha(col, alpha), border = col)
            } else {
                graphics::polygon(c(min(d$x), d$x, max(d$x)), c(0, d$y, 0),
                                  col = scales::alpha(col, alpha), border = col)
            }
        } else {
            grp_chr <- as.character(grp)
            keep <- !is.na(grp_chr) & is.finite(x_raw)
            if (!any(keep)) return(invisible(NULL))

            grp_chr <- grp_chr[keep]
            x_raw <- x_raw[keep]

            levs <- bb_discrete_levels(grp)
            levs <- levs[levs %in% unique(grp_chr)]

            ## The constant colour wins over any mapped one (the mapping is
            ## already removed for a constant aesthetic); a single constant
            ## gives every group the same fill.
            cols <- params$fill %||% params$col
            if (!is.null(cols)) {
                cols <- rep(cols, length.out = length(grp_chr))
            } else if (!is.null(mapping$fill)) {
                cols <- bb_fill(mapping, data, plot = plot)
            } else {
                cols <- bb_col(mapping, data, plot = plot)
            }

            if (!is.null(cols)) {
                # Fallback safely if dimensions don't match
                if (length(cols) != length(grp_chr)) {
                   cols <- rep(cols, length.out = length(grp_chr))
                }
                col_map <- unname(tapply(cols, grp_chr, function(z) z[[1]])[levs])
            } else {
                col_map <- rep("black", length(levs))
            }

            for (i in seq_along(levs)) {
                g <- levs[i]
                sub_x <- x_raw[grp_chr == g]
                if (length(sub_x) < 2) next

                d <- stats::density(sub_x, na.rm = TRUE,
                                    adjust = args$adjust %||% 1,
                                    kernel = args$kernel %||% "gaussian")
                c_col <- col_map[i]
                if (identical(orientation, "horizontal")) {
                    graphics::polygon(c(0, d$y, 0),
                                      c(min(d$x), d$x, max(d$x)),
                                      col = scales::alpha(c_col, alpha),
                                      border = c_col)
                } else {
                    graphics::polygon(c(min(d$x), d$x, max(d$x)),
                                      c(0, d$y, 0),
                                      col = scales::alpha(c_col, alpha),
                                      border = c_col)
                }
            }
        }
    }

    add_layer(plot, ly, "density layer")
}
