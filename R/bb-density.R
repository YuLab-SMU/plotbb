##' density layer
##'
##' @title bb_density
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param ... addition parameter for the layer, such as \code{adjust}, \code{kernel}.
##' @return A modified bbplot object
##' @importFrom stats density
##' @export
##' @author Guangchuang Yu
bb_density <- function(mapping = NULL, data = NULL, inherit = TRUE, ...) {
    build_layer(mapping, data, inherit = inherit, ..., layer = ly_density)
}

## Fill colour of a polygon layer.
##
## A constant `fill`/`col` applies to the whole layer: it is not a mapping and is
## never sent through a colour scale. Without a constant, the mapped `col` (or
## `fill`) decides.
bb_density_colour <- function(params, mapping, data, plot) {
    params$fill %||% params$col %||% bb_col(mapping, data, plot = plot)
}

ly_density <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)


    ly <- function() {
        if (is.null(mapping$x)) return(invisible(NULL))

        x_raw <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
        y_raw <- eval_mapping(mapping, "y", data)
        if (is.null(x_raw)) {
            bb_abort("`bb_density()` requires an `x` mapping.")
        }
        values <- bb_stat_values(x_raw, y_raw)
        x_raw <- values$x

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

            graphics::polygon(c(min(d$x), d$x, max(d$x)), c(0, d$y, 0), col = scales::alpha(col, alpha), border = col)
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
            } else {
                cols <- bb_col(mapping, data, plot = plot)

                # Support mapping fill directly for polygon-based layers.
                if (!is.null(mapping$fill)) {
                     fill_var <- eval_mapping(mapping, "fill", data)
                     scale_fill <- plot$scales$fill %||% plot$scales$col
                     if (is.null(scale_fill)) {
                         scale_fill <- structure(list(palette = NULL, type = "palette"), class = "bb_palette")
                     }
                     mapped_cols <- bb_scale_col_map(scale_fill, fill_var)
                     if (!is.null(mapped_cols)) cols <- mapped_cols
                }
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
                graphics::polygon(c(min(d$x), d$x, max(d$x)), c(0, d$y, 0), col = scales::alpha(c_col, alpha), border = c_col)
            }
        }
    }

    add_layer(plot, ly, "density layer")
}
