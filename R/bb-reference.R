##' reference lines
##'
##' `bb_hline()` and `bb_vline()` draw horizontal and vertical reference lines.
##' The position is either a constant vector (`yintercept` / `xintercept`) or a
##' mapping of the same name, so thresholds can come from the data as well.
##'
##' By default a reference line does **not** change the axis range: a line at the
##' edge of the data should not silently extend the canvas. Set
##' `expand_limits = TRUE` when the intercept is meant to be inside the visible
##' range even though the data do not reach it.
##'
##' A mapped intercept wins over the constant argument. With a mapped intercept,
##' `col` and `lty` may also be mapped and are then applied per line; with a
##' constant intercept those aesthetics are layer-wide constants.
##'
##' When the layer brings its own data, inherited mappings are evaluated on that
##' data too; use `inherit = FALSE` when the plot maps aesthetics the reference
##' lines do not carry.
##'
##' The layers follow the grammar contract: they inherit data and mapping, accept
##' their own, draw in `+` order, honour constant `col`, `lty` and `lwd`, and are
##' drawn once per facet panel.
##'
##' @title bb_hline
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param yintercept numeric y position(s) for horizontal lines
##' @param xintercept numeric x position(s) for vertical lines
##' @param expand_limits whether the intercept extends the axis range
##' @param ... additional parameters passed to \code{graphics::abline}, such as
##'   \code{col}, \code{lty} and \code{lwd}
##' @return A modified bbplot object
##' @importFrom graphics abline
##' @export
##' @author Guangchuang Yu
bb_hline <- function(yintercept = NULL, mapping = NULL, data = NULL,
                     inherit = TRUE, expand_limits = FALSE, ...) {
    if (!is.logical(expand_limits) || length(expand_limits) != 1L ||
        is.na(expand_limits)) {
        bb_abort("`expand_limits` in `bb_hline()` must be TRUE or FALSE.")
    }
    build_layer(mapping, data, inherit = inherit, ...,
                layer = ly_hline, yintercept = yintercept,
                expand_limits = expand_limits)
}

##' @rdname layer
##' @export
bb_vline <- function(xintercept = NULL, mapping = NULL, data = NULL,
                     inherit = TRUE, expand_limits = FALSE, ...) {
    if (!is.logical(expand_limits) || length(expand_limits) != 1L ||
        is.na(expand_limits)) {
        bb_abort("`expand_limits` in `bb_vline()` must be TRUE or FALSE.")
    }
    build_layer(mapping, data, inherit = inherit, ...,
                layer = ly_vline, xintercept = xintercept,
                expand_limits = expand_limits)
}

## Data are only required when the intercept comes from a mapping; a fixed
## reference line can be drawn on a canvas that has no data of its own.
bb_reference_data <- function(plot, data, mapping) {
    if (!is.null(data) || !is.null(plot$data)) {
        return(bb_layer_data(plot, data, mapping))
    }
    if (!is.null(mapping$yintercept) || !is.null(mapping$xintercept)) {
        bb_abort("a mapped reference line requires data: supply it in ",
                 "`bbplot()` or through the layer's `data` argument.")
    }
    NULL
}

bb_reference_values <- function(mapping, data, constant, aesthetic) {
    mapped <- if (is.null(data)) NULL else eval_mapping(mapping, aesthetic, data)
    values <- if (!is.null(mapped)) mapped else constant
    if (is.null(values)) {
        bb_abort("a reference line requires `", aesthetic, "` or a `",
                 aesthetic, "` mapping.")
    }
    if (!is.numeric(values)) {
        bb_abort("`", aesthetic, "` must be numeric.")
    }
    list(values = values, mapped = !is.null(mapped),
         n = if (!is.null(mapped)) length(mapped) else length(values))
}

ly_hline <- function(plot, mapping = NULL, data = NULL, inherit = TRUE,
                     yintercept = NULL, expand_limits = FALSE, ...) {
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)
    data <- bb_reference_data(plot, data, mapping)

    ly <- function() {
        ref <- bb_reference_values(mapping, data, yintercept, "yintercept")
        ok <- is.finite(ref$values)
        if (!any(ok)) {
            bb_abort("`bb_hline()` has no finite intercept to draw.")
        }
        values <- ref$values[ok]

        args <- params

        ## A mapped position can carry a colour/line type per line; a constant
        ## position only takes constants, which are already in `params`.
        if (ref$mapped && is.null(args$col) && !is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
            if (!is.null(col_vec)) {
                args$col <- rep(col_vec, length.out = ref$n)[ok]
            }
        }
        if (ref$mapped && is.null(args$lty) && !is.null(mapping$lty)) {
            lty_vec <- bb_lty(mapping, data, plot = plot)
            if (!is.null(lty_vec)) {
                args$lty <- rep(lty_vec, length.out = ref$n)[ok]
            }
        }

        do.call(graphics::abline, c(list(h = values), args))
        invisible(NULL)
    }

    add_layer(plot, ly, "horizontal reference line")
}

ly_vline <- function(plot, mapping = NULL, data = NULL, inherit = TRUE,
                     xintercept = NULL, expand_limits = FALSE, ...) {
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)
    data <- bb_reference_data(plot, data, mapping)

    ly <- function() {
        ref <- bb_reference_values(mapping, data, xintercept, "xintercept")
        ok <- is.finite(ref$values)
        if (!any(ok)) {
            bb_abort("`bb_vline()` has no finite intercept to draw.")
        }
        values <- ref$values[ok]

        args <- params

        if (ref$mapped && is.null(args$col) && !is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
            if (!is.null(col_vec)) {
                args$col <- rep(col_vec, length.out = ref$n)[ok]
            }
        }
        if (ref$mapped && is.null(args$lty) && !is.null(mapping$lty)) {
            lty_vec <- bb_lty(mapping, data, plot = plot)
            if (!is.null(lty_vec)) {
                args$lty <- rep(lty_vec, length.out = ref$n)[ok]
            }
        }

        do.call(graphics::abline, c(list(v = values), args))
        invisible(NULL)
    }

    add_layer(plot, ly, "vertical reference line")
}
