##' build a custom layer
##'
##' `bb_layer()` is the supported way to add your own drawing code to a
##' `bbplot`. It is an ordinary layer: it inherits the plot data and mapping,
##' accepts its own `data` and `mapping`, is drawn in `+` order, and takes part
##' in the same input validation as the built-in layers.
##'
##' The drawing function receives the resolved coordinates and, when the layer
##' has mappings, the mapped aesthetics, plus whatever fixed aesthetics you
##' pass through `...`:
##'
##' \preformatted{
##'   bb_layer(function(x, y, col, ...) { ... }, col = "tomato")
##' }
##'
##' A custom layer takes part in the ranges of its mapped `x`/`y` values, like
##' the built-in layers, and its `data` follows the `bb_facet_wrap()` panels.
##' Coordinates that the drawing function computes itself (a smooth curve, a
##' confidence band, a reference line) do not widen the axes on their own; pass
##' them explicitly in `limits`, or supply a `range` function for a statistical
##' layer. The scales and the legend are not extended automatically: if the
##' custom drawing needs a different scale, use the public scale constructors or
##' start from a canvas created by \code{as.bbplot()}.
##'
##' @title bb_layer
##' @param draw a function that draws the layer. Its first arguments are
##'   \code{x} and \code{y} (plus mapped aesthetics such as \code{col}); it
##'   receives the fixed aesthetics in \code{...}.
##' @param mapping aesthetic mapping for this layer. Defaults to inheriting the
##'   plot mapping.
##' @param data layer data. Defaults to the plot data.
##' @param inherit whether to inherit the plot mapping, see \code{bb_point()}.
##'   A custom layer's data is inherited from \code{bbplot()} unless given.
##' @param limits optional range the drawing function needs, as
##'   \code{list(x = c(low, high), y = c(low, high))}. Both elements are
##'   optional. Use it for coordinates that are not mapped from the data, such
##'   as a reference line or an annotation drawn outside the data range.
##' @param aesthetics optional mapped aesthetics the layer understands; include
##'   \code{"fill"} when a constant `fill` is an interior colour.
##' @param summary logical; TRUE when the layer computes its own coordinates and
##'   the plot-level x/y should not be added to the ranges.
##' @param range optional function \code{function(plot, data, mapping)} returning
##'   \code{list(x = ..., y = ...)} with computed ranges for a statistical
##'   layer.
##' @param ... fixed aesthetics and any other values passed to \code{draw}.
##' @param name shown when the plot's layers are printed.
##' @return A layer object to be added to a \code{bbplot} with \code{+}.
##' @seealso \code{\link{bb_point}}, \code{\link{bbplot_add}}
##' @export
##' @examples
##' library(plotbb)
##' p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl)))
##' p +
##'   bb_layer(function(x, y, col, cex = 1, ...) {
##'     graphics::points(x = x, y = y, col = col, pch = 19, cex = cex)
##'   }, cex = 1.6)
##'
##' ## a reference line below the data needs the axis to make room for it
##' p +
##'   bb_point(pch = 19) +
##'   bb_layer(function(x, y, ...) {
##'     graphics::abline(h = min(y) - 50, col = "firebrick", lty = 2)
##'   }, limits = list(y = range(mtcars$disp) + c(-60, 0)))
bb_layer <- function(draw, mapping = NULL, data = NULL, inherit = TRUE,
                     ..., name = "custom layer", limits = NULL,
                     aesthetics = NULL, summary = FALSE, range = NULL) {
    if (!is.function(draw)) {
        bb_abort("`draw` must be a function, not <", class(draw)[[1]], ">.")
    }
    limits <- bb_check_layer_limits(limits)
    if (!is.null(aesthetics)) {
        bad <- setdiff(aesthetics, c("col", "fill", "pch", "lty", "cex"))
        if (length(bad)) {
            bb_abort("`aesthetics` may only name col, fill, pch, lty or cex; ",
                     "got `", bad[[1]], "`.")
        }
        aesthetics <- unique(as.character(aesthetics))
    }
    if (!is.logical(summary) || length(summary) != 1L || is.na(summary)) {
        bb_abort("`summary` in `bb_layer()` must be TRUE or FALSE.")
    }
    if (!is.null(range) && !is.function(range)) {
        bb_abort("`range` in `bb_layer()` must be a function or NULL.")
    }
    params <- list(...)
    params$name <- name
    params$draw <- draw
    do.call(build_layer, c(list(mapping = mapping, data = data,
                                inherit = inherit, limits = limits,
                                layer = ly_custom,
                                meta = list(aesthetics = aesthetics,
                                            summary = isTRUE(summary),
                                            range = range)),
                           params))
}

## Validate a layer's declared ranges, so a typo (`limits = c(0, 10)`) is an
## error instead of silently ignored.
bb_check_layer_limits <- function(limits) {
    if (is.null(limits)) return(NULL)
    if (!is.list(limits) || is.data.frame(limits)) {
        bb_abort("`limits` must be a list with optional `x` and `y` elements, ",
                 "for example `list(x = c(0, 10))`.")
    }
    extra <- setdiff(names(limits), c("x", "y"))
    if (length(extra)) {
        bb_abort("`limits` may only contain `x` and `y`; got `",
                 extra[[1]], "`.")
    }
    if (!length(limits)) {
        bb_abort("`limits` must name at least one of `x` and `y`.")
    }
    for (a in c("x", "y")) {
        value <- limits[[a]]
        if (is.null(value)) next
        if (!is.numeric(value) || length(value) != 2L ||
            any(!is.finite(value))) {
            bb_abort("`limits$", a, "` must be two finite numbers.")
        }
        limits[[a]] <- sort(value)
    }
    limits
}

ly_custom <- function(plot, mapping = NULL, data = NULL, inherit = TRUE,
                      name = "custom layer", draw = NULL, ...) {
    if (!is.function(draw)) {
        bb_abort("`bb_layer()` needs a drawing function in `draw`.")
    }
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params,
                                   extra = "label")

    xy <- bb_eval_xy(mapping, data)
    x <- xy$x
    y <- xy$y
    n <- max(length(x) %||% 0L, length(y) %||% 0L, nrow(data) %||% 0L)
    if (n == 0L) {
        bb_abort("`bb_layer()` has no data to draw.")
    }
    if (!is.null(x) && !is.null(y) && length(x) != length(y) &&
        length(x) != 1L && length(y) != 1L) {
        bb_abort("x and y mapped in `bb_layer()` have different lengths (",
                 length(x), " and ", length(y), ").")
    }
    for (a in c("col", "fill", "pch", "lty", "cex", "label")) {
        mapped <- bb_mapped_length(mapping, data, a)
        if (a == "col" && !is.null(mapped)) mapped <- eval_mapping(mapping, a, data)
        bb_check_length(mapped, n, a, "bb_layer")
        bb_check_length(params[[a]], n, a, "bb_layer")
    }
    keep <- rep(TRUE, n)
    for (v in list(x, y)) {
        if (is.null(v)) next
        v <- rep(v, length.out = n)
        if (inherits(v, "Date") || inherits(v, "POSIXct")) {
            keep <- keep & is.finite(as.numeric(v))
        } else if (is.numeric(v)) {
            keep <- keep & is.finite(v)
        } else {
            keep <- keep & !is.na(v)
        }
    }
    if (!any(keep)) {
        bb_abort("`bb_layer()` has no finite coordinates to draw.")
    }
    params <- bb_layer_constants(params, keep, n)
    x <- if (is.null(x)) NULL else rep(x, length.out = n)[keep]
    y <- if (is.null(y)) NULL else rep(y, length.out = n)[keep]

    ## Mapped aesthetics are read when the plot is drawn, not when the layer is
    ## added: a scale added later in the `+` chain must still apply, and a
    ## constant aesthetic is never sent through a scale.
    mapped_names <- c("col", "fill", "pch", "lty", "cex", "label")
    mapped_names <- mapped_names[
        vapply(mapped_names, function(a) {
            !is.null(mapping[[a]]) && is.null(params[[a]])
        }, logical(1))
    ]

    ly <- function() {
        args <- params
        for (a in mapped_names) {
            value <- bb_probe_aesthetic(plot, mapping, data, a, keep, n)
            if (!is.null(value)) args[[a]] <- value
        }
        args$x <- x
        args$y <- y
        rlang::exec(draw, !!!args)
        invisible(NULL)
    }
    add_layer(plot, ly, name)
}
