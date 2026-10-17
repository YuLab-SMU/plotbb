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
##' them explicitly in `limits` when they should. The scales and the legend are
##' not extended automatically: if the custom drawing needs more room or a
##' different scale, draw a wider mapped data range, declare `limits`, or start
##' from a canvas created by \code{as.bbplot()}.
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
                     ..., name = "custom layer", limits = NULL) {
    if (!is.function(draw)) {
        bb_abort("`draw` must be a function, not <", class(draw)[[1]], ">.")
    }
    limits <- bb_check_layer_limits(limits)
    params <- list(...)
    params$name <- name
    params$draw <- draw
    do.call(build_layer, c(list(mapping = mapping, data = data,
                                inherit = inherit, limits = limits,
                                layer = ly_custom),
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

    prepared <- bb_prepare_aes(xy$x, xy$y, mapping, data, "bb_layer", params,
                               extra = "label")
    if (is.null(prepared)) {
        return(add_layer(plot, function() invisible(NULL), name))
    }
    keep <- prepared$keep
    n <- prepared$n
    params <- bb_layer_constants(params, keep, n)
    x <- prepared$x
    y <- prepared$y

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
