##' layer
##'
##' bbplot layers
##' @title layer
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param position one of 'identity' or 'jitter'
##' @param seed random seed used by `position = "jitter"`. Defaults to 1, so
##'   that jittered positions are reproducible; set \code{seed = NULL} to
##'   jitter from the current random state. The caller's random number stream
##'   is never modified.
##' @param inherit whether the layer inherits the plot mapping. With
##'   \code{inherit = FALSE} the layer uses only its own \code{mapping}, so a
##'   mapping that is not repeated locally is unavailable; the layer's
##'   \code{data} is still inherited from \code{bbplot()} unless given
##'   explicitly.
##' @param ... addition parameter for the layer
##' @return A modified bbplot object
##' @importFrom graphics points
##' @export
##' @examples
##' library(plotbb)
##' p <- bbplot(mtcars, bb_aes(mpg, disp, col=factor(cyl))) +
##'   bb_point() + bb_lm(bb_aes(group=cyl), lwd=2)
##' @author Guangchuang Yu
bb_point <- function(mapping = NULL, data = NULL, position = "identity", inherit = TRUE, ..., seed = 1) {
    build_layer(mapping, data, inherit = inherit, ...,  layer = ly_point,
                position = position, seed = seed)
}

##' jitter position for bb_point
##'
##' @title bb_position
##' @param ... parameters for the position, e.g. \code{width}, \code{height},
##'   \code{seed}.
##' @param type position type, one of 'identity' or 'jitter'.
##' @return a position object that can be passed to \code{bb_point()}.
##' @export
##' @author Guangchuang Yu
bb_position <- function(..., type = c("jitter", "identity")) {
    type <- match.arg(type)
    structure(c(list(type = type), list(...)), class = "bb_position")
}

ly_point <- function(plot, mapping = NULL, data = NULL, position = "identity", inherit = TRUE, seed = 1, ...) {
    if (inherits(position, "bb_position")) {
        pos <- position$type %||% "identity"
    } else {
        pos <- match.arg(position, c("identity", "jitter"))
        position <- structure(list(type = pos), class = "bb_position")
    }

    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)

    xy <- bb_eval_xy(mapping, data)
    prepared <- bb_prepare_aes(xy$x, xy$y, mapping, data, "bb_point", params)
    if (is.null(prepared)) {
        return(add_layer(plot, function() invisible(NULL), "point layer"))
    }
    x <- prepared$x
    y <- prepared$y
    keep <- prepared$keep
    params <- bb_layer_constants(params, keep, prepared$n)

    if (identical(position$type, "jitter")) {
        seed <- position$seed %||% seed
        width <- position$width %||% NULL
        height <- position$height %||% NULL
        jit <- bb_jitter_many(seed, x, y, width = width, height = height)
        x <- jit$x
        y <- jit$y
    }

    params <- modifyList(params, list(x = x, y = y))

    ## A constant such as `bb_point(col = "tomato")` applies to the whole layer
    ## and wins over a mapped aesthetic. Mapped aesthetics are resolved at draw
    ## time, so the scale in effect is the last one in the `+` chain rather than
    ## whichever scale existed when the layer was added (or, worse, being sent
    ## through a scale when it is a constant).
    constant <- vapply(c("col", "pch", "cex"), function(a) !is.null(params[[a]]),
                       logical(1))
    names(constant) <- c("col", "pch", "cex")

    ly <- function() {
        draw <- params
        if (!constant[["col"]] && !is.null(mapping$col)) {
            draw$col <- bb_aes_subset(bb_col(mapping, data, plot = plot),
                                      keep, prepared$n)
        }
        if (!constant[["pch"]] && !is.null(mapping$pch)) {
            draw$pch <- bb_aes_subset(bb_pch(mapping, data, plot = plot),
                                      keep, prepared$n)
        }
        if (!constant[["cex"]] && !is.null(mapping$cex)) {
            draw$cex <- bb_aes_subset(bb_cex(mapping, data, plot = plot),
                                      keep, prepared$n)
        }

        do.call(points, draw)
    }
    add_layer(plot, ly, "point layer")
}

##' @method bbplot_add bb_layer
##' @export
bbplot_add.bb_layer <- function(object, plot) {
    ly <- object$layer

    params_list <- object$params %||% list()
    fill_layers <- list(ly_custom, ly_density, ly_tile, ly_bar, ly_boxplot,
                        ly_hist, ly_lm, ly_ribbon, ly_violin, ly_smooth,
                        ly_bar_horizontal)
    meta <- object$meta %||% list()
    fill_ok <- any(vapply(fill_layers, identical, logical(1), ly)) ||
        "fill" %in% (meta$aesthetics %||% character())
    if (!is.null(params_list$fill) && !fill_ok) {
        bb_abort("`fill` is not an aesthetic of this layer: use `col` for its ",
                 "colour. `fill` is distinguished only where the drawing ",
                 "primitive has an interior separate from its border ",
                 "(`bb_density()`, `bb_tile()`, `bb_bar()`, `bb_boxplot()`, ",
                 "`bb_hist()`, the `bb_lm()` band and a custom `bb_layer()`).")
    }
    object$params <- NULL
    object$layer <- NULL
    ## `limits` is a plot-level declaration (read by bb_layer_limits()), not a
    ## drawing argument; do not leak it into the drawing function.
    object$limits <- NULL
    object$meta <- NULL
    if (is.null(object$inherit)) object$inherit <- TRUE

    ## `rlang::exec()` rather than do.call(): layer parameters must be passed
    ## by name. With do.call(), a list-valued argument such as `col` replaces
    ## the layer function's formal argument, so `bb_point(col = ...)` silently
    ## lost its colour.
    rlang::exec(ly, plot = plot, !!!object, !!!params_list)
}
