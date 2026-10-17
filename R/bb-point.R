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
##' @param ... addition parameter for the layer
##' @return A modified bbplot object
##' @importFrom graphics points
##' @export
##' @examples
##' library(plotbb)
##' p <- bbplot(mtcars, bb_aes(mpg, disp, col=factor(cyl))) +
##'   bb_point() + bb_lm(bb_aes(group=cyl), lwd=2)
##' @author Guangchuang Yu
bb_point <- function(mapping = NULL, data = NULL, position = "identity", ..., seed = 1) {
    build_layer(mapping, data, ...,  layer = ly_point, position = position, seed = seed)
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

ly_point <- function(plot, mapping = NULL, data = NULL, position = "identity", seed = 1, ...) {
    if (inherits(position, "bb_position")) {
        pos <- position$type %||% "identity"
    } else {
        pos <- match.arg(position, c("identity", "jitter"))
        position <- structure(list(type = pos), class = "bb_position")
    }

    data <- bb_layer_data(plot, data, mapping)
    mapping <- bb_mapping(plot, mapping)
    params <- list(...)

    xy <- bb_eval_xy(mapping, data)
    prepared <- bb_prepare_aes(xy$x, xy$y, mapping, data, "bb_point", params)
    if (is.null(prepared)) {
        return(add_layer(plot, function() invisible(NULL), "point layer"))
    }
    x <- prepared$x
    y <- prepared$y
    keep <- prepared$keep

    if (identical(position$type, "jitter")) {
        seed <- position$seed %||% seed
        width <- position$width %||% NULL
        height <- position$height %||% NULL
        jit <- bb_jitter_many(seed, x, y, width = width, height = height)
        x <- jit$x
        y <- jit$y
    }

    params <- modifyList(params, list(x = x, y = y))

    ly <- function() {
        if (!is.null(mapping$col) && is.null(params$col)) {
            col_vec <- bb_aes_subset(bb_col(mapping, data, plot = plot),
                                     keep, prepared$n)
            params <- modifyList(params, list(col = col_vec))
        }

        if (!is.null(mapping$pch) && is.null(params$pch)) {
            pch_vec <- bb_aes_subset(bb_pch(mapping, data, plot = plot),
                                     keep, prepared$n)
            params <- modifyList(params, list(pch = pch_vec))
        }

        if (!is.null(mapping$cex) && is.null(params$cex)) {
            cex_vec <- bb_aes_subset(bb_cex(mapping, data, plot = plot),
                                     keep, prepared$n)
            params <- modifyList(params, list(cex = cex_vec))
        }

        do.call(points, params)
    }
    add_layer(plot, ly, "point layer")
}

##' @method bbplot_add bb_layer
##' @export
bbplot_add.bb_layer <- function(object, plot) {
    ly <- object$layer

    params_list <- object$params %||% list()
    object$params <- NULL
    object$layer <- NULL

    ## `rlang::exec()` rather than do.call(): layer parameters must be passed
    ## by name. With do.call(), a list-valued argument such as `col` replaces
    ## the layer function's formal argument, so `bb_point(col = ...)` silently
    ## lost its colour.
    rlang::exec(ly, plot = plot, !!!object, !!!params_list)
}
