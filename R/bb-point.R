##' layer
##'
##' bbplot layers
##' @title layer
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param position one of 'identity' or 'jitter'
##' @param ... addition parameter for the layer
##' @return A modified bbplot object
##' @importFrom graphics points
##' @export
##' @examples
##' library(plotbb)
##' p <- bbplot(mtcars, bb_aes(mpg, disp, col=factor(cyl))) +
##'   bb_point() + bb_lm(bb_aes(group=cyl), lwd=2)
##' @author Guangchuang Yu
bb_point <- function(mapping = NULL, data = NULL, position = "identity", ...) {
    build_layer(mapping, data, ...,  layer = ly_point)
}

ly_point <- function(plot, mapping = NULL, data = NULL, position = "identity", ...) {
    if (inherits(position, "bb_position")) {
        pos <- position$type %||% "identity"
    } else {
        pos <- match.arg(position, c("identity", "jitter"))
        position <- structure(list(type = pos), class = "bb_position")
    }

    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)

    xy <- bb_eval_xy(mapping, data)
    x <- xy$x
    y <- xy$y

    if (identical(position$type, "jitter")) {
        w <- position$width %||% NULL
        h <- position$height %||% NULL
        x <- jitter(x, amount = w)
        y <- jitter(y, amount = h)
    }

    params <- list(...)
    params <- modifyList(params, list(x = x, y = y))

    ly <- function() {
        if (!is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
            params <- modifyList(params, list(col = col_vec))
        }

        if (!is.null(mapping$pch) && is.null(params$pch)) {
            pch_vec <- bb_pch(mapping, data, plot = plot)
            params <- modifyList(params, list(pch = pch_vec))
        }

        if (!is.null(mapping$cex) && is.null(params$cex)) {
            cex_vec <- bb_cex(mapping, data, plot = plot)
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

    do.call(ly, c(list(plot = plot), object, params_list))
}
