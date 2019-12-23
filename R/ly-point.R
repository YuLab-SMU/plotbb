##' layer
##'
##' bbplot layers
##' @title layer
##' @rdname layer
##' @param p bbplot object
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param position one of 'identity' or 'jitter'
##' @param ... addition parameter for the layer
##' @return bbplot object
##' @importFrom graphics points
##' @export
##' @author Guangchuang Yu
ly_point <- function(p, mapping = NULL, data = NULL, position = "identity", ...) {
    position <- match.arg(position, c("identity", "jitter"))

    data <- bb_data(p, data)
    mapping <- bb_mapping(p, mapping)

    x <- data[[xvar(mapping)]]
    y <- data[[yvar(mapping)]]

    if (position == "jitter") {
        x <- jitter(x)
        y <- jitter(y)
    }

    params <- list(...)
    params <- modifyList(params, list(x = x, y = y))

    if (!is.null(mapping$col)) {
        col_vec <- bb_col(mapping, data)
        params <- modifyList(params, list(col = col_vec))
    }

    ly <- function() do.call(points, params)
    add_layer(p, ly)
}



