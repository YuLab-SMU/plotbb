##' @rdname layer
##' @export
bb_tile <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ...,  layer = ly_tile)
}


ly_tile <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_layer_data(plot, data, mapping)
    mapping <- bb_mapping(plot, mapping)

    xy <- if (is.null(data)) list(x = NULL, y = NULL) else bb_eval_xy(mapping, data)
    if (is.null(xy$x) && !is.null(data) && !is.null(xvar(mapping))) xy$x <- data[[xvar(mapping)]]
    if (is.null(xy$y) && !is.null(data) && !is.null(yvar(mapping))) xy$y <- data[[yvar(mapping)]]
    x <- xy$x
    y <- xy$y
    params <- list(...)
    prepared <- bb_prepare_aes(x, y, mapping, data, "bb_tile", params)
    x <- prepared$x
    y <- prepared$y
    ok <- prepared$keep
    n <- prepared$n

    ly <- function() {
        col <- params$fill %||% params$col
        if (is.null(col)) col <- bb_col(mapping, data, plot = plot)
        if (is.null(col)) col <- rep(NA, length(x))
        if (length(col) == n) col <- col[ok]
        else if (length(col) == 1L) col <- rep(col, length(x))
        for (i in seq_along(x)) {
            rectangle(x[i], y[i], col = col[i])
        }
        invisible(NULL)
    }
    layer_name <- "heatmap layer"
    plot <- add_layer(plot, ly, layer_name)
    return(plot)
}

##' @importFrom graphics polygon
rectangle <- function(x, y, col, ...) {
    r0 = 0.5
    xx <- c(x - r0, x-r0, x + r0, x+ r0)
    yy <- c(y - r0, y + r0, y+r0, y - r0)
    polygon(xx, yy, col = col, ...)
}
