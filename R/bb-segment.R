##' @rdname layer
##' @export
bb_segment <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ..., layer = ly_segment)
}

ly_segment <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)

    xy <- bb_eval_xy(mapping, data)
    x <- xy$x
    y <- xy$y

    xend <- eval_mapping(mapping, "xend", data) %||% eval_mapping(mapping, "x1", data)
    yend <- eval_mapping(mapping, "yend", data) %||% eval_mapping(mapping, "y1", data)
    if (is.null(xend) || is.null(yend)) stop("bb_segment requires xend and yend mapping")

    params <- list(...)
    params <- modifyList(params, list(x0 = x, y0 = y, x1 = xend, y1 = yend))

    ly <- function() {
        args <- params
        if (is.null(args$col) && !is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
            args$col <- col_vec
        }
        do.call(graphics::segments, args)
        invisible(NULL)
    }

    add_layer(plot, ly, "segment layer")
}
