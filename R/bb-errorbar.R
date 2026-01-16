##' @rdname layer
##' @param orientation one of 'y' or 'x'
##' @param width bar width or errorbar cap width
##' @export
bb_errorbar <- function(mapping = NULL, data = NULL, orientation = "y", width = 0.5, ...) {
    build_layer(mapping, data, ..., layer = ly_errorbar, orientation = orientation, width = width)
}

ly_errorbar <- function(plot, mapping = NULL, data = NULL, orientation = "y", width = 0.5, ...) {
    orientation <- match.arg(orientation, c("y", "x"))

    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)

    params <- list(...)

    if (orientation == "y") {
        x <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))

        ymin <- eval_mapping(mapping, "ymin", data)
        ymax <- eval_mapping(mapping, "ymax", data)
        if (is.null(ymin) || is.null(ymax)) stop("bb_errorbar requires ymin and ymax mapping")

        ly <- function() {
            col_vec <- NULL
            if (is.null(params$col) && !is.null(mapping$col)) {
                col_vec <- bb_col(mapping, data, plot = plot)
            }
            if (!is.null(col_vec)) {
                graphics::segments(x0 = x, y0 = ymin, x1 = x, y1 = ymax, col = col_vec, ...)
                graphics::segments(x0 = x - width/2, y0 = ymin, x1 = x + width/2, y1 = ymin, col = col_vec, ...)
                graphics::segments(x0 = x - width/2, y0 = ymax, x1 = x + width/2, y1 = ymax, col = col_vec, ...)
            } else {
                graphics::segments(x0 = x, y0 = ymin, x1 = x, y1 = ymax, ...)
                graphics::segments(x0 = x - width/2, y0 = ymin, x1 = x + width/2, y1 = ymin, ...)
                graphics::segments(x0 = x - width/2, y0 = ymax, x1 = x + width/2, y1 = ymax, ...)
            }
            invisible(NULL)
        }

        return(add_layer(plot, ly, "errorbar layer"))
    }

    y <- bb_eval_or_fallback(mapping, data, "y", yvar(mapping))

    xmin <- eval_mapping(mapping, "xmin", data)
    xmax <- eval_mapping(mapping, "xmax", data)
    if (is.null(xmin) || is.null(xmax)) stop("bb_errorbar requires xmin and xmax mapping")

    ly <- function() {
        col_vec <- NULL
        if (is.null(params$col) && !is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
        }
        if (!is.null(col_vec)) {
            graphics::segments(x0 = xmin, y0 = y, x1 = xmax, y1 = y, col = col_vec, ...)
            graphics::segments(x0 = xmin, y0 = y - width/2, x1 = xmin, y1 = y + width/2, col = col_vec, ...)
            graphics::segments(x0 = xmax, y0 = y - width/2, x1 = xmax, y1 = y + width/2, col = col_vec, ...)
        } else {
            graphics::segments(x0 = xmin, y0 = y, x1 = xmax, y1 = y, ...)
            graphics::segments(x0 = xmin, y0 = y - width/2, x1 = xmin, y1 = y + width/2, ...)
            graphics::segments(x0 = xmax, y0 = y - width/2, x1 = xmax, y1 = y + width/2, ...)
        }
        invisible(NULL)
    }

    add_layer(plot, ly, "errorbar layer")
}
