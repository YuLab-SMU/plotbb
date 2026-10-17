##' @rdname layer
##' @param orientation one of 'y' or 'x'
##' @param width bar width or errorbar cap width
##' @export
bb_errorbar <- function(mapping = NULL, data = NULL, orientation = "y", width = 0.5,
                        inherit = TRUE, ...) {
    build_layer(mapping, data, inherit = inherit, ..., layer = ly_errorbar,
                orientation = orientation, width = width)
}

ly_errorbar <- function(plot, mapping = NULL, data = NULL, orientation = "y", width = 0.5,
                        inherit = TRUE, ...) {
    orientation <- match.arg(orientation, c("y", "x"))

    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)


    if (orientation == "y") {
        x <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))

        ymin <- eval_mapping(mapping, "ymin", data)
        ymax <- eval_mapping(mapping, "ymax", data)
        if (is.null(ymin) || is.null(ymax)) {
            bb_abort("`bb_errorbar()` requires `ymin` and `ymax` mappings.")
        }

        n <- max(length(x) %||% 0L, length(ymin), length(ymax))
        if (n == 0L) bb_abort("`bb_errorbar` has no data to draw.")
        for (nm in c("ymin", "ymax")) {
            bb_check_length(get(nm), n, nm, "bb_errorbar")
        }
        bb_check_length(x, n, "x", "bb_errorbar")
        prepared <- bb_prepare_aes(x, ymin, mapping, data, "bb_errorbar", params,
                                   n = n, extra = "ymax")
        ymin <- rep(ymin, length.out = n)
        ymax <- rep(ymax, length.out = n)
        x <- rep(x, length.out = n)
        ok <- is.finite(x) & is.finite(ymin) & is.finite(ymax) & prepared$keep
        if (!any(ok)) {
            bb_abort("`bb_errorbar` has no finite coordinates to draw.")
        }
        x <- x[ok]
        ymin <- ymin[ok]
        ymax <- ymax[ok]
        params <- bb_layer_constants(params, ok, n)

        ly <- function() {
            col_vec <- NULL
            if (is.null(params$col) && !is.null(mapping$col)) {
                col_vec <- bb_aes_subset(bb_col(mapping, data, plot = plot), ok, n)
            }
            args <- params
            if (!is.null(col_vec)) args$col <- col_vec
            do.call(graphics::segments, c(list(x0 = x, y0 = ymin, x1 = x, y1 = ymax), args))
            do.call(graphics::segments, c(list(x0 = x - width/2, y0 = ymin,
                                               x1 = x + width/2, y1 = ymin), args))
            do.call(graphics::segments, c(list(x0 = x - width/2, y0 = ymax,
                                               x1 = x + width/2, y1 = ymax), args))
            invisible(NULL)
        }

        return(add_layer(plot, ly, "errorbar layer"))
    }

    y <- bb_eval_or_fallback(mapping, data, "y", yvar(mapping))

    xmin <- eval_mapping(mapping, "xmin", data)
    xmax <- eval_mapping(mapping, "xmax", data)
    if (is.null(xmin) || is.null(xmax)) {
        bb_abort("`bb_errorbar()` requires `xmin` and `xmax` mappings.")
    }

    n <- max(length(y) %||% 0L, length(xmin), length(xmax))
    if (n == 0L) bb_abort("`bb_errorbar` has no data to draw.")
    for (nm in c("xmin", "xmax")) {
        bb_check_length(get(nm), n, nm, "bb_errorbar")
    }
    bb_check_length(y, n, "y", "bb_errorbar")
    prepared <- bb_prepare_aes(xmin, y, mapping, data, "bb_errorbar", params,
                               n = n, extra = "xmax")
    xmin <- rep(xmin, length.out = n)
    xmax <- rep(xmax, length.out = n)
    y <- rep(y, length.out = n)
    ok <- is.finite(y) & is.finite(xmin) & is.finite(xmax) & prepared$keep
    if (!any(ok)) {
        bb_abort("`bb_errorbar` has no finite coordinates to draw.")
    }
    y <- y[ok]
    xmin <- xmin[ok]
    xmax <- xmax[ok]
    params <- bb_layer_constants(params, ok, n)

    ly <- function() {
        col_vec <- NULL
        if (is.null(params$col) && !is.null(mapping$col)) {
            col_vec <- bb_aes_subset(bb_col(mapping, data, plot = plot), ok, n)
        }
        args <- params
        if (!is.null(col_vec)) args$col <- col_vec
        do.call(graphics::segments, c(list(x0 = xmin, y0 = y, x1 = xmax, y1 = y), args))
        do.call(graphics::segments, c(list(x0 = xmin, y0 = y - width/2,
                                           x1 = xmin, y1 = y + width/2), args))
        do.call(graphics::segments, c(list(x0 = xmax, y0 = y - width/2,
                                           x1 = xmax, y1 = y + width/2), args))
        invisible(NULL)
    }

    add_layer(plot, ly, "errorbar layer")
}
