##' @rdname layer
##' @export
bb_segment <- function(mapping = NULL, data = NULL, inherit = TRUE, ...) {
    build_layer(mapping, data, inherit = inherit, ..., layer = ly_segment)
}

ly_segment <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)

    xy <- bb_eval_xy(mapping, data)
    x <- xy$x
    y <- xy$y

    xend <- eval_mapping(mapping, "xend", data)
    yend <- eval_mapping(mapping, "yend", data)
    if (is.null(xend)) xend <- eval_mapping(mapping, "x1", data)
    if (is.null(yend)) yend <- eval_mapping(mapping, "y1", data)
    if (is.null(xend) || is.null(yend)) {
        bb_abort("`bb_segment()` requires `xend` and `yend` mappings.")
    }

    n <- max(length(x) %||% 0L, length(y) %||% 0L)
    if (n == 0L) {
        bb_abort("`bb_segment` has no data to draw.")
    }
    for (nm in c("xend", "yend")) {
        bb_check_length(get(nm), n, nm, "bb_segment")
    }
    if (length(x) != n && length(x) != 1L ||
        length(y) != n && length(y) != 1L) {
        bb_abort("x and y mapped in `bb_segment` have incompatible lengths.")
    }
    x <- rep(x, length.out = n)
    y <- rep(y, length.out = n)
    prepared <- bb_prepare_aes(x, y, mapping, data, "bb_segment", params,
                               n = n)
    ok <- bb_valid_rows(x, y) & is.finite(rep(xend, length.out = n)) &
        is.finite(rep(yend, length.out = n)) & prepared$keep
    if (!any(ok)) {
        bb_abort("`bb_segment` has no finite coordinates to draw.")
    }
    x <- rep(x, length.out = n)
    y <- rep(y, length.out = n)
    params <- bb_layer_constants(params, ok, n)
    params <- modifyList(params, list(x0 = x[ok], y0 = y[ok],
                                      x1 = rep(xend, length.out = n)[ok],
                                      y1 = rep(yend, length.out = n)[ok]))

    ly <- function() {
        args <- params
        if (is.null(args$col) && !is.null(mapping$col)) {
            col_vec <- bb_aes_subset(bb_col(mapping, data, plot = plot), ok, n)
            args$col <- col_vec
        }
        if (is.null(args$lty) && !is.null(mapping$lty)) {
            args$lty <- bb_aes_subset(bb_lty(mapping, data, plot = plot), ok, n)
        }
        do.call(graphics::segments, args)
        invisible(NULL)
    }

    add_layer(plot, ly, "segment layer")
}
