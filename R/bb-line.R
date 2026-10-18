##' @rdname layer
##' @export
bb_line <- function(mapping = NULL, data = NULL, inherit = TRUE, ...) {
    build_layer(mapping, data, ..., layer = ly_line)
}

ly_line <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)


    xy <- bb_eval_xy(mapping, data)
    x <- xy$x
    y <- xy$y

    prepared <- bb_prepare_aes(x, y, mapping, data, "bb_line", params)
    if (is.null(prepared)) {
        return(add_layer(plot, function() invisible(NULL), "line layer"))
    }
    x <- prepared$x
    y <- prepared$y
    keep <- prepared$keep
    params <- bb_layer_constants(params, keep, prepared$n)

    group <- eval_mapping(mapping, "group", data)

    ly <- function() {
        col_vec <- NULL
        if (is.null(params$col) && !is.null(mapping$col)) {
            col_vec <- bb_aes_subset(bb_col(mapping, data, plot = plot),
                                     keep, prepared$n)
        }
        lty_vec <- NULL
        if (is.null(params$lty) && !is.null(mapping$lty)) {
            lty_vec <- bb_aes_subset(bb_lty(mapping, data, plot = plot),
                                     keep, prepared$n)
        }

        if (!is.null(group)) {
            group <- rep(group, length.out = prepared$n)[keep]
            valid_group <- !is.na(group)
            if (!any(valid_group)) return(invisible(NULL))
            group <- group[valid_group]
            x <- x[valid_group]
            y <- y[valid_group]
            if (!is.null(col_vec)) col_vec <- col_vec[valid_group]
            if (!is.null(lty_vec)) lty_vec <- lty_vec[valid_group]
            ug <- unique(group)
            cols <- NULL
            ltypes <- NULL
            if (!is.null(col_vec)) {
                cols <- tapply(col_vec, group, function(z) z[[1]])
            }
            if (!is.null(lty_vec)) {
                ltypes <- tapply(lty_vec, group, function(z) z[[1]])
            }

            for (g in ug) {
                idx <- group == g
                args <- params
                args$x <- x[idx]
                args$y <- y[idx]
                if (!is.null(cols)) args$col <- unname(cols[[as.character(g)]])
                if (!is.null(ltypes)) {
                    args$lty <- unname(ltypes[[as.character(g)]])
                }
                do.call(graphics::lines, args)
            }
            return(invisible(NULL))
        }

        args <- params
        if (!is.null(col_vec) && length(unique(col_vec)) == 1) {
            args$col <- col_vec[[1]]
        }
        if (!is.null(lty_vec)) {
            ## One polyline is one graphical element: a mapped line type uses
            ## the first value, like `col` does. Row-level line types belong to
            ## `bb_segment()`.
            values <- unique(lty_vec[!is.na(lty_vec)])
            if (length(values)) args$lty <- values[[1]]
        }
        args$x <- x
        args$y <- y
        do.call(graphics::lines, args)
        invisible(NULL)
    }
    add_layer(plot, ly, "line layer")
}
