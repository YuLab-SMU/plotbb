##' @rdname layer
##' @export
bb_line <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ..., layer = ly_line)
}

ly_line <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_layer_data(plot, data, mapping)
    mapping <- bb_mapping(plot, mapping)

    params <- list(...)

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

    group <- eval_mapping(mapping, "group", data)

    ly <- function() {
        col_vec <- NULL
        if (is.null(params$col) && !is.null(mapping$col)) {
            col_vec <- bb_aes_subset(bb_col(mapping, data, plot = plot),
                                     keep, prepared$n)
        }

        if (!is.null(group)) {
            group <- rep(group, length.out = prepared$n)[keep]
            ug <- unique(group)
            cols <- NULL
            if (!is.null(col_vec)) {
                cols <- tapply(col_vec, group, function(z) z[[1]])
            }

            for (g in ug) {
                idx <- group == g
                args <- params
                args$x <- x[idx]
                args$y <- y[idx]
                if (!is.null(cols)) args$col <- unname(cols[[as.character(g)]])
                do.call(graphics::lines, args)
            }
            return(invisible(NULL))
        }

        args <- params
        if (!is.null(col_vec) && length(unique(col_vec)) == 1) {
            args$col <- col_vec[[1]]
        }
        args$x <- x
        args$y <- y
        do.call(graphics::lines, args)
        invisible(NULL)
    }
    add_layer(plot, ly, "line layer")
}
