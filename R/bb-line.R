##' @rdname layer
##' @export
bb_line <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ..., layer = ly_line)
}

ly_line <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)

    params <- list(...)

    xy <- bb_eval_xy(mapping, data)
    x <- xy$x
    y <- xy$y

    group <- eval_mapping(mapping, "group", data)

    ly <- function() {
        col_vec <- NULL
        if (is.null(params$col) && !is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
        }

        if (!is.null(group)) {
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
