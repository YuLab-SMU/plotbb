##' histogram layer
##'
##' @title bb_hist
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param ... addition parameter for the layer
##' @return A modified bbplot object
##' @importFrom graphics hist
##' @export
##' @author Guangchuang Yu
bb_hist <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ..., layer = ly_hist)
}

ly_hist <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_layer_data(plot, data, mapping)
    mapping <- bb_mapping(plot, mapping)

    params <- list(...)

    ly <- function() {
        if (is.null(mapping$x)) return(invisible(NULL))

        x <- bb_eval_one(mapping$x, data)
        if (is.null(x)) return(invisible(NULL))
        if (!is.numeric(x)) {
            bb_abort("`bb_hist()` requires a numeric `x` mapping.")
        }
        x <- x[is.finite(x)]
        if (!length(x)) return(invisible(NULL))
        if (length(x) < 2) {
            bb_warn("`bb_hist()` needs at least two finite values to draw a histogram; skipped.")
            return(invisible(NULL))
        }

        args <- params

        # Handle mapped color (fill) and border:
        # in base hist `col` is the fill colour.
        if (!is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
            args$col <- col_vec
        }

        # `add = TRUE` draws on the canvas created by bbplot()
        args$add <- TRUE

        do.call(graphics::hist, c(list(x = x), args))
    }

    add_layer(plot, ly, "histogram layer")
}
