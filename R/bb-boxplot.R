##' boxplot layer
##'
##' @title bb_boxplot
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param ... addition parameter for the layer
##' @return A modified bbplot object
##' @importFrom graphics boxplot
##' @export
##' @author Guangchuang Yu
bb_boxplot <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ..., layer = ly_boxplot)
}

ly_boxplot <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_layer_data(plot, data, mapping)
    mapping <- bb_mapping(plot, mapping)

    params <- list(...)
    if (is.null(params$add)) params$add <- TRUE
    if (is.null(params$axes)) params$axes <- FALSE

    ly <- function() {
        if (is.null(mapping$y)) {
            # no y mapping: nothing to draw; use bb_hist()/bb_density() for 1d data
            return(invisible(NULL))
        }

        y <- bb_eval_one(mapping$y, data)
        if (is.null(y)) return(invisible(NULL))
        if (!is.numeric(y)) {
            bb_abort("`bb_boxplot()` requires a numeric `y` mapping.")
        }

        x <- if (is.null(mapping$x)) NULL else bb_eval_one(mapping$x, data)
        n <- max(length(y), length(x) %||% 0L)
        ok <- is.finite(rep(y, length.out = n))
        if (!any(ok)) {
            bb_warn("`bb_boxplot()` has no finite values to draw; skipped.")
            return(invisible(NULL))
        }

        args <- params

        if (!is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
            col_vec <- rep(col_vec, length.out = n)[ok]
            if (!is.null(x)) {
                x <- rep(x, length.out = n)
                ## one colour per box, keyed by the x levels
                udata <- unique(data.frame(x = as.character(x[ok]), col = col_vec))
                args$col <- udata$col
            } else {
                args$col <- unique(col_vec)[1]
            }
        }

        if (!is.null(x)) {
            f <- stats::as.formula(paste(rlang::as_label(mapping$y), "~", rlang::as_label(mapping$x)))
            d <- data[ok, , drop = FALSE]
            do.call(graphics::boxplot, c(list(formula = f, data = d), args))
            return(invisible(NULL))
        }

        do.call(graphics::boxplot, c(list(x = y[ok]), args))
    }

    add_layer(plot, ly, "boxplot layer")
}
