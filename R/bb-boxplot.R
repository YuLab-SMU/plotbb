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
bb_boxplot <- function(mapping = NULL, data = NULL, inherit = TRUE, ...) {
    build_layer(mapping, data, inherit = inherit, ..., layer = ly_boxplot)
}

## One value per box, in the order the boxes are drawn.
##
## `boxplot()` orders boxes by factor levels (or by sorted values), not by the
## first appearance of a group in the data, so a per-box aesthetic must be
## reordered to match. Without this, a mapped line type or colour can land on
## the wrong box.
bb_box_values <- function(x, values, n) {
    x <- rep(x, length.out = n)
    values <- rep(values, length.out = n)
    udata <- unique(data.frame(x = x, value = values,
                               stringsAsFactors = FALSE))
    if (is.factor(x)) {
        ord <- order(match(as.character(udata$x), levels(x)))
    } else if (is.numeric(x)) {
        ord <- order(udata$x)
    } else {
        ord <- order(as.character(udata$x))
    }
    udata$value[ord]
}

ly_boxplot <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)
    ## A constant `fill` names the interior of the bars/boxes; after the
    ## mapping has been resolved (so the constant also removes a mapped fill),
    ## it becomes the `col` the base function understands.
    if (is.null(params$col) && !is.null(params$fill)) {
        params$col <- params$fill
        params$fill <- NULL
    }

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

        if (!is.null(mapping$col) || !is.null(mapping$fill)) {
            col_vec <- if (!is.null(mapping$fill)) {
                bb_fill(mapping, data, plot = plot)
            } else {
                bb_col(mapping, data, plot = plot)
            }
            col_vec <- rep(col_vec, length.out = n)[ok]
            if (!is.null(x)) {
                args$col <- bb_box_values(x[ok], col_vec, length(col_vec))
            } else {
                args$col <- unique(col_vec)[1]
            }
        }

        if (is.null(params$lty) && !is.null(mapping$lty)) {
            lty_vec <- rep(bb_lty(mapping, data, plot = plot), length.out = n)[ok]
            if (!is.null(x)) {
                args$lty <- bb_box_values(x[ok], lty_vec, length(lty_vec))
            } else {
                args$lty <- unique(lty_vec)[1]
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
