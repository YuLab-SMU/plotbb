##' boxplot layer
##'
##' @title bb_boxplot
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param orientation `"vertical"` (default; `x` groups, `y` values) or
##'   `"horizontal"` (`x` values, `y` groups).
##' @param ... addition parameter for the layer
##' @return A modified bbplot object
##' @importFrom graphics boxplot
##' @export
##' @author Guangchuang Yu
bb_boxplot <- function(mapping = NULL, data = NULL, inherit = TRUE,
                        orientation = c("vertical", "horizontal"), ...) {
    orientation <- match.arg(orientation)
    build_layer(mapping, data, inherit = inherit, orientation = orientation,
                ..., layer = ly_boxplot)
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
    orientation <- params$orientation %||% "vertical"
    params$orientation <- NULL
    if (!orientation %in% c("vertical", "horizontal")) {
        bb_abort("`orientation` in `bb_boxplot()` must be 'vertical' or ",
                 "'horizontal'.")
    }
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
        if (identical(orientation, "horizontal")) {
            value <- if (is.null(mapping$x)) NULL else
                bb_eval_one(mapping$x, data)
            group <- if (is.null(mapping$y)) NULL else
                bb_eval_one(mapping$y, data)
            value_name <- "x"
        } else {
            value <- if (is.null(mapping$y)) NULL else
                bb_eval_one(mapping$y, data)
            group <- if (is.null(mapping$x)) NULL else
                bb_eval_one(mapping$x, data)
            value_name <- "y"
        }
        if (is.null(value)) {
            return(invisible(NULL))
        }
        if (!is.numeric(value)) {
            bb_abort("`bb_boxplot()` requires a numeric `", value_name,
                     "` mapping.")
        }
        n <- max(length(value), length(group) %||% 0L)
        ok <- is.finite(rep(value, length.out = n))
        if (!any(ok)) {
            bb_warn("`bb_boxplot()` has no finite values to draw; skipped.")
            return(invisible(NULL))
        }

        args <- params
        value <- rep(value, length.out = n)
        group <- if (is.null(group)) NULL else rep(group, length.out = n)

        if (!is.null(mapping$col) || !is.null(mapping$fill)) {
            col_vec <- if (!is.null(mapping$fill)) {
                bb_fill(mapping, data, plot = plot)
            } else {
                bb_col(mapping, data, plot = plot)
            }
            col_vec <- bb_aes_subset(col_vec, ok, n)
            if (!is.null(group)) {
                args$col <- bb_box_values(group[ok], col_vec, length(col_vec))
            } else {
                args$col <- unique(col_vec)[1]
            }
        }

        if (is.null(params$lty) && !is.null(mapping$lty)) {
            lty_vec <- bb_aes_subset(bb_lty(mapping, data, plot = plot), ok, n)
            if (!is.null(group)) {
                args$lty <- bb_box_values(group[ok], lty_vec, length(lty_vec))
            } else {
                args$lty <- unique(lty_vec)[1]
            }
        }

        horizontal <- identical(orientation, "horizontal")
        if (!is.null(group)) {
            fit_data <- data.frame(.value = value[ok], .group = group[ok])
            do.call(graphics::boxplot,
                    c(list(formula = .value ~ .group, data = fit_data,
                           horizontal = horizontal), args))
            return(invisible(NULL))
        }
        do.call(graphics::boxplot,
                c(list(x = value[ok], horizontal = horizontal), args))
    }

    add_layer(plot, ly, "boxplot layer")
}
