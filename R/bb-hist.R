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
bb_hist <- function(mapping = NULL, data = NULL, inherit = TRUE, ...) {
    build_layer(mapping, data, inherit = inherit, ..., layer = ly_hist)
}

ly_hist <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
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


    ly <- function() {
        if (is.null(mapping$x)) return(invisible(NULL))

        x <- bb_eval_one(mapping$x, data)
        if (is.null(x)) return(invisible(NULL))
        values <- bb_stat_values(x, eval_mapping(mapping, "y", data))
        x <- values$x
        keep_finite <- is.finite(x)
        x <- x[keep_finite]
        if (length(x) < 2) {
            bb_warn("`bb_hist()` needs at least two finite values to draw a histogram; skipped.")
            return(invisible(NULL))
        }

        args <- params
        group <- bb_stat_group(values, mapping, data)
        if (is.null(group)) {
            # In base hist() the col argument is a vector of bar colours, not
            # an observation-level aesthetic. Use a mapped colour only when
            # all observations resolve to one colour; grouped data is handled
            # explicitly below.
            if (is.null(args$col)) {
                colour_values <- if (!is.null(mapping$fill)) {
                    eval_mapping(mapping, "fill", data)
                } else {
                    eval_mapping(mapping, "col", data)
                }
                if (!is.null(colour_values)) {
                    cols <- if (!is.null(mapping$fill)) {
                        bb_fill(mapping, data, plot = plot)
                    } else {
                        bb_col(mapping, data, plot = plot)
                    }
                    cols <- cols[keep_finite]
                    cols <- unique(cols[!is.na(cols)])
                    if (length(cols) == 1L) args$col <- cols[[1]]
                }
            }
            args$add <- TRUE
            do.call(graphics::hist, c(list(x = x), args))
            return(invisible(NULL))
        }

        ## Discrete x values are group labels; draw comparable histograms
        ## within each group. Align group and colour values using the same
        ## finite rows as the numeric observations.
        group <- group[keep_finite]
        levels <- bb_discrete_levels(group)
        if (!length(levels)) return(invisible(NULL))
        breaks <- args$breaks %||% "Sturges"
        all_breaks <- graphics::hist(x, breaks = breaks, plot = FALSE)$breaks

        group_cols <- args$col
        if (is.null(group_cols)) {
            colour_mapping <- mapping$col %||% mapping$fill
            if (!is.null(colour_mapping)) {
                colour_values <- if (!is.null(mapping$fill)) {
                    eval_mapping(mapping, "fill", data)
                } else {
                    eval_mapping(mapping, "col", data)
                }
                colour_values <- colour_values[keep_finite]
                raw_cols <- if (!is.null(mapping$fill)) {
                    bb_fill(mapping, data, plot = plot)
                } else {
                    bb_col(mapping, data, plot = plot)
                }
                raw_cols <- raw_cols[keep_finite]
                colour_levels <- bb_discrete_levels(colour_values)
                colour_cols <- unique(raw_cols[!is.na(raw_cols)])
                if (length(colour_cols) == length(colour_levels)) {
                    names(colour_cols) <- colour_levels
                    group_cols <- unname(colour_cols[levels])
                }
            }
        } else if (length(group_cols) == length(levels)) {
            # A vector supplied as a constant is interpreted per group.
            group_cols <- rep(group_cols, length.out = length(levels))
        }
        if (is.null(group_cols)) {
            group_cols <- grDevices::hcl.colors(length(levels), "Dark 3")
        }
        if (length(group_cols) == 1L) {
            group_cols <- rep(group_cols, length(levels))
        }
        if (length(group_cols) != length(levels)) {
            bb_abort("`col` in `bb_hist()` must have length 1 or the number of groups (",
                     length(levels), ").")
        }

        for (i in seq_along(levels)) {
            g <- levels[i]
            sub_x <- x[!is.na(group) & as.character(group) == g]
            if (length(sub_x) < 2) next
            a <- params
            a$breaks <- all_breaks
            a$col <- grDevices::adjustcolor(group_cols[i], alpha.f = 0.5)
            a$add <- TRUE
            a$plot <- TRUE
            ## `hist()` takes `freq` or `probability`, not both: honour an
            ## explicit `freq` and let `probability` win when it is given.
            if (is.null(a$freq)) a$freq <- !isTRUE(a$probability)
            a$probability <- NULL
            do.call(graphics::hist, c(list(x = sub_x), a))
        }
        invisible(NULL)
    }

    add_layer(plot, ly, "histogram layer")
}
