##' ribbon layer
##'
##' `bb_ribbon()` draws a band between `ymin` and `ymax` along `x`, one polygon
##' per group. It follows the grammar contract: it inherits the plot data and
##' mapping, accepts its own, draws in `+` order, honours constants and
##' contributes `x`, `ymin` and `ymax` to the ranges.
##'
##' Rows with a missing or non-finite `x`, `ymin` or `ymax` are dropped, and so
##' are rows with a missing group (with a warning). Within a group the points are
##' sorted by `x`, duplicates are kept, and a group needs at least two points to
##' form a polygon (smaller groups are skipped with a warning). `ymin` and `ymax`
##' may be given in either order: the band is drawn between them. `alpha`
##' (between 0 and 1), `border`, `lty` and `lwd` are layer-wide single values;
##' `fill` (or `col`) sets the interior and can be a constant or a mapping.
##'
##' When the layer brings its own data, inherited mappings are still evaluated
##' on that data: give the layer the variables it inherits, or set
##' `inherit = FALSE` when the plot has aesthetics the band does not carry.
##'
##' @title bb_ribbon
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param ... additional parameters passed to \code{graphics::polygon}, such
##'   as \code{alpha} (default 0.3), \code{border}, \code{lwd} or \code{lty}.
##' @return A modified bbplot object
##' @importFrom graphics polygon
##' @export
##' @author Guangchuang Yu
bb_ribbon <- function(mapping = NULL, data = NULL, inherit = TRUE, ...) {
    build_layer(mapping, data, inherit = inherit, ..., layer = ly_ribbon)
}

ly_ribbon <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)

    ly <- function() {
        x <- eval_mapping(mapping, "x", data)
        ymin <- eval_mapping(mapping, "ymin", data)
        ymax <- eval_mapping(mapping, "ymax", data)
        if (is.null(x) || is.null(ymin) || is.null(ymax)) {
            bb_abort("`bb_ribbon()` requires `x`, `ymin` and `ymax` mappings.")
        }

        if (!is.numeric(x) || !is.numeric(ymin) || !is.numeric(ymax)) {
            bb_abort("`bb_ribbon()` requires numeric `x`, `ymin` and `ymax`.",
                     " Use a numeric axis; a discrete band is a box plot or ",
                     "an error bar.")
        }

        n <- max(length(x), length(ymin), length(ymax))
        for (nm in c("x", "ymin", "ymax")) {
            bb_check_length(get(nm), n, nm, "bb_ribbon")
        }
        x <- rep(x, length.out = n)
        ymin <- rep(ymin, length.out = n)
        ymax <- rep(ymax, length.out = n)

        prepared <- bb_prepare_aes(x, ymin, mapping, data, "bb_ribbon",
                                   params, extra = "ymax", n = n)
        ok <- prepared$keep & is.finite(ymax)
        if (!any(ok)) {
            bb_abort("`bb_ribbon()` has no finite coordinates to draw.")
        }
        n_ok <- sum(ok)
        params <- bb_layer_constants(params, ok, n)

        x <- x[ok]
        ymin <- ymin[ok]
        ymax <- ymax[ok]

        alpha <- params$alpha %||% 0.3
        if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
            alpha < 0 || alpha > 1) {
            bb_abort("`alpha` in `bb_ribbon()` must be a single number ",
                     "between 0 and 1.")
        }
        border <- params$border
        if (is.null(border)) border <- NA
        if (length(border) != 1L) {
            bb_abort("`border` in `bb_ribbon()` must be a single colour.")
        }
        for (nm in c("lty", "lwd")) {
            value <- params[[nm]]
            if (!is.null(value) && length(value) != 1L) {
                bb_abort("`", nm, "` in `bb_ribbon()` must be a single value.")
            }
        }
        border <- rep(border, length.out = n_ok)

        fill <- params$fill %||% params$col
        if (is.null(fill)) {
            mapped_fill <- if (!is.null(mapping$fill)) {
                bb_fill(mapping, data, plot = plot)
            } else if (!is.null(mapping$col)) {
                bb_col(mapping, data, plot = plot)
            }
            fill <- if (is.null(mapped_fill)) {
                rep("grey60", n_ok)
            } else {
                mapped_fill[ok]
            }
        } else {
            ## A constant was already subset to the surviving rows by
            ## `bb_layer_constants()`; recycle over those rows, not over `n`.
            fill <- rep(fill, length.out = n_ok)
        }

        grp <- eval_mapping(mapping, "group", data)
        if (is.null(grp) && !is.null(mapping$fill)) {
            grp <- eval_mapping(mapping, "fill", data)
        }
        if (!is.null(grp)) {
            grp <- as.character(rep(grp, length.out = n)[ok])
            if (any(is.na(grp))) {
                bb_warn("row(s) with a missing group were dropped from ",
                        "`bb_ribbon()`.")
            }
        }

        extra <- params
        extra$fill <- NULL
        extra$col <- NULL
        extra$border <- NULL
        extra$alpha <- NULL

        draws <- if (is.null(grp)) {
            stats::setNames(list(seq_along(x)), "all")
        } else {
            split(seq_along(x), grp)
        }

        skipped <- character()
        for (name in names(draws)) {
            idx <- draws[[name]]
            if (length(idx) < 2L) {
                skipped <- c(skipped, name)
                next
            }
            idx <- idx[order(x[idx])]
            args <- c(
                list(
                    x = c(x[idx], rev(x[idx])),
                    y = c(ymax[idx], rev(ymin[idx])),
                    col = scales::alpha(fill[idx[[1]]], alpha),
                    border = border[idx[[1]]]
                ),
                extra
            )
            do.call(graphics::polygon, args)
        }
        if (length(skipped)) {
            bb_warn("group(s) with fewer than two finite points were skipped ",
                    "in `bb_ribbon()`: ",
                    paste(utils::head(skipped, 3), collapse = ", "))
        }
        invisible(NULL)
    }

    add_layer(plot, ly, "ribbon layer")
}
