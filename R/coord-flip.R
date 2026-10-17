##' Flip the positional axes
##'
##' `bb_coord_flip()` swaps the x and y axes. It is a coordinate operation, not a
##' data transformation: axes labels, positional scales and the titles swap, and
##' the supported layers are redrawn with x/y swapped.
##'
##' Supported layers are `bb_point()`, `bb_line()`, `bb_text()`,
##' `bb_segment()`, `bb_tile()`, `bb_layer()`, `bb_violin()`, `bb_boxplot()`,
##' `bb_bar()` and `bb_errorbar()` (the last four switch to the opposite
##' `orientation`), plus constant or mapped `bb_hline()`/`bb_vline()` (which
##' become the other reference line). Layers that have no orientation model
##' (density, histogram, ribbon, smooth, lm, signif) raise an error rather than
##' drawing in the wrong orientation.
##'
##' @title bb_coord_flip
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_coord_flip <- function() {
    structure(list(), class = "bb_coord_flip")
}

##' @method bbplot_add bb_coord_flip
##' @export
bbplot_add.bb_coord_flip <- function(object, plot) {
    plot$coord <- "flip"
    plot
}

bb_swap_mapping <- function(mapping) {
    out <- mapping
    for (pair in list(c("x", "y"), c("xend", "yend"),
                      c("xmin", "ymin"), c("xmax", "ymax"))) {
        a <- mapping[[pair[[1]]]]
        b <- mapping[[pair[[2]]]]
        out[[pair[[1]]]] <- b
        out[[pair[[2]]]] <- a
    }
    out[!vapply(out, is.null, logical(1))]
}

bb_swap_position_scale <- function(scale) {
    if (is.null(scale)) return(NULL)
    scale$aesthetic <- if (identical(scale$aesthetic, "x")) "y" else "x"
    scale
}

bb_swap_labs <- function(labs) {
    xlab <- labs$xlab
    labs$xlab <- labs$ylab
    labs$ylab <- xlab
    labs
}

bb_flipped_layer <- function(obj, plot = NULL) {
    ly <- obj$layer
    swap_limits <- function() {
        if (!is.null(obj$limits)) {
            old <- obj$limits
            obj$limits <- list(x = old$y, y = old$x)
            obj$limits <- obj$limits[!vapply(obj$limits, is.null, logical(1))]
        }
    }

    if (identical(ly, ly_bar) || identical(ly, ly_bar_horizontal)) {
        stat <- obj$params$stat %||% "identity"
        if (identical(stat, "count")) {
            ## A count bar has only a category mapping; it stays on x because
            ## counts are computed, not mapped.
            if (is.null(obj$mapping$x) && !is.null(plot$mapping$x)) {
                obj$mapping$x <- plot$mapping$x
            }
            obj$mapping$y <- NULL
        } else {
            obj$mapping <- bb_swap_mapping(obj$mapping)
        }
        obj$layer <- if (identical(ly, ly_bar_horizontal)) ly_bar else
            ly_bar_horizontal
        swap_limits()
        return(obj)
    }

    if (identical(ly, ly_errorbar)) {
        obj$mapping <- bb_swap_mapping(obj$mapping)
        current <- obj$params$orientation %||% "y"
        obj$params$orientation <- if (identical(current, "x")) "y" else "x"
        swap_limits()
        return(obj)
    }

    if (identical(ly, ly_violin) || identical(ly, ly_boxplot)) {
        obj$mapping <- bb_swap_mapping(obj$mapping)
        current <- obj$params$orientation %||% "vertical"
        obj$params$orientation <- if (identical(current, "horizontal")) {
            "vertical"
        } else {
            "horizontal"
        }
        swap_limits()
        return(obj)
    }

    supported <- list(ly_point, ly_line, ly_text, ly_segment, ly_tile, ly_custom)
    if (any(vapply(supported, identical, logical(1), ly))) {
        obj$mapping <- bb_swap_mapping(obj$mapping)
        swap_limits()
        return(obj)
    }

    bb_abort("`bb_coord_flip()` does not support this layer. Supported layers ",
             "are point, line, text, segment, tile, custom layers, violin, ",
             "boxplot, bar, errorbar and hline/vline.")
}

bb_flip_plot <- function(plot) {
    if (!identical(plot$coord, "flip")) return(plot)
    if (is.null(plot$data)) {
        bb_abort("`bb_coord_flip()` needs plot data.")
    }
    flipped <- bbplot(plot$data, bb_swap_mapping(plot$mapping),
                      auto_legend = FALSE)
    flipped$labs <- bb_swap_labs(plot$labs)
    flipped$theme <- plot$theme
    flipped$auto_legend <- plot$auto_legend
    scales <- plot$position_scales %||% list()
    flipped$position_scales <- list(x = bb_swap_position_scale(scales$y),
                                    y = bb_swap_position_scale(scales$x))
    flipped$position_scales <- flipped$position_scales[
        !vapply(flipped$position_scales, is.null, logical(1))
    ]

    for (obj in plot$adds) {
        if (inherits(obj, "bb_position_scale")) next
        if (inherits(obj, "bb_labs")) next
        if (inherits(obj, "bb_coord_flip")) next
        if (inherits(obj, "bb_layer") && identical(obj$layer, ly_hline)) {
            intercept <- obj$params$yintercept
            mapping <- obj$mapping
            if (!is.null(mapping$yintercept)) {
                mapping$xintercept <- mapping$yintercept
                mapping$yintercept <- NULL
            }
            extra <- obj$params
            extra$yintercept <- NULL
            extra$expand_limits <- NULL
            flipped <- flipped + do.call(bb_vline, c(list(
                xintercept = intercept, mapping = mapping, data = obj$data,
                inherit = obj$inherit %||% TRUE, expand_limits =
                    isTRUE(obj$params$expand_limits)
            ), extra))
            next
        }
        if (inherits(obj, "bb_layer") && identical(obj$layer, ly_vline)) {
            intercept <- obj$params$xintercept
            mapping <- obj$mapping
            if (!is.null(mapping$xintercept)) {
                mapping$yintercept <- mapping$xintercept
                mapping$xintercept <- NULL
            }
            extra <- obj$params
            extra$xintercept <- NULL
            extra$expand_limits <- NULL
            flipped <- flipped + do.call(bb_hline, c(list(
                yintercept = intercept, mapping = mapping, data = obj$data,
                inherit = obj$inherit %||% TRUE, expand_limits =
                    isTRUE(obj$params$expand_limits)
            ), extra))
            next
        }
        if (inherits(obj, "bb_layer")) {
            flipped <- flipped + bb_flipped_layer(obj, plot)
            next
        }
        if (inherits(obj, "bb_facet_wrap") || inherits(obj, "bb_facet_grid") ||
            inherits(obj, "bb_legend") || inherits(obj, "bb_theme") ||
            inherits(obj, "bb_grid") || inherits(obj, "bb_palette") ||
            inherits(obj, "bb_scale")) {
            flipped <- flipped + obj
            next
        }
        bb_abort("`bb_coord_flip()` does not know how to transform component <",
                 class(obj)[[1]], ">.")
    }
    flipped
}
