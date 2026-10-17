##' Automatic legends
##'
##' A plot with mapped aesthetics gets outside legends by default. Explicit
##' `bb_legend()` components take over the aesthetics they name; `auto_legend =
##' FALSE` on `bbplot()` disables the automatic ones.
##'
##' The automatic legend for an aesthetic is built from the plot-level mapping.
##' When only a layer maps the aesthetic, the legend is tied to that layer so its
##' title comes from the layer mapping. Attributes owned by a layer-local
##' mapping are not duplicated: the first layer that maps the aesthetic is used.
##'
##' @noRd
bb_auto_legends <- function(plot) {
    if (!isTRUE(plot$auto_legend)) return(list())

    aesthetics <- c("col", "fill", "pch", "lty", "cex")
    ## A free-colour facet cannot use an outside legend at all; leave legends to
    ## explicit inside ones.
    if (!is.null(plot$facet) && isTRUE(plot$facet$free_colour)) {
        return(list())
    }
    user <- Filter(function(obj) inherits(obj, "bb_legend"), plot$adds)
    used <- vapply(user, function(obj) obj$aesthetic %||% "col", character(1))

    layers <- Filter(function(obj) inherits(obj, "bb_layer"), plot$adds)
    layer_maps <- lapply(layers, function(obj) {
        bb_layer_aesthetics(plot, obj$mapping, obj$inherit %||% TRUE,
                            obj$params %||% list())
    })

    out <- list()
    for (aesthetic in setdiff(aesthetics, used)) {
        if (!is.null(plot$mapping[[aesthetic]])) {
            out[[length(out) + 1L]] <- bb_legend(
                position = "right", aesthetic = aesthetic, outside = TRUE
            )
            next
        }
        idx <- Position(function(mapping) !is.null(mapping[[aesthetic]]),
                        layer_maps)
        if (!is.na(idx)) {
            out[[length(out) + 1L]] <- bb_legend(
                position = "right", aesthetic = aesthetic, outside = TRUE,
                layer = idx
            )
        }
    }
    out
}
