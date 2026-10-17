##' Build a plot without drawing it
##'
##' `bb_build()` performs the computation that `print()` would perform, without
##' opening a device and without drawing: it trains the scales, resolves the
##' mappings, collects each layer's data and computes the limits (and the
##' per-panel limits of a faceted plot). The result is an ordinary list, meant
##' for inspection and for checking a plot before it is rendered.
##'
##' The build is side-effect free with respect to graphics: it does not call
##' `plot()`, does not change `par()` and does not touch `.Random.seed` (jitter
##' is applied when the plot is drawn, not when it is built). Training stores the
##' trained scales in the plot's internal environment; that is plot state, not
##' graphics state. Repeated calls on the same object return the same structure
##' and limits.
##'
##' @title bb_build
##' @param plot a \code{bbplot} object
##' @return A list with \code{data}, \code{mapping} (labels), \code{layers}
##'   (name, kind, data, mapping labels, inherit, parameter names), \code{scales}
##'   (trained), \code{limits}, \code{panels} (\code{NULL} or one entry per
##'   facet with its key, row count and limits), \code{labs}, \code{facet} (a
##'   serializable summary: variables, scales, nrow and ncol) and \code{warnings}
##'   collected while building.
##' @seealso \code{\link{bb_save}}
##' @export
##' @examples
##' p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl))) + bb_point()
##' str(bb_build(p), max.level = 2)
bb_build <- function(plot) {
    if (!inherits(plot, "bbplot")) {
        bb_abort("`bb_build()` requires a `bbplot` object.")
    }

    warnings <- character()
    built <- withCallingHandlers({
        prepared <- bb_prepare_plot(plot)
        p <- prepared$plot
        list(
            data = p$data,
            mapping = bb_mapping_labels(p$mapping),
            layers = bb_build_layers(p),
            scales = bb_get_scales(p),
            limits = prepared$limits,
            panels = bb_build_panels(p),
            labs = p$labs,
            facet = bb_build_facet(p$facet)
        )
    }, warning = function(w) {
        warnings <<- unique(c(warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
    })

    built$warnings <- warnings
    built
}

## A serializable description of the facet: the raw facet object carries the
## formula's quosure/environment, which should not be the only way to inspect it.
bb_build_facet <- function(facet) {
    if (is.null(facet)) return(NULL)
    list(
        vars = all.vars(rlang::f_rhs(facet$facets)),
        scales = facet$scales %||% "fixed",
        nrow = facet$nrow,
        ncol = facet$ncol
    )
}

## The computation shared by `print()` and `bb_build()`: train the scales,
## apply the automatic summary labels and compute the limits. Keeping it in one
## place is what guarantees that the built limits are the limits that drawing
## uses.
bb_prepare_plot <- function(plot) {
    plot <- bb_train_scales(plot)
    plot <- bb_summary_labs(plot)
    list(plot = plot, limits = bb_plot_limits(plot))
}

bb_mapping_labels <- function(mapping) {
    if (is.null(mapping) || !length(mapping)) return(character())
    vapply(mapping, function(q) rlang::as_label(q), character(1))
}

bb_build_layers <- function(plot) {
    is_grammar_layer <- function(obj) inherits(obj, "bb_layer")
    is_escape_layer <- function(obj) {
        inherits(obj, "formula") || inherits(obj, "expression") ||
            inherits(obj, "call") || is.function(obj)
    }
    objects <- Filter(function(obj) is_grammar_layer(obj) || is_escape_layer(obj),
                      plot$adds)
    if (!length(objects)) return(list())
    names <- names(plot$layer)
    Map(function(obj, i) {
        name <- if (length(names) >= i) names[[i]] else paste("layer", i)
        if (!is_grammar_layer(obj)) {
            ## The raw base-graphics escape hatch: it draws, but it has no
            ## mapping, data inheritance, scale or range contribution to report.
            return(list(
                name = name,
                kind = "function",
                data = NULL,
                data_source = "none",
                mapping = character(),
                inherit = FALSE,
                params = character()
            ))
        }
        mapping <- bb_layer_aesthetics(plot, obj$mapping,
                                       obj$inherit %||% TRUE,
                                       obj$params %||% list())
        data <- if (is.data.frame(obj$data)) obj$data else plot$data
        list(
            name = name,
            kind = "layer",
            data = data,
            data_source = if (is.data.frame(obj$data)) "layer" else "plot",
            mapping = bb_mapping_labels(mapping),
            inherit = obj$inherit %||% TRUE,
            params = names(obj$params %||% list())
        )
    }, objects, seq_along(objects))
}

## Per-panel limits for an inspected faceted plot. The panel is rebuilt exactly
## the way `bb_print_facet()` rebuilds it, but nothing is drawn; the limits are
## taken from the same helpers, and shared scales use the global limits like
## the printer does.
bb_build_panels <- function(plot) {
    facet <- plot$facet
    if (is.null(facet)) return(NULL)
    data <- plot$data
    if (is.null(data)) {
        bb_abort("`bb_facet_wrap()` requires plot data; `bb_build()` cannot ",
                 "report its panels without data.")
    }

    expr <- rlang::f_rhs(facet$facets)
    facet_vars <- all.vars(expr)
    values <- rlang::eval_tidy(expr, data = data)
    keys_chr <- as.character(values)
    keys <- sort(unique(keys_chr[!is.na(keys_chr)]))
    if (!length(keys)) return(list())
    bb_facet_layout(facet, length(keys))
    scales <- facet$scales %||% "fixed"
    global <- bb_plot_limits(plot)

    lapply(keys, function(key) {
        idx <- !is.na(keys_chr) & keys_chr == key
        panel_data <- data[idx, , drop = FALSE]
        panel <- .bbplot_initial(plot$canvas, panel_data, plot$mapping)

        for (obj in plot$adds) {
            if (inherits(obj, "bb_facet_wrap")) next
            if (inherits(obj, "bb_legend") && isTRUE(obj$outside)) next
            if (isTRUE(attr(obj, "bb_generated"))) next
            if (inherits(obj, "bb_layer") && is.data.frame(obj$data)) {
                if (length(facet_vars) &&
                    !all(facet_vars %in% names(obj$data)) &&
                    nrow(obj$data) != nrow(data)) {
                    bb_warn("layer data does not carry the facet variable (",
                            paste(facet_vars, collapse = ", "),
                            ") and has a different number of rows; it is ",
                            "drawn in every panel without being split.")
                }
                obj$data <- bb_facet_layer_data(obj$data, facet, key, idx,
                                                nrow(data))
                if (nrow(obj$data) == 0L) next
            }
            panel <- panel + obj
        }
        panel <- bb_set_scales(panel, bb_get_scales(plot))
        panel <- bb_summary_labs(panel)
        lim <- bb_plot_limits(panel)

        list(
            key = key,
            n = sum(idx),
            xlim = if (scales %in% c("free", "free_x")) lim$xlim else global$xlim,
            ylim = if (scales %in% c("free", "free_y")) lim$ylim else global$ylim
        )
    })
}
