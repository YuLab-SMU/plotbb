##' Facet a bbplot object into a row/column grid.
##'
##' @title bb_facet_grid
##' @param rows a one-sided formula for the row variable, e.g. \code{~cyl}.
##' @param cols a one-sided formula for the column variable, e.g. \code{~am}.
##' @param scales scale behavior across panels, one of 'fixed', 'free',
##'   'free_x', or 'free_y'. With free scales, x is shared within a column and y
##'   within a row, matching the grid layout.
##' @param labeller optional function applied to each row/column key.
##' @param drop if FALSE, unused factor levels still get panels.
##' @param strip_width optional positive integer; wraps long strip labels.
##' @details At least one of \code{rows} and \code{cols} is required. Panels are
##'   the full combination of the row and column keys; a combination without
##'   rows is drawn empty rather than skipped. Row labels are placed at the
##'   right, column labels at the top. For \code{bb_build()}, each panel is
##'   reported with its row and column key, label, row count and limits.
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_facet_grid <- function(rows = NULL, cols = NULL, scales = "fixed",
                          labeller = NULL, drop = TRUE, strip_width = NULL) {
    if (is.null(rows) && is.null(cols)) {
        bb_abort("`bb_facet_grid()` needs at least one of `rows` and `cols`.")
    }
    for (nm in c("rows", "cols")) {
        value <- get(nm)
        if (!is.null(value) && !inherits(value, "formula")) {
            bb_abort("`", nm, "` in `bb_facet_grid()` must be a one-sided ",
                     "formula or NULL.")
        }
    }
    scales <- match.arg(scales, c("fixed", "free", "free_x", "free_y"))
    if (!is.null(labeller) && !is.function(labeller)) {
        bb_abort("`labeller` in `bb_facet_grid()` must be a function or NULL.")
    }
    if (!is.logical(drop) || length(drop) != 1L || is.na(drop)) {
        bb_abort("`drop` in `bb_facet_grid()` must be TRUE or FALSE.")
    }
    if (!is.null(strip_width)) {
        if (!is.numeric(strip_width) || length(strip_width) != 1L ||
            is.na(strip_width) || strip_width < 1 ||
            strip_width != as.integer(strip_width)) {
            bb_abort("`strip_width` in `bb_facet_grid()` must be a positive ",
                     "integer or NULL.")
        }
        strip_width <- as.integer(strip_width)
    }
    structure(
        list(
            rows = rows,
            cols = cols,
            scales = scales,
            labeller = labeller,
            drop = drop,
            strip_width = strip_width
        ),
        class = "bb_facet_grid"
    )
}

##' @method bbplot_add bb_facet_grid
##' @export
bbplot_add.bb_facet_grid <- function(object, plot) {
    plot$facet <- object
    plot
}

bb_facet_base_canvas <- function(plot, data, xlim, ylim) {
    xy <- bb_eval_xy(plot$mapping, data)
    if (!is.null(xy$x) && !is.null(xy$y) &&
        length(xy$x) > 0L && length(xy$y) > 0L) {
        args <- list(xy$x, xy$y, type = "n", xlab = "", ylab = "")
        if (!is.null(xlim)) args$xlim <- xlim
        if (!is.null(ylim)) args$ylim <- ylim
        do.call(graphics::plot, args)
        return(invisible(NULL))
    }
    if (!is.null(xlim) && !is.null(ylim)) {
        graphics::plot.new()
        graphics::plot.window(xlim = xlim, ylim = ylim)
        graphics::box()
        return(invisible(NULL))
    }
    bb_call_canvas(plot$canvas, xlim = xlim, ylim = ylim)
    invisible(NULL)
}

bb_grid_layer_data <- function(layer_data, plot, facet, row_expr, col_expr,
                               row_key, col_key, idx) {
    vars <- unique(c(all.vars(row_expr), all.vars(col_expr)))
    if (length(vars) && all(vars %in% names(layer_data))) {
        row_val <- rlang::eval_tidy(row_expr, data = layer_data)
        col_val <- rlang::eval_tidy(col_expr, data = layer_data)
        keep <- !is.na(as.character(row_val)) &
            as.character(row_val) == row_key &
            !is.na(as.character(col_val)) &
            as.character(col_val) == col_key
        return(layer_data[keep, , drop = FALSE])
    }
    if (nrow(layer_data) == nrow(plot$data)) {
        return(layer_data[idx, , drop = FALSE])
    }
    bb_warn("layer data does not carry the facet variable(s) (",
            paste(vars, collapse = ", "), ") and has a different number of ",
            "rows; it is drawn in every panel without being split.")
    layer_data
}

## Build every panel of a grid once. Both the printer and `bb_build()` use this
## list, which is what keeps the built limits identical to the drawn limits.
bb_grid_specs <- function(plot) {
    facet <- plot$facet
    data <- plot$data
    row_expr <- if (!is.null(facet$rows)) rlang::f_rhs(facet$rows) else NULL
    col_expr <- if (!is.null(facet$cols)) rlang::f_rhs(facet$cols) else NULL
    row_val <- if (is.null(row_expr)) rep("all", nrow(data)) else
        rlang::eval_tidy(row_expr, data = data)
    col_val <- if (is.null(col_expr)) rep("all", nrow(data)) else
        rlang::eval_tidy(col_expr, data = data)
    row_lev <- if (is.null(row_expr)) "all" else bb_facet_levels(facet, row_val)
    col_lev <- if (is.null(col_expr)) "all" else bb_facet_levels(facet, col_val)
    nrow <- length(row_lev)
    ncol <- length(col_lev)
    if (nrow == 0L || ncol == 0L) return(list())

    adds <- plot$adds
    global <- bb_plot_limits(plot)
    specs <- vector("list", nrow * ncol)
    k <- 0L
    for (i in seq_len(nrow)) {
        for (j in seq_len(ncol)) {
            k <- k + 1L
            row_chr <- as.character(row_val)
            col_chr <- as.character(col_val)
            idx <- !is.na(row_chr) & row_chr == row_lev[i] &
                !is.na(col_chr) & col_chr == col_lev[j]
            panel_data <- data[idx, , drop = FALSE]
            empty_panel <- !any(idx)
            ## Capture this panel's data; a plain closure in the loop would
            ## see the last panel's data by the time printing happens.
            canvas <- local({
                panel_rows <- panel_data
                function(xlim = NULL, ylim = NULL) {
                    bb_facet_base_canvas(plot, panel_rows, xlim, ylim)
                }
            })
            panel <- .bbplot_initial(canvas, panel_data, plot$mapping)
            if (!empty_panel) {
                for (obj in adds) {
                    if (inherits(obj, "bb_facet_wrap") ||
                        inherits(obj, "bb_facet_grid")) next
                    if (inherits(obj, "bb_legend") && isTRUE(obj$outside)) next
                    if (isTRUE(attr(obj, "bb_generated"))) next
                    if (inherits(obj, "bb_layer") && is.data.frame(obj$data)) {
                        obj$data <- bb_grid_layer_data(
                            obj$data, plot, facet, row_expr, col_expr,
                            row_lev[i], col_lev[j], idx
                        )
                        if (nrow(obj$data) == 0L) next
                    }
                    panel <- panel + obj
                }
            }
            panel <- bb_set_scales(panel, bb_get_scales(plot))
            panel <- bb_summary_labs(panel)
            lim <- if (empty_panel) NULL else bb_plot_limits(panel)
            specs[[k]] <- list(
                i = i, j = j,
                key = paste(row_lev[i], col_lev[j], sep = "|"),
                row = row_lev[i], col = col_lev[j],
                row_label = if (is.null(row_expr)) "" else
                    bb_facet_label(facet, row_lev[i]),
                col_label = if (is.null(col_expr)) "" else
                    bb_facet_label(facet, col_lev[j]),
                data = panel_data, idx = idx,
                n = sum(idx), empty = empty_panel,
                plot = panel, lim = lim
            )
        }
    }

    panel_range <- function(indices, axis) {
        limits <- lapply(indices, function(k) specs[[k]]$lim[[axis]])
        limits <- Filter(Negate(is.null), limits)
        if (!length(limits)) return(global[[axis]])
        range(unlist(limits, use.names = FALSE))
    }
    x_by_col <- lapply(seq_len(ncol), function(j) {
        panel_range(which(vapply(specs, `[[`, 0L, "j") == j), "xlim")
    })
    y_by_row <- lapply(seq_len(nrow), function(i) {
        panel_range(which(vapply(specs, `[[`, 0L, "i") == i), "ylim")
    })

    scales <- facet$scales %||% "fixed"
    for (k in seq_along(specs)) {
        spec <- specs[[k]]
        spec$xlim <- if (spec$empty) {
            global$xlim
        } else if (scales %in% c("free", "free_x")) {
            x_by_col[[spec$j]]
        } else {
            global$xlim
        }
        spec$ylim <- if (spec$empty) {
            global$ylim
        } else if (scales %in% c("free", "free_y")) {
            y_by_row[[spec$i]]
        } else {
            global$ylim
        }
        specs[[k]] <- spec
    }
    specs
}

bb_print_facet_grid <- function(x) {
    facet <- x$facet
    data <- x$data
    if (is.null(data)) {
        stop("bb_facet_grid requires plot data")
    }
    x <- bb_train_scales(x)
    specs <- bb_grid_specs(x)
    if (!length(specs)) return(invisible(NULL))

    nrow <- max(vapply(specs, `[[`, 0L, "i"))
    ncol <- max(vapply(specs, `[[`, 0L, "j"))
    scales <- facet$scales %||% "fixed"
    adds <- x$adds
    outside_legends <- Filter(
        function(obj) inherits(obj, "bb_legend") && isTRUE(obj$outside),
        adds
    )

    global_main <- x$labs$main
    global_sub <- x$labs$sub
    top_oma <- if (!is.null(global_main) && !is.null(global_sub)) {
        4
    } else if (!is.null(global_main) || !is.null(global_sub)) {
        3
    } else {
        0
    }

    if (length(outside_legends)) {
        panel_layout <- matrix(seq_len(nrow * ncol), nrow = nrow, ncol = ncol,
                               byrow = TRUE)
        legend_idx <- max(panel_layout) + 1
        layout_mat <- cbind(panel_layout, rep(legend_idx, nrow))
        graphics::layout(layout_mat, widths = c(rep(1, ncol), 0.38))
        graphics::par(oma = c(0, 0, top_oma, 0))
    } else {
        par(mfrow = c(nrow, ncol), oma = c(0, 0, top_oma, 0))
    }

    for (k in seq_along(specs)) {
        spec <- specs[[k]]
        top_lines <- max(1L, length(strsplit(spec$col_label, "\n",
                                             fixed = TRUE)[[1]]))
        graphics::par(mar = c(4, 4, 2 + top_lines, 4.5))
        p <- spec$plot
        p$labs$main <- if (spec$i == 1L) spec$col_label else NULL
        p$labs$sub <- NULL
        bb_call_canvas(p$canvas, xlim = spec$xlim, ylim = spec$ylim)
        if (!is.null(p$panel.first)) eval(p$panel.first())
        for (ly in p$layer) eval(ly())
        do.call(title, p$labs)
        if (spec$j == ncol && nzchar(spec$row_label)) {
            graphics::mtext(spec$row_label, side = 4, line = 1, las = 0,
                            cex = 0.9)
        }
    }

    if (length(outside_legends)) {
        graphics::par(mar = c(0, 0, 0, 0), xpd = NA)
        graphics::plot.new()
        for (j in seq_along(outside_legends)) {
            legend_obj <- outside_legends[[j]]
            legend_obj$outside <- FALSE
            legend_obj$position <- 0.05
            legend_obj$params$y <- 0.85 - (j - 1) * 0.25
            bb_draw_legend(legend_obj, x)
        }
    }

    if (!is.null(global_main)) {
        graphics::mtext(global_main, side = 3, outer = TRUE,
                        line = if (!is.null(global_sub)) 1.5 else 1, font = 2)
    }
    if (!is.null(global_sub)) {
        graphics::mtext(global_sub, side = 3, outer = TRUE, line = 0.2)
    }

    .bbplot <- get_plotbb_env()
    assign(".last_plot", x, envir = .bbplot)
    invisible(x)
}
