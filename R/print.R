##' @method print bbplot
##' @export
print.bbplot <- function(x, ...) {
    if (!is.null(x$facet)) {
        old.par <- par(no.readonly=TRUE)
        on.exit(suppressWarnings(par(old.par, no.readonly = TRUE)))

        if (length(x$theme)) {
            par(x$theme, no.readonly = TRUE)
        }

        facet <- x$facet
        data <- x$data
        if (is.null(data)) stop("bb_facet_wrap requires plot data")

        facet_val <- rlang::eval_tidy(rlang::f_rhs(facet$facets), data = data)
        facet_chr <- as.character(facet_val)
        facet_chr <- facet_chr[!is.na(facet_chr)]
        lev <- sort(unique(facet_chr))
        if (length(lev) == 0) return(invisible(NULL))

        lim_global <- bb_plot_limits(x)

        scales <- facet$scales %||% "fixed"
        scales <- match.arg(scales, c("fixed", "free", "free_x", "free_y"))

        ncol <- facet$ncol %||% ceiling(sqrt(length(lev)))
        nrow <- facet$nrow %||% ceiling(length(lev) / ncol)
        par(mfrow = c(nrow, ncol))

        for (v in lev) {
            idx <- as.character(facet_val) == v
            d <- data[idx, , drop = FALSE]

            base_canvas <- function(xlim = NULL, ylim = NULL) {
                xy <- bb_eval_xy(x$mapping, d)
                if (!is.null(xy$x) && !is.null(xy$y)) {
                    args <- list(xy$x, xy$y, type = "n", xlab = "", ylab = "")
                    if (!is.null(xlim)) args$xlim <- xlim
                    if (!is.null(ylim)) args$ylim <- ylim
                    do.call(graphics::plot, args)
                    return(invisible(NULL))
                }
                bb_call_canvas(x$canvas, xlim = xlim, ylim = ylim)
                invisible(NULL)
            }

            p <- .bbplot_initial(base_canvas, d, x$mapping)

            adds <- x$adds
            if (length(adds)) {
                for (obj in adds) {
                    if (inherits(obj, "bb_facet_wrap")) next
                    p <- p + obj
                }
            }

            if (identical(scales, "fixed")) {
                lim <- lim_global
            } else {
                lim_panel <- bb_plot_limits(p)
                lim <- list(
                    xlim = if (scales %in% c("free", "free_x")) lim_panel$xlim else lim_global$xlim,
                    ylim = if (scales %in% c("free", "free_y")) lim_panel$ylim else lim_global$ylim
                )
            }

            if (is.null(p$labs$main) && is.null(p$labs$sub)) {
                p <- p + bb_title(paste0(rlang::as_label(rlang::f_rhs(facet$facets)), "=", v))
            } else if (!is.null(p$labs$main) && is.null(p$labs$sub)) {
                p <- p + bb_sub(paste0(rlang::as_label(rlang::f_rhs(facet$facets)), "=", v))
            }

            bb_call_canvas(p$canvas, xlim = lim$xlim, ylim = lim$ylim)
            if (!is.null(p$panel.first)) {
                eval(p$panel.first())
            }

            for (ly in p$layer) {
                eval(ly())
            }
            do.call(title, p$labs)
        }

        .bbplot <- get_plotbb_env()
        assign(".last_plot", x, envir = .bbplot)
        return(invisible(x))
    }

    if (length(x$theme)) {
        old.par <- par(no.readonly=TRUE) #, new = TRUE)
        on.exit(suppressWarnings(par(old.par, no.readonly = TRUE)))
        par(x$theme, no.readonly = TRUE)
    }
    .bbplot <- get_plotbb_env()
    last_plot <- .bbplot$.last_plot

    if (!is.null(last_plot) && length(last_plot$theme))
        suppressWarnings(par(new = TRUE))

    lim <- bb_plot_limits(x)

    bb_call_canvas(x$canvas, xlim = lim$xlim, ylim = lim$ylim)
    if (!is.null(x$panel.first)) {
        eval(x$panel.first())
    } 
    
    

    for(ly in x$layer) {
        eval(ly())
    }

    do.call(title, x$labs)

    assign(".last_plot", x, envir = .bbplot)
}


##' @method print bbplot_layer_list
##' @export
print.bbplot_layer_list <- function(x, ...) {
    cat(length(x), "layers added to the plot\n")
    cat(paste0("  ", seq_along(x), ". ", names(x), collapse="\n"), "\n")
}

##' Facet a bbplot object into multiple panels.
##'
##' @title bb_facet_wrap
##' @param facets a formula specifying faceting variable, e.g. \code{~cyl}.
##' @param ncol number of columns.
##' @param nrow number of rows.
##' @param scales scale behavior across facets, one of 'fixed', 'free', 'free_x', or 'free_y'.
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_facet_wrap <- function(facets, ncol = NULL, nrow = NULL, scales = "fixed") {
    stopifnot(inherits(facets, "formula"))
    scales <- match.arg(scales, c("fixed", "free", "free_x", "free_y"))
    structure(
        list(
            facets = facets,
            ncol = ncol,
            nrow = nrow,
            scales = scales
        ),
        class = "bb_facet_wrap"
    )
}

##' @method bbplot_add bb_facet_wrap
##' @export
bbplot_add.bb_facet_wrap <- function(object, plot) {
    plot$facet <- object
    plot
}
