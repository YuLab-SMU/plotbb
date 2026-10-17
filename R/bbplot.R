##' bbplot 
##'
##' a proof of concept for grammar of graphics based on base plot.
##' The bbplot class contains data (input data), mapping (aesthetic mapping),
##' layer (a list of plot layers), theme (theme setting) and
##' labs (label setting, including title, subtitle, x and y labels).
##' @title bbplot
##' @param data data
##' @param mapping variable mapping
##' @param auto_legend if TRUE (the default), mapped aesthetics get outside
##'   legends automatically; explicit `bb_legend()` components take over the
##'   aesthetics they name.
##'
##' @details
##' `+` adds components (layers, themes, labels, scales, legends, facets) to a
##' single plot and draws them in the order they were added. It does not
##' combine two complete plots: arrange several plots with
##' \code{aplot::plot_list()} instead.
##'
##' Every layer inherits the plot \code{data} and \code{mapping} and may
##' override them: a local mapping overrides the global mapping aesthetic by
##' aesthetic, and \code{inherit = FALSE} opts out of the global mapping
##' entirely. Aesthetic constants such as \code{bb_point(col = "tomato")} apply
##' to the whole layer and win over a mapped aesthetic.
##' @return bbplot object
##' @importFrom graphics plot
##' @export
##' @examples
##' library(plotbb)
##' p <- bbplot(mtcars, bb_aes(mpg, disp, col=factor(cyl)))
##' p + bb_grid(col='grey50', lty='dashed') + bb_point(pch=19)
##' @author Guangchuang Yu
bbplot <- function(data, mapping = bb_aes(), auto_legend = TRUE) {
    if (!is.logical(auto_legend) || length(auto_legend) != 1L ||
        is.na(auto_legend)) {
        bb_abort("`auto_legend` in `bbplot()` must be TRUE or FALSE.")
    }
    p <- function(xlim = NULL, ylim = NULL, log = "", xaxt = NULL,
                  yaxt = NULL, ...) {
        xy <- bb_eval_xy(mapping, data)
        
        if (!is.null(xlim) && !is.null(ylim)) {
            args <- list(
                x = NA,
                y = NA,
                type = "n",
                xlab = "",
                ylab = "",
                xlim = xlim,
                ylim = ylim
            )
            if (nzchar(log)) args$log <- log
            if (!is.null(xaxt)) args$xaxt <- xaxt
            if (!is.null(yaxt)) args$yaxt <- yaxt
            do.call(graphics::plot, c(args, list(...)))
            return(invisible(NULL))
        }

        args <- list(
            xy$x %||% NA,
            xy$y %||% NA,
            type = "n",
            xlab = "",
            ylab = ""
        )
        if (!is.null(xlim)) args$xlim <- xlim
        if (!is.null(ylim)) args$ylim <- ylim
        if (nzchar(log)) args$log <- log
        if (!is.null(xaxt)) args$xaxt <- xaxt
        if (!is.null(yaxt)) args$yaxt <- yaxt
        do.call(graphics::plot, c(args, list(...)))
    }
    
    plot <- .bbplot_initial(p, data, mapping) +
        bb_labs(
            xlab = if (!is.null(mapping$x)) rlang::as_label(mapping$x) else NULL,
            ylab = if (!is.null(mapping$y)) rlang::as_label(mapping$y) else NULL
        )
    plot$auto_legend <- isTRUE(auto_legend)
    plot
}

