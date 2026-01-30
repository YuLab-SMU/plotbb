##' bbplot 
##'
##' a proof of concept for grammar of graphics based on base plot.
##' The bbplot class contains data (input data), mapping (aesthetic mapping),
##' layer (a list of plot layers), theme (theme setting) and
##' labs (label setting, including title, subtitle, x and y labels).
##' @title bbplot
##' @param data data
##' @param mapping variable mapping
##' @return bbplot object
##' @importFrom graphics plot
##' @export
##' @examples
##' library(plotbb)
##' p <- bbplot(mtcars, bb_aes(mpg, disp, col=factor(cyl)))
##' p + bb_grid(col='grey50', lty='dashed') + bb_point(pch=19)
##' @author Guangchuang Yu
bbplot <- function(data, mapping = bb_aes()) {
    p <- function(xlim = NULL, ylim = NULL, ...) {
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
        do.call(graphics::plot, c(args, list(...)))
    }
    
    .bbplot_initial(p, data, mapping) +
        bb_labs(
            xlab = if (!is.null(mapping$x)) rlang::as_label(mapping$x) else NULL,
            ylab = if (!is.null(mapping$y)) rlang::as_label(mapping$y) else NULL
        )
}

