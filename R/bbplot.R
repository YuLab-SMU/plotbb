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
    xx <- xvar(mapping)
    yy <- yvar(mapping)

    p <- function() {
        plot(data[[xx]], data[[yy]],
             type = 'n',
             xlab = "", 
             ylab = "")
    }
    
    .bbplot_initial(p, data, mapping) +
        bb_labs(xlab = xx, ylab = yy)
}

