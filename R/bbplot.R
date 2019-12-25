##' bbplot 
##'
##' a proof of concept for grammar of graphics based on base plot
##' @title bbplot
##' @param data data
##' @param mapping variable mapping
##' @return bbplot object
##' @importFrom graphics plot
##' @export
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


 
