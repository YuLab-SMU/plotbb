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
    
    structure(list(
        canvas = p,
        data = data,
        mapping = mapping,
        layer = structure(
            list(),
            class = "bbplot_layer_list"),
        theme = list(),
        labs = list(main = NULL,
                    sub = NULL,
                    xlab = xx,
                    ylab = yy)
    ), class = "bbplot")
}


 
