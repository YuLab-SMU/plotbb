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
bbplot <- function(data, mapping = amp()) {
    xvar <- quo_name(mapping$x)
    yvar <- quo_name(mapping$y)

    p <- function() {
        plot(data[[xvar]], data[[yvar]],
             type = 'n',
             xlab = xvar,
             ylab = yvar)
    }

    structure(list(
        plot = p,
        data = data,
        mapping = mapping,
        xvar = xvar,
        yvar = yvar,
        layer = list()
    ), class = "bbplot")
    
}

##' aesthetic mapping
##'
##'
##' aesthetic mapping for bbplot
##'
##' @title amp
##' @param x x variable
##' @param y y variable
##' @param ... other mappings
##' @importFrom rlang enquos
##' @export
##' @author Guangchuang Yu
amp <- function(x, y, ...) {
    enquos(x = x, y = y, ..., .ignore_empty = "all")
}

##' @method print bbplot
##' @export
print.bbplot <- function(x, ...) {
    eval(x$plot())
    for(ly in x$layer) {
        eval(ly())
    }
}

