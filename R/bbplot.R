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
    xx <- xvar(mapping)
    yy <- yvar(mapping)

    p <- function() {
        plot(data[[xx]], data[[yy]],
             type = 'n',
             xlab = "", 
             ylab = "")

    }
    
    res <- structure(list(
        plot = p,
        data = data,
        mapping = mapping,
        layer = list(),
        theme = list(),
        labs = list(title = NULL,
                    sub = NULL,
                    xlab = NULL,
                    ylab = NULL)
    ), class = "bbplot")

    bbylab(bbxlab(res, xx), yy) 
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
    if (length(x$theme)) {
        old.par <- par(no.readonly=TRUE) #, new = TRUE)
        on.exit(suppressWarnings(par(old.par, no.readonly = TRUE)))
        par(x$theme, no.readonly = TRUE)
    }
    suppressWarnings(par(new = TRUE))
    eval(x$plot())
    for(ly in x$layer) {
        eval(ly())
    }

    for (lab in x$labs) {
        if (!is.null(lab)) eval(lab())
    }

}

