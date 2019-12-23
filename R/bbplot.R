##' bbplot 
##'
##' a proof of concept for grammar of graphics based on base plot
##' @title bbplot
##' @param data data
##' @param mapping variable mapping
##' @return bbplot object
##' @export
##' @author Guangchuang Yu
bbplot <- function(data, mapping) {
    xvar <- mapping[["x"]] %||% mapping[[1]]
    yvar <- mapping[['y']] %||% mapping[[2]]

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

##' @method print bbplot
##' @export
print.bbplot <- function(x, ...) {
    eval(x$plot())
    for(ly in x$layer) {
        eval(ly())
    }
}

##' layer
##'
##' bbplot layers
##' @title layer
##' @rdname layer
##' @param p bbplot object
##' @param ... addition parameter for the layer
##' @return bbplot object
##' @export
##' @author Guangchuang Yu
ly_point <- function(p, ...) {
    x <- p$data[[p$xvar]]
    y <- p$data[[p$yvar]]

    ly <- function() points(x, y, ...)
    add_layer(p, ly)
}

##' @rdname layer
##' @export
ly_lm <- function(p, ...) {
    x <- p$data[[p$xvar]]
    y <- p$data[[p$yvar]]

    ly <- function() abline(lm(y ~ x, data = p$data), ...)
    add_layer(p, ly)
}


add_layer <- function(p, ly) {
    nlayer <- length(p$layer)
    if (nlayer == 0) {
        p$layer <- list(ly)
    } else {
        p$layer[[nlayer + 1]] <- ly
    }
    return(p)
}



