##' convert a base plot function to a bbplot object
##'
##' the base plot function will be plotted as a canvas and users can apply theme and add layers to it
##' @title as.bbplot
##' @rdname as-bbplot
##' @param fun a function that plot something in base graphics
##' @return A bbplot object
##' @export
##' @author Guangchuang Yu
as.bbplot <- function(fun) {
    stopifnot(is.function(fun))
    .bbplot_initial(fun)
}

.bbplot_initial <- function(canvas, data = NULL, mapping = bb_aes()) {
    p <- structure(list(
        canvas = canvas,
        data = data,
        mapping = mapping,
        scales = list(col = NULL),
        layer = structure(
            list(),
            class = "bbplot_layer_list"),
        theme = list(),
        env = new.env(),
        labs = list(main = NULL,
                    sub = NULL,
                    xlab = NULL,
                    ylab = NULL),
        panel.first = NULL,
        facet = NULL,
        adds = list()
    ), class = "bbplot")
    assign("palette", NULL, envir = p$env)
    assign("scales", list(col = NULL), envir = p$env)
    return(p)
}
