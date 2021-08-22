##' @method print bbplot
##' @export
print.bbplot <- function(x, ...) {
    if (length(x$theme)) {
        old.par <- par(no.readonly=TRUE) #, new = TRUE)
        on.exit(suppressWarnings(par(old.par, no.readonly = TRUE)))
        par(x$theme, no.readonly = TRUE)
    }
    .bbplot <- get_plotbb_env()
    last_plot <- .bbplot$.last_plot

    if (!is.null(last_plot) && length(last_plot$theme))
        suppressWarnings(par(new = TRUE))

    eval(x$canvas())
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

