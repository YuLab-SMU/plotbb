##' add grid lines
##'
##' 
##' @title bb_grid
##' @rdname bb-grid
##' @param col line color
##' @param lty line type
##' @param lwd line width
##' @param bg whether plot the grid lines as background
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_grid <- function(col = 'lightgray', lty = par("lty"), lwd = par("lwd"), bg = TRUE) {
    structure(list(col = col,
                   lty = lty,
                   lwd = lwd,
                   bg = bg
                   ),
              class = "bb_grid")
}

##' @importFrom graphics grid
ly_grid <- function(plot, col, lty, lwd) {
    ly <- function() {
        grid(NULL, NULL, col = col, lty = lty, lwd = lwd)        
    }
    add_layer(plot, ly, "background grid line")
}

##' @method bbplot_add bb_grid
##' @export
bbplot_add.bb_grid <- function(object, plot) {
    bg <- object$bg
    object <- object[names(object) != 'bg']
    object$nx <- NULL
    object$ny <- NULL
    ly <- function() do.call(grid, object)
    if (bg) {
        ## plot$canvas <- function() plot$canvas(panel.first = ly)
        plot$panel.first <- ly
        return(plot)
    }
    add_layer(plot, ly, "background grid line")
}
