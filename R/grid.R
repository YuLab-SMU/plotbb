##' add grid lines
##'
##' 
##' @title bb_grid
##' @rdname bb-grid
##' @param col line color
##' @param lty line type
##' @param lwd line width
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_grid <- function(col = 'lightgray', lty = par("lty"), lwd = par("lwd")) {
    structure(list(col = col,
                   lty = lty,
                   lwd = lwd,
                   layer = ly_grid
                   ),
              class = "bb_layer")
}

##' @importFrom graphics grid
ly_grid <- function(plot, col, lty, lwd) {
    ly <- function() {
        grid(NULL, NULL, col = col, lty = lty, lwd = lwd)        
    }
    plot <- add_layer(plot, ly, "background grid line")
}
