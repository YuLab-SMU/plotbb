##' change col palette
##'
##' 
##' @title bb_scale_col_palette
##' @rdname bb-scale
##' @param palette color palette
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_scale_col_palette <- function(palette = NULL) {
    structure(list(palette = palette), class = "bb_palette")
}


