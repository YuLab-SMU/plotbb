##' histogram layer
##'
##' @title bb_hist
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param ... addition parameter for the layer
##' @return A modified bbplot object
##' @importFrom graphics hist
##' @export
##' @author Guangchuang Yu
bb_hist <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ..., layer = ly_hist)
}

ly_hist <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)
    
    params <- list(...)

    ly <- function() {
        if (is.null(mapping$x)) return(invisible(NULL))
        
        x <- bb_eval_one(mapping$x, data)
        args <- params
        
        # Handle mapped color (fill) and border?
        if (!is.null(mapping$col)) {
            # In base hist, 'col' is fill color. 'border' is border.
            # In ggplot, 'col' is border, 'fill' is fill.
            # Let's align with base plotbb style. 
            # bb_point uses 'col'.
            # Let's assume 'col' -> fill color for histograms if mimicking base?
            # Or 'col' -> border?
            # hist(..., col = "gray") fills it.
            
            col_vec <- bb_col(mapping, data, plot = plot)
            # hist doesn't support vector of colors easily if it's just one distribution?
            # actually `col` in hist can be a vector, "recycled as needed".
            # But if it's one histogram, we just want one color usually.
            # Unless we are stacking? bb_hist probably just wraps hist(), which is 1 variable.
            
            args$col <- col_vec
        }
        
        # Note: hist helper `add=TRUE` is needed if we are adding to existing plot
        # But bbplot creates a customized canvas.
        # If we use `bbplot() + bb_hist()`, the canvas is initialized to range of x/y.
        # Histogram has frequency on Y axis.
        # Standard `bbplot` might set Y limits based on data$y.
        # If we use `bb_hist`, y is count. Mapping doesn't know about it.
        # This is tricky in this architecture.
        # Use `plot = TRUE` to draw it?
        # If we just call `hist(..., add=TRUE)`, it fits on existing plot.
        # Does `bbplot` setup the correct coordinates?
        # If user does `bbplot(mtcars, bb_aes(mpg)) + bb_hist()`,
        # `bbplot` sees x=mpg, y=NULL.
        # `bbplot` implementation of default canvas:
        # xy <- bb_eval_xy(mapping, data)
        # args <- list(xy$x, xy$y, type='n', ...)
        # If y is NULL, `plot(x)` might do something different or fail if type='n' with missing y.
        # `plot(x, type='n')` works (index plot).
        
        # We need to handle `add=TRUE` inside `ly()`.
        
        args$add <- TRUE
        # We might need to ensure 'freq' or 'density' matches usage.
        
        do.call(graphics::hist, c(list(x = x), args))
    }
    
    add_layer(plot, ly, "histogram layer")
}
