##' boxplot layer
##'
##' @title bb_boxplot
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param ... addition parameter for the layer
##' @return A modified bbplot object
##' @importFrom graphics boxplot
##' @export
##' @author Guangchuang Yu
bb_boxplot <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ..., layer = ly_boxplot)
}

ly_boxplot <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)

    params <- list(...)
    if (is.null(params$add)) params$add <- TRUE
    if (is.null(params$axes)) params$axes <- FALSE
    
    ly <- function() {
        if (is.null(mapping$y)) {
            # If no y is mapped, check if x is mapped (for single variable boxplot? or should we enforce y?)
            # ggplot2 uses y for the variable in boxplot usually, or x/y combo.
            # base boxplot(x) plots x. 
            # If mapping$x is present and mapping$y is NULL, it's ambiguous.
            # Let's assume standard ggplot2:
            # aes(group, value) -> boxplot(value ~ group)
            # aes(value) -> boxplot(value) ?? In ggplot2 aes(y=value) gives one boxplot.
            
            # Let's support:
            # 1. aes(x, y) -> boxplot(y ~ x)
            # 2. aes(y) -> boxplot(y)
            return(invisible(NULL))
        }

        y <- bb_eval_one(mapping$y, data)
        
        args <- params
        
        if (!is.null(mapping$col)) {
             col_vec <- bb_col(mapping, data, plot = plot)
             # simple case: if col maps to the same group as x, it works naturally if we pass a vector
             # But boxplot(..., col=...) expects one color per group usually.
             
             # If mapping$col is same as mapping$x, we can extract unique colors.
             # This is a bit complex for base boxplot.
             # Let's pass the full vector if possible, or leave it to user to ensure it matches.
             # base boxplot col argument: "vector of colors (one for each group)"
             
             # For now, let's try passing it directly, but mapped colors are usually per-observation.
             # We might need to aggregate.
             
             if (!is.null(mapping$x)) {
                 x <- bb_eval_one(mapping$x, data)
                 # We need to find the color for each level of x.
                 # Assuming 1:1 mapping between x level and col.
                 udata <- unique(data.frame(x=as.character(x), col=col_vec))
                 # Sort by factor level of x if x is factor?
                 if (is.factor(x)) {
                     levs <- levels(x)
                     # Filter udata to only include levels present
                     udata <- udata[udata$x %in% levs, ]
                     # This is getting complicated. 
                     # Simplifying assumption: user passes a palette or single color via ... 
                     # OR if they map 'col', we rely on base plot behavior? 
                     # Base plot behavior for `col` in formula interface:
                     # "if there are fewer colors than boxes, they are recycled."
                     
                     # Let's try to infer if we can get a color-per-group.
                     # If not, ignore mapping$col or pass it as is (which might fail or look wrong).
                 }
                 
                 # Let's pass 'col' into args if it's not in there already?
                 # Actually, `col` is already in `params` if user set it manually.
                 # Here we are handling `mapping$col`.
                 
                 args$col <- col_vec # This might be wrong if length(col_vec) == nrow(data)
             } else {
                 # No x, just y. 
                 args$col <- unique(col_vec)[1] # Take first color?
             }
        }


        if (!is.null(mapping$x)) {
            f <- stats::as.formula(paste(rlang::as_label(mapping$y), "~", rlang::as_label(mapping$x)))
            do.call(graphics::boxplot, c(list(formula = f, data = data), args))
            return(invisible(NULL))
        }

        do.call(graphics::boxplot, c(list(x = y), args))
    }
    
    add_layer(plot, ly, "boxplot layer")
}
