##' density layer
##'
##' @title bb_density
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param ... addition parameter for the layer, such as \code{adjust}, \code{kernel}.
##' @return A modified bbplot object
##' @importFrom stats density
##' @export
##' @author Guangchuang Yu
bb_density <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ..., layer = ly_density)
}

ly_density <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)
    
    params <- list(...)

    ly <- function() {
        if (is.null(mapping$x)) return(invisible(NULL))
        
        x_raw <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
        if (is.null(x_raw) || !is.numeric(x_raw)) return(invisible(NULL))
        
        grp <- eval_mapping(mapping, "col", data) %||% eval_mapping(mapping, "group", data) %||% eval_mapping(mapping, "fill", data)
        
        args <- params
        alpha <- args$alpha %||% 0.5
        
        if (is.null(grp)) {
            d <- stats::density(x_raw, na.rm = TRUE, 
                                adjust = args$adjust %||% 1, 
                                kernel = args$kernel %||% "gaussian")
            col <- bb_col(mapping, data, plot = plot) %||% "black"
            if (length(col) > 0) col <- col[1]
            
            graphics::polygon(c(min(d$x), d$x, max(d$x)), c(0, d$y, 0), col = scales::alpha(col, alpha), border = col)
        } else {
            grp_chr <- as.character(grp)
            keep <- !is.na(grp_chr) & !is.na(x_raw)
            if (!any(keep)) return(invisible(NULL))
            
            grp_chr <- grp_chr[keep]
            x_raw <- x_raw[keep]
            
            levs <- bb_discrete_levels(grp)
            levs <- levs[levs %in% unique(grp_chr)]
            
            cols <- bb_col(mapping, data, plot = plot)
            
            # Support mapping fill directly for polygon-based layers.
            if (!is.null(mapping$fill)) {
                 fill_var <- eval_mapping(mapping, "fill", data)
                 scale_fill <- plot$scales$fill %||% plot$scales$col
                 if (is.null(scale_fill)) {
                     scale_fill <- structure(list(palette = NULL, type = "palette"), class = "bb_palette")
                 }
                 mapped_cols <- bb_scale_col_map(scale_fill, fill_var)
                 if (!is.null(mapped_cols)) cols <- mapped_cols
            }
            
            if (!is.null(cols)) {
                # Fallback safely if dimensions don't match
                if (length(cols) != length(grp_chr)) {
                   cols <- rep(cols, length.out = length(grp_chr))
                }
                col_map <- unname(tapply(cols, grp_chr, function(z) z[[1]])[levs])
            } else {
                col_map <- rep("black", length(levs))
            }
            
            for (i in seq_along(levs)) {
                g <- levs[i]
                sub_x <- x_raw[grp_chr == g]
                if (length(sub_x) < 2) next
                
                d <- stats::density(sub_x, na.rm = TRUE, 
                                    adjust = args$adjust %||% 1, 
                                    kernel = args$kernel %||% "gaussian")
                c_col <- col_map[i]
                graphics::polygon(c(min(d$x), d$x, max(d$x)), c(0, d$y, 0), col = scales::alpha(c_col, alpha), border = c_col)
            }
        }
    }
    
    add_layer(plot, ly, "density layer")
}
