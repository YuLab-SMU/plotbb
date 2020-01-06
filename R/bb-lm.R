##' @rdname layer
##' @export
bb_lm <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ...,  layer = ly_lm)
}

##' @importFrom graphics segments
##' @importFrom graphics par
ly_lm <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)

    layer_name <- "linear regression layer"
    formula <- stats::as.formula(paste(yvar(mapping), '~', xvar(mapping)))
    if (is.null(mapping$group)) {
        d2 <- lm_data(formula, data)
        ly <- function() {
            segments(x0 = d2$x0,
                     y0 = d2$y0,
                     x1 = d2$x1,
                     y1 = d2$y1, ...)
        }
        plot <- add_layer(plot, ly, layer_name)
        return(plot)
    }

    params <- list(...)
    grp <- parse_mapping(mapping, 'group', data)
    ugrp <- unique(grp)

    ly <- function() {
        cols <- NULL
        if (is.null(params$col) && !is.null(mapping$col)) {
            cols <- unique(bb_col(mapping, data,
                                  palette = get("palette", envir = plot$env)))
        }

        d2 <- lapply(ugrp, function(g) {
            d <- data[grp == g, ]
            lm_data(formula, d)
        }) %>% do.call('rbind', .)

        if (is.null(cols)) {
            segments(x0 = d2$x0,
                     y0 = d2$y0,
                     x1 = d2$x1,
                     y1 = d2$y1, ...)
        } else {
            d2$col <- cols
            segments(x0 = d2$x0,
                     y0 = d2$y0,
                     x1 = d2$x1,
                     y1 = d2$y1,
                     col = d2$col, ...)
        }    
    }

    #with_env(ly, lm_env(d2))
    plot <- add_layer(plot, ly, layer_name)
    return(plot)
}


lm_data <- function(formula, data) {
    s <- stats::lm(formula, data = data)
    x <- as.character(formula)[3]
    
    x0 <- min(data[[x]])
    x1 <- max(data[[x]])
    y0 <- s$coefficients[1] + x0 * s$coefficients[2]
    y1 <- s$coefficients[1] + x1 * s$coefficients[2]
    data.frame(x0 = x0, x1 = x1,
               y0 = y0, y1 = y1)
}

lm_env <- function(data) {
    lm_env <- new.env()
    lm_env$x0 <- data$x0
    lm_env$x1 <- data$x1
    lm_env$y0 <- data$y0
    lm_env$y1 <- data$y1
    if (!is.null(data$col))
        lm_env$col <- data$col
    return(lm_env)
}

## .ly_lm <- function(formula, data, col = par("fg"), lty = par("lty"), lwd = par("lwd")) {
##     s <- stats::lm(formula, data = data)
##     x <- as.character(formula)[3]

##     x0 <- min(data[[x]])
##     x1 <- max(data[[x]])
##     y0 <- s$coefficients[1] + x0 * s$coefficients[2]
##     y1 <- s$coefficients[1] + x1 * s$coefficients[2]
##     lm_env <- new.env()
##     lm_env$x0 <- x0
##     lm_env$x1 <- x1
##     lm_env$y0 <- y0
##     lm_env$y1 <- y1
##     lm_env$col <- col
##     lm_env$lty <- lty
##     lm_env$lwd <- lwd
##     ly <- function() segments(x0 = x0, y0 = y0,
##                               x1 = x1, y1 = y1,
##                               col = col, lty = lty,
##                               lwd = lwd)
##     with_env(ly, lm_env)
## }
