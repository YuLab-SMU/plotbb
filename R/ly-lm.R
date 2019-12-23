##' @rdname layer
##' @export
ly_lm <- function(p, mapping = NULL, data = NULL, ...) {
    data <- bb_data(p, data)
    mapping <- bb_mapping(p, mapping)

    formula <- stats::as.formula(paste(yvar(mapping), '~', xvar(mapping)))
    params <- list(...)
    if (is.null(mapping$group)) {
        ly <- .ly_lm(formula, data, ...)
        p <- add_layer(p, ly)        
        return(p)
    }

    grp <- parse_mapping(mapping, 'group', data)
    ugrp <- unique(grp)
    cols <- NULL
    if (is.null(params$col) && !is.null(mapping$col)) {
        cols <- unique(bb_col(mapping, data))
    }
    for (i in seq_along(ugrp)) {
        d <- data[grp == ugrp[i], ]
        if (!is.null(cols)) {
            ly <- .ly_lm(formula, d, ..., col = cols[i])
        } else {
            ly <- .ly_lm(formula, d, ...)
        }
        
        p <- add_layer(p, ly)
    }
    
    return(p)
}


##' @importFrom graphics segments
##' @importFrom graphics par
.ly_lm <- function(formula, data, col = par("fg"), lty = par("lty"), lwd = par("lwd")) {
    s <- stats::lm(formula, data = data)
    x <- as.character(formula)[3]

    x0 <- min(data[[x]])
    x1 <- max(data[[x]])
    y0 <- s$coefficients[1] + x0 * s$coefficients[2]
    y1 <- s$coefficients[1] + x1 * s$coefficients[2]
    lm_env <- new.env()
    lm_env$x0 <- x0
    lm_env$x1 <- x1
    lm_env$y0 <- y0
    lm_env$y1 <- y1
    lm_env$col <- col
    lm_env$lty <- lty
    lm_env$lwd <- lwd
    ly <- function() segments(x0 = x0, y0 = y0,
                              x1 = x1, y1 = y1,
                              col = col, lty = lty,
                              lwd = lwd)
    with_env(ly, lm_env)
}
