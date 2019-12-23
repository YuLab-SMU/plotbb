##' @rdname layer
##' @export
ly_lm <- function(p, mapping = NULL, data = NULL, ...) {
    data <- bb_data(p, data)
    mapping <- bb_mapping(p, mapping)

    formula <- stats::as.formula(paste(yvar(mapping), '~', xvar(mapping)))

    if (!is.null(mapping$group)) {
        grp <- parse_mapping(mapping, 'group', data)
        for (g in unique(grp)) {
            d <- data[grp == g, ]
            ly <- .ly_lm(formula, d, ...)
            p <- add_layer(p, ly)
        }
    } else {
        ly <- .ly_lm(formula, data, ...)
        p <- add_layer(p, ly)        
    }

    return(p)
}


##' @importFrom graphics segments
.ly_lm <- function(formula, data, ...) {
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
    ly <- function() segments(x0, y0, x1, y1, ...)
    with_env(ly, lm_env)
}
