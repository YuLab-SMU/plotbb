

add_layer <- function(p, ly) {
    nlayer <- length(p$layer)
    if (nlayer == 0) {
        p$layer <- list(ly)
    } else {
        p$layer[[nlayer + 1]] <- ly
    }
    return(p)
}


