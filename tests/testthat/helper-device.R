## The 0.2.5 grammar contract: data/mapping inheritance, constants versus
## mappings, draw order, the custom-layer entry point and the boundary of `+`.

with_test_pdf <- function(code) {
    f <- tempfile(fileext = ".pdf")
    grDevices::pdf(f)
    on.exit({
        if (grDevices::dev.cur() > 1L) grDevices::dev.off()
        unlink(f)
    }, add = TRUE)
    force(code)
    invisible(NULL)
}

layer_mapping_of <- function(p, i = length(p$adds)) {
    bb_layer_mapping(p, p$adds[[i]])
}
