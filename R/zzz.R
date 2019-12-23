.onAttach <- function(libname, pkgname) {
    pos <- 1
    envir <- as.environment(pos)
    assign(".bbplot", new.env(), envir = envir)
}
