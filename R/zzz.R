.onAttach <- function(libname, pkgname) {
    .plotbb_env_initial()
}


.plotbb_env_initial <- function() {
    pos <- 1
    envir <- as.environment(pos)
    assign(".bbplot", new.env(), envir = envir)
}
