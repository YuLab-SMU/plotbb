get_plotbb_env <- function() {
    if(!exists(".bbplot")) .plotbb_env_initial()
    get(".bbplot")
}

with_env <- function(f, e=parent.frame()) {
    stopifnot(is.function(f))
    environment(f) <- e
    f
}


plot_fun <- function(x) {
    if (inherits(x, "formula")) {
        ## convert to expression
        x <- parse(text=as.character(x)[2])
    } 

    function() {
        if (inherits(x, "function"))
            return(x())
        eval(x)
    }
}


bb_data <- function(p, data) {
    data %||% p$data    
}

##' @importFrom utils modifyList
bb_mapping <- function(p, mapping) {
    if (!is.null(mapping)) {
        mapping <- modifyList(p$mapping, mapping)
    } else {
        mapping <- p$mapping
    }

    return(mapping)
}

xvar <- function(mapping) {
    get_mapping(mapping, 'x')
}

yvar <- function(mapping) {
    get_mapping(mapping, 'y')
}

##' @importFrom rlang quo_name
get_mapping <- function(mapping, name) {
    quo_name(mapping[[name]])
}

parse_mapping <- function(mapping, name, data) {
    v <- get_mapping(mapping, name)
    v2 <- gsub(".*\\((\\w+)\\).*", "\\1", v)
    eval(parse(text=sub(v2, "data[[v2]]", v)))
}

##' @importFrom scales col_numeric
##' @importFrom scales col_factor
bb_col <- function(mapping, data, palette = NULL) {
    col_var <- parse_mapping(mapping, 'col', data)
    if (is.numeric(col_var)) {
        if (is.null(palette)) palette <- "viridis"
        f <- scales::col_numeric(palette, col_var)
        col_vec <- f(col_var)
    } else {
        ucol <- sort(unique(col_var))
        if (is.null(palette)) palette <- "Set2"
        f <- scales::col_factor(palette, ucol)
        ## cols <- colorspace::rainbow_hcl(length(ucol))
        cols <- f(ucol)
        names(cols) <- ucol
        col_vec <- cols[as.character(col_var)]
    }
    return(col_vec)
}


`%||%` <- function (a, b) {
    if (!is.null(a)) 
        a
    else b
}

##' @importFrom magrittr %>%
##' @export
magrittr::`%>%`


utils::globalVariables(".")
