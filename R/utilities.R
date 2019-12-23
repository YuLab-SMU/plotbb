with_env <- function(f, e=parent.frame()) {
    stopifnot(is.function(f))
    environment(f) <- e
    f
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

bb_col <- function(mapping, data) {
    col_var <- parse_mapping(mapping, 'col', data)
    if (is.numeric(col_var)) {
        stop("not supported yet")
    } else {
        ucol <- unique(col_var)
        cols <- colorspace::rainbow_hcl(length(ucol))
        names(cols) <- ucol
        col_vec <- cols[col_var]
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
