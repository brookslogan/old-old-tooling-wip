
##' @importFrom pipeR %>>%
`%pipeR>>%` = pipeR::`%>>%`

paste0p = function(...) {
    if (any(sapply(list(...), length) == 0L)) {
        character(0L)
    } else {
        paste0(...)
    }
}
