
##' @include caldate.R
NULL

calweek_of = function(obj, ...) {
    UseMethod("calweek_of", obj)
}
calweek_of.caldate = function(obj, starting.wday, owning.wday, convention.name=NULL) {
    owning.caldate.days.from.origin = obj - (get_wday0(obj) - starting.wday) %% 7L + (owning.wday - starting.wday) %% 7L
    structure(
        owning.caldate.days.from.origin,
        class="calweek",
        starting.wday=starting.wday,
        owning.wday=owning.wday,
        convention.name=convention.name
    )
    ## fixme vs. numbering by week...
    ## xxx structure vs class<-, attributes<-, etc.
}
## fixme todo of integer, default, character

`[.calweek` = function(x, i, ...) {
    result = unclass(x)[i]
    class(result) <- "calweek"
    attributes(result) <- attributes(x)[c("starting.wday","owning.wday","convention.name")]
    result
}
`[[.calweek` = function(x, i, ...) {
    result = unclass(x)[[i]]
    class(result) <- "calweek"
    attributes(result) <- attributes(x)[c("starting.wday","owning.wday","convention.name")]
    result
}

print.calweek = function(x, ...) {
    owning.caldate = x
    attributes(owning.caldate) <- NULL
    class(owning.caldate) <- "caldate"
    convention.name = attr(x, "convention.name")
    if (is.null(convention.name)) {
        cat(paste0('<calweek>:\n'))
    } else {
        cat(paste0('<calweek, "',convention.name,'" convention>:\n'))
    }
    print(get_year(owning.caldate)*100L + n1_of_same_wday_in_year_up_to(owning.caldate))
    invisible(x)
}


## shift for adjusting tz? advance for moving between intervals? advance_after_end(epimonth, weeks)? weeks / depiweek / ...?
