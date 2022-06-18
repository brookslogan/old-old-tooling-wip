
methods::setOldClass("weeksteps")

weeksteps = function(n.weeks) {
    if (!is.numeric(n.weeks)) {
        stop ('n.weeks must be numeric.')
    }
    n.weeks.as.integer = as.integer(n.weeks)
    if (!all(n.weeks.as.integer == n.weeks)) {
        stop ('n.weeks must have all integer values.')
    }
    result = n.weeks
    class(result) <- "weeksteps"
    result
}

`-.weeksteps` = function(e1, e2) {
    if (!missing(e2)) {
        if (is(e1, "weeksteps")) {
            stop ('weeksteps not supported as first argument of binary `-`.')
        } else {
            stop ('subtracting weeksteps from this class of object is not supported.')
        }
    } else {
        result = -unclass(e1)
        class(result) <- "weeksteps"
        result
    }
}

## fixme vs. nweeks/n_weeks? but then n_weeks_in might be confusing
