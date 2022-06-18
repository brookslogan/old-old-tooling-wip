


fetch_resource_uncached = function(fetch_current, response_looks_usable, now) {
    response = fetch_current()
    response.looks.usable = response_looks_usable(response)
    if (isTRUE(response.looks.usable)) {
        response.info = list(
            response=response,
            fetch.time=now
        )
        return (response.info)
    } else {
        stop (paste0("Attempt to fetch resource did not yield a response that looked usable; response_looks_usable output:\n",toString(response.looks.usable),if(is.null(response.looks.usable)){"(NULL)"}else{""},"; response:\n",paste(capture.output(str(response)),collapse="\n"),"\n"))
    }
}

fetch_resource =
    function(fetch_current,
             response_looks_usable,
             cache,
             cache.key,
             cache.invalidation.period,
             verbose=FALSE
             ) {
        now = Sys.time()
        ## todo vectorize, allowing multiple keys and results at once?
        cached.response.info = query_cache(cache, cache.key)
        if (is.null(cached.response.info)) {
            if (verbose) {
                message("No response was previously cached; fetching...\n")
            }
            response.info = fetch_resource_uncached(fetch_current, response_looks_usable, now)
            store_value(cache, cache.key, response.info)
            return (response.info[["response"]])
        } else {
            ## todo allow/force fetching the timestamp without the result
            cache.staleness = difftime(now, cached.response.info[["fetch.time"]])
            if (cache.staleness >= cache.invalidation.period) {
                if (verbose) {
                    message("Cached value is too stale; re-fetching...\n")
                }
                response.info = fetch_resource_uncached(fetch_current, response_looks_usable, now)
                store_value(cache, cache.key, response.info)
                return (response.info[["response"]])
            } else {
                if (verbose) {
                    message("Cached value is fresh enough; returning.\n")
                }
                ## xxx add option to re-check usability of cached responses?
                return (cached.response.info[["response"]])
            }
        }
    }
