
## todo reverse what is specializing what here?
cache_path = function(cache, branch.names) Reduce(`/`, branch.names, cache)

query_cache = function(cache, key, ...) UseMethod("query_cache", cache)
store_value = function(cache, key, value, ...) UseMethod("store_value", cache)
## todo vectorized generics

simple_rds_cache = function(root.dirpath) {
    if (!(
        is.character(root.dirpath) &&
        length(root.dirpath) == 1L &&
        !is.na(root.dirpath)
        ## todo sanitation checking...
    )) {
        stop ("root.dirpath must be a length-1 non-NA character vector.")
    }
    structure(
        root.dirpath,
        class="simple_rds_cache"
    )
}

`/.simple_rds_cache` = function(e1, e2) UseMethod("/.simple_rds_cache", e2)
`/.simple_rds_cache.character` = function(e1, e2) {
    if (!(
        is.character(e2) &&
        length(e2) == 1L &&
        !is.na(e2)
        ## todo sanitation checking...
    )) {
        stop ("e2 (suffix) must be a length-1 non-NA character vector.")
    }
    structure(
        file.path(unclass(e1), e2),
        class="simple_rds_cache"
    )
}

query_cache.simple_rds_cache = function(cache, key, ...) {
    ## todo checks
    filepath = paste0(file.path(unclass(cache), key), ".rds")
    result =
        if (file.exists(filepath)) {
            readRDS(filepath)
        } else {
            NULL
        }
    result
}

store_value.simple_rds_cache = function(cache, key, value, ...) {
    ## todo checks
    cache.dirpath = unclass(cache)
    if (!dir.exists(cache.dirpath)) {
        dir.create(cache.dirpath, recursive=TRUE, mode="0700")
    }
    filepath = paste0(file.path(cache.dirpath, key), ".rds")
    cnx = file(filepath, "w+b")
    Sys.chmod(filepath, "0600")
    saveRDS(value, cnx, ...)
    close(cnx)
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
        cached.response.info = query_cache(cache, cache.key)[[1L]]
        if (is.null(cached.response.info)) {
            if (verbose) {
                message("No response was previously cached; fetching...\n")
            }
            response.info = fetch_resource_uncached(fetch_current, response_looks_usable, now)
            store_value(cache, cache.key, list(response.info))
            return (response.info[["response"]])
        } else {
            ## todo allow/force fetching the timestamp without the result
            cache.staleness = difftime(now, cached.response.info[["fetch.time"]])
            if (cache.staleness >= cache.invalidation.period) {
                if (verbose) {
                    message("Cached value is too stale; re-fetching...\n")
                }
                response.info = fetch_resource_uncached(fetch_current, response_looks_usable, now)
                store_value(cache, cache.key, list(response.info))
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


null_cache = function() {
    structure(
        list(),
        class="null_cache"
    )
}

`/.null_cache` = function(e1, e2) UseMethod("/.null_cache", e2)
`/.null_cache.character` = function(e1, e2) {
    e1
}
query_cache.null_cache = function(cache, key, ...) {
    return (NULL)
}
store_value.null_cache = function(cache, key, value, ...) {
}

## todo lease-style cache? try to renew after some point, but only reject later
## xxx can save be incomplete and read to failing cache read attempt?
