




## is appending to paths generalizable to all types of drivers? would behavior
## be consistent?
## - file.path & creating-`[[` style (use generic on `/`?)
## - paste0 style
## - what about databases & tables? could still use this format with either blob fetches or maintain fetch metadata tables (but key might need to be blob type, is that valid or quick to search on?)...; is more knowledge of fetching mechanism needed to improve interplay? does there need to be a separation between record-level storage and record-set-level fetching?
## - different name requirements, forms of name sanitation...
## - pkg name: refreshR?
## - fetching history --- any issues saving whole results, but returning only subsets, possibly reformatted?
## - chained caches... what happens when need to fetch? guess could be driver-dependent & have driver-dependent guarantees not only for what will or will not happen when a total miss appears to occur but also whether the subsequent caches will necessarily be consulted on earlier misses at all.  Both factor into when fetches will occur.


## xxx vs "no_next_cache" / "fetch_driver" w/ next.cache renamed on.miss
## xxx vs. singleton
cache_miss_driver = function() {
    structure(
        list(),
        class = "cache_miss_driver"
    )
}

## xxx "cache_driver" vs. "refreshr_cache"? "refreshR" vs. "refreshr"
## fixme check for name conflicts with, e.g., storr
rds_cache_driver = function(cache.root.dirpath, next.cache=cache_miss_driver()) {
    structure(
        list(
            dirpath = cache.root.dirpath,
            next.cache = next.cache
        ),
        class="rds_cache_driver"
    )
}

`/.rds_cache_driver` = function(e1, e2) UseMethod("/.rds_cache_driver", e2)

## xxx perhaps not that pipe-friendly...
`/.rds_cache_driver.character` = function(e1, e2) {
    e1[["dirpath"]] = file.path(e1[["dirpath"]], e2)
}

## xxx have to read the entire cached response itself to check the cache time...; maintain two files instead?
## --- could later allow query to return boolean or timestamp or listish; boolean would say whether is in cache; timestamp the latest time for the key; listish would return cache result
## --- alternatively, could use capability-checking method? or have two versions of fetch and use S3 to delegate to one or the other?

query_cache = function(cache.driver, key, ...) UseMethod("query_cache", cache.driver)

query_cache.rds_cache_driver = function(cache.driver, key, ...) {
    cache.filepath = file.path(unclass(cache.driver)[["dirpath"]], paste0(key,".rds"))
    if (!file.exists(cache.filepath)) {
        return (query_cache(unclass(cache.driver)[["next.cache"]], key, ...)) # xxx dots aren't a very specific way of forwarding for this type of recursion... but can still be convenient in some cases (verbose toggles?)
        ## xxx should recursion be done within each cache driver or by the fetch method? (list of drivers?) or make new driver that is a chain of drivers? ("fallback")
    } else {
        return (readRDS(cache.filepath))
    }
}

cache_value = function(cache.driver, key, ...) UseMethod("cache_value", cache.driver)

cache_value.rds_cache_driver = function(cache.driver, key, ...) {
    cache.filepath = file.path(unclass(cache.driver)[["dirpath"]], paste0(key,".rds"))
    if (!file.exists(cache.filepath)) {
        return (NULL)
    } else {
        return (readRDS(cache.filepath))
    }
}


query_cache = function(cache, keys, ...) UseMethod("query_cache", cache)
store_value = function(cache, keys, values, ...) UseMethod("store_value", cache)

query_cache.simple_rds_cache = function(cache, keys, ...) {
    ## todo check keys
    filepaths = glue::glue(file.path(unclass(cache), keys), ".rds")
    results = lapply(filepaths, function(filepath) {
        if (file.exists(filepath)) {
            readRDS(filepath)
        } else {
            NULL
        }
    })
    results
}

store_value.simple_rds_cache = function(cache, keys, values, ...) {
    ## todo checks
    cache.dirpath = unclass(cache)
    if (!dir.exists(cache.dirpath)) {
        dir.create(cache.dirpath, recursive=TRUE, mode="0700")
    }
    filepaths = glue::glue(file.path(cache.dirpath, keys), ".rds")
    for (keyval.i in seq_along(keys)) {
        filepath = filepaths[[keyval.i]]
        cnx = file(filepath, "w+b")
        Sys.chmod(filepath, "0600")
        saveRDS(values[[keyval.i]], cnx, ...)
        close(cnx)
    }
}


## xxx what functions are needed? checking if something is stored? checking if stored + getting simultaneously? getting if stored assuming is there? storing?

## xxx does concept of no-store-calculate-on-demand apply in this framework?  perhaps one way of comparing performance impact except optimistic about impact since adding overhead... but main question is about interface... recalc-on-demand and not storing seems like it requires the help of a 1-D / multi-D lazy list concept... array_proxy...

## xxx array_proxy handling cache batching & generation minibatching?  what if keyset changes?  also, overhead worries...

## xxx smart memory usage parallelism for map_join? only passing symbols needed & subsections of data worked on that are relevant? minibatching? smart minibatching based on memory sections needed?

## xxx record issue history? check that something that is static stays static?

## should rds/db/fallback/etc. drivers handle path storage and construction or another class built on top?

## xxx should timestamp be optional? what should be managing the timestamps?

## todo make vectorized version an S3 with default to looping on single version
## - or make a scalarized version an S3 with default to extracting from vectorized version
## - make ready_cache something to call on a cache/subcache before calling single or vectorized store?
