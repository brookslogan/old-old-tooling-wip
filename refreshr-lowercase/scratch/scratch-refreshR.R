
devtools::load_all("..")

## xxx common code for making path?

test.driver = rds_cache_driver("/tmp/test-refreshR-cache")

test.driver/"asdf" # perhaps not that pipe-friendly...


## xxx what functions are needed? checking if something is stored? checking if stored + getting simultaneously? getting if stored assuming is there? storing?

## xxx does concept of no-store-calculate-on-demand apply in this framework?  perhaps one way of comparing performance impact except optimistic about impact since adding overhead... but main question is about interface... recalc-on-demand and not storing seems like it requires the help of a 1-D / multi-D lazy list concept... array_proxy...

## add = function(x,y) UseMethod("add", x)
## add.a = function(x,y) UseMethod("add.a", y)
## add.a.b = function(x,y) x+y
## add.c = function(x,y) UseMethod("add.c", y)
## add.c.d = function(x,y) x+y

## methods("add")
## methods("add.a")

## methods(,"a")
## methods(,"b")
## methods(,"a.b")
## methods(,"a....b")

## todo vectorize fetch?

## todo timestamps without results in fetch... should caches handle, or should fetch handle?

## todo when fetching, recheck cached results for usability? check for timestamp?

## fixme testthat is MIT but depends on rlang... tests must be under GPL-3+?

## todo cache cleaning functions

## todo rename caches to cache.drivers / ...? so searching "cache" does not also turn up cache.related.things

## xxx streaming-aware structure?

## xxx classes to represent something that could potentially be "refreshed" according to staleness/lease rules, with special function for forcing update? (but forcing update how deeply, if caching is used within? what about resources that are supposed to not update but are using the same methods with infinite staleness allowed?)
