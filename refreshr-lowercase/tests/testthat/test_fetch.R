
with_tmpdir = function(fn) {
    cache.root.dirpath = tempfile()
    on.exit(unlink(cache.root.dirpath, recursive=TRUE))
    dir.create(cache.root.dirpath)
    fn(cache.root.dirpath)
}

test_fetch_42 = function(tmpdir, key, invalidation.secs, verbose) {
    fetch_resource(
        function() return (42L),
        is.integer,
        simple_rds_cache(tmpdir),
        key,
        as.difftime(invalidation.secs, units="secs"),
        verbose=verbose
    )
}

testthat::test_that("fetch_resource works as expected", {
    with_tmpdir(function(tmpdir) {
        testthat::expect_identical(test_fetch_42(tmpdir, "42a", Inf, FALSE), 42L)
        testthat::expect_silent(test_fetch_42(tmpdir, "42a", Inf, FALSE))
        testthat::expect_message(test_fetch_42(tmpdir, "42b", Inf, TRUE), "No response was previously cached; fetching...\n")
        testthat::expect_message(test_fetch_42(tmpdir, "42b", Inf, TRUE), "Cached value is fresh enough; returning.\n")
        testthat::expect_message(test_fetch_42(tmpdir, "42b", 0, TRUE), "Cached value is too stale; re-fetching...\n")
    })
})

