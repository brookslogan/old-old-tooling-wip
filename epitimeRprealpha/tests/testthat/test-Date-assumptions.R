
test_that("Expected Date epoch is mapped to integer 0", {
    expect_identical(as.integer(as.Date("1970-01-01")), 0L)
})
