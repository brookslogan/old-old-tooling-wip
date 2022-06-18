
test_that("mod negative by positive is nonnegative", {
    expect_identical(-2L %% 7L, 5L)
})
