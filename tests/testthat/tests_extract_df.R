context("extract_df")

test_that("extract_df returns the correct output", {

  expect_equivalent(class(extract_df("demography_5")), "data.frame")

  expect_equivalent(class(extract_df(c("demography_5", "population_1"))),
                    "list")

  expect_error(extract_df(c("demhy_5", "population_1")), regexp = NULL)

})
