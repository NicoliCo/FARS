testthat::test_that("fars_summarize_years", {
    farsSummary <- FARS::fars_summarize_years(c(2013,2014,2015))
    testthat::expect_that(farsSummary, is_a("tbl_df"))
    testthat::expect_equal(as.numeric(farsSummary[6,2]), 2692)
    testthat::expect_equal(as.numeric(farsSummary[11,3]), 2714)
    testthat::expect_equal(as.numeric(farsSummary[3,4]), 2385)
})
