test_that("toOrdinalDate2 returns correctly formatted ordinal dates", {
    # Input dates
    dates <- c("2022-02-01", "2023-05-15", "2024-12-31")

    # Expected output
    expected_output <- c("Feb 1st, 2022", "May 15th, 2023", "Dec 31st, 2024")

    # Perform the function call
    result <- toOrdinalDate2(dates)

    # Compare the result with the expected output
    expect_equal(result, expected_output)
})
