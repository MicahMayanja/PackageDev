test_that("import_dm returns expected output", {
  result <- import_dm()

  # Returns a data frame
  expect_s3_class(result, "data.frame")

  # Has exactly the expected columns
  expected_cols <- c("USUBJID", "AGE", "AGEU", "SEX", "RACE", "ARM")
  expect_named(result, expected_cols, ignore.order = FALSE)

  # Has rows
  expect_gt(nrow(result), 0)

  # Column types are correct
  expect_type(result$USUBJID, "character")
  expect_type(result$AGE, "double")
  expect_type(result$AGEU, "character")
  expect_type(result$SEX, "character")
  expect_type(result$RACE, "character")
  expect_type(result$ARM, "character")

  # No extra columns
  expect_equal(ncol(result), length(expected_cols))
})
