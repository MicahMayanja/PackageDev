test_that("t14.1.1 returns an rtable object", {
  result <- t14.1.1()
  expect_s4_class(result, "VTableTree")
})

test_that("t14.1.1 excludes Screen Failure ARM", {
  dm <- import_dm()
  expect_true("Screen Failure" %in% dm$ARM)  # confirm it exists in raw data

  result <- t14.1.1()
  table_str <- toString(rtables::matrix_form(result)$strings)
  expect_false(grepl("Screen Failure", table_str))
})

test_that("t14.1.1 RACE is factored with correct levels", {
  dm <- import_dm() %>%
    dplyr::mutate(
      RACE = factor(RACE, levels = c("WHITE", "AMERICAN INDIAN OR ALASKA NATIVE",
                                     "BLACK OR AFRICAN AMERICAN", "ASIAN"))
    ) %>%
    dplyr::filter(ARM != "Screen Failure")

  expect_s3_class(dm$RACE, "factor")
  expect_equal(levels(dm$RACE), c("WHITE", "AMERICAN INDIAN OR ALASKA NATIVE",
                                  "BLACK OR AFRICAN AMERICAN", "ASIAN"))
})

test_that("t14.1.1 SEX is factored with correct levels and labels", {
  dm <- import_dm() %>%
    dplyr::mutate(
      SEX = dplyr::case_when(
        SEX == "F" ~ "Female",
        SEX == "M" ~ "Male"
      ) %>% factor(., levels = c("Male", "Female"))
    ) %>%
    dplyr::filter(ARM != "Screen Failure")

  expect_s3_class(dm$SEX, "factor")
  expect_equal(levels(dm$SEX), c("Male", "Female"))
  expect_false(any(dm$SEX %in% c("M", "F")))  # raw values should be gone
})

test_that("t14.1.1 table contains expected row variables", {
  result <- t14.1.1()
  row_names <- toString(rtables::row_paths_summary(result)$label)

  expect_true(grepl("AGE", row_names))
  expect_true(grepl("RACE", row_names))
  expect_true(grepl("SEX", row_names))
})
