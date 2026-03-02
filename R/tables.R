#' t14.1.1
#'
#' @returns Summary table of demographics
#' @import tern
#' @import rtables
#' @import dplyr
#' @export
#'
#' @examples \dontrun{
#' t14.1.1()}
t14.1.1 <- function() {

  dm <- import_dm() %>%
    dplyr::mutate(
      RACE = factor(RACE, levels = c("WHITE", "AMERICAN INDIAN OR ALASKA NATIVE",
                                     "BLACK OR AFRICAN AMERICAN", "ASIAN" )),
      SEX = dplyr::case_when(
        SEX == "F" ~ "Female",
        SEX == "M" ~ "Male"
      ) %>% factor(., levels = c("Male", "Female"))
    ) %>%
    dplyr::filter(ARM != "Screen Failure")

  vars <- c("AGE", "RACE", "SEX")

  tbl <- rtables::basic_table(show_colcounts = TRUE) %>%
    rtables::split_cols_by("ARM") %>%
    tern::analyze_vars(
      vars = vars
    ) %>%
    rtables::build_table(dm)

  tbl
}
