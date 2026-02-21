#' import_dm
#'
#' @returns DM dataset selected for specific variables
#'
#' @export
#' @examples \dontrun{
#' import_dm}
import_dm <- function() {

  df <- pharmaversesdtm::dm |> 
    dplyr::select(
      USUBJID, AGE, AGEU, SEX, RACE, ARM
    )


}