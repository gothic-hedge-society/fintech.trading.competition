#' Add an instructor to the `instructors` data object
#'
#' Takes in character vectors containing instructors' names and emails and
#' identifies them as instructors within the package, meaning that performance
#' metrics won't be calculated for these accounts.
#'
#' @param name Character vector. Instructors' name as it appears in
#'   the instructor's IBKR paper trader account.
#' @param email Character vector. Email as it appears in the the instructors'
#'   IBKR paper trader account.
#'
#' @export
#'
add_instructor <- function(names, email){
  instructors <- tibble::tibble(name = names, email = emails) %>%
    dplyr::bind_rows(instructors) %>%
    unique()

  usethis::use_data(instructors, overwrite = TRUE)
}
