devtools::load_all(".")

load(
  file = rprojroot::find_package_root_file() %>%
    dirname() %>%
    file.path(
      "fintech.trading.competition.private", "data",
      "participating_student_reports.rda"
    )
)

tier_1_sharpes <- participating_student_reports %>%
  dplyr::filter(tier == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(trader_name, cumulative_sharpe) %>% {

  }

tier_1_sharpes$cumulative_sharpe %>%
  purrr::reduce(zoo::merge.zoo)

usethis::use_data(refresh_data_from_private.R, overwrite = TRUE)
