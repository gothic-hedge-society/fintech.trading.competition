## code to prepare `registry` dataset goes here

library('magrittr')

registry <- readr::read_csv(
  file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "invited.csv",
    fsep = "\\"
  )
) %>%
  dplyr::inner_join(
    readr::read_csv(
      file.path(
        Sys.getenv("APP_BASE_PATH"),
        "duke_fintech_trading_competition_2022",
        "wufoo_registrants.csv",
        fsep = "\\"
      )
    ),
    by=c('invited' = 'email')
  ) %>%
  dplyr::select(tradername) %>%
  unique()

usethis::use_data(registry, overwrite = TRUE)
