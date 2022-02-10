## code to prepare `ref_data` dataset goes here

library('magrittr')

ref_data <- Sys.getenv("APP_BASE_PATH") %>%
  file.path(
    ., "duke_fintech_trading_competition_2022",  "benchmark.csv",
    fsep = "\\"
  ) %>%
  readr::read_csv(col_types = 'Dnn') %>% {
    dplyr::inner_join(
      .,
      Sys.getenv("APP_BASE_PATH") %>%
        file.path(
          ., "duke_fintech_trading_competition_2022",  "usdt_3mo_cmt.csv",
          fsep = "\\"
        ) %>%
        readr::read_csv(col_types = 'Dn'),
      by = "Date"
    )
  } %>%
  dplyr::mutate(
    "SP500"   = round(.$SP500, 2),
    "SHY"     = round(.$SP500, 2),
    "BTC-USD" = round(.$SP500, 2)
  )


usethis::use_data(ref_data, overwrite = TRUE)
