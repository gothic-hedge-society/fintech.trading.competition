library('magrittr')

new_data <- c("%5EGSPC", "SHY", "BTC-USD") %>% # Put Yahoo ticker symbols here
  lapply(
    function(symb){
      paste0(
        "https://query1.finance.yahoo.com/v7/finance/download/", symb,
        "?period1=", as.numeric(as.POSIXct(Sys.Date() - 365)),
        "&period2=", as.numeric(as.POSIXct(Sys.Date())),
        "&interval=1d&events=history&includeAdjustedClose=true"
      ) %>%
        readr::read_csv(col_types = "Dnnnnnn") %>%
        dplyr::select(Date, `Adj Close`)
    }
  ) %>%
  purrr::reduce(dplyr::inner_join, by = "Date") %>%
  magrittr::set_colnames(
    c("Date", "SP500", "SHY", "BTC-USD") # Put desired names here
  )

benchmark_path <- file.path(
  Sys.getenv("APP_BASE_PATH"),
  paste0("duke_fintech_trading_competition_", "2022"),
  "benchmark.csv",
  fsep = "\\"
)

if (file.exists(benchmark_path)){
  old_data <- readr::read_csv(benchmark_path)
  benchmark_data <- dplyr::bind_rows(new_data, old_data) %>%
    dplyr::arrange(Date) %>%
    unique()
} else {
  benchmark_data <- new_data
}

readr::write_csv(new_data, benchmark_path)
