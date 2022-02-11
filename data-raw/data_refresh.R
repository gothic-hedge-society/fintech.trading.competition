## code to prepare `registry` dataset goes here

library('magrittr')

# Load up the School Recode Key
school_recode_key <- suppressMessages(
  readr::read_csv(
    file.path(
      Sys.getenv("APP_BASE_PATH"),
      "duke_fintech_trading_competition_2022",
      "school_recode_key.csv",
      fsep = "\\"
    )
  )
)

# Load up the WuFoo Registry
wufoo_registry <- suppressMessages(
  readr::read_csv(
    file.path(
      Sys.getenv("APP_BASE_PATH"),
      "duke_fintech_trading_competition_2022",
      "registry.csv",
      fsep = "\\"
    )
  )
) %>%
  dplyr::mutate(
    'school' = School %>%
      vapply(
        function(school){
          unique(
            school_recode_key$correct_name[
              school_recode_key$form_name == school
            ]
          )
        },
        character(1)
      )
  ) %>%
  dplyr::select(-School)

# Load up the full, unfiltered Registry flex statement
flex_statement_full <- Sys.getenv("APP_BASE_PATH") %>%
  file.path("..", "Downloads", fsep = "\\") %>%
  list.files(pattern = "^Registry(.*).xml$", full.names = TRUE) %>%
  gsub("/", "\\\\", .) %>%
  stats::setNames(.,.) %>%
  vapply(
    function(flex_file_path){
      if(grepl("\\(\\d{1,}).xml", flex_file_path)){
        gsub("^(.*)\\(", "", flex_file_path) %>%
          gsub("\\)\\.xml$", "", .) %>%
          as.numeric()
      } else {
        0
      }
    },
    numeric(1)
  ) %>% {
    names(.[which.max(.)])
  } %>%
  xml2::read_xml() %>%
  xml2::xml_child("FlexStatements") %>%
  xml2::xml_children() %>%
  xml2::xml_children() %>%
  xml2::xml_attrs() %>%
  purrr::reduce(dplyr::bind_rows) %>%
  magrittr::set_colnames(c("account_id", "name", "email"))

# Chanel messed up her email. Need to use a 'key' that the students pick
#   instead of an email in the WuFoo form.
flex_statement_full$email[
  flex_statement_full$name == "Chanel Zhu Ms"
] <- "chanelchu@utexas.edu"

# Save the Registry flex statement data
readr::write_csv(
  flex_statement_full,
  file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "registry_flex_statement.csv",
    fsep = "\\"
  )
)

cutoff_date = as.Date("2022-01-25")

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
  dplyr::filter(.$Date >= cutoff_date) %>%
  dplyr::mutate(
    "SP500"    = round(.$SP500, 2),
    "SHY"      = round(.$SP500, 2),
    "BTC-USD"  = round(.$SP500, 2),
    "3_mo_apy" = (1+.$`3_mo`/2)^2 - 1
  )

usethis::use_data(ref_data, overwrite = TRUE)

rf <- (prod(ref_data$`3_mo_apy` + 1)^(1/nrow(ref_data)) - 1)/252

usethis::use_data(rf, overwrite = TRUE)

trader_stats <- Sys.getenv("APP_BASE_PATH") %>%
  file.path("..", "Downloads", fsep = "\\") %>%
  list.files(pattern = "^Mark-to-Market(.*).xml$", full.names = TRUE) %>%
  gsub("/", "\\\\", .) %>%
  stats::setNames(.,.) %>%
  vapply(
    function(flex_file_path){
      if(grepl("\\(\\d{1,}).xml", flex_file_path)){
        gsub("^(.*)\\(", "", flex_file_path) %>%
          gsub("\\)\\.xml$", "", .) %>%
          as.numeric()
      } else {
        0
      }
    },
    numeric(1)
  ) %>% {
    names(.[which.max(.)])
  } %>%
  xml2::read_xml() %>%
  xml2::xml_child("FlexStatements") %>%
  xml2::as_list() %>%
  lapply(
    function(FlexStatement){

      trade_record <- FlexStatement$EquitySummaryInBase %>%
        lapply(
          function(equity_summary){
            tibble::tibble(
              "account_id" = attr(equity_summary, "accountId"),
              "Date"       = as.Date(attr(equity_summary, "reportDate")),
              "total"      = as.numeric(attr(equity_summary, "total"))
            )
          }
        ) %>%
        purrr::reduce(dplyr::bind_rows) %>%
        dplyr::filter(.$Date >= cutoff_date)

      if(all(trade_record$total == 1000000) | any(trade_record == 0)){
        trade_record <- NULL
      } else {
        trade_record <- tibble::tibble(
          "account_id"       = unique(trade_record$account_id),
          "portfolio_value"  = list(
            tibble::tibble(
              'Date'  = trade_record$Date,
              'value' = trade_record$total
            )
          )
        )
      }

      trade_record

    }
  ) %>%
  purrr::reduce(dplyr::bind_rows)

# Save the trader_stats flex statement data
readr::write_csv(
  flex_statement_full,
  file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "trader_stats.csv",
    fsep = "\\"
  )
)


full_competition_data <- wufoo_registry %>%
  dplyr::left_join(flex_statement_full, by = "email") %>%
  dplyr::left_join(trader_stats, by = 'account_id')


# Save the full competition data
readr::write_csv(
  full_competition_data,
  file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "full_competition_data.csv",
    fsep = "\\"
  )
)

participating <- full_competition_data %>%
  dplyr::filter(
    vapply(
      full_competition_data$portfolio_value,
      function(pv){
        !is.null(pv)
      },
      FUN.VALUE = logical(1)
    )
  )

standings_as_of_date <- participating$portfolio_value %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::select(Date) %>%
  unique() %>%
  tibble::deframe() %>% {
    stats::setNames(as.numeric(.), as.character(.))
  } %>% {
    as.Date(names(.[which.max(.)]))
  }

for(i in 1:nrow(participating)){
  standings_as_of_date <- which(
    participating[i,'portfolio_value'][[1]][[1]]$Date <= standings_as_of_date
  ) %>%
    max() %>% {
      participating[i,'portfolio_value'][[1]][[1]]$Date[.]
    }
}

usethis::use_data(standings_as_of_date, overwrite = TRUE)

participating <- participating %>%
  dplyr::mutate(
    'portfolio_value' = portfolio_value %>%
      lapply(
        function(pv){
          dplyr::filter(pv, pv$Date <= standings_as_of_date)
        }
      ),
    "portfolio_return" = portfolio_value %>%
      lapply(
        function(pv){
          tibble::tibble(
            'Date'   = pv$Date[-1],
            'return' = log(pv$value[-1]/pv$value[-nrow(pv)])
          )
        }
      ),
    "gmrr" = portfolio_return %>%
      vapply(
        function(pr){
          prod(pr$return + 1)^(1/nrow(pr)) - 1
        },
        FUN.VALUE = numeric(1)
      ),
    "excess_gmrr" = gmrr - rf,
    "vol" = portfolio_return %>%
      vapply(
        function(pr){
          sd(unlist(pr$return))
        },
        FUN.VALUE = numeric(1)
      ),
    "Sharpe" = excess_gmrr/vol
  ) %>%
  dplyr::arrange(dplyr::desc(Sharpe))

if(any(duplicated(participating$email))){
  stop('duplicated email!!!')
}

standings <- participating %>%
  dplyr::select(
    tradername, school, country, portfolio_value, portfolio_return, gmrr,
    excess_gmrr, vol, Sharpe
  )

usethis::use_data(standings, overwrite = TRUE)

Sys.Date() %>%
  as.character() %>%
  gsub("-", "\\.", .) %>% {
    suppressMessages(desc::desc_set_version(.))
    invisible()
  }

