## Full refresh of all app data.

rm(list=ls())
library('magrittr')

competition_start_date <- as.Date("2022-01-25")

save_a_csv <- function(df, csv_name){

  if(!grepl("\\.csv$", csv_name)){
    csv_name <- paste0(csv_name, ".csv")
  }

  readr::write_csv(
    df,
    file.path(
      Sys.getenv("APP_BASE_PATH"),
      "duke_fintech_trading_competition_2022",
      csv_name,
      fsep = "\\"
    )
  )

}

read_a_csv <- function(csv_name){

  if(!grepl("\\.csv$", csv_name)){
    csv_name <- paste0(csv_name, ".csv")
  }

  suppressMessages(
    readr::read_csv(
      file.path(
        Sys.getenv("APP_BASE_PATH"),
        "duke_fintech_trading_competition_2022",
        csv_name,
        fsep = "\\"
      )
    )
  )

}

# ref_data #####################################################################
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
  dplyr::filter(.$Date >= competition_start_date) %>%
  dplyr::mutate(
    "SP500"    = round(.$SP500, 2),
    "SHY"      = round(.$SP500, 2),
    "BTC-USD"  = round(.$SP500, 2),
    "3_mo_apy" = (1+.$`3_mo`/2)^2 - 1
  ) %>%
  tibble::add_column("3_mo_td"  = .$`3_mo_apy`/252)

# country_recode_key ###########################################################
country_recode_key <- read_a_csv("country_recode_key")

# school_recode_key ############################################################
school_recode_key <- read_a_csv("school_recode_key")

recode <- function(df_col, key){
  vapply(
    df_col,
    function(x){
      unique(key$correct_name[key$form_name == x])
    },
    character(1)
  )
}

# valid_wufoo_registrants ######################################################
valid_wufoo_registrants <- read_a_csv("valid_wufoo_registrants") %>%
  dplyr::mutate(
    'school'  = recode(School, school_recode_key),
    'country' = recode(country, country_recode_key)
  ) %>%
  dplyr::select(-School)

# registry_flex_statement ######################################################
registry_flex_statement <- Sys.getenv("APP_BASE_PATH") %>%
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
registry_flex_statement$email[
  registry_flex_statement$name == "Chanel Zhu Ms"
] <- "chanelchu@utexas.edu"
registry_flex_statement$email[
  registry_flex_statement$name == "Sreebhavana Pasumarthi"
] <- "pasumars@email.sc.edu"

# Save the Registry flex statement data
readr::write_csv(
  registry_flex_statement,
  file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "registry_flex_statement.csv",
    fsep = "\\"
  )
)

# account_value_df #############################################################
account_value_df <- Sys.getenv("APP_BASE_PATH") %>%
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

      eod_history <- FlexStatement$EquitySummaryInBase %>%
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
        dplyr::filter(Date >= competition_start_date)

      if(all(eod_history$total == 1000000) | any(eod_history == 0)) return(NULL)

      tibble::tibble(
        "account_id"       = unique(eod_history$account_id),
        "email"            = registry_flex_statement$email[
          registry_flex_statement$account_id == account_id
        ],
        "tradername"       = valid_wufoo_registrants$tradername[
          valid_wufoo_registrants$email == email
        ],
        "portfolio_value"  = list(
          tibble::tibble(
            'Date'  = eod_history$Date,
            'value' = eod_history$total
          )
        )
      )

    }
  ) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::filter( # remove extra tradernames
    !(tradername %in% c("lnli", "UC_SteveLiu", "SS333", "SS3333"))
  )

# eod_account_value ############################################################
eod_account_value <- account_value_df$portfolio_value %>%
  purrr::reduce(dplyr::inner_join, by = "Date") %>%
  magrittr::set_colnames(c('Date', account_value_df$tradername))

# eod_returns ##################################################################
eod_returns <- suppressMessages(
  log(
    dplyr::select(eod_account_value, -Date)[-1,] / dplyr::select(
      eod_account_value, -Date
    )[-nrow(eod_account_value),]
  )
) %>%
  tibble::add_column(
    "Date"       = eod_account_value$Date[-1],
    .before      = TRUE,
    .name_repair = "minimal"
  )

# eod_excess_returns ###########################################################
eod_excess_returns <- eod_returns
rf <- ref_data$`3_mo_td`[match(eod_returns$Date, ref_data$Date)]/100
for(trader in colnames(eod_excess_returns)[-1]){
  eod_excess_returns[,trader] <- eod_excess_returns[,trader] - rf
}

# portfolio_return #############################################################
eod_ex_returns_plus_1      <- eod_excess_returns
eod_ex_returns_plus_1[,-1] <- eod_ex_returns_plus_1[,-1] + 1

cumulative_eod_excess_returns <- eod_ex_returns_plus_1$Date %>%
  lapply(
    function(eod_date){
      dplyr::filter(eod_ex_returns_plus_1, Date <= eod_date) %>%
        dplyr::select(-Date) %>%
        apply(
          MARGIN   = 2,
          FUN      = function(x){
            px <- prod(x)
            if(px < 0) return(mean(x))
            px^(1/length(x)) - 1
          }
        ) %>%
        tibble::as_tibble_row() %>%
        tibble::add_column(
          "Date"       = eod_date,
          .before      = TRUE,
          .name_repair = "minimal"
        )
    }
  ) %>%
  purrr::reduce(dplyr::bind_rows)

# cumulative_vol ###############################################################
cumulative_vol <- eod_returns$Date[-1] %>%
  lapply(
    function(eod_date){
      dplyr::filter(eod_returns, Date <= eod_date) %>%
        dplyr::select(-Date) %>%
        apply(
          MARGIN   = 2,
          FUN      = function(x){
            vol <- sd(x)
            if(vol == 0) return(NA)
            vol
          }
        ) %>%
        tibble::as_tibble_row() %>%
        tibble::add_column(
          "Date"       = eod_date,
          .before      = TRUE,
          .name_repair = "minimal"
        )
    }
  ) %>%
  purrr::reduce(dplyr::bind_rows) %>% {
    .[-1,]
  }

# sharpes ######################################################################
sharpes <- (cumulative_eod_excess_returns[-(1:2),-1]/cumulative_vol[,-1]) %>%
  tibble::add_column(
    "Date"       = cumulative_vol$Date,
    .before      = TRUE,
    .name_repair = "minimal"
  )

usethis::use_data(ref_data,                      overwrite = TRUE)
usethis::use_data(eod_account_value,             overwrite = TRUE)
usethis::use_data(eod_returns,                   overwrite = TRUE)
usethis::use_data(eod_excess_returns,            overwrite = TRUE)
usethis::use_data(cumulative_eod_excess_returns, overwrite = TRUE)
usethis::use_data(cumulative_vol,                overwrite = TRUE)
usethis::use_data(sharpes,                       overwrite = TRUE)




