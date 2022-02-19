## Full refresh of all app data.

rm(list=ls())
library('magrittr')

competition_start_date <- as.Date("2022-01-25")

save_a_csv <- function(df, csv_name=deparse(substitute(df))){

  if(!grepl("\\.csv$", csv_name)){
    csv_name <- paste0(csv_name, ".csv")
  }

  readr::write_excel_csv(
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
  dplyr::transmute(
    "Date"        = Date,
    "SP500"       = round(.$SP500, 2),
    "SP500_rtn"   = c(NA, log(SP500[-1]/SP500[-length(SP500)])),
    "SHY"         = round(.$SP500, 2),
    "SHY_rtn"     = c(NA, log(SHY[-1]/SHY[-length(SHY)])),
    "BTC-USD"     = round(.$SP500, 2),
    "BTC-USD_rtn" = c(NA, log(`BTC-USD`[-1]/`BTC-USD`[-length(`BTC-USD`)])),
    "3_mo"        = `3_mo`,
    "3_mo_apy"    = (1+.$`3_mo`/2)^2 - 1,
    "3_mo_td"     = `3_mo_apy`/252,
  ) %>%
  dplyr::filter(.$Date >= competition_start_date)
save_a_csv(ref_data)

recode <- function(df_col, key){
  vapply(
    df_col,
    function(x){
      key$correct_name[key$form_name == x]
    },
    character(1)
  )
}

# cleaned_wufoo_valid_registrants ##############################################
cleaned_wufoo_valid_registrants <- read_a_csv("wufoo_valid_registrants") %>%
  dplyr::mutate(
    'school'  = recode(school,  read_a_csv("school_recode_key")),
    'country' = recode(country, read_a_csv("country_recode_key"))
  )
save_a_csv(cleaned_wufoo_valid_registrants)

# flex_statement_df ############################################################
flex_statement_df <- Sys.getenv("APP_BASE_PATH") %>%
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
  xml2::xml_children() %>%
  xml2::xml_children() %>%
  lapply(
    function(FlexStatement){
      tibble::tibble(
        "account_id" = FlexStatement %>%
          xml2::xml_child("EquitySummaryInBase") %>%
          xml2::xml_child("EquitySummaryByReportDateInBase") %>%
          xml2::xml_attr("accountId"),
        "name_ibkr" = FlexStatement %>%
          xml2::xml_child("AccountInformation") %>%
          xml2::xml_attr("name"),
        "email_ibkr" = FlexStatement %>%
          xml2::xml_child("AccountInformation") %>%
          xml2::xml_attr("primaryEmail") %>%
          tolower(),
        "portfolio_value" = list(
          FlexStatement %>%
            xml2::xml_child("EquitySummaryInBase") %>%
            xml2::xml_children() %>%
            xml2::xml_attrs() %>%
            purrr::reduce(dplyr::bind_rows) %>%
            dplyr::transmute(
              "Date"        = as.Date(reportDate),
              "total_long"  = as.numeric(totalLong),
              "total_short" = as.numeric(totalShort),
              "total"       = as.numeric(total)
            ) %>%
            dplyr::filter(Date >= competition_start_date)
        ),
        "status" = portfolio_value[[1]]$total %>% {
          if(all(. == 0)){
            "limbo"
          } else if (length(setdiff(., c(0, 1000000))) == 0){
            "inactive"
          } else {
            "active"
          }
        },
        "participating_S2022" = portfolio_value[[1]]$total %>%
          head(7) %>% {
            length(setdiff(., c(0, 1000000))) > 0
          }
      )
    }
  ) %>%
  purrr::reduce(dplyr::bind_rows)

# Chanel messed up her email. Need to use a 'key' that the students pick
#   instead of an email in the WuFoo form.
flex_statement_df$email_ibkr[
  flex_statement_df$name_ibkr == "Chanel Zhu Ms"
] <- "chanelchu@utexas.edu"
flex_statement_df$email_ibkr[
  flex_statement_df$name_ibkr == "Sreebhavana Pasumarthi"
] <- "pasumars@email.sc.edu"
# Save the Registry flex statement data
save_a_csv(flex_statement_df)

# account_value_df #############################################################
account_value_df <- flex_statement_df %>%
  dplyr::filter(status == "active") %>%
  dplyr::left_join(
    cleaned_wufoo_valid_registrants,
    by = c("email_ibkr" = "email")
  ) %>%
  dplyr::filter(!is.na(tradername)) %>%
  dplyr::filter( # remove extra tradernames
    !(
      tradername %in% c(
        "lnli", "UC_SteveLiu", "SS333", "SS3333", "Maxislife_7"
      )
    )
  )
save_a_csv(account_value_df)

# eod_account_value ############################################################
eod_account_value <- account_value_df$portfolio_value %>%
  lapply(
    function(x){x[,c("Date", "total")]}
  ) %>%
  purrr::reduce(dplyr::inner_join, by = "Date") %>% {
    colnames(.) <- c("Date", account_value_df$tradername)
    . <- tibble::as_tibble(., .name_repair = "minimal")
    Encoding(colnames(.)) <- "UTF-8"
    .[. == 0] <- 1000000
    .
  }
save_a_csv(eod_account_value)

# eod_returns ##################################################################
eod_returns <- log(
  as.matrix(eod_account_value[-1,-1]) /
    as.matrix(eod_account_value[-nrow(eod_account_value),-1])
) %>% {
  colnames(.) <- account_value_df$tradername
  . <- tibble::as_tibble(., .name_repair = "minimal")
  Encoding(colnames(.)) <- "UTF-8"
  .
} %>%
  tibble::add_column(
    "Date"       = eod_account_value$Date[-1],
    .before      = TRUE,
    .name_repair = "minimal"
  )
save_a_csv(eod_returns)

# eod_excess_returns ###########################################################
rf <- ref_data$`3_mo_td`[match(eod_returns$Date, ref_data$Date)]/100
eod_excess_returns <- eod_returns %>%
  dplyr::mutate(
    dplyr::across(!Date, function(x){x - rf})
  )
save_a_csv(eod_excess_returns)

# portfolio_return #############################################################
eod_ex_returns_plus_1 <- eod_excess_returns %>%
  dplyr::mutate(
    dplyr::across(!Date, function(x){x + 1})
  )
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
        tibble::as_tibble_row(.name_repair = "minimal") %>%
        tibble::add_column(
          "Date"       = eod_date,
          .before      = TRUE,
          .name_repair = "minimal"
        )
    }
  ) %>%
  purrr::reduce(dplyr::bind_rows) %>% {
    colnames(.) <- colnames(eod_ex_returns_plus_1)
    . <- tibble::as_tibble(., .name_repair = "minimal")
    Encoding(colnames(.)) <- "UTF-8"
    .
  }
save_a_csv(cumulative_eod_excess_returns)

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
  purrr::reduce(dplyr::bind_rows)
save_a_csv(cumulative_vol)

# sharpes ######################################################################
sharpes <- (
  as.matrix(cumulative_eod_excess_returns[-1,-1]) / as.matrix(
    cumulative_vol[,-1]
  )
) %>% {
  colnames(.) <- account_value_df$tradername
  . <- tibble::as_tibble(., .name_repair = "minimal") %>%
    tibble::add_column(
      "Date"       = cumulative_vol$Date,
      .before      = TRUE,
      .name_repair = "minimal"
    )
  Encoding(colnames(.)) <- "UTF-8"
  .
}
save_a_csv(sharpes)

# standings ####################################################################
standings <- tibble::tibble(
  "tradername" = colnames(sharpes)[-1],
  "Sharpe"     = as.numeric(sharpes[nrow(sharpes),-1])
) %>%
  dplyr::left_join(cleaned_wufoo_valid_registrants, by = "tradername") %>%
  dplyr::select(tradername, school, country, Sharpe) %>%
  dplyr::arrange(dplyr::desc(Sharpe)) %>%
  tibble::add_column(
    "Rank"       = 1:nrow(.),
    .before      = TRUE,
    .name_repair = "minimal"
  )
save_a_csv(standings)

# trader_cov ################################################################
# trader_cov <- eod_returns[9:nrow(eod_returns), "Date"] %>%
#   tibble::deframe() %>%
#   stats::setNames(.,.) %>%
#   lapply(
#     function(rtn_dt){
#       rtn_dt <<- rtn_dt
#       stop()
#       eod_returns %>%
#         dplyr::filter(Date <= rtn_dt) %>%
#         dplyr::select(-Date) %>%
#         cov() %>%
#         as.dist() %>%
#         cmdscale() %>%
#
#     }
#   )

# last_updated #################################################################
last_updated <- Sys.getenv("APP_BASE_PATH") %>%
  file.path("duke_fintech_trading_competition_2022", fsep = "\\") %>%
  list.files(full.names = TRUE) %>%
  file.info() %>% {
    rownames(.) <- basename(rownames(.)) %>%
      gsub("\\.csv$", "", .)
    .
  } %>%
  tibble::as_tibble(rownames = "file") %>%
  dplyr::select(file, mtime) %>% {
    stats::setNames(.$mtime, .$file)
  } %>%
  as.list()

# save R data ##################################################################
usethis::use_data(ref_data,                           overwrite = TRUE)
usethis::use_data(eod_account_value,                  overwrite = TRUE)
usethis::use_data(eod_returns,                        overwrite = TRUE)
usethis::use_data(eod_excess_returns,                 overwrite = TRUE)
usethis::use_data(cumulative_eod_excess_returns,      overwrite = TRUE)
usethis::use_data(cumulative_vol,                     overwrite = TRUE)
usethis::use_data(sharpes,                            overwrite = TRUE)
usethis::use_data(standings,                          overwrite = TRUE)
usethis::use_data(trader_correl,                      overwrite = TRUE)
usethis::use_data(last_updated,                       overwrite = TRUE)
