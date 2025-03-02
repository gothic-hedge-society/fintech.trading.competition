#' Updates all base data that the app needs to generate the site.
#'
#' @export
#'
refresh_base_data <- function(){

  start_date <- as.Date("2025-02-07")
  rf         <- 0.0433/252

  secrets_path <- file.path(rprojroot::find_package_root_file(), 'secrets')

  NAV_and_CASH <- fintech.trading.competition::fetch_flex_query(1142446)
  save(NAV_and_CASH, file = file.path(secrets_path, 'NAV_and_CASH.xml'))

  library(quantmod)

  # Set the date range (6 months back from today)
  end_date <- Sys.Date()
  start_date <- end_date - 180

  # Fetch S&P 500 data using the ticker ^GSPC
  getSymbols(
    "^GSPC",
    src = "yahoo",
    from = start_date,
    to = end_date
  )

  # Extract closing prices
  sp500_closing <- Cl(GSPC)  # Cl() extracts closing prices

  # Create a simple data frame with dates and closing prices
  sp500 <- data.frame(
    Date = index(sp500_closing),
    Closing_Price = as.numeric(sp500_closing)
  )
  usethis::use_data(sp500)



  # Participants ---------------------------------------------------------------
  participants <- readr::read_csv(
    file.path(
      secrets_path, "duke-fintech-trading-competition-2025_entries.csv"
    ),
    show_col_types = FALSE
  )


  # Discord Members ------------------------------------------------------------
  discord_members <- readr::read_csv(
    file.path(secrets_path, "discord_member_dump.txt"),
    col_names = "discord_name",
    show_col_types = FALSE
  ) %>%
    tidyr::separate("discord_name", c("discord_name", "id"), sep = " ")


  # participating_student_reports ----------------------------------------------
  participating_student_reports <- readLines(
    file.path(secrets_path, )
  ) %>%
    xml2::read_xml() %>%
    xml2::xml_child("FlexStatements") %>%
    xml2::xml_children() %>%
    lapply(
      function(xml_statement){
        dplyr::inner_join(
          xml_statement %>%
            xml2::xml_child("AccountInformation ") %>%
            xml2::xml_attrs() %>%
            tibble::as_tibble_row(),
          xml_statement %>%
            xml2::xml_child("EquitySummaryInBase") %>%
            xml2::xml_children() %>%
            lapply(
              function(EquitySummaryByReportDateInBase){
                xml2::xml_attrs(EquitySummaryByReportDateInBase) %>%
                  tibble::as_tibble_row()  %>%
                  dplyr::select(accountId, reportDate, total)
              }
            ) %>%
            purrr::reduce(dplyr::bind_rows),
          by = "accountId"
        )
      }
    ) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    remove_empty_cols() %>%
    dplyr::select(accountId, primaryEmail, reportDate, total) %>%
    dplyr::mutate(
      "reportDate" = as.Date(reportDate, format="%Y%m%d"),
      "total"      = as.numeric(total)
    ) %>%
    dplyr::filter(
      reportDate >= start_date &
        accountId != "DIE581998"
    ) %>%
    dplyr::nest_by(accountId, primaryEmail, .key = "statement") %>%
    dplyr::mutate(
      "statement" = statement %>% {
        .$total[.$total == 0] <- 1000000
        tibble::column_to_rownames(., "reportDate") %>%
          xts::as.xts()
      } %>%
        list(),
      "daily_returns" = statement %>% {
        daily_returns <- .
        colnames(daily_returns) <- "daily_returns"
        daily_rtns(daily_returns)
      },
      "cumulative_returns" = daily_returns %>% {
        cumulative_returns <- .
        colnames(cumulative_returns) <- "cumulative_returns"
        for(i in 2:nrow(cumulative_returns)){
          cumulative_returns[i,1] <- gmrr(cumulative_returns[1:i,1])
        }
        list(cumulative_returns)
      },
      "cumulative_vol" = daily_returns %>% {
        cumulative_vol <- .
        colnames(cumulative_vol) <- "cumulative_vol"
        for(i in 2:nrow(cumulative_vol)){
          cumulative_vol[i,1] <- sd(cumulative_vol[1:i,1])
        }
        list(cumulative_vol)
      },
      "cumulative_sharpe" = list(
        "rtn" = cumulative_returns,
        "vol" = cumulative_vol
      ) %>% {
        cumulative_sharpe <- (.$rtn - rf) / .$vol
        colnames(cumulative_sharpe) <- "cumulative_sharpe"
        list(cumulative_sharpe)
      },
      "ex_gmrr" = cumulative_returns %>% {
        as.numeric(.$cumulative_returns[nrow(.)]) - rf
      },
      "sharpe" = cumulative_sharpe %>% {
        as.numeric(.$cumulative_sharpe[nrow(.)])
      }
    ) %>%
    dplyr::inner_join(
      participants[, c("email", "trader_name")],
      by           = dplyr::join_by(primaryEmail == email),
      relationship = "many-to-many"
    ) %>%
    dplyr::arrange(dplyr::desc(ex_gmrr)) %>% {

      .$tier <- head(
        lapply(
          1:ceiling(nrow(.) / 10),
          FUN = function(x){
            rep(x, 10)
          }
        ) %>%
          unlist(),
        nrow(.)
      )
      .
    } %>%
    dplyr::ungroup() %>%
    dplyr::group_by(tier) %>%
    dplyr::arrange(dplyr::desc(sharpe)) %>% {
      .$tier_rank <- head(
        lapply(
          1:ceiling(nrow(.) / 10),
          FUN = function(x){
            1:10
          }
        ) %>%
          unlist(),
        nrow(.)
      )
      .
    } %>%
    dplyr::ungroup() %>% {
      .$rank <- 1:nrow(.)
      .
    } %>%
    dplyr::select(
      rank, tier, tier_rank, accountId, trader_name, ex_gmrr, sharpe,
      dplyr::everything()
    )

  # In Discord but not Wufoo ---------------------------------------------------
  usethis::ui_info("Users in discord but not in Wufoo")
  print(
    sort(
      setdiff(
        discord_members$discord_name,
        tolower(c(participants$discord_name, "Carl-bot", "jakevestal"))
      )
    )
  )

  # In Wufoo but not Discord ---------------------------------------------------
  usethis::ui_info("Users in Wufoo but not in Discord")
  print(
    paste0(
      participants$email[
        tolower(participants$discord_name) %in% setdiff(
          tolower(c(participants$discord_name, "Carl-bot", "jakevestal")),
          discord_members$discord_name
        )
      ],
      collapse = "; "
    )
  )


  sharpes <- participating_student_reports %>%
    dplyr::filter(tier == 1) %>%
    dplyr::select(cumulative_sharpe) %>%
    unlist(recursive = FALSE) %>%
    purrr::reduce(zoo::merge.zoo) %>%
    stats::setNames(
      participating_student_reports %>%
        dplyr::filter(tier == 1) %>%
        dplyr::select(trader_name) %>%
        unlist()
    )

  returns <- participating_student_reports %>%
    dplyr::filter(tier == 1) %>%
    dplyr::select(cumulative_returns) %>%
    unlist(recursive = FALSE) %>%
    purrr::reduce(zoo::merge.zoo) %>%
    stats::setNames(
      participating_student_reports %>%
        dplyr::filter(tier == 1) %>%
        dplyr::select(trader_name) %>%
        unlist()
    )

  sharpe_ordered_info <- participants[
    match(colnames(sharpes), participants$trader_name),
    c(
      "trader_name",
      "university",
      "website"
    )
  ] %>% {
    .[is.na(.)] <- ""
    .
  } %>%
    dplyr::select(trader_name, university, website) %>%
    dplyr::mutate(
      "trader_name" = gsub("#(.*)$", "", trader_name)
    )

  file.copy(
    rprojroot::find_package_root_file() %>%
      file.path("data", "sharpe_ordered_info.rda"),
    rprojroot::find_package_root_file() %>%
      dirname() %>%
      file.path(
        "fintech.trading.competition", "data", "sharpe_ordered_info.rda"
      ),
    overwrite = TRUE
  )

}

