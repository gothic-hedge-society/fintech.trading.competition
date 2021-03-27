#' Update the student.statements.rda and benchmark.rda package data objects
#'
#' Imports the results of a Flex Query run within IBKR and downloaded as an xml
#' file, fetches the relevant data, and saves a \link[tibble]{tibble} containing
#' the student statement information needed to calculate standings and
#' performance metrics as the package data object *student_reports.rda*. Same
#' for the benchmark.
#'
#' @param filename Name of the xml file containing student statement data.
#'
#' Refreshes benchmark.rda, gspc.rda, performance_reports.rda, rfr.rda, and
#' student_reports.rda. Stores as package data objects.
#'
#' @export
#'
refresh_all_data <- function(){

  devtools::load_all(".")

  start_date <- as.Date("2021-03-11")

  # Participants ---------------------------------------------------------------
  participants <- rprojroot::find_package_root_file() %>%
    file.path(., "inst", "participants", "joined_names_n_emails.xlsx") %>%
    readxl::read_xlsx() %>%
    dplyr::filter(!is.na(ID)) %>%
    dplyr::select(accountId, ID, name, trader_name, primaryEmail) %>%
    dplyr::left_join(
      rprojroot::find_package_root_file() %>%
        file.path(., "inst", "participants", "participants.xlsx") %>%
        readxl::read_xlsx() %>%
        dplyr::filter(!is.na(trader_name)) %>%
        dplyr::select(ID, School),
      by = "ID"
    )

  # Flex_query -----------------------------------------------------------------
  flex_query <- rprojroot::find_package_root_file() %>%
    file.path(
      ., "inst", "ibkr_trade_statements", "FINTECH_Trading_Competition.xml"
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
    dplyr::nest_by(accountId, name, primaryEmail, .key = "statement") %>%
    dplyr::filter(accountId %in% participants$accountId) %>%
    dplyr::mutate(
      statement = list(
        xts::xts(
          statement[,setdiff(colnames(statement), "reportDate")],
          order.by = as.Date(statement$reportDate)
        )[paste0(start_date, "/")]
      )
    )

  statement_dates <- as.Date(zoo::index(flex_query[1,]$statement[[1]]))

  most_recent_common_date <- flex_query$statement %>%
    vapply(
      function(stmt){
        as.character(
          zoo::index(stmt[max(which(as.numeric(stmt$total) > 0)),])
        )
      },
      character(1)
    ) %>%
    unique() %>%
    as.Date() %>%
    sort() %>% {
      as.Date(.[1])
    }

  statement_dates <- statement_dates[
    which(statement_dates <= most_recent_common_date)
  ]

  # ^GSPC ----------------------------------------------------------------------
  gspc <- paste0(
    "https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=",
    as.numeric(as.POSIXct(statement_dates[1])),
    "&period2=",
    as.numeric(as.POSIXct(Sys.Date())),
    "&interval=1d&events=history&includeAdjustedClose=true"
  ) %>%
    readr::read_csv(col_types = "Dnnnnnn") %>%
    as.data.frame() %>%
    tibble::column_to_rownames("Date") %>%
    xts::as.xts() %>% {
      .[statement_dates]
    }

  # Benchmark ------------------------------------------------------------------
  benchmark <- gspc[statement_dates]$`Adj Close` %>%
    daily_rtns() %>% {
      . <- .[[1]]
      colnames(.) <- "SP500 Daily Rtn"
      .
    }

  # year-to-date rfr
  rfr <- paste0(
    "https://www.treasury.gov/resource-center/data-chart-center/",
    "interest-rates/pages/TextView.aspx?data=yieldYear&year=",
    format(Sys.Date(), format = "%Y")
  ) %>%
    get_usdt_data() %>% {
      .[statement_dates, "3_mo"]
    } %>% {
      xts::xts(zoo::coredata(.) / 252, order.by = zoo::index(.))
    } %>% {
      .[statement_dates]
    }

  participating_student_reports <- flex_query %>%
    dplyr::mutate(
      "total"            = list(statement$total[as.character(statement_dates)]),
      "daily_returns"    = statement$total[as.character(statement_dates)] %>%
        daily_rtns(),
      "rfr"              = list(rfr),
      "daily_excess_rtn" = list(
        xts::xts(
          zoo::coredata(daily_returns) - zoo::coredata(rfr[[1]]),
          order.by = zoo::index(daily_returns)
        )
      ),
      "excess_gmrr"      = gmrr(daily_excess_rtn),
      "daily_vol"        = sd(daily_returns),
      "Sharpe"           = excess_gmrr / daily_vol
    ) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(
      dplyr::select(participants, "accountId", "trader_name", "School")
    ) %>%
    dplyr::select(
      "accountId",     "trader_name", "School",           "total",
      "daily_returns", "rfr",         "daily_excess_rtn", "excess_gmrr",
      "daily_vol",     "Sharpe"
    ) %>%
    dplyr::filter(!is.infinite(Sharpe) && !is.na(Sharpe)) %>%
    dplyr::arrange(dplyr::desc(Sharpe))

  usethis::use_data(gspc,                          overwrite = TRUE)
  usethis::use_data(participants,                  overwrite = TRUE)
  usethis::use_data(participating_student_reports, overwrite = TRUE)
  usethis::use_data(benchmark,                     overwrite = TRUE)
  usethis::use_data(rfr,                           overwrite = TRUE)

  devtools::load_all(".")

}

