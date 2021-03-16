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

  # Participants ---------------------------------------------------------------
  participants <- rprojroot::find_package_root_file() %>%
    file.path(., "inst", "participants", "participants.xlsx") %>%
    readxl::read_xlsx()

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
    dplyr::nest_by(accountId, name, primaryEmail, .key = "statement")

  #
  #
  #   %>%
  #     dplyr::filter(
  #       primaryEmail %in% participants$Email[
  #         which(participants$Role == "participant")
  #       ]
  #     )  %>%
  #     dplyr::mutate(
  #       "statement" = list(
  #         xts::xts(
  #           as.data.frame(statement),
  #           order.by = as.Date(statement$reportDate)
  #         )[paste0(start_date, "/")]
  #       )
  #     ) %>% {
  #       check_vec <- setdiff(
  #         list.dirs(
  #           file.path(
  #             "C:", "Users", "Jake", "Desktop", "fintech.trading.competition",
  #             "inst", "performance_reports", semester
  #           ),
  #           recursive = FALSE
  #         ) %>%
  #           basename(),
  #         .$name
  #       ) %>%
  #         setdiff(instructors)
  #
  #       if(isTRUE(length(check_vec) > 0)){
  #         usethis::ui_oops(
  #           paste0(
  #             "The following students are NOT included in the IBKR statement:\n",
  #             paste(check_vec, collapse = ", ")
  #           )
  #         )
  #         stop("Missing Students")
  #       }
  #
  #       .
  #
  #     } %>%
  #     dplyr::mutate("participating" = length(unique(statement$total)) > 1)

  statement_dates <- as.Date(zoo::index(student_reports[1,]$statement[[1]]))

  # ^GSPC ----------------------------------------------------------------------
  if(length(setdiff(statement_dates, as.Date(zoo::index(gspc)))) > 0){

    usethis::ui_info("Updating gspc.rda")

    gspc <- paste0(
      "https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=",
      as.numeric(as.POSIXct(start_date)),
      "&period2=",
      as.numeric(as.POSIXct(Sys.Date())),
      "&interval=1d&events=history&includeAdjustedClose=true"
    ) %>%
      readr::read_csv(col_types = "Dnnnnnn") %>%
      as.data.frame() %>%
      tibble::column_to_rownames("Date") %>%
      xts::as.xts()

    usethis::use_data(gspc, overwrite = TRUE)

  }

  # Benchmark ------------------------------------------------------------------
  benchmark <- gspc[statement_dates]$`Adj Close` %>%
    daily_rtns() %>% {
      . <- .[[1]]
      colnames(.) <- "SP500 Daily Rtn"
      .
    }

  # Risk-free Rate (Daily CMT) -------------------------------------------------
  get_usdt_data <- function(url){
    xml2::read_html(url) %>%
      rvest::html_node(".t-chart") %>%
      rvest::html_table() %>%
      tibble::as_tibble() %>%
      dplyr::na_if("N/A") %>% {
        xts::xts(
          dplyr::select(., -"Date"),
          order.by = as.Date(.$Date, format = "%m/%d/%y")
        )
      } %>% {
        storage.mode(.) <- "numeric"
        colnames(.) <- gsub(" ", "_", colnames(.))
        .
      }
  }

  # year-to-date rfr
  rfr <- paste0(
    "https://www.treasury.gov/resource-center/data-chart-center/",
    "interest-rates/pages/TextView.aspx?data=yieldYear&year=",
    format(Sys.Date(), format = "%Y")
  ) %>%
    get_usdt_data()

  # Add missing years, if any
  for(
    rfr_year in setdiff(
      format(statement_dates, format = "%Y"), format(rfr, format = "%Y")
    )
  ){
    rfr <- paste0(
      "https://www.treasury.gov/resource-center/data-chart-center/",
      "interest-rates/pages/TextView.aspx?data=yieldYear&year=",
      rfr_year
    ) %>%
      get_usdt_data() %>%
      xts::rbind.xts(rfr)

    Sys.sleep(5)
  }

  rfr <- rfr[statement_dates, "3_mo"] %>% {
    xts::xts(zoo::coredata(.) / 252, order.by = zoo::index(.))
  }

  participating_student_reports <- student_reports[
    which(student_reports$participating),
  ] %>%
    dplyr::mutate(
      "daily_returns"    = daily_rtns(statement$total),
      "daily_excess_rtn" = list(
        xts::xts(
          zoo::coredata(daily_returns) - zoo::coredata(benchmark),
          order.by = zoo::index(daily_returns)
        )
      ),
      "excess_gmrr"      = gmrr(daily_excess_rtn),
      "daily_vol"        = sd(daily_returns),
      "Sharpe"           = excess_gmrr / daily_vol
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "accountId",   "daily_returns", "daily_excess_rtn",
      "excess_gmrr", "daily_vol",     "Sharpe"
    )

  student_reports <- dplyr::full_join(
    dplyr::ungroup(student_reports),
    participating_student_reports,
    by = "accountId"
  ) %>%
    dplyr::arrange(accountId)

  usethis::use_data(participants,    overwrite = TRUE)
  usethis::use_data(student_reports, overwrite = TRUE)
  usethis::use_data(benchmark,       overwrite = TRUE)
  usethis::use_data(rfr,             overwrite = TRUE)

  devtools::load_all(".")

}
