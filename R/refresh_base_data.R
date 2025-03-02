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
refresh_base_data <- function(){

  devtools::load_all(".")

  start_date <- as.Date("2025-02-07")
  rf         <- 0.0433/252

  # Send the GET request
  flex_req <- httr::GET(
    url = paste0(
      "https://ndcdyn.interactivebrokers.com/",
      "AccountManagement/FlexWebService/SendRequest"
    ),
    query = list(
      t = ibkr_flex_web_token,   # configured API token in Client Portal
      q = 1142446,               # id of saved query created in Client Portal
      v = 3                      # flex version
    )
  )

  # Error codes found here:
  # https://www.interactivebrokers.com/campus/ibkr-api-page/flex-web-service/#error-codes

  xml_content <- httr::content(flex_req, "text", encoding = "UTF-8")

  # Check the response
  if (httr::status_code(flex_req) == 200) {
    usethis::ui_done(paste0("Request successful:\n", xml_content))
  } else {
    usethis::ui_oops(paste0("Error:", httr::status_code(flex_req)))
    usethis::ui_oops(xml_content)
  }

  # Parse the XML from the response text
  tree <- xml2::read_xml(xml_content)
  root <- xml2::xml_root(tree)

  # Loop through child nodes to check status and get ReferenceCode
  ref_code <- NULL
  for (child in xml2::xml_children(root)) {
    tag  <- xml2::xml_name(child)
    text <- xml2::xml_text(child)

    if (tag == "Status") {
      if (text != "Success") {
        cat("Failed to generate Flex statement. Stopping...\n")
        stop("Script terminated due to failure in Flex statement generation")
      }
    } else if (tag == "ReferenceCode") {
      ref_code <- text
    }
  }

  # Check if ref_code was found (optional safety check)
  if (is.null(ref_code)) {
    stop("ReferenceCode not found in the response")
  }

  # Print status and wait 20 seconds
  usethis::ui_info("Hold for Request")
  Sys.sleep(20)

  # Send the GET request to retrieve the statement
  receive_url <- httr::GET(
    url = paste0(
      "https://ndcdyn.interactivebrokers.com/AccountManagement/",
      "FlexWebService/GetStatement"
    ),
    query = list(
      t = ibkr_flex_web_token,
      q = ref_code,
      v = 3
    ),
    config = httr::config(followlocation = TRUE)
  )

  # Check if request was successful
  if (httr::status_code(receive_url) != 200) {
    stop("Failed to retrieve statement: ", httr::status_code(receive_url))
  }

  flex_query <- httr::content(receive_url)
  usethis::use_data()




  # Participants ---------------------------------------------------------------
  participants <- rprojroot::find_package_root_file() %>%
    file.path(
      ., "inst", "duke-fintech-trading-competition-2025_entries.csv"
    ) %>%
    readr::read_csv(show_col_types = FALSE)

  # Discord Members ------------------------------------------------------------
  discord_members <- rprojroot::find_package_root_file() %>%
    file.path(
      ., "inst", "discord_member_dump.txt"
    ) %>%
    readr::read_csv(col_names = "discord_name", show_col_types = FALSE) %>%
    tidyr::separate("discord_name", c("discord_name", "id"), sep = " ")

  # participating_student_reports ----------------------------------------------
  participating_student_reports <- rprojroot::find_package_root_file() %>%
    file.path(
      ., "inst", "ibkr_trade_statements", "tcom2025.xml"
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

