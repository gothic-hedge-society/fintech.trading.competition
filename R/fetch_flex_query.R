#' Fetches the specified flex query from IBKR
#'
#' Operates in sync; will wait until it gets its result (5 min max). You have
#' to first save a flex query in client portal and get its ID.
#'
#' @export
#'
fetch_flex_query <- function(flex_params, start_date, sp500, rf){

  ref_codes <- flex_params %>%
    purrr::imap(
      function(q_id, flex_web_token){
        httr::GET(
          url = paste0(
            "https://ndcdyn.interactivebrokers.com/",
            "AccountManagement/FlexWebService/SendRequest"
          ),
          query = list(
            t = flex_web_token,   # configured API token in Client Portal
            q = q_id,             # id of saved query created in Client Portal
            v = 3                 # flex version
          )
        )
      }
    ) %>%
    lapply(
      function(flex_req){
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
            usethis::ui_done(paste0("Reference Code: ", ref_code))
          }
        }
        # Check if ref_code was found (optional safety check)
        if (is.null(ref_code)) {
          stop("ReferenceCode not found in the response")
        }
        ref_code
      }
    )

  # Initialize a progress bar with 20 steps (for 20 seconds)
  pb <- progress::progress_bar$new(
    format = "  Waiting [:bar] :percent (:elapsed / :eta)",
    total = 20,        # Total steps (one per second)
    clear = FALSE,     # Leaves the bar visible after completion
    width = 60         # Width of the bar in characters
  )

  # Simulate a 20-second wait with progress updates
  cat("Hold for Request.\n")
  for (i in 1:20) {
    pb$tick()          # Advance the progress bar by one step
    Sys.sleep(1)       # Wait 1 second per step
  }

  received_statements <- ref_codes %>%
    purrr::imap(
      function(ref_code, flex_web_token){
        httr::GET(
          url = paste0(
            "https://ndcdyn.interactivebrokers.com/AccountManagement/",
            "FlexWebService/GetStatement"
          ),
          query = list(
            t = flex_web_token,
            q = ref_code,
            v = 3
          ),
          config = httr::config(followlocation = TRUE)
        )
      }
    )

  received_statements %>%
    lapply(
      function(received_url){
        if (httr::status_code(received_url) != 200) {
          stop(
            "Failed to retrieve statement: ",
            httr::status_code(received_url)
          )
        }
        httr::content(received_url, options = c('HUGE')) %>%
          xml2::xml_child("FlexStatements") %>%
          xml2::xml_find_all('//FlexStatement') %>%
          lapply(
            function(xml_statement){
              statement_row <- xml_statement %>%
                xml2::xml_child("AccountInformation ") %>%
                xml2::xml_attrs() %>%
                tibble::as_tibble_row()

              statement <- xml_statement %>%
                xml2::xml_child("EquitySummaryInBase") %>%
                xml2::xml_children() %>%
                lapply(
                  function(EquitySummaryByReportDateInBase){
                    xml2::xml_attrs(EquitySummaryByReportDateInBase) %>%
                      tibble::as_tibble_row()  %>%
                      dplyr::select(reportDate, total)
                  }
                ) %>%
                purrr::reduce(dplyr::bind_rows) %>%
                dplyr::rename(date = 'reportDate', NAV = 'total') %>%
                dplyr::mutate(
                  'date' = as.Date(date, format = "%Y%m%d"),
                  'NAV' = as.numeric(NAV)
                ) %>%
                dplyr::filter(date >= start_date) %>% {
                  .$NAV[.$NAV == 0] <- 1000000
                  .
                } %>%
                dplyr::left_join(sp500, by='date') %>%
                dplyr::rename(sp500_close = "Closing_Price") %>%
                dplyr::mutate(
                  'daily_return' = c(NA, log(NAV[-1]/NAV[-length(NAV)])),
                  'sp500_return' = c(
                    NA,
                    log(sp500_close[-1]/sp500_close[-length(sp500_close)])
                  ),
                  'gmrr'  = NA,
                  'vol'   = NA,
                  'alpha' = NA,
                  'beta'  = NA
                )


              for (i in 2:nrow(statement)){
                statement[i, 'gmrr'] = prod(
                  statement$daily_return[2:i] + 1
                )^(1/(i-1)) - 1
                statement[i, 'vol'] = sd(statement$daily_return[2:i])
              }

              for (i in 3:nrow(statement)){
                tryCatch(
                  {
                    fit <- lm(
                      statement$daily_return[2:i] ~ statement$sp500_return[2:i]
                    )
                    statement[i, 'alpha'] <- fit$coefficients[1]
                    statement[i, 'beta'] <- fit$coefficients[2]
                  }
                  ,
                  error = function(e){
                    bad_xml_statement <<- xml_statement
                    usethis::ui_oops(e)
                    usethis::ui_info(statement_row)
                    usethis::ui_info(i)
                    statement[i, 'alpha'] <- NA
                    statement[i, 'beta']  <- NA
                    stop('global var bad_xml_statement assigned')
                  }
                )
              }

              statement_row %>%
                dplyr::mutate(
                  'statement' =  list(
                    dplyr::mutate(
                      statement,
                      'excess_rtn' = gmrr - rf,
                      'sharpe' = excess_rtn / vol,
                      'rank' = NA
                    )
                  )
                )

            }
          )
      }
    ) %>%
    purrr::reduce(dplyr::bind_rows)
}
