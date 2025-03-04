#' Updates all base data that the app needs to generate the site.
#'
#' @export
#'
refresh_base_data <- function(){

  start_date <- as.Date("2025-02-07")
  rf         <- 0.0433/252
  tier_size  <- 10

  secrets_path <- file.path(rprojroot::find_package_root_file(), 'secrets')

  NAV_and_CASH <- fintech.trading.competition::fetch_flex_query(1142446)
  xml2::write_xml(
    NAV_and_CASH,
    file = file.path(secrets_path, 'NAV_and_CASH.xml')
  )

  # Participants ---------------------------------------------------------------
  participants <- readr::read_csv(
    file.path(
      secrets_path, "duke-fintech-trading-competition-2025_entries.csv"
    ),
    show_col_types = FALSE
  )

  # Supplemental Account Map ---------------------------------------------------
  sup_acc_map <- readr::read_csv(
    file.path(
      secrets_path, "supplemental_acct_map.csv"
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

  # Benchmarks -----------------------------------------------------------------
  library(quantmod)

  # Fetch S&P 500 data using the ticker ^GSPC
  getSymbols(
    "^GSPC",
    src = "yahoo",
    from = start_date - 30,
    to = Sys.Date()
  )

  # Extract closing prices
  sp500_closing <- Cl(GSPC)  # Cl() extracts closing prices

  # Create a simple data frame with dates and closing prices
  sp500 <- data.frame(
    date = index(sp500_closing),
    Closing_Price = as.numeric(sp500_closing)
  )
  usethis::use_data(sp500, overwrite = TRUE)


  # participating_student_reports ----------------------------------------------
  NAV_and_CASH_xml <- NAV_and_CASH %>%
    xml2::xml_child("FlexStatements")

  xml2::xml_add_child(
    NAV_and_CASH_xml,
    xml2::read_xml(file.path(secrets_path, 'NAV_and_CASH_qfi.xml')) %>%
      xml2::xml_child("FlexStatements")
  )

  participating_student_reports <- NAV_and_CASH_xml %>%
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
                'tier' = NA,
                'rank' = NA
              )
            )
          )

      }
    ) %>%
    purrr::reduce(dplyr::bind_rows)
  # %>%
  #   dplyr::filter(
  #   !(accountId %in% c('DUH462396', 'DUH391881'))) # dedupe accounts

  participating_student_reports$trader_name <- participants$trader_name[
    match(
      participating_student_reports$primaryEmail, participants$email
    )
  ]

  acct_mask <-  file.path(secrets_path, 'supplemental_acct_map.csv') %>%
    readr::read_csv()

  participating_student_reports$trader_name[
    match(
      acct_mask$ibkr_id, participating_student_reports$accountId
    )
  ] <- acct_mask$trader_name

  no_tradernames <- which(is.na(participating_student_reports$trader_name))
  participating_student_reports$trader_name[
    no_tradernames
  ] <- participating_student_reports$accountId[no_tradernames]

  participating_student_reports$university <- participants$university[
    match(
      participating_student_reports$trader_name,
      participants$trader_name
    )
  ]
  participating_student_reports$website <- participants$website[
    match(
      participating_student_reports$trader_name,
      participants$trader_name
    )
  ]

  standings <- participating_student_reports$statement %>%
    lapply(
      function(stmt){
        stmt %>%
          dplyr::filter(!is.na(sharpe)) %>%
          dplyr::select(date) %>%
          tibble::deframe()
      }
    ) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    as.Date() %>%
    stats::setNames(.,.) %>%
    lapply(
      function(trade_dt){

        standings <- participating_student_reports$trader_name %>%
          lapply(
            function(trader){
              tibble::tibble(
                'trader' = trader,
                'university' = participating_student_reports %>%
                  dplyr::filter(trader_name == trader) %>%
                  dplyr::select(university) %>% {
                    .$university[[1]]
                  },
                'gmrr' = participating_student_reports %>%
                  dplyr::filter(trader_name == trader) %>%
                  dplyr::select(statement) %>% {
                    .$statement[[1]]
                  } %>%
                  dplyr::filter(date == trade_dt) %>%
                  dplyr::select(gmrr) %>%
                  tibble::deframe(),
                'sharpe' = participating_student_reports %>%
                  dplyr::filter(trader_name == trader) %>%
                  dplyr::select(statement) %>% {
                    .$statement[[1]]
                  } %>%
                  dplyr::filter(date == trade_dt) %>%
                  dplyr::select(sharpe) %>%
                  tibble::deframe(),
                'tier' = NA,
                'rank' = NA
              )
            }
          ) %>%
          purrr::reduce(dplyr::bind_rows) %>%
          dplyr::arrange(dplyr::desc(gmrr))

        n_complete_tiers <- nrow(standings) %/% tier_size
        n_in_last_tier <- nrow(standings) %% tier_size

        standings$tier <- 1:n_complete_tiers %>%
          lapply(
            function(x){
              rep(x, tier_size)
            }
          ) %>%
          unlist() %>%
          c(., rep(n_complete_tiers + 1, n_in_last_tier))

        standings <- standings %>%
          dplyr::group_by(tier) %>%
          dplyr::arrange(dplyr::desc(sharpe), .by_group = TRUE) %>%
          dplyr::ungroup()

        standings$rank <- rep(1:tier_size, n_complete_tiers) %>% {
          if(n_in_last_tier == 0){
            .
          } else {
            c(., 1:n_in_last_tier)
          }
        }

        standings$overall_rank <- 1:nrow(standings)

        standings

      }
    )

  usethis::use_data(standings, overwrite = TRUE)

  save(
    participating_student_reports,
    file = file.path(secrets_path, 'participating_student_reports.rda')
  )

  reports <- participating_student_reports %>%
    dplyr::select(trader_name, university, website, statement)

  usethis::use_data(standings, overwrite = TRUE)
  usethis::use_data(reports, overwrite = TRUE)


}

