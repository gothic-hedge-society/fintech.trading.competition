#' Updates all base data that the app needs to generate the site.
#'
#' @export
#'
refresh_base_data <- function(){
  library(magrittr)

  start_date <- as.Date("2025-02-07")
  rf         <- 0.0433/252
  tier_size  <- 10

  known_duplicates <- c('DUH279253', 'DUH462396', 'DUH232073', 'DUH465763')
  secrets_path <- file.path(rprojroot::find_package_root_file(), 'secrets')
  shiny_path <- file.path(
    rprojroot::find_package_root_file(), "..", 'ftc.shinyapp', 'data'
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

  # Perform Flex Queries -------------------------------------------------------
  load(
    file.path(
      rprojroot::find_package_root_file(),
      'secrets',
      'ibkr_flex_web_token.rda'
    )
  )
  load(
    file.path(
      rprojroot::find_package_root_file(),
      'secrets',
      'ibkr_flex_web_token_1.rda'
    )
  )

  NAV_and_CASH <- fintech.trading.competition::fetch_flex_query(
    flex_params = stats::setNames(
      c(1142446, 1165361),
      c(ibkr_flex_web_token, ibkr_flex_web_token_1)
    ),
    start_date = start_date,
    sp500      = sp500,
    rf         = rf
  )

  # participating_student_reports ----------------------------------------------
  # for now, just NAV_and_CASH
  # will splice in trades later(?)
  participating_student_reports <- NAV_and_CASH %>%
    dplyr::filter(!(accountId %in% known_duplicates))


  participating_student_reports$trader_name <- participants$trader_name[
    match(
      participating_student_reports$primaryEmail, participants$email
    )
  ]

  acct_mask <-  file.path(secrets_path, 'supplemental_acct_map.csv') %>%
    readr::read_csv(show_col_types = FALSE) %>%
    dplyr::mutate(
      'mask' = match(ibkr_id, participating_student_reports$accountId)
    ) %>%
    tidyr::drop_na()

  participating_student_reports$trader_name[
    acct_mask$mask
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
                'vol' = participating_student_reports %>%
                  dplyr::filter(trader_name == trader) %>%
                  dplyr::select(statement) %>% {
                    .$statement[[1]]
                  } %>%
                  dplyr::filter(date == trade_dt) %>%
                  dplyr::select(vol) %>%
                  tibble::deframe(),
                'sharpe' = participating_student_reports %>%
                  dplyr::filter(trader_name == trader) %>%
                  dplyr::select(statement) %>% {
                    .$statement[[1]]
                  } %>%
                  dplyr::filter(date == trade_dt) %>%
                  dplyr::select(sharpe) %>%
                  tibble::deframe(),
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

        standings %>%
          tidyr::unite('rank', tier, rank, sep = ".") %>%
          dplyr::select(rank, dplyr::everything())


      }
    )

  for(competition_date in names(standings)){
    standings_on_cdate <- standings[[competition_date]]
    for(trader_ in standings_on_cdate$trader){
      chonk <- standings_on_cdate[
        standings_on_cdate$trader == trader_,
        'rank'
      ] %>%
        tibble::deframe() %>%
        as.numeric()
      if(length(chonk) > 1){
        usethis::ui_info(trader_)
        stop('duplicate account')
      }
      participating_student_reports[
        participating_student_reports$trader_name == trader_,
        'statement'
      ][[1]][[1]]$rank[
        participating_student_reports[
          participating_student_reports$trader_name == trader_,
          'statement'
        ][[1]][[1]]$date == as.Date(competition_date)
      ] <- chonk
    }
  }

  reports <- participating_student_reports %>%
    dplyr::select(trader_name, university, website, statement)

  # This package
  usethis::use_data(sp500, overwrite = TRUE)
  usethis::use_data(standings, overwrite = TRUE)
  usethis::use_data(reports, overwrite = TRUE)
  save(
    participating_student_reports,
    file = file.path(secrets_path, 'participating_student_reports.rda')
  )

  # Shiny App
  save(
    standings,
    file = file.path(shiny_path, 'standings.rda')
  )
  save(
    reports,
    file = file.path(shiny_path, 'reports.rda')
  )


}

