#' Updates all base data that the app needs to generate the site.
#'
#' @export
#'
refresh_base_data <- function(){
  library(magrittr)

  start_date <- as.Date("2026-02-07")
  rf         <- 0.0369/252
  tier_size  <- 10

  shiny_path <- file.path(
    rprojroot::find_package_root_file(), "..", 'ftc.shinyapp', 'data'
  )
  secrets_path <- file.path(rprojroot::find_package_root_file(), 'secrets')

  # Participants ---------------------------------------------------------------
  participants <- readr::read_csv(
    file.path(
      rprojroot::find_package_root_file(), 'inst', 'has_ibkr_accounts.csv'
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

  # 1380818 is names only
  # 1142446 is names and nav

  full_nav_query <- fintech.trading.competition::fetch_flex_query(
    flex_params = stats::setNames(
      1142446, ibkr_flex_web_token
    ),
    start_date = start_date,
    sp500      = sp500,
    rf         = rf
  ) %>%
    purrr::reduce(dplyr::bind_rows)

  participating_student_reports <- dplyr::inner_join(
    full_nav_query,
    participants,
    by = dplyr::join_by(accountId)
  ) %>%
    dplyr::select(-masterName, -primaryEmail) %>%
    dplyr::distinct(trader_name, .keep_all = TRUE)


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
                # 'organization' = participating_student_reports %>%
                #   dplyr::filter(trader_name == trader) %>%
                #   dplyr::select(organization) %>% {
                #     .$organization[[1]]
                #   },
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
        usethis::ui_warn('duplicate account')
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
    dplyr::select(trader_name, statement, website, organization)


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

