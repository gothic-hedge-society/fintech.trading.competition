
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

testthat::test_that(
  "No duplicated tradernames", {
    tn_dup <- trader_key %>%
      dplyr::filter(
        !is.na(account_id) & !(status %in% c("deleted", "pending deletion"))
      ) %>%
      dplyr::select(tradername) %>%
      tibble::deframe() %>% {
        .[which(duplicated(.))]
      }
    testthat::expect(
      length(tn_dup) == 0,
      failure_message = paste0(
        "Duplicated tradernames: ",
        paste0(tn_dup, collapse = ", ")
      )
    )
  }
)

testthat::test_that(
  "No duplicated account IDs", {
    id_dup <- trader_key %>%
      dplyr::filter(
        !is.na(account_id) & !(status %in% c("deleted", "pending deletion"))
      ) %>%
      dplyr::select(account_id) %>%
      tibble::deframe() %>% {
        .[which(duplicated(.))]
      }
    testthat::expect(
      length(id_dup) == 0,
      failure_message = paste0(
        "Duplicated account IDs: ",
        paste0(id_dup, collapse = ", ")
      )
    )
  }
)

testthat::test_that(
  "No trader has more than one active account", {
    multitraders <- trader_key %>%
      dplyr::filter(status == "active") %>%
      dplyr::select(tradername) %>%
      tibble::deframe() %>%
      stats::setNames(.,.) %>%
      vapply(
        function(tname){
          trader_key %>%
            dplyr::filter(tradername == tname & status == "active") %>%
            dplyr::select(account_id) %>%
            tibble::deframe() %>%
            unique() %>%
            length()
        },
        numeric(1)
      ) %>% {
        names(.[which(. > 1)])
      }
    testthat::expect(
      length(multitraders) == 0,
      failure_message = paste0(
        "Traders trading with more than one account: ",
        paste0(multitraders, collapse = ", ")
      )
    )
  }
)

testthat::test_that(
  "No email address has more than one active account", {
    tkp <- read_a_csv("trader_key_private")
    multi_emails <- tkp %>%
      dplyr::filter(status == "active") %>%
      dplyr::select(email_ibkr) %>%
      tibble::deframe() %>%
      stats::setNames(.,.) %>%
      vapply(
        function(email){
          tkp %>%
            dplyr::filter(email_ibkr == email & status == "active") %>%
            dplyr::select(account_id) %>%
            tibble::deframe() %>%
            unique() %>%
            length()
        },
        numeric(1)
      ) %>% {
        names(.[which(. > 1)])
      }
    testthat::expect(
      length(multi_emails) == 0,
      failure_message = paste0(
        "Email addresses with more than one active account: ",
        paste0(multi_emails, collapse = ", ")
      )
    )
  }
)
