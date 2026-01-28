#' The rectifier
#'
#' @export
run_rectifier <- function(){

  load("./secrets/wufoo_username.rda")

  base_url <- "https://dukefinance.wufoo.com/api/v3/"
  username <- wufoo_username
  password <- "footastic"

  form_name <- "duke-fintech-trading-competition-xi09vqi1wmt7cs"

  count_url       <- paste0(
    base_url, "forms/", form_name, "/entries/count.json"
  )
  registrants_url <- paste0(
    base_url, "forms/", form_name,
    "/entries.json?sort=EntryId&sortDirection=DESC&pageSize=100"
  )

  entries_count <- httr::GET(
    count_url, httr::authenticate(username, password)
  ) %>%
    httr::content() %>% {
      .$EntryCount
    } %>%
    as.numeric()

  number_of_wufoo_queries <- ceiling(entries_count / 100)

  registrants <- list()

  for(i in 1:number_of_wufoo_queries){
    registrants <- c(
      registrants,
      httr::GET(
        paste0(registrants_url, "&pageStart=", 100*(i-1)),
        httr::authenticate(username, password)
      ) %>%
        httr::content() %>% {
          .$Entries
        }
    )
  }

  registrants <- registrants %>%
    unique() %>%
    purrr::map(
      function(x){
        x %>%
          lapply(
            FUN = function(xx){
              if(is.null(xx)) return(NA)
              xx
            }
          ) %>%
          tibble::as_tibble()
      }
    ) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::select(Field635, Field5, Field532) %>%
    dplyr::rename(
      email = "Field635", tradername = "Field5", discord_username = "Field532"
    )

  ibkr_info <- fetch_account_information()
  ibkr_info %>%
    readr::write_csv(
      file=file.path(
        rprojroot::find_package_root_file(), 'secrets', 'ibkr_info.csv'
      )
    )

  dplyr::inner_join(
    ibkr_info[c("accountId", "primaryEmail")],
    registrants,
    by = c("primaryEmail" = "email")
  ) %>%
    dplyr::select(accountId, tradername, discord_username) %>%
    readr::write_csv(
      file=file.path(
        rprojroot::find_package_root_file(), 'inst', 'has_ibkr_accounts.csv'
      )
    )


  setdiff(registrants$email, ibkr_info$primaryEmail) %>%
    paste0(collapse = "; ") %>%
    writeLines(
      file.path(
        rprojroot::find_package_root_file(), 'secrets', 'no_ibkr_account.csv'
      )
    )

}
