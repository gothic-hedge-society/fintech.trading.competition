## code to prepare `registry` dataset goes here

library('magrittr')

registry <- xml2::read_xml(
  file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "EoD_Balances_and_IDs.xml",
    fsep = "\\"
  )
) %>%
  xml2::xml_child('FlexStatements') %>%
  xml2::xml_contents() %>%
  lapply(
    function(FlexStatement){
      FlexStatement %>%
        xml2::xml_child('AccountInformation') %>% {
          tibble::tibble(
            'account_id' = xml2::xml_attr(., 'accountId'),
            'name'       = xml2::xml_attr(., 'name'),
            'email'      = xml2::xml_attr(., 'primaryEmail')
          )
        }
    }
  ) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::nest_by(account_id, name, email) %>%
  dplyr::select(-data) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(
    readr::read_csv(
      file.path(
        Sys.getenv("APP_BASE_PATH"),
        "duke_fintech_trading_competition_2022",
        "wufoo_registrants.csv",
        fsep = "\\"
      )
    ) %>%
      dplyr::select(email, tradername),
    by = 'email'
  ) %>%
  dplyr::filter(!duplicated(email)) %>%
  dplyr::select(tradername, account_id) %>%
  dplyr::rename(status = account_id) %>%
  dplyr::arrange(tradername)

registry$status[is.na(registry$status)]  <- "pending account creation"

usethis::use_data(registry, overwrite = TRUE)
