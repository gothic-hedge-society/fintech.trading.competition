## code to prepare `registry` dataset goes here

library('magrittr')

school_recode_key <- readr::read_csv(
  file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "school_recode_key.csv",
    fsep = "\\"
  )
)

wufoo_registry <- readr::read_csv(
  file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "registry.csv",
    fsep = "\\"
  )
) %>%
  dplyr::mutate(
    'School' = School %>%
      vapply(
        function(school){
          unique(
            school_recode_key$correct_name[
              school_recode_key$form_name == school
            ]
          )
        },
        character(1)
      )
  )

# For participants who created more than one account and messaged me
specified_account_ids <- tibble::tibble(
  email      = "2016110029@email.szu.edu.cn",
  account_id = "DU5047458"
)

flex_statement <- list.files(
  path       = file.path(
    Sys.getenv("APP_BASE_PATH"), "..", "Downloads", fsep = "\\"
  ),
  pattern    = "EoD_Balances_and_IDs",
  full.names = TRUE
) %>%
  gsub("/", "\\\\", .) %>%
  stats::setNames(.,.) %>%
  vapply(
    function(flex_file_path){
      gsub("^(.*)\\(", "", flex_file_path) %>%
        gsub("\\)\\.xml$", "", .) %>%
        as.numeric()
    },
    numeric(1)
  ) %>% {
    names(.[which.max(.)])
  } %>%
  xml2::read_xml() %>%
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
  dplyr::filter(
    !(email %in% specified_account_ids$email) | (
      account_id %in% specified_account_ids$account_id
    )
  ) %>% {
    .[!duplicated(.$email),]
  }

na_replace <- "account pending creation"
full_registry <- dplyr::left_join(
  wufoo_registry, flex_statement[c("account_id", "name", "email")]
) %>% {
  .$account_id[is.na(.$account_id)] <- na_replace
  .
}

readr::write_csv(
  full_registry,
  file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "full_registry.csv",
    fsep = "\\"
  )
)

registry <- full_registry[c("tradername", "account_id")]

school_stats <- full_registry %>%
  dplyr::nest_by(School) %>%
  dplyr::summarise(
    'completion' = length(which(data$account_id != na_replace))/length(
      data$account_id
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(School)


usethis::use_data(registry, overwrite = TRUE)
usethis::use_data(school_stats, overwrite = TRUE)
