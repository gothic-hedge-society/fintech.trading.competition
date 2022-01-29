## code to prepare `registry` dataset goes here

library('magrittr')

# Load up the School Recode Key
school_recode_key <- suppressMessages(
  readr::read_csv(
    file.path(
      Sys.getenv("APP_BASE_PATH"),
      "duke_fintech_trading_competition_2022",
      "school_recode_key.csv",
      fsep = "\\"
    )
  )
)

# Load up the WuFoo Registry
wufoo_registry <- suppressMessages(
  readr::read_csv(
    file.path(
      Sys.getenv("APP_BASE_PATH"),
      "duke_fintech_trading_competition_2022",
      "registry.csv",
      fsep = "\\"
    )
  )
) %>%
  dplyr::mutate(
    'school' = School %>%
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

# Load up the full, unfiltered flex statement
flex_statement_full <- list.files(
  path       = file.path(
    Sys.getenv("APP_BASE_PATH"), "..", "Downloads", fsep = "\\"
  ),
  pattern    = "\\).xml",
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
  dplyr::nest_by(account_id, name, email)

# Chanel messed up her email. Need to use a 'key' that the students pick
#   instead of an email in the WuFoo form.
flex_statement_full$email[
  flex_statement_full$name == "Chanel Zhu Ms"
] <- "chanelchu@utexas.edu"

# For participants who created more than one account and messaged me
specified_account_ids <- suppressMessages(
  readr::read_csv(
    file.path(
      Sys.getenv("APP_BASE_PATH"),
      "duke_fintech_trading_competition_2022",
      "specified_account_ids.csv",
      fsep = "\\"
    )
  )
)

# Filter the full flex statement down to valid accounts only
flex_statement <- flex_statement_full %>%
  dplyr::filter(
    !(email %in% specified_account_ids$email) | (
      account_id %in% specified_account_ids$account_id
    )
  ) %>% {
    .[!duplicated(.$email),]
  }

# Create full registry by left-joining WuFoo data with the flex statement
full_registry <- dplyr::left_join(
  wufoo_registry, flex_statement[c("account_id", "name", "email")], by = "email"
) %>% {
  .$account_id[is.na(.$account_id)] <- "not yet active"
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

registry <- full_registry[c("tradername", "account_id")] %>%
  dplyr::arrange(tradername)


school_stats <- full_registry %>%
  dplyr::nest_by(school) %>% {
    suppressMessages(
      dplyr::summarise(
        .,
        'completion' = length(
          which(data$account_id != "not yet active")
        )/length(data$account_id)
      )
    )
  } %>%
  dplyr::ungroup() %>%
  dplyr::arrange(school)

Sys.Date() %>%
  as.character() %>%
  gsub("-", "\\.", .) %>% {
    suppressMessages(desc::desc_set_version(.))
    invisible()
  }

usethis::use_data(registry, overwrite = TRUE)
usethis::use_data(school_stats, overwrite = TRUE)
