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
  ) %>%
  dplyr::select(-School)

# Load up the full, unfiltered flex statement
flex_statement_full <- Sys.getenv("APP_BASE_PATH") %>%
  file.path("..", "Downloads", fsep = "\\") %>%
  list.files(pattern = "^Registry(.*).xml$", full.names = TRUE) %>%
  gsub("/", "\\\\", .) %>%
  stats::setNames(.,.) %>%
  vapply(
    function(flex_file_path){
      if(grepl("\\(\\d{1,}).xml", flex_file_path)){
        gsub("^(.*)\\(", "", flex_file_path) %>%
          gsub("\\)\\.xml$", "", .) %>%
          as.numeric()
      } else {
        0
      }
    },
    numeric(1)
  ) %>% {
    names(.[which.max(.)])
  } %>%
  xml2::read_xml() %>%
  xml2::xml_child("FlexStatements") %>%
  xml2::xml_children() %>%
  xml2::xml_children() %>%
  xml2::xml_attrs() %>%
  purrr::reduce(dplyr::bind_rows) %>%
  magrittr::set_colnames(c("account_id", "name", "email"))

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

# full join the WuFoo data with the flex statement registry
full_registry <- dplyr::full_join(wufoo_registry, flex_statement, by = "email")

registry <- full_registry[c("tradername", "school", "account_id")] %>%
  dplyr::arrange(tradername)
registry$account_id[which(is.na(registry$account_id))] <- "not yet active"

# Save the registry data
readr::write_csv(
  full_registry,
  file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "full_registry.csv",
    fsep = "\\"
  )
)

Sys.Date() %>%
  as.character() %>%
  gsub("-", "\\.", .) %>% {
    suppressMessages(desc::desc_set_version(.))
    invisible()
  }

usethis::use_data(registry, overwrite = TRUE)
