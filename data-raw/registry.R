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
specified_account_ids <- tibble::tibble(
  email      = c("2016110029@email.szu.edu.cn", "eyz@princeton.edu"),
  account_id = c("DU5047458", "DU5124984")
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

# Accounts that are valid but don't appear in flex statement yet
limbo_accounts <- suppressMessages(
  readr::read_csv(
    file.path(
      Sys.getenv("APP_BASE_PATH"),
      "duke_fintech_trading_competition_2022",
      "limbo_accounts.csv",
      fsep = "\\"
    )
  )
) %>%
  dplyr::filter(
    !is.na(pending_account_id) & !(
      pending_account_id %in% flex_statement$account_id
    )
  ) %>%
  dplyr::arrange(email)

# Create full registry by left-joining WuFoo data with the flex statement
full_registry <- dplyr::left_join(
  wufoo_registry, flex_statement[c("account_id", "name", "email")], by = "email"
) %>% {
  .$account_id[
    intersect(which(is.na(.$account_id)), match(limbo_accounts$email, .$email))
  ] <- paste0(
    limbo_accounts$pending_account_id, " (limbo)"
  )
  .$account_id[is.na(.$account_id)] <- "account pending creation"
  .
}

# Accounts that are written down in 'limbo' or appearing in the filtered flex
#   statement are valid.
valid_accounts <- c(
  limbo_accounts$account_id,
  flex_statement$account_id[!is.na(flex_statement$account_id)]
) %>%
  unique()

# All other accounts are NOT valid and need to be pruned.
accounts_to_prune <- flex_statement_full %>%
  dplyr::filter(
    account_id %in% setdiff(flex_statement_full$account_id, valid_accounts)
  ) %>%
  dplyr::select(-data)

if(isTRUE(nrow(accounts_to_prune) > 0)){

  prune_string <- paste0(accounts_to_prune$account_id, collapse = ", ")

  usethis::ui_info(paste0("Need to prune these accounts: ", prune_string))
  clipr::write_clip(prune_string)
  usethis::ui_info("This string has been copied to the clipboard.")

  accounts_to_prune_path <- file.path(
    Sys.getenv("APP_BASE_PATH"),
    "duke_fintech_trading_competition_2022",
    "accounts_to_prune.csv",
    fsep = "\\"
  )

  if(file.exists(accounts_to_prune_path)){
    accounts_to_prune <- accounts_to_prune_path %>% {
      suppressMessages(readr::read_csv(.))
    } %>%
      dplyr::bind_rows(accounts_to_prune) %>%
      unique()
  }

  readr::write_csv(accounts_to_prune, accounts_to_prune_path)

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
          which(data$account_id != "account pending creation")
        )/length(data$account_id),
        'limbo' = sum(grepl("limbo", data$account_id))
      )
    )
  } %>%
  dplyr::ungroup() %>%
  dplyr::arrange(school)

Sys.Date() %>%
  as.character() %>%
  gsub("-", "\\.", .) %>%
  suppressMessages(desc::desc_set_version())

usethis::use_data(registry, overwrite = TRUE)
usethis::use_data(school_stats, overwrite = TRUE)
