library('magrittr')

max_character_count <- 2000 # limit set by IBKR in the invite page

need_invites <- file.path(
  Sys.getenv("APP_BASE_PATH"),
  paste0("duke_fintech_trading_competition_", "2022"),
  "wufoo_registrants.csv",
  fsep = "\\"
) %>%
  readr::read_csv() %>%
  dplyr::select(email) %>%
  dplyr::filter(email %>% grepl('.edu$', .)) %>%
  tibble::deframe()

if(
  file.exists(
    file.path(
      Sys.getenv("APP_BASE_PATH"), "invited", "invited.csv", fsep = "\\"
    )
  )
){
  need_invites <- setdiff(
    need_invites,
    readr::read_csv(
      file.path(
        Sys.getenv("APP_BASE_PATH"), "invited", "invited.csv", fsep = "\\"
      )
    ) %>%
      dplyr::select(email) %>%
      tibble::deframe()
  )
}


chunks <- split(need_invites, ceiling(seq_along(need_invites)/10))


for(chunk in chunks){

  chunk_txt <- paste0(chunk, collapse = ", ")

  clipr::write_clip(chunk_txt)

  usethis::ui_info(paste0("Inviting:", chunk_txt))

  invites_made <- usethis::ui_yeah(
    "Contents copied to clipboard. Make invites and press [ENTER] to continue."
  )

  if(invites_made){

    if(
      file.exists(
        file.path(
          Sys.getenv("APP_BASE_PATH"), "invited", "invited.csv", fsep = "\\"
        )
      )
    ){
      invitees <- dplyr::bind_rows(
        tibble::tibble(
          'email'        = chunk,
          'invited_date' = Sys.time()
        ),
        readr::read_csv(
          file.path(
            Sys.getenv("APP_BASE_PATH"), "invited", "invited.csv", fsep = "\\"
          )
        )
      )
    } else {
      invitees <- tibble::tibble(
        'email'        = chunk,
        'invited_date' = Sys.time()
      )
    }

    invitees %>%
      unique() %>%
      readr::write_csv(
        file.path(
          Sys.getenv("APP_BASE_PATH"), "invited", "invited.csv",
          fsep = "\\"
        )
      )

  }

}


