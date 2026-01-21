#' Fetches just descriptive account information for everyone who has an account
#'
#'
#' @export
#'
fetch_account_information <- function(){

  load(
    file.path(
      rprojroot::find_package_root_file(),
      'secrets',
      'ibkr_flex_web_token.rda'
    )
  )

  # 1380818 is names only
  # 1142446 is names and nav

  ref_codes <- stats::setNames(1380818, ibkr_flex_web_token) %>%
    purrr::imap(
      function(q_id, flex_web_token){
        httr::GET(
          url = paste0(
            "https://ndcdyn.interactivebrokers.com/",
            "AccountManagement/FlexWebService/SendRequest"
          ),
          query = list(
            t = flex_web_token,   # configured API token in Client Portal
            q = q_id,             # id of saved query created in Client Portal
            v = 3                 # flex version
          )
        )
      }
    ) %>%
    lapply(
      function(flex_req){
        xml_content <- httr::content(flex_req, "text", encoding = "UTF-8")
        # Check the response
        if (httr::status_code(flex_req) == 200) {
          usethis::ui_done(paste0("Request successful:\n", xml_content))
        } else {
          usethis::ui_oops(paste0("Error:", httr::status_code(flex_req)))
          usethis::ui_oops(xml_content)
        }
        # Parse the XML from the response text
        tree <- xml2::read_xml(xml_content)
        root <- xml2::xml_root(tree)
        # Loop through child nodes to check status and get ReferenceCode
        ref_code <- NULL
        for (child in xml2::xml_children(root)) {
          tag  <- xml2::xml_name(child)
          text <- xml2::xml_text(child)

          if (tag == "Status") {
            if (text != "Success") {
              cat("Failed to generate Flex statement. Stopping...\n")
              stop("Script terminated due to failure in Flex statement generation")
            }
          } else if (tag == "ReferenceCode") {
            ref_code <- text
            usethis::ui_done(paste0("Reference Code: ", ref_code))
          }
        }
        # Check if ref_code was found (optional safety check)
        if (is.null(ref_code)) {
          stop("ReferenceCode not found in the response")
        }
        ref_code
      }
    )

  # Initialize a progress bar with 20 steps (for 20 seconds)
  pb <- progress::progress_bar$new(
    format = "  Waiting [:bar] :percent (:elapsed / :eta)",
    total = 20,        # Total steps (one per second)
    clear = FALSE,     # Leaves the bar visible after completion
    width = 60         # Width of the bar in characters
  )

  # Simulate a 20-second wait with progress updates
  cat("Hold for Request.\n")
  for (i in 1:20) {
    pb$tick()          # Advance the progress bar by one step
    Sys.sleep(1)       # Wait 1 second per step
  }

  fetched_flex_query <- ref_codes %>%
    purrr::imap(
      function(ref_code, flex_web_token){
        httr::GET(
          url = paste0(
            "https://ndcdyn.interactivebrokers.com/AccountManagement/",
            "FlexWebService/GetStatement"
          ),
          query = list(
            t = flex_web_token,
            q = ref_code,
            v = 3
          ),
          config = httr::config(followlocation = TRUE)
        )
      }
    )

  fetched_flex_query %>%
    lapply(
      function(received_url){
        if (httr::status_code(received_url) != 200) {
          stop(
            "Failed to retrieve statement: ",
            httr::status_code(received_url)
          )
        }
        httr::content(received_url, options = c('HUGE')) %>%
          xml2::xml_child("FlexStatements") %>%
          xml2::xml_find_all('//FlexStatement') %>%
          lapply(
            function(xml_statement){
              xml_statement %>%
                xml2::xml_child("AccountInformation ") %>%
                xml2::xml_attrs() %>%
                tibble::as_tibble_row()
            }
          )
      }
    ) %>% {
      .[[1]]
    } %>%
    purrr::reduce(dplyr::bind_rows)

}
