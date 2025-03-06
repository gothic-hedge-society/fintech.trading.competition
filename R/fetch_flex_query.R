#' Fetches the specified flex query from IBKR
#'
#' Operates in sync; will wait until it gets its result (5 min max). You have
#' to first save a flex query in client portal and get its ID.
#'
#' @export
#'
fetch_flex_query <- function(q_id){
  # q_id <- 1142446

  load(
    file.path(
      rprojroot::find_package_root_file(),
      'secrets',
      'ibkr_flex_web_token.rda'
    )
  )

  # Send the GET request
  flex_req <- httr::GET(
    url = paste0(
      "https://ndcdyn.interactivebrokers.com/",
      "AccountManagement/FlexWebService/SendRequest"
    ),
    query = list(
      t = ibkr_flex_web_token,   # configured API token in Client Portal
      q = q_id,                  # id of saved query created in Client Portal
      v = 3                      # flex version
    )
  )

  # Error codes found here:
  # https://www.interactivebrokers.com/campus/ibkr-api-page/flex-web-service/#error-codes

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

  # Send the GET request to retrieve the statement
  receive_url <- httr::GET(
    url = paste0(
      "https://ndcdyn.interactivebrokers.com/AccountManagement/",
      "FlexWebService/GetStatement"
    ),
    query = list(
      t = ibkr_flex_web_token,
      q = ref_code,
      v = 3
    ),
    config = httr::config(followlocation = TRUE)
  )

  # Check if request was successful
  if (httr::status_code(receive_url) != 200) {
    stop("Failed to retrieve statement: ", httr::status_code(receive_url))
  }

  return(httr::content(receive_url, options = c('HUGE')))

}
