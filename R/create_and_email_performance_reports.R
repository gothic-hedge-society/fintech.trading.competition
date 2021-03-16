#' Email Performance Reports
#'
#' @export
#'
create_and_email_performance_reports <- function(
  subject     = "2020 FINTECH Trading Competition Update",
  body        = "Updated standings attached!",
  attachments = TRUE
){

  library(RDCOMClient)

  OutApp  <- COMCreate("Outlook.Application")

  if(attachments){
    create_performance_report_pdf_and_excel_files()
  }

  participant_dirs <- list.dirs(
    file.path(
      "C:", "Users", "Jake", "Desktop", "fintech.trading.competition", "inst",
      "performance_reports", semester
    ),
    recursive = FALSE
  ) %>%
    sort()

  for(participant_dir in participant_dirs){
    usethis::ui_info(paste0("Emailing: ", basename(participant_dir)))
    outMail <- OutApp$CreateItem(0)
    outMail[["To"]]      <- list.files(participant_dir, pattern = ".pdf$") %>%
      basename() %>%
      gsub(".pdf$", replacement = "", .)
    outMail[["subject"]] <- subject
    outMail[["body"]]    <- body
    if(attachments){
      outMail[["attachments"]]$Add(
        list.files(participant_dir, pattern = ".pdf$", full.names = TRUE)
      )
    }

    try(outMail$Send())

    Sys.sleep(3)
  }

}
