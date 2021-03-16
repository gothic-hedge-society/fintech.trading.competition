#' Create Individual Report PDF & Excel Files
#'
#' @export
#'
create_performance_report_pdf_and_excel_files <- function(excel = FALSE){

  perf_reports      <- performance_reports()
  reports_directory <- file.path("inst", "performance_reports", semester)

  perf_reports$name %>%
    purrr::walk(
      function(name){
        file.path(reports_directory, name) %>% {
          if(!dir.exists(.)){dir.create(.)}
        }
      }
    )

  for(i in 1:nrow(perf_reports)){
    usethis::ui_info(paste0("Creating report for: ", perf_reports$name[i]))
    render_pdf(i, perf_reports)
    if(excel && any(perf_reports$name[i] != instructors)){
      render_excel(i, perf_reports)
    }
  }

}

