
# Get Risk-free Rate (Daily CMT) -------------------------------------------------
get_usdt_data <- function(url){
  xml2::read_html(url) %>%
    rvest::html_node(".t-chart") %>%
    rvest::html_table() %>%
    tibble::as_tibble() %>%
    dplyr::na_if("N/A") %>% {
      xts::xts(
        dplyr::select(., -"Date"),
        order.by = as.Date(.$Date, format = "%m/%d/%y")
      )
    } %>% {
      storage.mode(.) <- "numeric"
      colnames(.) <- gsub(" ", "_", colnames(.))
      .
    }
}


# Data Munging -----------------------------------------------------------------
# remove cols that all equal "", NA, 0, or "<NA>"
remove_empty_cols <- function(a_tibble){
  for(col_name in colnames(a_tibble)){
    if(
      all(a_tibble[col_name] == "") ||
      all(is.na(a_tibble[col_name])) ||
      all(a_tibble[col_name] == 0) ||
      all(a_tibble[col_name] == "<NA>")
    ){
      a_tibble <- a_tibble[, setdiff(colnames(a_tibble), col_name)]
    }
  }
  a_tibble
}

# Supress cat, by Hadley Wickham:
#   http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# Rendering --------------------------------------------------------------------
render_pdf <- function(i, perf_reports){
  if(any(perf_reports$name[i] == instructors)){

    rmarkdown::render(
      file.path("inst", "instructor_report.Rmd"),
      output_dir  = file.path(
        "inst", "performance_reports", semester, perf_reports$name[i]
      ),
      output_file = paste0(perf_reports$primaryEmail[i], ".pdf"),
      params      = list(perf_reports = perf_reports),
      quiet       = TRUE
    )
  } else {
    rmarkdown::render(
      file.path("inst", "student_report.Rmd"),
      output_dir = file.path(
        "inst", "performance_reports", semester, perf_reports$name[i]
      ),
      output_file = paste0(perf_reports$primaryEmail[i], ".pdf"),
      params      = list(i = i, perf_reports = perf_reports),
      quiet       = TRUE
    )
  }
}

# render_excel <- function(i, perf_reports){
#
#   # perf_reports <- performance_reports()
#   # i            <- 9
#
#   wb <- openxlsx::createWorkbook()
#
#   openxlsx::addWorksheet(wb, perf_reports$name[i])
#
#   student_sheet  <- perf_reports$name[i]
#
#   currency_style <- openxlsx::createStyle(
#     numFmt = "currency", halign = "center"
#   )
#
#   header_style <- openxlsx::createStyle(
#     halign         = "center",
#     valign         = "center",
#     border         = "TopBottomLeftRight",
#     borderStyle    = "medium",
#     textDecoration = "bold",
#     fgFill         = "#FFF2CC",
#     wrapText       = TRUE
#   )
#
#   percent_style <- openxlsx::createStyle(
#     numFmt = "0.00000%", halign = "center"
#   )
#
#   # Raw Data Cells & Styles
#   historical_portfolio_value <- zoo::merge.zoo(
#     perf_reports[i, "statement"][[1]][[1]]$total,
#     zoo::index(perf_reports[1, "statement"][[1]][[1]]) %>%
#       risk_free_rate(),
#     all = c(TRUE, FALSE)
#   ) %>%
#     zoo::merge.zoo(
#       gspc(zoo::index(perf_reports[1, "statement"][[1]][[1]]))$Close
#     ) %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column("Date") %>%
#     dplyr::mutate(
#       "Portfolio Value"            = round(as.numeric(total), digits = 2),
#       "SP500 Benchmark"            = round(as.numeric(Close), digits = 2),
#       "3-mo TBill Rate, %/cal. yr" = as.numeric(`3_mo`)
#     ) %>%
#     dplyr::select(
#       Date, `Portfolio Value`, `SP500 Benchmark`, `3-mo TBill Rate, %/cal. yr`
#     )
#   openxlsx::writeData(
#     wb, sheet = perf_reports$name[i], x = historical_portfolio_value
#   )
#   openxlsx::addStyle(
#     wb,
#     sheet = student_sheet,
#     style = header_style,
#     rows  = 1,
#     cols  = 1:4
#   )
#   openxlsx::addStyle(
#     wb,
#     sheet = student_sheet,
#     style = currency_style,
#     rows  = 2:(nrow(historical_portfolio_value) + 1),
#     cols  = 2
#   )
#   openxlsx::addStyle(
#     wb,
#     sheet = student_sheet,
#     style = currency_style,
#     rows  = 2:(nrow(historical_portfolio_value) + 1),
#     cols  = 3
#   )
#   openxlsx::addStyle(
#     wb,
#     sheet = student_sheet,
#     style = percent_style,
#     rows  = 2:(nrow(historical_portfolio_value) + 1),
#     cols  = 4
#   )
#
#   # Log Returns and Styles
#   openxlsx::addStyle(
#     wb,
#     sheet = student_sheet,
#     style = header_style,
#     rows  = 1:2,
#     cols  = 6
#   )
#   openxlsx::mergeCells(wb, sheet = student_sheet, cols = 6, rows = 1:2)
#   openxlsx::writeData(
#     wb,
#     sheet    = student_sheet,
#     x        = "Portfolio CCR, %/trd. day",
#     startCol = 6
#   )
#   openxlsx::writeFormula(
#     wb,
#     sheet = student_sheet,
#     x     = paste0(
#       "ln(B",
#       3:(nrow(historical_portfolio_value) + 1),
#       "/B",
#       2:nrow(historical_portfolio_value),
#       ")"
#     ),
#     startCol = 6,
#     startRow = 3
#   )
#   openxlsx::addStyle(
#     wb,
#     sheet = student_sheet,
#     style = percent_style,
#     rows  = 3:(nrow(historical_portfolio_value) + 1),
#     cols  = 6
#   )
#   openxlsx::addStyle(
#     wb,
#     sheet = student_sheet,
#     style = header_style,
#     rows  = 1:2,
#     cols  = 7
#   )
#   openxlsx::mergeCells(wb, sheet = student_sheet, cols = 7, rows = 1:2)
#   openxlsx::writeData(
#     wb,
#     sheet    = student_sheet,
#     x        = "Benchmark CCR, %/trd. day",
#     startCol = 7
#   )
#   openxlsx::writeFormula(
#     wb,
#     sheet = student_sheet,
#     x     = paste0(
#       "ln(C",
#       3:(nrow(historical_portfolio_value) + 1),
#       "/C",
#       2:nrow(historical_portfolio_value),
#       ")"
#     ),
#     startCol = 7,
#     startRow = 3
#   )
#   openxlsx::addStyle(
#     wb,
#     sheet = student_sheet,
#     style = percent_style,
#     rows  = 3:(nrow(historical_portfolio_value) + 1),
#     cols  = 7
#   )
#
#   openxlsx::setColWidths(wb, sheet = student_sheet, cols = 2, widths = 14)
#
#   output_file <- file.path(
#     "inst", "performance_reports", semester,
#     perf_reports$name[i], paste0(perf_reports$name[i], ".xlsx")
#   )
#
#   openxlsx::saveWorkbook(wb, file = output_file, overwrite = TRUE)
#
# }
