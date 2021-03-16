#' Daily Rtns
#' @export
#'
daily_rtns <- function(value_xts){

  numerator               <- value_xts
  storage.mode(numerator) <- "numeric"
  colnames(numerator)     <- "numerator"

  denominator               <- lag(value_xts)
  storage.mode(denominator) <- "numeric"
  colnames(denominator)     <- "denominator"

  zoo::merge.zoo(numerator, denominator)[-1,] %>% {
    list(
      xts::xts(
        log(zoo::coredata(.$numerator) / zoo::coredata(.$denominator)),
        order.by = zoo::index(.)
      ) %>%
        magrittr::set_colnames("daily_rtn")
    )
  }

}
