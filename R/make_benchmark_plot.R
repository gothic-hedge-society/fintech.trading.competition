#' Create a Benchmark Plot
#'
#' @export
#'
make_benchmark_plot <- function(portfolio_returns, benchmark_asset, x_lab){

  benchmark_data <- dplyr::inner_join(portfolio_returns, benchmark_asset) %>%
    dplyr::select(-Date) %>%
    magrittr::set_colnames(c("x", "y"))

  lm_for_trendline <- lm(benchmark_data$y ~ benchmark_data$x)

  ggplot2::ggplot(data = benchmark_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab("Portfolio Daily Log Returns") +
    ggplot2::geom_abline(
      slope     = lm_for_trendline$coefficients[2],
      intercept = lm_for_trendline$coefficients[1],
      linetype  = "longdash",
      color     = "#d15c79",
      size      = 2
    ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(scale = 1, accuracy = 0.01)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1, accuracy = 0.01)
    ) +
    ggplot2::theme(
      axis.text    = ggplot2::element_text(size = 12, face = "bold"),
      axis.title   = ggplot2::element_text(size = 12, face = "bold"),
      panel.border = ggplot2::element_rect(
        color = "black", fill = NA, size = 1
      )
    ) +
    ggplot2::geom_label(
      data = tibble::tibble(
        xpos = -Inf,
        ypos =  Inf,
        annotateText = paste0(
          "Alpha = ",
          round(
            lm_for_trendline$coefficients[1]*252,
            digits = 3
          ),
          "; Beta = ",
          round(
            lm_for_trendline$coefficients[2],
            digits = 3
          )
        ),
        hjustvar = "inward",
        vjustvar = "inward"
      ),
      ggplot2::aes(
        x     = xpos,
        y     = ypos,
        hjust = hjustvar,
        vjust = vjustvar,
        label = annotateText
      )
    )

}
