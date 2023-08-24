new_standings <- standings %>%
  dplyr::arrange(dplyr::desc(return)) %>% {
    n <- 10
    n_brackets  <- nrow(.) %/% n
    remainder <- nrow(.) %% n
    bracket <- rep(n_brackets, n + remainder)
    for (i in (n_brackets-1):1){
      bracket <- c(rep(i, n), bracket)
    }
    .$bracket <- bracket
    .
  } %>%
  dplyr::select(bracket, dplyr::everything()) %>%
  dplyr::group_by(bracket) %>%
  dplyr::arrange(dplyr::desc(Sharpe), .by_group = TRUE) %>%
  dplyr::rename("Old Rank" = "Rank") %>%
  dplyr::ungroup() %>% {
    dplyr::mutate(., "New Rank" = 1:nrow(.))
  } %>%
  dplyr::select(
    `New Rank`, `Old Rank`, bracket, tradername, school, country, return,
    volatility, Sharpe
  )

print(
  dplyr::select(
    standings, Rank, tradername, school, country, return, volatility, Sharpe
  )
)
print(new_standings)
