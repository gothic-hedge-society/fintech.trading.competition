
chunks <- list(
  "cumulative_eod_excess_returns", "cumulative_vol", "eod_account_value",
  "eod_excess_returns", "eod_returns", "sharpes"
)

for(chunk_1 in 1:(length(chunks)-1)){
  for(chunk_2 in (chunk_1+1):length(chunks)){
    testthat::test_that(
      paste0("Columns equal: ", chunk_1, ", ", chunk_2),{
        testthat::expect_equal(
          colnames(get0(chunks[[chunk_1]])),
          colnames(get0(chunks[[chunk_2]]))
        )
      }
    )
  }
}
