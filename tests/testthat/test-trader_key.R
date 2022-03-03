# testthat::test_that(
#   "No duplicated tradernames", {
#     tradername_dup <- trader_key$tradername[duplicated(trader_key$tradername)]
#     testthat::expect(
#       length(tradername_dup) == 0,
#       failure_message = paste0(
#         "Duplicated tradernames: ",
#         paste0(tradername_dup, collapse = ", ")
#       )
#     )
#   }
# )
#
# testthat::test_that(
#   "No duplicated account IDs", {
#     id_dup <- trader_key$account_id[duplicated(trader_key$account_id)]
#     testthat::expect(
#       length(id_dup) == 0,
#       failure_message = paste0(
#         "Duplicated account IDs: ",
#         paste0(id_dup, collapse = ", ")
#       )
#     )
#   }
# )
#
# testthat::test_that(
#   "No tradername is missing", {
#     testthat::expect_true(!any(is.na(trader_key$tradername)))
#   }
# )
