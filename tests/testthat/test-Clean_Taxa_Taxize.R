# testthat unit test


test_that("The function returns the correct number of rows", {
  expect_equal(nrow(Clean_Taxa_Taxize(Taxons = c("Canis lupus", "C. lupus"))), 1)
})

test_that("The function returns the correct columns", {
  expect_equal(colnames(Clean_Taxa_Taxize(Taxons = c("Canis lupus", "C. lupus"))), c( "Taxa", "score", "matched_name2", "TaxaID"))
})
