# ------------------------------------------------------------------------------
# Tests for devRateInfo
# ------------------------------------------------------------------------------
test_that("print devRateModel",{
  res <- devRateInfo(eq = taylor_81)
  expect_equal(
    object = res,
    expected = NULL
  )
})
# ------------------------------------------------------------------------------
# Tests for XXXX
# ------------------------------------------------------------------------------








# devtools::test()
# covr::package_coverage()
# covr::report()
# devtools::document()







