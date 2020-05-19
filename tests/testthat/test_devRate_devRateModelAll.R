test_that("devRateModelAll", {
  myDf <- exTropicalMoth$raw$larva
  fitAll <- devRateModelAll(df = myDf,
                            algo = "LM",
                            control = list(maxiter = 500))
  expect_is(fitAll, "list")
})
