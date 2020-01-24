# Tests for devRateModel

myT <- 5:15
myDev <- -0.05 + rnorm(n = length(myT), mean = myT, sd = 1) * 0.01
myDf <- data.frame(myT, myDev)

test_that("Returns NLS object",{
  res <- devRateModel(eq = campbell_74, df = myDf)
  expect_equal(
    object = class(res),
    expected = "nls")
  res2 <- devRateModel(eq = campbell_74, temp = myT, devRate = myDev)
  expect_equal(
    object = class(res2),
    expected = "nls")
})

test_that("NLS stopCode 0 (convergence)",{
  res <- devRateModel(eq = campbell_74, df = myDf)
  expect_equal(
    object = res$convInfo$stopCode,
    expected = 0)
})

# stinner ----------------------------------------------------------------------

# rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
#   13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
#   25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
#
# test_that("",{
#   res <- devRateModel(
#     eq = stinner_74,
#     temp = rawDevEggs[,1],
#     devRate = rawDevEggs[,2],
#     startValues = list(
#       list(C = 0.1, k1 = 4, k2 = -0.2),
#       list(Topt = 30)
#     )
#   )
# })













