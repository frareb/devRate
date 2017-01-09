
test_that("devRateModel janisch_32 Phthorimaea operculella (Lepidoptera:Gelechiidae)", {
  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
                         13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
                         25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
  rawDevLarva <- matrix(c(10, 0.010, 10, 0.014, 10, 0.019, 13, 0.034, 15, 0.024,
                          15.5, 0.029, 15.5, 0.034, 15.5, 0.039, 17, 0.067, 20, 0.050, 25, 0.076,
                          25, 0.056, 30, 0.0003, 35, 0.0002), ncol = 2, byrow = TRUE)
  rawDevPupa <- matrix(c(10, 0.001, 10, 0.008, 10, 0.012, 13, 0.044, 15, 0.017,
                         15, 0.044, 15.5, 0.039, 16, 0.034, 15.5, 0.037, 16, 0.051, 17, 0.051,
                         20, 0.080, 20, 0.092, 25, 0.102, 25, 0.073, 30, 0.005,
                         35, 0.0002), ncol = 2, byrow = TRUE)
  mEggs <- devRateModel(eq = janisch_32, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
                        startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))
  mLarva <- devRateModel(eq = janisch_32, temp = rawDevLarva[,1], devRate = rawDevLarva[,2],
                         startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))
  mPupa <- devRateModel(eq = janisch_32, temp = rawDevPupa[,1], devRate = rawDevPupa[,2],
                        startValues = list(Dmin = 5, Topt = 30, aa = 0.2, bb = 0.1))

  expect_is(mEggs, "nls")
  expect_is(mLarva, "nls")
  expect_is(mPupa, "nls")
  sapply(stats::predict(mEggs), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mLarva), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mPupa), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mEggs), function(i){expect_lt(object = i, expected = 1)})
  sapply(stats::predict(mLarva), function(i){expect_lt(object = i, expected = 1)})
  sapply(stats::predict(mPupa), function(i){expect_lt(object = i, expected = 1)})

})

test_that("devRateModel davidson_44 Phthorimaea operculella (Lepidoptera:Gelechiidae)", {
  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
                         13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
                         25, 0.200, 30, 0.200, 30, 0.180), ncol = 2, byrow = TRUE)
  rawDevLarva <- matrix(c(10, 0.010, 10, 0.014, 10, 0.019, 13, 0.034, 15, 0.024,
                          15.5, 0.029, 15.5, 0.034, 15.5, 0.039, 17, 0.067, 20, 0.050, 25, 0.076,
                          25, 0.056), ncol = 2, byrow = TRUE)
  rawDevPupa <- matrix(c(10, 0.001, 10, 0.008, 10, 0.012, 13, 0.044, 15, 0.017,
                         15, 0.044, 15.5, 0.039, 16, 0.034, 15.5, 0.037, 16, 0.051, 17, 0.051,
                         20, 0.080, 20, 0.092, 25, 0.102, 25, 0.073), ncol = 2, byrow = TRUE)
  mEggs <- devRateModel(eq = davidson_44, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
                        startValues = list(aa = 5, bb = -0.2, K = 0.3))
  mLarva <- devRateModel(eq = davidson_44, temp = rawDevLarva[,1], devRate = rawDevLarva[,2],
                         startValues = list(aa = 5, bb = -0.25, K = 0.08))
  mPupa <- devRateModel(eq = davidson_44, temp = rawDevPupa[,1], devRate = rawDevPupa[,2],
                        startValues = list(aa = 5, bb = -0.2, K = 0.3))

  expect_is(mEggs, "nls")
  expect_is(mLarva, "nls")
  expect_is(mPupa, "nls")
  sapply(stats::predict(mEggs), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mLarva), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mPupa), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mEggs), function(i){expect_lt(object = i, expected = 1)})
  sapply(stats::predict(mLarva), function(i){expect_lt(object = i, expected = 1)})
  sapply(stats::predict(mPupa), function(i){expect_lt(object = i, expected = 1)})
})

test_that("devRateModel campbell_74 Phthorimaea operculella (Lepidoptera:Gelechiidae)", {
  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
                         13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
                         25, 0.200, 30, 0.200, 30, 0.180), ncol = 2, byrow = TRUE)
  rawDevLarva <- matrix(c(10, 0.010, 10, 0.014, 10, 0.019, 13, 0.034, 15, 0.024,
                          15.5, 0.029, 15.5, 0.034, 15.5, 0.039, 17, 0.067, 20, 0.050, 25, 0.076,
                          25, 0.056), ncol = 2, byrow = TRUE)
  rawDevPupa <- matrix(c(10, 0.001, 10, 0.008, 10, 0.012, 13, 0.044, 15, 0.017,
                         15, 0.044, 15.5, 0.039, 16, 0.034, 15.5, 0.037, 16, 0.051, 17, 0.051,
                         20, 0.080, 20, 0.092, 25, 0.102, 25, 0.073), ncol = 2, byrow = TRUE)
  mEggs <- devRateModel(eq = campbell_74, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
                        startValues = list(aa = 0, bb = 0))
  mLarva <- devRateModel(eq = campbell_74, temp = rawDevLarva[,1], devRate = rawDevLarva[,2],
                         startValues = list(aa = 0, bb = 0))
  mPupa <- devRateModel(eq = campbell_74, temp = rawDevPupa[,1], devRate = rawDevPupa[,2],
                        startValues = list(aa = 0, bb = 0))

  expect_is(mEggs, "nls")
  expect_is(mLarva, "nls")
  expect_is(mPupa, "nls")
  sapply(stats::predict(mEggs), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mLarva), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mPupa), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mEggs), function(i){expect_lt(object = i, expected = 1)})
  sapply(stats::predict(mLarva), function(i){expect_lt(object = i, expected = 1)})
  sapply(stats::predict(mPupa), function(i){expect_lt(object = i, expected = 1)})
})

test_that("devRateModel stinner_74 Phthorimaea operculella (Lepidoptera:Gelechiidae)", {
  rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
                         13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
                         25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
  rawDevLarva <- matrix(c(10, 0.010, 10, 0.014, 10, 0.019, 13, 0.034, 15, 0.024,
                          15.5, 0.029, 15.5, 0.034, 15.5, 0.039, 17, 0.067, 20, 0.050, 25, 0.076,
                          25, 0.056, 30, 0.0003, 35, 0.0002), ncol = 2, byrow = TRUE)
  rawDevPupa <- matrix(c(10, 0.001, 10, 0.008, 10, 0.012, 13, 0.044, 15, 0.017,
                         15, 0.044, 15.5, 0.039, 16, 0.034, 15.5, 0.037, 16, 0.051, 17, 0.051,
                         20, 0.080, 20, 0.092, 25, 0.102, 25, 0.073, 30, 0.005,
                         35, 0.0002), ncol = 2, byrow = TRUE)
  mEggs <- devRateModel(eq = stinner_74, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
                        startValues = list(list(C = 0.1, k1 = 4, k2 = -0.2), list(Topt = 30)))
  mLarva <- devRateModel(eq = stinner_74, temp = rawDevLarva[,1], devRate = rawDevLarva[,2],
                         startValues = list(list(C = 0.08, k1 = 3, k2 = -0.2), list(Topt = 25)))
  mPupa <- devRateModel(eq = stinner_74, temp = rawDevPupa[,1], devRate = rawDevPupa[,2],
                        startValues = list(list(C = 0.1, k1 = 4, k2 = -0.2), list(Topt = 25)))

  expect_is(mEggs[[1]], "nls")
  expect_is(mLarva[[1]], "nls")
  expect_is(mPupa[[1]], "nls")
  sapply(stats::predict(mEggs[[1]]), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mLarva[[1]]), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mPupa[[1]]), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mEggs[[1]]), function(i){expect_lt(object = i, expected = 1)})
  sapply(stats::predict(mLarva[[1]]), function(i){expect_lt(object = i, expected = 1)})
  sapply(stats::predict(mPupa[[1]]), function(i){expect_lt(object = i, expected = 1)})
  expect_is(mEggs[[2]], "nls")
  expect_is(mLarva[[2]], "nls")
  expect_is(mPupa[[2]], "nls")
  sapply(stats::predict(mEggs[[2]]), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mLarva[[2]]), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mPupa[[2]]), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(mEggs[[2]]), function(i){expect_lt(object = i, expected = 1)})
  sapply(stats::predict(mLarva[[2]]), function(i){expect_lt(object = i, expected = 1)})
  sapply(stats::predict(mPupa[[2]]), function(i){expect_lt(object = i, expected = 1)})
})

test_that("devRateModel logan6_76 Episimus utilis (Lepidoptera:Tortricidae)", {
  xx <- seq(from = 0, to = 33, by = 1)
  yy <- c(0.0005252046, 0.0002251286, 0.0006460717, 0.0017880213, 0.0015611891,
          0.0012853886, 0.0010841702, 0.0000000000, 0.0029119885, 0.0051535582,
          0.0049455628, 0.0049839564, 0.0042565719, 0.0075039314, 0.0073359365,
          0.0069161169, 0.0104414407, 0.0109888804, 0.0121261595, 0.0153213235,
          0.0147817972, 0.0193366467, 0.0202140539, 0.0221893465, 0.0260741628,
          0.0275310077, 0.0298545694, 0.0317042536, 0.0332244901, 0.0302047526,
          0.0292701892, 0.0267738132, 0.0155003324, 0.0011891955)

  testNLS <- devRateModel(eq = logan6_76, temp = xx, devRate = yy,
               startValues = list(phi = 0.000800, bb = 0.169, Tmax = 33.003, deltaT = 3.895))

  expect_is(testNLS, "nls")
  sapply(stats::predict(testNLS), function(i){expect_gt(object = i, expected = 0)})
  sapply(stats::predict(testNLS), function(i){expect_lt(object = i, expected = 1)})

})



# phi = 0.000800
# bb = 0.169000
# Tmax = 33.00300
# deltaT = 3.895000
# xx <- seq(from = 0, to = Tmax, by = 1)
# rnorm(n = length(xx), mean = eval({x = xx; parse(text = logan6_76$eqAlt[1])}), sd = 0.001) # 0.00000001
# yy[yy < 0] <- 0



# logan10_76
# sharpeDeMichele_77
# analytis_77
# schoolfield_81
# schoolfieldHigh_81
# schoolfieldLow_81
# taylor_81
# wang_82
# poly2
# harcourtYee_82
# poly4
# ratkowsky_82
# rootsq_82
# bieri1_83
# hilbertLogan_83
# wagner_88
# lamb_92
# lactin1_95
# lactin2_95
# beta_95
# wangengel_98
# briere1_99
# briere2_99
# bayoh_03
# kontodimas_04
# damos_08
# damos_11
# hansen_11
# shi_11
# perf2_11
# regniere_12



### when no data in BDD
# plot(rawDevLarva[,1], rawDevLarva[,2], lwd = 2) ### logan 6
# phi <- 0.4
# bb <- 0.14
# Tmax <- 30
# deltaT <- 7.1
# points(x = 0:50, y = eval({x = 0:50; parse(text = logan6_76$eqAlt[1])}), type = 'l', lwd = 2, col = 2)


