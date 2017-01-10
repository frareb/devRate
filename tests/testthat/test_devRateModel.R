
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
  expect_is(mEggs[[2]], "nls")
  expect_is(mLarva[[2]], "nls")
  expect_is(mPupa[[2]], "nls")
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
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel logan10_76 Cydia pomonella (Lepidoptera:Tortricidae)", {
  xx <- seq(from = 0, to = 36, by = 1)
  yy <- c(0.0007729213, 0.0001253524, 0.0007717062, 0.0010629336, 0.0014643530,
          0.0004597164, 0.0013529063, 0.0016926055, 0.0020639653, 0.0029521922,
          0.0015773307, 0.0026982888, 0.0045119329, 0.0046683817, 0.0052173935,
          0.0060213544, 0.0077865111, 0.0087443970, 0.0092949713, 0.0107126719,
          0.0116619374, 0.0136489680, 0.0146848849, 0.0162357028, 0.0171430946,
          0.0178698728, 0.0194753703, 0.0191674932, 0.0193544746, 0.0172297562,
          0.0153346383, 0.0126856251, 0.0083347235, 0.0016592859, 0.0000000000,
          0.0000000000, 0.0000000000)
  testNLS <- devRateModel(eq = logan10_76, temp = xx, devRate = yy,
                          startValues = list(alpha = 0.08450, bb = 0.173400, cc = 102.2182, Tmax = 38.7735, deltaT = 6.40240))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel sharpeDeMichele_77 Drosophila melanogaster (Diptera:Drosophilidae)", {
  xx <- seq(from = 0, to = 36, by = 1)
  yy <- c(0.000000000, 0.000000000, 0.000000000, 0.007279878, 0.010780900,
          0.002023176, 0.004599434, 0.030801276, 0.003611483, 0.022008346,
          0.022513837, 0.029477274, 0.038279933, 0.049745442, 0.020752379,
          0.042697777, 0.044835351, 0.055853683, 0.072617792, 0.064999242,
          0.068376525, 0.067887024, 0.087232515, 0.096994747, 0.069810120,
          0.099206646, 0.094854215, 0.098206028, 0.090197283, 0.101336347,
          0.090258319, 0.071312951, 0.077009260, 0.068088433, 0.064604557,
          0.037714537, 0.053549824)
  testNLS <- devRateModel(eq = sharpeDeMichele_77, temp = xx, devRate = yy,
                          startValues = list(aa = 19.43, bb = 10490, cc = -156.9, dd = -44373, ff = 226.6, gg = 69113))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel analytis_77 Nephus bisignatus (Coleoptera:Coccinellidae)", {
  xx <- seq(from = 5, to = 33, by = 1)
  yy <- c(0.003624885, 0.000000000, 0.001950942, 0.001846166, 0.001969783,
          0.004169708, 0.003567134, 0.004989870, 0.007621175, 0.009034580,
          0.007648763, 0.011395174, 0.014138975, 0.014511722, 0.018423493,
          0.019787992, 0.020790183, 0.023420162, 0.024488401, 0.028266525,
          0.031751702, 0.031811957, 0.032410839, 0.035262203, 0.035145591,
          0.038331467, 0.038510757, 0.036173298, 0.023066068)
  testNLS <- devRateModel(eq = analytis_77, temp = xx, devRate = yy,
                          startValues = list(aa = 1.0e-04, bb = 1.7766, cc = 0.1740, Tmin = 4.9125, Tmax = 33.0781))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel schoolfield_81 Melanoplus sanguinipes (Orthoptera:Acrididae)", {
  xx <- seq(from = 0, to = 50, by = 1)
  yy <- c(0.0005738992, 0.0037753270, 0.0019711243, 0.0013993445, 0.0000000000,
          0.0018258747, 0.0015857669, 0.0000000000, 0.0028721502, 0.0053867347,
          0.0068066704, 0.0071001306, 0.0066717568, 0.0048745851, 0.0080201138,
          0.0067507888, 0.0085149901, 0.0111779350, 0.0091441154, 0.0120987686,
          0.0147621464, 0.0194343995, 0.0198100212, 0.0193765233, 0.0238609486,
          0.0232133053, 0.0198710750, 0.0292669974, 0.0252871388, 0.0282169641,
          0.0349077453, 0.0390198824, 0.0391263725, 0.0438380968, 0.0453967965,
          0.0487420375, 0.0521477311, 0.0523109706, 0.0553442142, 0.0544577634,
          0.0566073337, 0.0527249056, 0.0529586715, 0.0537847167, 0.0489292577,
          0.0486422935, 0.0449730753, 0.0400146305, 0.0346307260, 0.0352004855,
          0.0263990779)

  testNLS <- devRateModel(eq = schoolfield_81, temp = xx, devRate = yy,
                          startValues = list(p25 = 0.0455, aa = 8814.36, bb = -14877.95, cc = 298.81, dd = 47258.52, ee = 316.695))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel schoolfieldHigh_81 Melanoplus sanguinipes (Orthoptera:Acrididae)", {
  xx <- seq(from = -5, to = 30, by = 1)
  yy <- c(0.0005315358, 0.0081099947, 0.0166569706, 0.0045348182, 0.0023591495,
          0.0085177076, 0.0122157105, 0.0193775061, 0.0037427740, 0.0241785522,
          0.0147844460, 0.0216203187, 0.0246449326, 0.0194719553, 0.0147972427,
          0.0261072237, 0.0265193061, 0.0270178896, 0.0252153311, 0.0241325856,
          0.0239119132, 0.0379386934, 0.0186132170, 0.0282008402, 0.0306502169,
          0.0354469603, 0.0277423794, 0.0374357233, 0.0381423032, 0.0473560141,
          0.0419966062, 0.0522594310, 0.0473526517, 0.0509561741, 0.0474212992,
          0.0567305166)
  testNLS <- devRateModel(eq = schoolfieldHigh_81, temp = xx, devRate = yy,
                          startValues = list(p25 = 0.0455, aa = 8814.36, dd = 47258.52, ee = 316.695))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel schoolfieldLow_81 Melanoplus sanguinipes (Orthoptera:Acrididae)", {
  xx <- seq(from = 15, to = 40, by = 1)
  yy <- c(0.007408536, 0.008365150, 0.009468100, 0.010714038, 0.012081506,
          0.013243063, 0.014691625, 0.016625860, 0.018090679, 0.020252121,
          0.022297409, 0.024531523, 0.026979481, 0.029683609, 0.032354563,
          0.035106640, 0.038020835, 0.041563266, 0.044988835, 0.048447192,
          0.052393433, 0.056512979, 0.060679163, 0.065064254, 0.069769445,
          0.074526602)
  testNLS <- devRateModel(eq = schoolfieldLow_81, temp = xx, devRate = yy,
                          startValues = list(p25 = 0.0455, aa = 8814.36, bb = -14877.95, cc = 298.81))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel taylor_81 Geocoris articolor (Hemiptera:Lygaeidae)", {
  xx <- seq(from = 0, to = 50, by = 1)
  yy <- c(0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00,
          3.243648e-03, 7.814825e-04, 5.814596e-03, 9.819378e-03, 7.814990e-03,
          0.000000e+00, 5.430783e-05, 1.056332e-02, 0.000000e+00, 1.295533e-02,
          0.000000e+00, 0.000000e+00, 7.823721e-03, 1.247843e-03, 5.706782e-03,
          8.286514e-03, 7.482038e-03, 2.459962e-02, 5.102975e-03, 3.485625e-02,
          3.060154e-02, 1.493131e-02, 2.509750e-02, 2.822221e-02, 2.497632e-02,
          3.772156e-02, 3.043415e-02, 3.034768e-02, 3.756987e-02, 5.572242e-02,
          5.400489e-02, 5.740899e-02, 4.994192e-02, 5.559589e-02, 5.372400e-02,
          4.867726e-02, 5.998619e-02, 4.504179e-02, 2.689224e-02, 4.373311e-02,
          4.414157e-02, 3.948465e-02, 3.335472e-02, 4.921421e-02, 3.708899e-02,
          1.838826e-02)
  testNLS <- devRateModel(eq = taylor_81, temp = xx, devRate = yy,
                          startValues = list(Rm = 0.0550, Tm = 37.2000, To = 8.8000))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel wang_82 Nurscia albofasciata (Araneae:Titanoecidae)", {
  xx <- seq(from = 10, to = 40, by = 1)
  yy <- c(0.000000e+00, 0.000000e+00, 0.000000e+00, 2.189162e-04, 2.153789e-03,
          2.826689e-03, 3.093816e-03, 4.513258e-03, 3.204745e-03, 6.003303e-03,
          4.793105e-03, 7.987115e-03, 1.027355e-02, 1.158569e-02, 1.157333e-02,
          1.267494e-02, 1.388047e-02, 1.725236e-02, 1.623073e-02, 1.928007e-02,
          2.160226e-02, 2.136966e-02, 2.274513e-02, 2.475865e-02, 2.655935e-02,
          2.790608e-02, 2.371863e-02, 2.379099e-02, 2.017513e-02, 1.228895e-02,
          5.543522e-05)
  testNLS <- devRateModel(eq = wang_82, temp = xx, devRate = yy,
                          startValues = list(K = 0.045, r = 0.15, T0 = 31, TL = 13, TH = 40,  aa =2))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel poly2 Plodia interplInctella (Lepidoptera:Pyralidae)", {
  xx <- seq(from = 15, to = 40, by = 1)
  yy <- c(0.000000000, 0.000000000, 0.008714114, 0.009226288, 0.008974961,
          0.012786594, 0.022019003, 0.036118170, 0.029093556, 0.032973127,
          0.031510768, 0.042017463, 0.032786228, 0.042400972, 0.052407155,
          0.046048385, 0.044539889, 0.047925037, 0.051333902, 0.045657589,
          0.044514010, 0.041387678, 0.038145555, 0.048391190, 0.045908961,
          0.050747954)
  testNLS <- devRateModel(eq = poly2, temp = xx, devRate = yy,
                          startValues = list(a0 = -0.13003, a1 = 0.01022, a2 = -0.000144))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

test_that("devRateModel harcourtYee_82 Hypera postica (Coleoptera:Curculionidae)", {
  xx <- seq(from = 15, to = 40, by = 1)
  yy <- c(0.04787425, 0.07107279, 0.05511158, 0.07248420, 0.08392225, 0.08329395,
          0.07825801, 0.11772809, 0.14566401, 0.15290571, 0.17476144, 0.16426242,
          0.20056165, 0.18852016, 0.20116038, 0.23733901, 0.22235097, 0.22490219,
          0.25511832, 0.21187438, 0.24725272, 0.24015939, 0.25575154, 0.25208030,
          0.22387521, 0.22734355)
  testNLS <- devRateModel(eq = harcourtYee_82, temp = xx, devRate = yy,
                          startValues = list(a0 = 0.1434000, a1 = -0.028270, a2 = 1.8240e-03, a3 = -0.000026629))
  expect_is(testNLS, "nls")
  expect_gte(stats::cor(yy, stats::predict(testNLS)), 0.90)
})

# aa = 19.43
# bb = 10490
# cc = -156.9
# dd = -44373
# ff = 226.6
# gg = 69113
# yy <- rnorm(n = length(xx), mean = eval({x = xx; parse(text = taylor_81$eqAlt[1])}), sd = 0.01)
# yy[yy < 0] <- 0
# plot(xx, yy)

#
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
# plot(rawDevLarva[,1], rawDevLarva[,2], lwd = 2)
# phi <- 0.4
# bb <- 0.14
# Tmax <- 30
# deltaT <- 7.1
# points(x = 0:50, y = eval({x = 0:50; parse(text = logan6_76$eqAlt[1])}), type = 'l', lwd = 2, col = 2)
