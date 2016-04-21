devRateModel <- function(eq, temp, devRate, startValues, guessStartValues = list(FALSE, "", ""), ...){
  # cat(eq$name, "model by", eq$refShort, "\n\n")
  if(guessStartValues[[1]] == TRUE){
    if(guessStartValues[[2]] == ""){
      meanStartVal <- apply(eq$startVal[,5:(ncol(eq$startVal)-1)], MARGIN = 2, FUN = sum)
    } else {

      meanStartVal <- apply(eq$startVal[,5:(ncol(eq$startVal)-1)][(eq$startVal[,1] == guessStartValues[[2]] & eq$startVal[,4] == guessStartValues[[3]]),], MARGIN = 2, FUN = sum)
      # mettre une condition sur l'ordre et le stade et gérer exception si manquant avec warnings
    }
    guessSV <- as.list(as.data.frame(t(meanStartVal)))
    nls_devRate <- nls(eq[[1]], data = data.frame(rT = devRate, T = temp), start = guessSV, ...)
  } else {
    nls_devRate <- nls(eq[[1]], data = data.frame(rT = devRate, T = temp), start = startValues, ...)
  }


  # nls_devRate <- minpack.lm::nlsLM(eq[[1]], data = data.frame(rT = devRate, T = temp), start = startValues)
  return(nls_devRate)
}











xxx <- matrix(c(14.94699968663465, 0.06642066420664205,15.638002263904788, 0.08191881918819188,14.854908644407068, 0.10405904059040588,
15.938577193397585, 0.10073800738007377,17.023604724782082, 0.08800738007380071,16.802170534703613, 0.1212177121771218,
19.011875907320597, 0.1212177121771218,19.960925259165936, 0.15000000000000002,18.91930522424808, 0.16217712177121774,
19.95820729437797, 0.1688191881918819,20.954501269449434, 0.170479704797048,20.950344382126662, 0.19926199261992622,
22.938535624524356, 0.2330258302583026,23.760959793306775, 0.23856088560885613,23.366934839192414, 0.266789667896679,
25.92406006382421, 0.2612546125461255,26.872869595247078, 0.2916974169741698,23.837942148918888, 0.3055350553505536,
28.862739580602046, 0.3138376383763838,29.20776122839218, 0.32490774907749087,28.94531774607174, 0.3420664206642067,
30.983551516624356, 0.3293357933579336,32.76142024851791, 0.31937269372693733,31.37877557285105, 0.2928044280442805,
33.28678685400372, 0.28173431734317345,33.073426618148325, 0.2590405904059041,35.20455083233675, 0.20313653136531368,
33.045767329423725, 0.15055350553505537,35.82480638497893, 0.10848708487084868,34.60499977616761, 0.15442804428044282,
33.9878618890175, 0.2274907749077491,26.609067130532658, 0.31826568265682664,22.424760339457812, 0.19040590405904062,
17.31850390427648, 0.14612546125461257,17.19515626698728, 0.10018450184501843,14.207233623462752, 0.08856088560885605,
13.821602384134758, 0.05867158671586714,18.53239494266693, 0.14114391143911442,22.030975205765923, 0.21697416974169742,
25.142725127424583, 0.2712177121771218,27.8200803238535, 0.33321033210332107,30.547637928718974, 0.3476014760147602,
29.904279675379076, 0.3022140221402214,33.11035896320835, 0.30332103321033216,34.1211221037687, 0.20479704797047976,
32.92961430480856, 0.3547970479704798,30.111084819687015, 0.3702952029520296,28.825807235542026, 0.2695571955719558,
24.10398293757634, 0.2634686346863469,21.870935043839175, 0.12509225092250925), ncol = 2, byrow = TRUE)

sampleDataset <- data.frame(T = xxx[,1], rT = xxx[,2])


# devRateModel(eq = campbell_74, temp = sampleDataset$T, devRate = sampleDataset$rT, guessStartValues = list(TRUE, "Hemiptera", "all"))
# devRateModel(eq = campbell_74, temp = sampleDataset$T, devRate = sampleDataset$rT, guessStartValues = list(TRUE, "Gelechiidae", "all"))

tjanisch_32 <- devRateModel(eq = janisch_32, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0.01, bb = 0.01, Topt = 30, Tmin = 8))
tdavidson_44 <- devRateModel(eq = davidson_44, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(K = 0, aa = 0, bb = 0))
tcampbell_74 <- devRateModel(eq = campbell_74, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
stinner_74 <- devRateModel(eq = stinner_74, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(C = 0, k1 = 0, k2 = 0))
logan6_76 <- devRateModel(eq = logan6_76, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(phi = 0, bb = 0, Tmax = 0, deltaT = 2))
logan10_76 <- devRateModel(eq = logan10_76, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(alpha = 0.06, bb = 0.2, cc = 60, Tmax = 35, deltaT = 2))

sharpeDeMichele_77 <- devRateModel(eq = sharpeDeMichele_77, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
analytis_77 <- devRateModel(eq = analytis_77, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
schoolfield_81 <- devRateModel(eq = schoolfield_81, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
taylor_81 <- devRateModel(eq = taylor_81, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
poly2 <- devRateModel(eq = poly2, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
harcourtYee_82 <- devRateModel(eq = harcourtYee_82, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
poly4 <- devRateModel(eq = poly4, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
hilbertLogan_83 <- devRateModel(eq = hilbertLogan_83, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
lamb_92 <- devRateModel(eq = lamb_92, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
lactin1_95 <- devRateModel(eq = lactin1_95, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
lactin2_95 <- devRateModel(eq = lactin2_95, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
briere1_99 <- devRateModel(eq = briere1_99, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
briere2_99 <- devRateModel(eq = briere2_99, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
kontodimas_04 <- devRateModel(eq = kontodimas_04, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
damos_08 <- devRateModel(eq = damos_08, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))
damos_11 <- devRateModel(eq = damos_11, temp = sampleDataset$T, devRate = sampleDataset$rT, start = list(aa = 0, bb = 0))



