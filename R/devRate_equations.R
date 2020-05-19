### y ~ I(x)
getCSV <- function(myCSV){
  return(data.frame(
    ordersp = myCSV$ORDER,
    familysp = myCSV$FAMILY,
    genussp = myCSV$GENUS,
    species = myCSV$SPECIES,
    genSp = paste(myCSV$GENUS, myCSV$SPECIES),
    stage = myCSV$STAGE,
    # param = as.list(myCSV[,7:(ncol(myCSV)-1)]),
    param = as.list(myCSV[,7:(which(names(myCSV)=="contributor") - 1)]),
    # DOI = myCSV$DOI,
    # journal = myCSV$journal,
    # year = myCSV$year,
    ref = myCSV$REF
  ))
}

modelsBDD <- read.table("./inst/data/models.csv", header = TRUE, sep = ';', dec = ',')

for (i in 1:nrow(modelsBDD)){
  if(!is.na(modelsBDD$eq2[i])){
    eqV <- c(formula(as.character(modelsBDD$eq[i])), formula(as.character(modelsBDD$eq2[i])))
  } else {
    eqV <- formula(as.character(modelsBDD$eq[i]))
  }
  if(!is.na(modelsBDD$eq2[i])){
    eqAltV <- c(as.character(modelsBDD$eqAlt[i]), as.character(modelsBDD$eqAlt2[i]))
  } else {
    eqAltV <- as.character(modelsBDD$eqAlt[i])
  }
  nameV <- as.character(modelsBDD$name[i])
  refV <- as.character(modelsBDD$ref[i])
  refShortV <- as.character(modelsBDD$refShort[i])
  comV <- as.character(modelsBDD$com[i])
  idV <-  as.character(modelsBDD$id[i])
  assign(x = as.character(modelsBDD$variable[i]), value =
           list(
             eq = eqV,
             eqAlt = eqAltV,
             name = nameV,
             ref = refV,
             refShort = refShortV,
             startVal = getCSV(myCSV = read.table(
               paste0("./inst/data/", modelsBDD$startVal[i]),
               skip = 2, header = TRUE, sep = ',', dec = '.')),
             com = comV,
             id = idV
           )
  )
}

save(janisch_32, file = "./data/janisch_32.RData")
save(davidson_44, file = "./data/davidson_44.RData")
save(campbell_74, file = "./data/campbell_74.RData")
save(stinner_74, file = "./data/stinner_74.RData")
save(logan6_76, file = "./data/logan6_76.RData")
save(logan10_76, file = "./data/logan10_76.RData")
save(sharpeDeMichele_77, file = "./data/sharpeDeMichele_77.RData")
save(analytis_77, file = "./data/analytis_77.RData")
save(schoolfield_81, file = "./data/schoolfield_81.RData")
save(schoolfieldHigh_81, file = "./data/schoolfieldHigh_81.RData")
save(schoolfieldLow_81, file = "./data/schoolfieldLow_81.RData")
save(taylor_81, file = "./data/taylor_81.RData")
save(wang_82, file = "./data/wang_82.RData")
save(poly2, file = "./data/poly2.RData")
save(harcourtYee_82, file = "./data/harcourtYee_82.RData")
save(poly4, file = "./data/poly4.RData")
save(ratkowsky_82, file = "./data/ratkowsky_82.RData")
save(ratkowsky_83, file = "./data/ratkowsky_83.RData")
save(rootsq_82, file = "./data/rootsq_82.RData")
save(bieri1_83, file = "./data/bieri1_83.RData")
save(hilbertLogan_83, file = "./data/hilbertLogan_83.RData")
save(wagner_88, file = "./data/wagner_88.RData")
save(lamb_92, file = "./data/lamb_92.RData")
save(lactin1_95, file = "./data/lactin1_95.RData")
save(lactin2_95, file = "./data/lactin2_95.RData")
save(beta_95, file = "./data/beta_95.RData")
save(beta_16, file = "./data/beta_16.RData")
save(wangengel_98, file = "./data/wangengel_98.RData")
save(briere1_99, file = "./data/briere1_99.RData")
save(briere2_99, file = "./data/briere2_99.RData")
save(bayoh_03, file = "./data/bayoh_03.RData")
save(kontodimas_04, file = "./data/kontodimas_04.RData")
save(damos_08, file = "./data/damos_08.RData")
save(damos_11, file = "./data/damos_11.RData")
save(shi_11, file = "./data/shi_11.RData")
save(perf2_11, file = "./data/perf2_11.RData")
save(regniere_12, file = "./data/regniere_12.RData")

devRateEqList <- list(
  janisch_32 = janisch_32,
  davidson_44 = davidson_44,
  campbell_74 = campbell_74,
  stinner_74 = stinner_74,
  logan6_76 = logan6_76,
  logan10_76 = logan10_76,
  sharpeDeMichele_77 = sharpeDeMichele_77,
  analytis_77 = analytis_77,
  schoolfield_81 = schoolfield_81,
  schoolfieldHigh_81 = schoolfieldHigh_81,
  schoolfieldLow_81 = schoolfieldLow_81,
  taylor_81 = taylor_81,
  wang_82 = wang_82,
  poly2 = poly2,
  harcourtYee_82 = harcourtYee_82,
  poly4 = poly4,
  ratkowsky_82 = ratkowsky_82,
  ratkowsky_83 = ratkowsky_83,
  rootsq_82 = rootsq_82,
  bieri1_83 = bieri1_83,
  hilbertLogan_83 = hilbertLogan_83,
  wagner_88 = wagner_88,
  lamb_92 = lamb_92,
  lactin1_95 = lactin1_95,
  lactin2_95 = lactin2_95,
  beta_95 = beta_95,
  beta_16 = beta_16,
  wangengel_98 = wangengel_98,
  briere1_99 = briere1_99,
  briere2_99 = briere2_99,
  bayoh_03 = bayoh_03,
  kontodimas_04 = kontodimas_04,
  damos_08 = damos_08,
  damos_11 = damos_11,
  # hansen_11 = hansen_11,
  shi_11 = shi_11,
  perf2_11 = perf2_11,
  regniere_12 = regniere_12
)
save(devRateEqList, file = "./data/devRateEqList.RData")
