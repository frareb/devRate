### y ~ I(x)
getCSV <- function(myCSV){
  return(data.frame(
    ordersp = myCSV$ORDER,
    familysp = myCSV$FAMILY,
    genussp = myCSV$GENUS,
    species = myCSV$SPECIES,
    genSp = paste(myCSV$GENUS, myCSV$SPECIES),
    stage = myCSV$STAGE,
    param = as.list(myCSV[,7:(ncol(myCSV)-1)]),
    ref = myCSV$REF
  ))
}

janisch32CSV <- read.table("./data/devRate - janisch_32.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
janisch_32 <- list(eq = rT ~ (Dmin/2 * (exp(aa*(T - Topt)) + exp(-bb*(T - Topt))))^(-1),
                   eqAlt = "(Dmin/2 * (exp(aa*(x - Topt)) + exp(-bb*(x - Topt))))^(-1)",
                   name = "Janisch (Analytis modification)",
                   ref = "Janisch, E. (1932) The influence of temperature on the life-history of insects. Transactions of the Royal Entomological Society of London 80(2): 137-68.\nAnalytis, S. (1977) Uber die Relation zwischen biologischer Entwicklung und Temperatur bei phytopathogenen Pilzen. Journal of Phytopathology 90(1): 64-76.",
                   refShort = "Janisch 1932",
                   startVal = getCSV(myCSV = janisch32CSV),
                   com = "",
                   id = "eq010"
)
save(janisch_32, file = "./data/janisch_32.RData")

davidson44CSV <- read.table("./data/devRate - davidson_44.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
davidson_44 <- list(eq = rT ~ K / (1 + exp(aa + bb * T)),
                    eqAlt = "K / (1 + exp(aa + bb * x))",
                    name = "Logistic",
                    ref = "Davidson, J. (1944). On the relationship between temperature and rate of development of insects at constant temperatures. The Journal of Animal Ecology:26-38.",
                    refShort = "Davidson 1944",
                    startVal = getCSV(myCSV = davidson44CSV),
                    com = '"[...] data on the rate of development at temperatures above the peak should not be included when calculating the formula for the temperature-velocity curve." Davidson 1944.',
                    id = "eq020"
)
save(davidson_44, file = "./data/davidson_44.RData")

campbell74CSV <- read.table("./data/devRate - campbell_74.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
campbell_74 <- list(eq = rT ~ aa + bb * T,
                    eqAlt = "aa + bb * x",
                    name = "Linear",
                    ref = "Campbell, A., B. Frazer, N. Gilbert, A. Gutierrez, and M. Mackauer. (1974). Temperature requirements of some aphids and their parasites. Journal of applied ecology, 431-438.",
                    refShort = "Campbell et al. 1974",
                    startVal = getCSV(myCSV = campbell74CSV),
                    com = '"Occasionally, the value for the highest temperature had to be rejected when it did not fit the straight line through the other points." Campbell et al. 1974',
                    id = "eq030"
)
save(campbell_74, file = "./data/campbell_74.RData")

stinner74CSV <- read.table("./data/devRate - stinner_74.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
stinner_74 <- list(eq = c(rT ~ C / (1 + exp(k1 + k2 * T)), rT ~ C / (1 + exp(k1 + k2 * (2 * Topt - T)))),
                   eqAlt = c("C / (1 + exp(k1 + k2 * x))", "C / (1 + exp(k1 + k2 * (2 * Topt - x)))"),
                   name = "Logistic",
                   ref = "Stinner, R., Gutierrez, A. & Butler, G. (1974) An algorithm for temperature-dependent growth rate simulation. The Canadian Entomologist, 106, 519-524.",
                   refShort = "Stinner et al. 1974",
                   startVal = getCSV(myCSV = stinner74CSV),
                   com = '"[...] the relationship [is] inverted when the temperature is above an optimum [...] T = 2 * Topt - T for T >= Topt." Stinner et al. 1974.',
                   id = "eq040"
)
save(stinner_74, file = "./data/stinner_74.RData")

logan676CSV <- read.table("./data/devRate - logan6_76.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
logan6_76 <- list(eq = rT ~ phi * (exp(bb * T) - exp(bb * Tmax - (Tmax - T)/deltaT)) ,
                  eqAlt = "phi * (exp(bb * x) - exp(bb * Tmax - (Tmax - x)/deltaT))",
                  name = "Logan-6",
                  ref = "Logan, J. A., Wollkind, D. J., Hoyt, S. C., and Tanigoshi, L. K. (1976). An analytic model for description of temperature dependent rate phenomena in arthropods. Environmental Entomology, 5(6), 1133-1140.",
                  refShort = "Logan et al. 1976",
                  startVal = getCSV(myCSV = logan676CSV),
                  com = '',
                  id = "eq050"
)
save(logan6_76, file = "./data/logan6_76.RData")

logan1076CSV <- read.table("./data/devRate - logan10_76.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
logan10_76 <- list(eq = rT ~ alpha * (1/(1 + cc * exp(- bb * T)) - exp(-((Tmax - T)/deltaT))) ,
                   eqAlt = "alpha * (1/(1 + cc * exp(- bb * x)) - exp(-((Tmax - x)/deltaT)))",
                   name = "Logan-10",
                   ref = "Logan, J. A., Wollkind, D. J., Hoyt, S. C., and Tanigoshi, L. K. (1976). An analytic model for description of temperature dependent rate phenomena in arthropods. Environmental Entomology, 5(6), 1133-1140.",
                   refShort = "Logan et al. 1976",
                   startVal = getCSV(myCSV = logan1076CSV),
                   com = '',
                   id = "eq060"
)
save(logan10_76, file = "./data/logan10_76.RData")

sharpeDeMichele77CSV <- read.table("./data/devRate - sharpeDeMichele_77.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
sharpeDeMichele_77 <- list(eq = rT ~ ((T + deg) * exp((aa - bb/(T + deg))/1.987)) / (1 + exp((cc - dd/(T + deg))/1.987) + exp((ff - gg/(T + deg))/1.987)) ,
                           eqAlt = "((x + deg) * exp((aa - bb/(x + deg))/1.987)) / (1 + exp((cc - dd/(x + deg))/1.987) + exp((ff - gg/(x + deg))/1.987))",
                           name = "Sharpe and DeMichele",
                           ref = "Sharpe, P.J. & DeMichele, D.W. (1977) Reaction kinetics of poikilotherm development. Journal of Theoretical Biology, 64, 649-670.",
                           refShort = "Sharpe and DeMichele 1977",
                           startVal = getCSV(myCSV = sharpeDeMichele77CSV),
                           com = 'Temperature is transformed into Kelvin within the equation when param.deg = 273.16.',
                           id = "eq070"
)
save(sharpeDeMichele_77, file = "./data/sharpeDeMichele_77.RData")

analytis_77 <- list(eq = rT ~ aa * (T - Tmin)^bb * (Tmax - T)^cc ,
                    eqAlt = "aa * (x - Tmin)^bb * (Tmax - x)^cc",
                    name = "Analytis",
                    ref = "Analytis, S. (1977) Uber die Relation zwischen biologischer Entwicklung und Temperatur bei phytopathogenen Pilzen. Journal of Phytopathology 90(1): 64-76.",
                    refShort = "Analytis 1977",
                    startVal = data.frame(
                      ordersp = c(rep("Coleoptera", 1)),
                      familysp = c(rep("Coccinellidae", 1)),
                      sp = c("Nephus bisignatus"),
                      stage = c(rep("all", 1)),
                      param = list(
                        aa = c(0.0001),
                        bb = c(1.7766),
                        cc = c(0.1740),
                        Tmin = c(4.9125),
                        Tmax = c(33.0781)
                      ),
                      ref = c(rep("Kontodimas et al. 2004", 1))
                    ),
                    com = '',
                    id = "eq080"
)
save(analytis_77, file = "./data/analytis_77.RData")

schoolfield_81 <- list(eq = rT ~ (p25 * (T + 273.16)/298 * exp(aa/1.987 * (1/298 - 1/(T + 273.16)))) / (1 + exp(bb/1.987 * (1/cc - 1/(T + 273.16))) + exp(dd/1.987 * (1/ee - 1/(T + 273.16)))),
                       eqAlt = "(p25 * (x + 273.16)/298 * exp(aa/1.987 * (1/298 - 1/(x + 273.16)))) / (1 + exp(bb/1.987 * (1/cc - 1/(x + 273.16))) + exp(dd/1.987 * (1/ee - 1/(x + 273.16))))",
                       name = "Schoolfield",
                       ref = "Schoolfield, R., Sharpe, P. & Magnuson, C. (1981) Non-linear regression of biological temperature-dependent rate models based on absolute reaction-rate theory. Journal of theoretical biology, 88, 719-731.",
                       refShort = "Schoolfield et al. 1981",
                       startVal = data.frame(
                         ordersp = c("Orthoptera"),
                         familysp = c("Acrididae"),
                         sp = c("Melanoplus sanguinipes"),
                         stage = c("all"),
                         param = list(
                           p25 = c(0.0455),
                           aa = c(8814.36),
                           bb = c(-14877.95),
                           cc = c(298.81),
                           dd = c(47258.52),
                           ee = c(316.695)
                         ),
                         ref = c("Hilbert and Logan 1983")
                       ),
                       com = 'Temperature is transformed into Kelvin within the equation (T + 273.16).',
                       id = "eq090"
)
save(schoolfield_81, file = "./data/schoolfield_81.RData")

taylor_81 <- list(eq = rT ~ Rm * exp(-1/2 * ((T - Tm)/To)^2) ,
                  eqAlt = "Rm * exp(-1/2 * ((x - Tm)/To)^2)",
                  name = "Gauss",
                  ref = "Taylor, F. (1981) Ecology and evolution of physiological time in insects. American Naturalist, 1-23. \nLamb, RJ. (1992) Developmental rate of Acyrthosiphon pisum (Homoptera: Aphididae) at low temperatures: implications for estimating rate parameters for insects. Environmental Entomology 21(1): 10-19.",
                  refShort = "Taylor 1981",
                  startVal = data.frame(
                    ordersp = c(rep("Hemiptera", 15),
                                rep("Coleoptera", 13),
                                rep("Lepidoptera", 11),
                                rep("Diptera", 13),
                                rep("Hymenoptera", 9),
                                rep("Hemiptera", 2)),
                    familysp = c(rep("Lygaeidae",3), rep("Miridae",2), rep("Aphidae",8), rep("Cicadellidae",2),
                                 "Bruchidae", rep("Chrysomelidae", 2), "Cucujidae", rep("Curculionidae", 3), rep("Dermestidae", 4), rep("Tenebrionidae", 2),
                                 "Arctiidae", rep("Noctuidae", 7), rep("Pyralidae", 2), "Tortricidae",
                                 rep("Chloropidae", 3), rep("Culicidae", 4), rep("Drosophilidae", 2), rep("Muscidae", 4),
                                 rep("Braconidae", 9), rep("Aphididae", 2)),
                    sp = c("Geocoris articolor", "Geocoris pallens", "Geocoris punctipes", "Lygus desetinus", "Lygus hesperus", "Acyrthosiphon pisum", "Acyrthosiphon pisum", "Brevicoryne brassicae", "Brevicoryne brassicae", "Hyadaphis pseudobrassicae", "Macrosiphum euphorbiae", "Myzus persicae", "Myzus persicae","Circulifer tenelus", "Empoasca fabae",
                           "Callosobruchus rhodesianus", "Crioceris asparagi", "Oulema melanopus", "Cryptolestes ferrugineus", "Anthonomus grandis", "Hypera brunneipennis", "Hypera postica", rep("Dermestes frischii", 4), "Tribolium castaneum", "Tribolium confusum",
                           "Hyphantria cunea", "Agrostis segetum", "Amanthes c-nigrum", "Mamestra configurata", "Pseudaletia unipunctata", "Simyra henrici", "Spodoptera frugiperda", "Triphaena pronuba", "Anagasta kuehniella", "Ostrinia nubilalis", "Epiphyas postvittana",
                           "Hippelates bishoppi", "Hippelates pallipes", "Hippelates pusio", "Aedes flavescens", "Aedes vexans", "Anopheles quadrimaculatus", "Toxorhynchites brevipalpis", "Drosophila melanogaster", "Drosophila melanogaster", "Haematobia stimulans", "Lyperosia irritans", "Musca domestica", "Stomoxys calcitans",
                           "Apanteles operculella", "Apanteles scutellaris", "Apanteles subandinus", "Aphelinus semiflavus", "Aphidius rapae", rep("Bracon mellitor", 2), "Praon palitans", "Trioxys utilus", rep("Acyrthosiphon pisum", 2)),
                    stage = c(rep("all", 15), rep("all", 7), "45% RH", "60% RH", "75% RH", "90% RH", rep("all", 31), "female", "male", rep("all", 2), rep("all", 2)),
                    param = list(
                      Rm = c(c(5.5, 6.3, 4.4, 5.6, 6.2, 16.5, 15.1, 9.7, 10.0, 14.4, 14.0, 12.9, 15.5, 5.2, 7.1,
                           3.9, 7.1, 5.1, 4.7, 7.5, 5.8, 7.4, 1.9, 2.5, 3.2, 3.9, 4.9, 3.8,
                           3.1, 2.6, 2.6, 2.7, 3.7, 2.9, 5.6, 1.8, 2.6, 4.3, 3.2,
                           7.5, 8.4, 7.9, 5.1, 14.2, 11.4, 6.2, 13.1, 12.2, 8.1, 12.1, 12.5, 8.6,
                           7.4, 10.0, 8.9, 10.0, 9.8, 11.3, 10.8, 8.2, 10.5, 19.9, 19.0)/100),
                      Tm = c(37.2, 37.0, 33.8, 32.1, 36.2, 26.2, 27.5, 26.4, 26.6, 26.0, 30.6, 24.7, 26.3, 35.2, 30.7,
                           31.2, 37.6, 33.7, 36.5, 36.7, 36.8, 37.8, 31.6, 33.2, 34.0, 34.3, 34.6, 32.8,
                           32.9, 33.9, 28.3, 32.2, 32.5, 37.7, 37.4, 28.8, 29.8, 32.5, 27.2,
                           35.0, 32.1, 32.5, 22.2, 26.3, 32.8, 28.4, 30.2, 29.2, 29.5, 34.1, 33.6, 32.2,
                           38.1, 35.2, 36.1, 31.8, 27.0, 42.5, 36.4, 27.6, 28.7, 26.1, 26.3),
                      To = c(8.8, 8.6, 7.7, 9.8, 12.8, 9.0, 11.0, 10.5, 8.4, 9.1, 14.6, 9.3, 10.4, 9.2, 8.9,
                           8.0, 13.3, 11.5, 9.4, 9.4, 12.3, 11.9, 6.2, 7.1, 7.7, 8.4, 7.1, 7.2,
                           9.5, 12.1, 9.8, 12.2, 10.8, 13.6, 11.3, 10.2, 9.0, 9.7, 9.0,
                           9.8, 7.3, 7.5, 6.5, 7.7, 9.7, 6.1, 8.6, 8.4, 8.8, 10.1, 9.7, 9.6,
                           11.4, 10.1, 11.5, 9.8, 9.2, 15.3, 11.9, 7.7, 8.8, 9.9, 10.3)
                    ),
                    ref = c(rep("Taylor 1981", 61), rep("Lamb 1992", 2))
                  ),
                  com = '"The curve must be truncated to the right of Tm because of lethal effects of short exposures to high temperatures. The rate at which development rate falls away from Tm is measured by To." Taylor 1981',
                  id = "eq100"
)
save(taylor_81, file = "./data/taylor_81.RData")

poly2 <- list(eq = rT ~ a0 + a1 * T + a2 * T^2 ,
              eqAlt = "a0 + a1 * x + a2 * x^2",
              name = "Second-order polynomial",
              ref = "-",
              refShort = "-",
              startVal = data.frame(
                ordersp = c(""),
                familysp = c(""),
                sp = c(""),
                stage = c(""),
                param = list(
                  a0 = c(0),
                  a1 = c(0),
                  a2 = c(0)
                ),
                ref = c("")
              ),
              com = '',
              id = "eq110"
)
save(poly2, file = "./data/poly2.RData")

harcourtYee_82 <- list(eq = rT ~ a0 + a1 * T + a2 * T^2 + a3 * T^3 ,
                       eqAlt = "a0 + a1 * x + a2 * x^2 + a3 * x^3",
                       name = "Third-order polynomial",
                       ref = "Harcourt, D. and Yee, J. (1982) Polynomial algorithm for predicting the duration of insect life stages. Environmental Entomology, 11, 581-584.",
                       refShort = "Harcourt and Yee 1982",
                       startVal = data.frame(
                         ordersp = c(rep("Coleoptera", 7)),
                         familysp = c(rep("Curculionidae", 7)),
                         sp = c(rep("Hypera postica", 7)),
                         stage = c("Egg", "L1", "L2", "L3", "L4", "Prepupa", "Pupa"),
                         param = list(
                           a0 = c(0.14340, 0.07294, -0.0053838, 0.12049, -0.13123, -0.16043, 0.15234),
                           a1 = c(-0.02827, -0.01538, 0.005996, -0.025729, 0.023465, 0.024015, -0.022886),
                           a2 = c(0.001824, 0.0015249, -0.000085058, 0.0021796, -0.00093071, -0.00056046, 0.0012900),
                           a3 = c(-0.000026629, -0.000012930, 0.000020124, -0.000030130, 0.000025595, 0.000022486, -0.000011439)
                         ),
                         ref = c(rep("Harcourt and Yee 1982", 7))
                       ),
                       com = '',
                       id = "eq120"
)
save(harcourtYee_82, file = "./data/harcourtYee_82.RData")

poly4 <- list(eq = rT ~ a0 + a1 * T + a2 * T^2 + a3 * T^3 + a4 * T^4 ,
              eqAlt = "a0 + a1 * x + a2 * x^2 + a3 * x^3 + a4 * x^4",
              name = "Forth-order polynomial",
              ref = "-",
              refShort = "-",
              startVal = data.frame(
                ordersp = c("Lepidoptera"),
                familysp = c("Gelechiidae"),
                sp = c("Tuta absoluta"),
                stage = c("all"),
                param = list(
                  a0 = c(-0.27),
                  a1 = c(0.049),
                  a2 = c(-0.0031),
                  a3 = c(0.000094),
                  a4 = c(-0.000001)
                ),
                ref = c("Ozgokce et al. 2016")
              ),
              com = '',
              id = "eq130"
)
save(poly4, file = "./data/poly4.RData")

hilbertLogan_83 <- list(eq = rT ~ phi * (((T-Tb)^2 / ((T-Tb)^2 + aa^2)) - exp(-(Tmax - (T-Tb))/deltaT)) ,
                        eqAlt = "phi * (((x-Tb)^2 / ((x-Tb)^2 + aa^2)) - exp(-(Tmax - (x-Tb))/deltaT))",
                        name = "Holling type III",
                        ref = "Hilbert, DW, y JA Logan (1983) Empirical model of nymphal development for the migratory grasshopper, Melanoplus sanguinipes (Orthoptera: Acrididae). Environmental Entomology 12(1): 1-5.",
                        refShort = "Hilbert and Logan 1983",
                        startVal = data.frame(
                          ordersp = c("Orthoptera"),
                          familysp = c("Acrididae"),
                          sp = c("Melanoplus sanguinipes"),
                          stage = c("all"),
                          param = list(
                            phi = c(0.2676),
                            aa = c(58.62),
                            Tb = c(7.657),
                            Tmax = c(45.02),
                            deltaT = c(3.433)
                          ),
                          ref = c("Hilbert and Logan 1983")
                        ),
                        com = '',
                        id = "eq140"
)
save(hilbertLogan_83, file = "./data/hilbertLogan_83.RData")

lamb_92 <- list(eq = c(rT ~ Rm * exp(-1/2 * ((T - Tmax)/To)^2), rT ~ Rm * exp(-1/2 * ((T - Tmax)/T1)^2)) ,
                eqAlt = c("Rm * exp(-1/2 * ((x - Tmax)/To)^2)", "Rm * exp(-1/2 * ((x - Tmax)/T1)^2)"),
                name = "Lamb",
                ref = "Lamb, R. J., Gerber, G. H., & Atkinson, G. F. (1984). Comparison of developmental rate curves applied to egg hatching data of Entomoscelis americana Brown (Coleoptera: Chrysomelidae). Environmental entomology, 13(3), 868-872. \nLamb, RJ. (1992) Developmental rate of Acyrthosiphon pisum (Homoptera: Aphididae) at low temperatures: implications for estimating rate parameters for insects. Environmental Entomology 21(1): 10-19.",
                refShort = "Lamb 1992",
                startVal = data.frame(
                  ordersp = c(rep("Coleoptera", 3)),
                  familysp = c("Chrysomelidae", rep("Coccinellidae", 2)),
                  sp = c("Entomoscelis americana", "Nephus includens", "Nephus bisignatus"),
                  stage = c("Eggs"),
                  param = list(
                    Rm = c(0.05393, 0.0441, 0.0338),
                    Tmax = c(33.40, 34.9999, 32.4999),
                    To = c(10.75, 11.0220, 10.7097),
                    T1 = c(3.377, -0.000021, 0.000023)
                  ),
                  ref = c("Lamb et al. 1984", rep("Kontodimas et al. 2004", 2))
                ),
                com = '',
                id = "eq150"
)
save(lamb_92, file = "./data/lamb_92.RData")

lactin1_95 <- list(eq = rT ~ exp(aa * T) - exp(aa * Tmax - (Tmax - T)/deltaT) ,
                   eqAlt = "exp(aa * x) - exp(aa * Tmax - (Tmax - x)/deltaT)",
                   name = "Lactin-1",
                   ref = "Lactin, Derek J, NJ Holliday, DL Johnson, y R Craigen (995) Improved rate model of temperature-dependent development by arthropods. Environmental Entomology 24(1): 68-75.",
                   refShort = "Lactin et al. 1995",
                   startVal = data.frame(
                     ordersp = c(rep("Coleoptera", 5)),
                     familysp = c(rep("Chrysomelidae", 5)),
                     sp = c(rep("Leptinotarsa decemlineata", 5)),
                     stage = c("Eggs", "L1", "L2", "L3", "L4"),
                     param = list(
                       aa = c(0.155430, 0.154034, 0.154035, 0.169451, 0.166364),
                       Tmax = c(38.048732, 38.953357, 37.526100, 36.397259, 35.914673),
                       deltaT = c(6.421234, 6.467896, 6.460183, 5.883764, 5.997446)
                     ),
                     ref = c(rep("Lactin et al. 1995", 5))
                   ),
                   com = '',
                   id = "eq160"
)
save(lactin1_95, file = "./data/lactin1_95.RData")

lactin2_95 <- list(eq = rT ~ exp(aa * T) - exp(aa * Tmax - (Tmax - T)/deltaT) + bb ,
                   eqAlt = "exp(aa * x) - exp(aa * Tmax - (Tmax - x)/deltaT) + bb",
                   name = "Lactin-2",
                   ref = "Lactin, Derek J, NJ Holliday, DL Johnson, y R Craigen (995) Improved rate model of temperature-dependent development by arthropods. Environmental Entomology 24(1): 68-75.",
                   refShort = "Lactin et al. 1995",
                   startVal = data.frame(
                     ordersp = c(rep("Coleoptera", 7)),
                     familysp = c(rep("Chrysomelidae", 5), rep("Coccinellidae", 2)),
                     sp = c(rep("Leptinotarsa decemlineata", 5), "Nephus includens", "Nephus bisignatus"),
                     stage = c("Eggs", "L1", "L2", "L3", "L4", "all", "all"),
                     param = list(
                       aa = c(0.139034, 0.096314, 0.105906, 0.019087, 0.146066, 0.0019, 0.0017),
                       Tmax = c(38.890035, 44.514120, 40.725042, 36.873879, 36.643691, 38.2976, 39.6875),
                       deltaT = c(7.167110, 10.074502, 9.161844, 1.690717, 6.813403, 0.7152, 1.5488),
                       bb = c(-0.026410, -0.238647, -0.249972, -1.212223, -0.051544, -1.0214, -1.0168)
                     ),
                     ref = c(rep("Lactin et al. 1995", 5), rep("Kontodimas et al. 2004", 2))
                   ),
                   com = '',
                   id = "eq170"
)
save(lactin2_95, file = "./data/lactin2_95.RData")

briere1_99 <- list(eq = rT ~ aa * T * (T - Tmin) * (Tmax - T)^(1 / 2) ,
                   eqAlt = "aa * x * (x - Tmin) * (Tmax - x)^(1 / 2)",
                   name = "Briere-1",
                   ref = "Briere, J.F., Pracros, P., le Roux, A.Y. and Pierre, S. (1999) A novel rate model of temperature-dependent development for arthropods. Environmental Entomology, 28, 22-29.",
                   refShort = "Briere et al. 1999",
                   startVal = data.frame(
                     ordersp = c(""),
                     familysp = c(""),
                     sp = c(""),
                     stage = c(""),
                     param = list(
                       aa = c(0),
                       Tmin = c(0),
                       Tmax = c(0)
                     ),
                     ref = c("")
                   ),
                   com = '',
                   id = "eq180"
)
save(briere1_99, file = "./data/briere1_99.RData")

briere2_99 <- list(eq = rT ~ aa * T * (T - Tmin) * (Tmax - T)^(1 / bb) ,
                   eqAlt = "aa * x * (x - Tmin) * (Tmax - x)^(1 / bb)",
                   name = "Briere-2",
                   ref = "Briere, J.F., Pracros, P., le Roux, A.Y. and Pierre, S. (1999) A novel rate model of temperature-dependent development for arthropods. Environmental Entomology, 28, 22-29.",
                   refShort = "Briere et al. 1999",
                   startVal = data.frame(
                     ordersp = c(""),
                     familysp = c(""),
                     sp = c(""),
                     stage = c(""),
                     param = list(
                       aa = c(0),
                       Tmax = c(0),
                       Tmin = c(0),
                       bb = c(0)
                     ),
                     ref = c("")
                   ),
                   com = '',
                   id = "eq190"
)
save(briere2_99, file = "./data/briere2_99.RData")









kontodimas_04 <- list(eq = rT ~ aa * (T - Tmin)^2 * (Tmax - T) ,
                      eqAlt = "aa * (x - Tmin)^2 * (Tmax - x)",
                      name = "Equation 16",
                      ref = "Kontodimas, D.C., Eliopoulos, P.A., Stathas, G.J. and Economou, L.P. (2004) Comparative temperature-dependent development of Nephus includens (Kirsch) and Nephus bisignatus (Boheman)(Coleoptera: Coccinellidae) preying on Planococcus citri (Risso)(Homoptera: Pseudococcidae): evaluation of a linear and various nonlinear models using specific criteria. Environmental Entomology 33(1): 1-11.",
                      refShort = "Kontodimas et al. 2004",
                      startVal = data.frame(
                        ordersp = c(""),
                        familysp = c(""),
                        sp = c(""),
                        stage = c(""),
                        param = list(
                          aa = c(0),
                          Tmin = c(0),
                          Tmax = c(0)
                        ),
                        ref = c("")
                      ),
                      com = "",
                      id = "eq200"
)
save(kontodimas_04, file = "./data/kontodimas_04.RData")

damos_08 <- list(eq = rT ~ aa * (bb - T / 10) * (T / 10)^cc,
                 eqAlt = "aa * (bb - x / 10) * (x / 10)^cc",
                 name = "Simplified beta type",
                 ref = "Damos, P.T., and Savopoulou-Soultani, M. (2008). Temperature-dependent bionomics and modeling of Anarsia lineatella (Lepidoptera: Gelechiidae) in the laboratory. Journal of economic entomology, 101(5), 1557-1567.",
                 refShort = "Damos and Savopoulou 2008",
                 startVal = data.frame(
                   ordersp = "Lepidoptera",
                   familysp = "Gelechiidae",
                   sp = "Anarsia lineatella",
                   stage = "all",
                   param = list(
                     aa = 0.0003,
                     bb = 3.8297,
                     cc = 4.8760
                   ),
                   ref = "Damos and Savopoulou 2008"
                 ),
                 com = '',
                 id = "eq210"
)
save(damos_08, file = "./data/damos_08.RData")

damos_11 <- list(eq = rT ~ aa / (1 + bb * T + cc * T^2),
                 eqAlt = "aa / (1 + bb * x + cc * x^2)",
                 name = "Inverse second-order polynomial",
                 ref = "Damos, P., and Savopoulou-Soultani, M. (2011) Temperature-driven models for insect development and vital thermal requirements. Psyche: A Journal of Entomology, 2012.",
                 startVal = data.frame(
                   ordersp = c(""),
                   familysp = c(""),
                   sp = c(""),
                   stage = c(""),
                   param = list(
                     aa = c(0),
                     bb = c(0),
                     cc = c(0)
                   ),
                   ref = c("")
                 ),
                 com = '',
                 id = "eq220"
)
save(damos_11, file = "./data/damos_11.RData")

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
  taylor_81 = taylor_81,
  poly2 = poly2,
  harcourtYee_82 = harcourtYee_82,
  poly4 = poly4,
  hilbertLogan_83 = hilbertLogan_83,
  lamb_92 = lamb_92,
  lactin1_95 = lactin1_95,
  lactin2_95 = lactin2_95,
  briere1_99 = briere1_99,
  briere2_99 = briere2_99,
  ## other equations
  kontodimas_04 = kontodimas_04,
  damos_08 = damos_08,
  damos_11 = damos_11
)
save(devRateEqList, file = "./data/devRateEqList.RData")
