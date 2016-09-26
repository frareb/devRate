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
                   com = "None",
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
                  com = '"[...] developmental rate approaches zero asymptotically and no true lower threshold of development is predicted" Hilbert and Logan, 1983',
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
                   com = '"[...] developmental rate approaches zero asymptotically and no true lower threshold of development is predicted" Hilbert and Logan, 1983',
                   id = "eq060"
)
save(logan10_76, file = "./data/logan10_76.RData")

sharpeDeMichele77CSV <- read.table("./data/devRate - sharpeDeMichele_77.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
sharpeDeMichele_77 <- list(eq = rT ~ ((T + 273.16) * exp((aa - bb/(T + 273.16))/1.987)) / (1 + exp((cc - dd/(T + 273.16))/1.987) + exp((ff - gg/(T + 273.16))/1.987)) ,
                           eqAlt = "((x + 273.16) * exp((aa - bb/(x + 273.16))/1.987)) / (1 + exp((cc - dd/(x + 273.16))/1.987) + exp((ff - gg/(x + 273.16))/1.987))",
                           name = "Sharpe and DeMichele",
                           ref = "Sharpe, P.J. & DeMichele, D.W. (1977) Reaction kinetics of poikilotherm development. Journal of Theoretical Biology, 64, 649-670.",
                           refShort = "Sharpe and DeMichele 1977",
                           startVal = getCSV(myCSV = sharpeDeMichele77CSV),
                           com = 'Temperature is transformed into Kelvin within the equation (T + 273.16).',
                           id = "eq070"
)
save(sharpeDeMichele_77, file = "./data/sharpeDeMichele_77.RData")

analytis77CSV <- read.table("./data/devRate - analytis_77.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
analytis_77 <- list(eq = rT ~ aa * (T - Tmin)^bb * (Tmax - T)^cc ,
                    eqAlt = "aa * (x - Tmin)^bb * (Tmax - x)^cc",
                    name = "Analytis",
                    ref = "Analytis, S. (1977) Uber die Relation zwischen biologischer Entwicklung und Temperatur bei phytopathogenen Pilzen. Journal of Phytopathology 90(1): 64-76.",
                    refShort = "Analytis 1977",
                    startVal = getCSV(myCSV = analytis77CSV),
                    com = 'None',
                    id = "eq080"
)
save(analytis_77, file = "./data/analytis_77.RData")

schoolfield81CSV <- read.table("./data/devRate - schoolfield_81.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
schoolfield_81 <- list(eq = rT ~ (p25 * (T + 273.16)/298 * exp(aa/1.987 * (1/298 - 1/(T + 273.16)))) / (1 + exp(bb/1.987 * (1/cc - 1/(T + 273.16))) + exp(dd/1.987 * (1/ee - 1/(T + 273.16)))),
                       eqAlt = "(p25 * (x + 273.16)/298 * exp(aa/1.987 * (1/298 - 1/(x + 273.16)))) / (1 + exp(bb/1.987 * (1/cc - 1/(x + 273.16))) + exp(dd/1.987 * (1/ee - 1/(x + 273.16))))",
                       name = "Schoolfield",
                       ref = "Schoolfield, R., Sharpe, P. & Magnuson, C. (1981) Non-linear regression of biological temperature-dependent rate models based on absolute reaction-rate theory. Journal of theoretical biology, 88, 719-731.",
                       refShort = "Schoolfield et al. 1981",
                       startVal = getCSV(myCSV = schoolfield81CSV),
                       com = 'Temperature is transformed into Kelvin within the equation (T + 273.16).',
                       id = "eq090"
)
save(schoolfield_81, file = "./data/schoolfield_81.RData")

taylor81CSV <- read.table("./data/devRate - taylor_81.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
taylor_81 <- list(eq = rT ~ Rm * exp(-1/2 * ((T - Tm)/To)^2) ,
                  eqAlt = "Rm * exp(-1/2 * ((x - Tm)/To)^2)",
                  name = "Gauss",
                  ref = "Taylor, F. (1981) Ecology and evolution of physiological time in insects. American Naturalist, 1-23. \nLamb, RJ. (1992) Developmental rate of Acyrthosiphon pisum (Homoptera: Aphididae) at low temperatures: implications for estimating rate parameters for insects. Environmental Entomology 21(1): 10-19.",
                  refShort = "Taylor 1981",
                  startVal = getCSV(myCSV = taylor81CSV),
                  com = '"The curve must be truncated to the right of Tm because of lethal effects of short exposures to high temperatures. The rate at which development rate falls away from Tm is measured by To." Taylor 1981',
                  id = "eq100"
)
save(taylor_81, file = "./data/taylor_81.RData")


poly2CSV <- read.table("./data/devRate - poly2.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
poly2 <- list(eq = rT ~ a0 + a1 * T + a2 * T^2 ,
              eqAlt = "a0 + a1 * x + a2 * x^2",
              name = "Second-order polynomial",
              ref = "-",
              refShort = "-",
              startVal = getCSV(myCSV = poly2CSV),
              com = 'None',
              id = "eq110"
)
save(poly2, file = "./data/poly2.RData")

harcourtYee82CSV <- read.table("./data/devRate - harcourtYee_82.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
harcourtYee_82 <- list(eq = rT ~ a0 + a1 * T + a2 * T^2 + a3 * T^3 ,
                       eqAlt = "a0 + a1 * x + a2 * x^2 + a3 * x^3",
                       name = "Third-order polynomial",
                       ref = "Harcourt, D. and Yee, J. (1982) Polynomial algorithm for predicting the duration of insect life stages. Environmental Entomology, 11, 581-584.",
                       refShort = "Harcourt and Yee 1982",
                       startVal = getCSV(myCSV = harcourtYee82CSV),
                       com = 'None',
                       id = "eq120"
)
save(harcourtYee_82, file = "./data/harcourtYee_82.RData")

poly4CSV <- read.table("./data/devRate - poly4.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
poly4 <- list(eq = rT ~ a0 + a1 * T + a2 * T^2 + a3 * T^3 + a4 * T^4 ,
              eqAlt = "a0 + a1 * x + a2 * x^2 + a3 * x^3 + a4 * x^4",
              name = "Forth-order polynomial",
              ref = "-",
              refShort = "-",
              startVal = getCSV(myCSV = poly4CSV),
              com = 'None',
              id = "eq130"
)
save(poly4, file = "./data/poly4.RData")

hilbertLogan83CSV <- read.table("./data/devRate - hilbertLogan_83.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
hilbertLogan_83 <- list(eq = rT ~ phi * (((T-Tb)^2 / ((T-Tb)^2 + aa^2)) - exp(-(Tmax - (T-Tb))/deltaT)) ,
                        eqAlt = "phi * (((x-Tb)^2 / ((x-Tb)^2 + aa^2)) - exp(-(Tmax - (x-Tb))/deltaT))",
                        name = "Holling type III",
                        ref = "Hilbert, DW, y JA Logan (1983) Empirical model of nymphal development for the migratory grasshopper, Melanoplus sanguinipes (Orthoptera: Acrididae). Environmental Entomology 12(1): 1-5.",
                        refShort = "Hilbert and Logan 1983",
                        startVal = getCSV(myCSV = hilbertLogan83CSV),
                        com = 'None',
                        id = "eq140"
)
save(hilbertLogan_83, file = "./data/hilbertLogan_83.RData")

lamb92CSV <- read.table("./data/devRate - lamb_92.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
lamb_92 <- list(eq = c(rT ~ Rm * exp(-1/2 * ((T - Tmax)/To)^2), rT ~ Rm * exp(-1/2 * ((T - Tmax)/T1)^2)) ,
                eqAlt = c("Rm * exp(-1/2 * ((x - Tmax)/To)^2)", "Rm * exp(-1/2 * ((x - Tmax)/T1)^2)"),
                name = "Lamb",
                ref = "Lamb, R. J., Gerber, G. H., & Atkinson, G. F. (1984). Comparison of developmental rate curves applied to egg hatching data of Entomoscelis americana Brown (Coleoptera: Chrysomelidae). Environmental entomology, 13(3), 868-872. \nLamb, RJ. (1992) Developmental rate of Acyrthosiphon pisum (Homoptera: Aphididae) at low temperatures: implications for estimating rate parameters for insects. Environmental Entomology 21(1): 10-19.",
                refShort = "Lamb 1992",
                startVal = getCSV(myCSV = lamb92CSV),
                com = 'None',
                id = "eq150"
)
save(lamb_92, file = "./data/lamb_92.RData")

lactin195CSV <- read.table("./data/devRate - lactin1_95.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
lactin1_95 <- list(eq = rT ~ exp(aa * T) - exp(aa * Tmax - (Tmax - T)/deltaT) ,
                   eqAlt = "exp(aa * x) - exp(aa * Tmax - (Tmax - x)/deltaT)",
                   name = "Lactin-1",
                   ref = "Lactin, Derek J, NJ Holliday, DL Johnson, y R Craigen (995) Improved rate model of temperature-dependent development by arthropods. Environmental Entomology 24(1): 68-75.",
                   refShort = "Lactin et al. 1995",
                   startVal = getCSV(myCSV = lactin195CSV),
                   com = 'None',
                   id = "eq160"
)
save(lactin1_95, file = "./data/lactin1_95.RData")

lactin295CSV <- read.table("./data/devRate - lactin2_95.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
lactin2_95 <- list(eq = rT ~ exp(aa * T) - exp(aa * Tmax - (Tmax - T)/deltaT) + bb ,
                   eqAlt = "exp(aa * x) - exp(aa * Tmax - (Tmax - x)/deltaT) + bb",
                   name = "Lactin-2",
                   ref = "Lactin, Derek J, NJ Holliday, DL Johnson, y R Craigen (995) Improved rate model of temperature-dependent development by arthropods. Environmental Entomology 24(1): 68-75.",
                   refShort = "Lactin et al. 1995",
                   startVal = getCSV(myCSV = lactin295CSV),
                   com = 'None',
                   id = "eq170"
)
save(lactin2_95, file = "./data/lactin2_95.RData")

briere199CSV <- read.table("./data/devRate - briere1_99.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
briere1_99 <- list(eq = rT ~ aa * T * (T - Tmin) * (Tmax - T)^(1 / 2) ,
                   eqAlt = "aa * x * (x - Tmin) * (Tmax - x)^(1 / 2)",
                   name = "Briere-1",
                   ref = "Briere, J.F., Pracros, P., le Roux, A.Y. and Pierre, S. (1999) A novel rate model of temperature-dependent development for arthropods. Environmental Entomology, 28, 22-29.",
                   refShort = "Briere et al. 1999",
                   startVal = getCSV(myCSV = briere199CSV),
                   com = 'None',
                   id = "eq180"
)
save(briere1_99, file = "./data/briere1_99.RData")

briere299CSV <- read.table("./data/devRate - briere2_99.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
briere2_99 <- list(eq = rT ~ aa * T * (T - Tmin) * (Tmax - T)^(1 / bb) ,
                   eqAlt = "aa * x * (x - Tmin) * (Tmax - x)^(1 / bb)",
                   name = "Briere-2",
                   ref = "Briere, J.F., Pracros, P., le Roux, A.Y. and Pierre, S. (1999) A novel rate model of temperature-dependent development for arthropods. Environmental Entomology, 28, 22-29.",
                   refShort = "Briere et al. 1999",
                   startVal = getCSV(myCSV = briere299CSV),
                   com = 'None',
                   id = "eq190"
)
save(briere2_99, file = "./data/briere2_99.RData")

kontodimas04CSV <- read.table("./data/devRate - kontodimas_04.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
kontodimas_04 <- list(eq = rT ~ aa * (T - Tmin)^2 * (Tmax - T) ,
                      eqAlt = "aa * (x - Tmin)^2 * (Tmax - x)",
                      name = "Equation 16",
                      ref = "Kontodimas, D.C., Eliopoulos, P.A., Stathas, G.J. and Economou, L.P. (2004) Comparative temperature-dependent development of Nephus includens (Kirsch) and Nephus bisignatus (Boheman)(Coleoptera: Coccinellidae) preying on Planococcus citri (Risso)(Homoptera: Pseudococcidae): evaluation of a linear and various nonlinear models using specific criteria. Environmental Entomology 33(1): 1-11.",
                      refShort = "Kontodimas et al. 2004",
                      startVal = getCSV(myCSV = kontodimas04CSV),
                      com = "None",
                      id = "eq200"
)
save(kontodimas_04, file = "./data/kontodimas_04.RData")

damos08CSV <- read.table("./data/devRate - damos_08.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
damos_08 <- list(eq = rT ~ aa * (bb - T / 10) * (T / 10)^cc,
                 eqAlt = "aa * (bb - x / 10) * (x / 10)^cc",
                 name = "Simplified beta type",
                 ref = "Damos, P.T., and Savopoulou-Soultani, M. (2008). Temperature-dependent bionomics and modeling of Anarsia lineatella (Lepidoptera: Gelechiidae) in the laboratory. Journal of economic entomology, 101(5), 1557-1567.",
                 refShort = "Damos and Savopoulou 2008",
                 startVal = getCSV(myCSV = damos08CSV),
                 com = 'None',
                 id = "eq210"
)
save(damos_08, file = "./data/damos_08.RData")

damos11CSV <- read.table("./data/devRate - damos_11.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
damos_11 <- list(eq = rT ~ aa / (1 + bb * T + cc * T^2),
                 eqAlt = "aa / (1 + bb * x + cc * x^2)",
                 name = "Inverse second-order polynomial",
                 ref = "Damos, P., and Savopoulou-Soultani, M. (2011) Temperature-driven models for insect development and vital thermal requirements. Psyche: A Journal of Entomology, 2012.",
                 refShort = "Damos and Savopoulou 2011",
                 startVal = getCSV(myCSV = damos11CSV),
                 com = 'None',
                 id = "eq220"
)
save(damos_11, file = "./data/damos_11.RData")

wang82CSV <- read.table("./data/devRate - wang_82.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
wang_82 <- list(eq = rT ~ (K / (1 + exp(-r*(T - T0)))) * (1 - exp(-(T - TL)/aa)) * (1-exp(-(TH - T)/aa)),
                eqAlt = "(K / (1 + exp(-r*(x - T0)))) * (1 - exp(-(x - TL)/aa)) * (1-exp(-(TH - x)/aa))",
                name = "Wang",
                ref = "Wang, R., Lan, Z. and Ding, Y. (1982) Studies on mathematical models of the relationship between insect development and temperature. Acta Ecol. Sin, 2, 47-57.",
                refShort = "Wang et al. 1982",
                startVal = getCSV(myCSV = wang82CSV),
                com = 'None',
                id = "eq230"
)
save(wang_82, file = "./data/wang_82.RData")

hansen11CSV <- read.table("./data/devRate - hansen_11.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
hansen_11 <- list(eq = rT ~ p2 * ( (exp(p3 * (T - p1)) - 1) * (exp(p3 * (p5 - p1)) - 1) * exp((T - p5) / p4) ),
                  eqAlt = "p2 * ( (exp(p3 * (x - p1)) - 1) * (exp(p3 * (p5 - p1)) - 1) * exp((T - p5) / p4) )",
                  name = "Hansen",
                  ref = "Hansen, E.M., Bentz, B.J., Powell, J.A., Gray, D.R., and Vandygriff, J.C. (2011) Prepupal diapause and instar IV developmental rates of the spruce beetle, Dendroctonus rufipennis (Coleoptera: Curculionidae, Scolytinae). Journal of insect physiology 57(10): 1347-57.",
                  refShort = "Hansen et al. 2011",
                  startVal = getCSV(myCSV = hansen11CSV),
                  com = 'None',
                  id = "eq240"
)
save(hansen_11, file = "./data/hansen_11.RData")

shi11CSV <- read.table("./data/devRate - shi_11.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
shi_11 <- list(eq = rT ~ cc * (exp(-k1 * (T - T1))) * (1 - exp(k2 * (T - T2))),
                  eqAlt = "cc * (exp(-k1 * (x - T1))) * (1 - exp(k2 * (x - T2)))",
                  name = "Shi",
                  ref = "Shi, P., Ge, F., Sun, Y., and Chen, C. (2011) A simple model for describing the effect of temperature on insect developmental rate. Journal of Asia-Pacific Entomology 14(1): 15-20. doi:10.1016/j.aspen.2010.11.008.",
                  refShort = "Shi et al. 2011",
                  startVal = getCSV(myCSV = shi11CSV),
                  com = 'None',
                  id = "eq250"
)
save(shi_11, file = "./data/shi_11.RData")

regniere12CSV <- read.table("./data/devRate - regniere_12.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
regniere_12 <- list(eq = rT ~ phi * (exp(bb * (T - Tb)) - ((Tm - T)/(Tm - Tb)) * exp(-bb * (T - Tb) / deltab) - ((T - Tb)/(Tm - Tb)) * exp(bb * (Tm - Tb) - (Tm - T)/deltam)),
               eqAlt = "phi * (exp(bb * (x - Tb)) - ((Tm - x)/(Tm - Tb)) * exp(-bb * (x - Tb) / deltab) - ((x - Tb)/(Tm - Tb)) * exp(bb * (Tm - Tb) - (Tm - x)/deltam))",
               name = "Regniere",
               ref = "Regniere, J., Powell, J., Bentz, B., and Nealis, V. (2012) Effects of temperature on development, survival and reproduction of insects: experimental design, data analysis and modeling. Journal of Insect Physiology 58(5): 634-47.",
               refShort = "Regniere et al. 2012",
               startVal = getCSV(myCSV = regniere12CSV),
               com = 'None',
               id = "eq260"
)
save(regniere_12, file = "./data/regniere_12.RData")

ratkowsky82CSV <- read.table("./data/devRate - ratkowsky_82.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
ratkowsky_82 <- list(eq = rT ~ (sqrt(cc) * k1 * (T - T1) * (1 - exp(k2 * (T - T2))))^2,
                    eqAlt = "(sqrt(cc) * k1 * (x - T1) * (1 - exp(k2 * (x - T2))))^2",
                    name = "Ratkowsky",
                    ref = "Ratkowsky, D.A., Olley, J., McMeekin, T.A., and Ball, A. (1982) Relationship between temperature and growth rate of bacterial cultures. Journal of Bacteriology 149(1): 1-5.",
                    refShort = "Ratkowsky et al. 1982",
                    startVal = getCSV(myCSV = ratkowsky82CSV),
                    com = 'None',
                    id = "eq270"
)
save(ratkowsky_82, file = "./data/ratkowsky_82.RData")

wangengel98CSV <- read.table("./data/devRate - wangengel_98.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
wangengel_98 <- list(eq = rT ~ (2 * (T - Tmin)^aa * (Topt - Tmin)^aa - (T - Tmin)^(2 * aa)) / ((Topt - Tmin)^(2 * aa)),
                     eqAlt = "(2 * (x - Tmin)^aa * (Topt - Tmin)^aa - (x - Tmin)^(2 * aa)) / ((Topt - Tmin)^(2 * aa))",
                     name = "Wang Engel",
                     ref = "Wang, E., and Engel, T. (1998) Simulation of phenological development of wheat crops. Agricultural systems 58(1): 1-24.",
                     refShort = "Wang and Engel 1998",
                     startVal = getCSV(myCSV = wangengel98CSV),
                     com = 'None',
                     id = "eq280"
)
save(wangengel_98, file = "./data/wangengel_98.RData")

rootsq82CSV <- read.table("./data/devRate - rootsq_82.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
rootsq_82 <- list(eq = rT ~ (bb * (T - Tb))^2,
                     eqAlt = "(bb * (x - Tb))^2",
                     name = "Root square",
                     ref = "Ratkowsky, D.A., Olley, J., McMeekin, T.A., and Ball, A. (1982) Relationship between temperature and growth rate of bacterial cultures. Journal of Bacteriology 149(1): 1-5.",
                     refShort = "Ratkowsky et al. 1982",
                     startVal = getCSV(myCSV = rootsq82CSV),
                     com = 'None',
                     id = "eq290"
)
save(rootsq_82, file = "./data/rootsq_82.RData")

perf211CSV <- read.table("./data/devRate - perf2_11.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
perf2_11 <- list(eq = rT ~ cc * (T - T1) * (1 - exp(k * (T - T2))),
                  eqAlt = "cc * (x - T1) * (1 - exp(k * (x - T2)))",
                  name = "Performance-2",
                  ref = "Shi, P., Ge, F., Sun, Y., and Chen, C. (2011) A simple model for describing the effect of temperature on insect developmental rate. Journal of Asia-Pacific Entomology 14(1): 15-20.",
                  refShort = "Shi et al. 2011",
                  startVal = getCSV(myCSV = perf211CSV),
                  com = 'None',
                  id = "eq300"
)
save(perf2_11, file = "./data/perf2_11.RData")

beta95CSV <- read.table("./data/devRate - beta_95.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
beta_95 <- list(eq = rT ~ rm * ((T2 - T)/(T2 - Tm)) * ((T - T1)/(Tm - T1))^((Tm - T1)/(T2 - Tm)),
                  eqAlt = "rm * ((T2 - x)/(T2 - Tm)) * ((x - T1)/(Tm - T1))^((Tm - T1)/(T2 - Tm))",
                  name = "Beta",
                  ref = "Yin, X., Kropff, M.J., McLaren, G., and Visperas, R.M. (1995) A nonlinear model for crop development as a function of temperature. Agricultural and Forest Meteorology 77(1): 1-16.",
                  refShort = "Yin et al. 1995",
                  startVal = getCSV(myCSV = beta95CSV),
                  com = 'None',
                  id = "eq310"
)
save(beta_95, file = "./data/beta_95.RData")

bayoh03CSV <- read.table("./data/devRate - bayoh_03.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
bayoh_03 <- list(eq = rT ~ aa + bb * T + cc * exp(T) + dd * exp(-T),
                eqAlt = "aa + bb * x + cc * exp(x) + dd * exp(-x)",
                name = "Bayoh",
                ref = "Bayoh, M.N., Lindsay, S.W. (2003) Effect of temperature on the development of the aquatic stages of Anopheles gambiae sensu stricto (Diptera: Culicidae). Bulletin of entomological research 93(5): 375-81.",
                refShort = "Bayoh and Lindsay 2003",
                startVal = getCSV(myCSV = bayoh03CSV),
                com = 'None',
                id = "eq320"
)
save(bayoh_03, file = "./data/bayoh_03.RData")

# logan6203CSV <- read.table("./data/devRate - logan6_2_03.csv", skip = 2, header = TRUE, sep = ',', dec = '.')
# logan6_2_03 <- list(eq = rT ~ (exp(bb * T) - exp(bb * Tmax - (Tmax - T)/deltaT)) + cc,
#                  eqAlt = "(exp(bb * x) - exp(bb * Tmax - (Tmax - x)/deltaT)) + cc",
#                  name = "Logan6_2",
#                  ref = "Fantinou, A.A., Perdikis, D.C., Chatzoglou, C.S. (2003) Development of immature stages of Sesamia nonagrioides (Lepidoptera: Noctuidae) under alternating and constant temperatures. Environmental Entomology 32(6): 1337-42.",
#                  refShort = "Fatinou et al. 2003",
#                  startVal = getCSV(myCSV = logan6203CSV),
#                  com = 'None',
#                  id = "eq330"
# )
# save(logan6_2_03, file = "./data/logan6_2_03.RData")

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
  wang_82 = wang_82,
  poly2 = poly2,
  harcourtYee_82 = harcourtYee_82,
  poly4 = poly4,
  ratkowsky_82 = ratkowsky_82,
  rootsq_82 = rootsq_82,
  hilbertLogan_83 = hilbertLogan_83,
  lamb_92 = lamb_92,
  lactin1_95 = lactin1_95,
  lactin2_95 = lactin2_95,
  beta_95 = beta_95,
  wangengel_98 = wangengel_98,
  briere1_99 = briere1_99,
  briere2_99 = briere2_99,
  bayoh_03 = bayoh_03,
  kontodimas_04 = kontodimas_04,
  damos_08 = damos_08,
  damos_11 = damos_11,
  hansen_11 = hansen_11,
  shi_11 = shi_11,
  perf2_11 = perf2_11,
  regniere_12 = regniere_12
)
save(devRateEqList, file = "./data/devRateEqList.RData")
