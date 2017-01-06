#' @title Beta equation of development rate as a function of temperature.
#'
#' @description Yin, X., Kropff, M.J., McLaren, G., and Visperas, R.M. (1995) A nonlinear model for crop
#'   development as a function of temperature. Agricultural and Forest Meteorology 77(1): 1-16.
#'
#' @details Equation:
#' rT ~ exp(mu) * (T - Tb)^aa * (Tc - T)^bb
#'
#' @details where rT is the development rate, T the temperature, mu, aa,
#' and bb the model parameters, Tb the base temperature, and Tc the ceiling
#' temperature.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1016/0168-1923(95)02236-Q}
"beta_95"

#' @title Performance-2 equation of development rate as a function of temperature.
#'
#' @description Shi, P., Ge, F., Sun, Y., and Chen, C. (2011) A simple model for describing the effect of
#' temperature on insect developmental rate. Journal of Asia-Pacific Entomology 14(1): 15-20.
#' @description Wang, L., P. Shi, C. Chen, and F. Xue. 2013. Effect of temperature on the development
#' of Laodelphax striatellus (Homoptera: Delphacidae). J. Econ. Entomol. 106: 107-114.
#' @description Shi, P. J., Reddy, G. V., Chen, L., and Ge, F. (2016). Comparison of Thermal
#' Performance Equations in Describing Temperature-Dependent Developmental Rates
#' of Insects:(I) Empirical Models. Annals of the Entomological Society of America, 109(2), 211-215.
#'
#' @details Equation:
#' rT ~ cc * (T - T1) * (1 - exp(k * (T - T2)))
#'
#' @details where rT is the development rate, T the temperature, T1 and T2 the conceptual
#' lower and upper developmental thresholds at which development rates equal zero,
#' and cc and k constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1016/j.aspen.2010.11.008}
"perf2_11"

#' @title Root square equation of development rate as a function of temperature.
#'
#' @description Ratkowsky, D.A., Olley, J., McMeekin, T.A., and Ball, A. (1982) Relationship between
#' temperature and growth rate of bacterial cultures. Journal of Bacteriology 149(1): 1-5.
#'
#' @details Equation:
#' rT ~ (bb * (T - Tb))^2
#'
#' @details where rT is the development rate, T the temperature, bb the slope of the
#' regression line, and Tb a conceptual temperature of no metabolic significance.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://jb.asm.org/content/149/1/1}
"rootsq_82"

#' @title Wang and Engel equation of development rate as a function of temperature.
#'
#' @description Wang, E., and Engel, T. (1998) Simulation of phenological development of wheat crops.
#' Agricultural systems 58(1): 1-24.
#'
#' @details Equation:
#' rT ~ (2 * (T - Tmin)^aa * (Topt - Tmin)^aa - (T - Tmin)^(2 * aa)) / ((Topt - Tmin)^(2 * aa))
#'
#' @details where rT is the development rate, T the temperature, Tmin the minimum temperature,
#' Topt the optimum temperature, and aa a constant.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1016/S0308-521X(98)00028-6}
"wangengel_98"

#' @title Ratkowsky equation of development rate as a function of temperature.
#'
#' @description Ratkowsky, D.A., Olley, J., McMeekin, T.A., and Ball, A. (1982) Relationship between
#' temperature and growth rate of bacterial cultures. Journal of Bacteriology 149(1): 1-5.
#' @description Ratkowsky, D.A., R.K. Lowry, T.A. McMeekin, A.N. Stokes, and R.E. Chandler. 1983.
#' Model for bacterial culture growth rate throughout the entire biokinetic temperature range.
#' Journal of Bacteriology 154: 1222-1226.
#'
#' @details Equation:
#' rT ~ (sqrt(cc) * k1 * (T - T1) * (1 - exp(k2 * (T - T2))))^2
#'
#' @details where rT is the development rate, T the temperature, T1 and T2 the minimum
#' and maximum temperatures at which rate of growth is zero, sqrt(cc) * k1 the slope of the
#' regression as in the rootsq_82 equation, and k2 a constant. In Ratkowsky et al. 1983,
#' sqrt(cc) * k1 is simplified as a single constant.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://jb.asm.org/content/149/1/1}
#' @source \url{http://jb.asm.org/content/154/3/1222}
"ratkowsky_82"

#' @title Regniere equation of development rate as a function of temperature.
#'
#' @description Regniere, J., Powell, J., Bentz, B., and Nealis, V. (2012) Effects of temperature on
#' development, survival and reproduction of insects: experimental design, data analysis
#' and modeling. Journal of Insect Physiology 58(5): 634-47.
#'
#' @details Equation:
#' rT ~ phi * (exp(bb * (T - Tb)) - ((Tm - T)/(Tm - Tb)) * exp(-bb * (T - Tb) / deltab) - ((T - Tb)/(Tm - Tb)) * exp(bb * (Tm - Tb) - (Tm - T)/deltam))
#'
#' @details where rT is the development rate, T the temperature, and the others
#' biophysical parameters (see source).
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1016/j.jinsphys.2012.01.010}
"regniere_12"

#' @title Shi equation of development rate as a function of temperature.
#'
#' @description Shi, P., Ge, F., Sun, Y., and Chen, C. (2011) A simple model for describing the effect of
#' temperature on insect developmental rate. Journal of Asia-Pacific Entomology 14(1): 15-20.
#'
#' @details Equation:
#' rT ~ cc * (1 - exp(-k1 * (T - T1))) * (1 - exp(k2 * (T - T2)))
#'
#' @details where rT is the development rate, T the temperature, T1 and T2 the conceptual
#' lower and upper developmental thresholds at which development rates equal zero,
#' and cc k1, and k2 constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1016/j.aspen.2010.11.008}
"shi_11"

#' @title Hansen equation of development rate as a function of temperature.
#'
#' @description Hansen, E.M., Bentz, B.J., Powell, J.A., Gray, D.R., and Vandygriff, J.C. (2011) Prepupal
#' diapause and instar IV developmental rates of the spruce beetle, Dendroctonus rufipennis
#' (Coleoptera: Curculionidae, Scolytinae). Journal of insect physiology 57(10): 1347-57.
#'
#' @details Equation:
#' rT ~ p2 * ( (exp(p3 * (T - p1)) - 1) * (exp(p3 * (p5 - p1)) - 1) * exp((T - p5) / p4) )
#'
#' @details where rT is the development rate, T the temperature, p1 the lower developmental
#' treshold, p2 the peak rate control parameter, p3 the low temperature acceleration of
#' rates, p4 the width of upper thermal boundary layer, and p5 the upper developmental
#' threshold.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1016/j.jinsphys.2011.06.011}
"hansen_11"

#' @title Janisch equation of development rate as a function of temperature.
#'
#' @description Janisch, E. (1932) The influence of temperature on the life-history of insects.
#' Transactions of the Royal Entomological Society of London 80(2): 137-68.
#' @description Analytis, S. (1977) Uber die Relation zwischen biologischer Entwicklung und
#' Temperatur bei phytopathogenen Pilzen. Journal of Phytopathology 90(1): 64-76.
#' @description Analytis, S. (1981). Relationship between temperature and development
#' times in phytopathogenic fungus and in plant pests: a mathematical model. Agric.
#' Res.(Athens), 5, 133-159.
#' @description Kontodimas, D.C., Eliopoulos, P.A., Stathas, G.J. and Economou, L.P. (2004) Comparative
#' temperature-dependent development of Nephus includens (Kirsch) and Nephus bisignatus
#' (Boheman)(Coleoptera: Coccinellidae) preying on Planococcus citri
#' (Risso)(Homoptera: Pseudococcidae): evaluation of a linear and various nonlinear models
#' using specific criteria. Environmental Entomology 33(1): 1-11.
#'
#' @details Equation:
#' rT ~ (Dmin/2 * (exp(aa*(T - Topt)) + exp(-bb*(T - Topt))))^(-1)
#'
#' @details where rT is the development rate, T the temperature,
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1111/j.1365-2311.1932.tb03305.x}
"janisch_32"

#' @title Davidson equation of development rate as a function of temperature.
#'
#' @description Davidson, J. (1944). On the relationship between temperature and rate of development of insects
#' at constant temperatures. The Journal of Animal Ecology:26-38.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.2307/1326}
"davidson_44"

#' @title Campbell et al. equation of development rate as a function of temperature.
#'
#' @description Campbell, A., Frazer, B. D., Gilbert, N. G. A. P., Gutierrez, A. P., & Mackauer, M. (1974).
#' Temperature requirements of some aphids and their parasites. Journal of applied ecology, 431-438.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.2307/2402197}
"campbell_74"

#' @title Stinner et al equation of development rate as a function of temperature.
#'
#' @description Stinner, R., Gutierrez, A. & Butler, G. (1974) An algorithm for temperature-dependent growth
#' rate simulation. The Canadian Entomologist, 106, 519-524.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.4039/Ent106519-5}
"stinner_74"

#' @title Logan et al. equation 6 of development rate as a function of temperature.
#'
#' @description Logan, J. A., Wollkind, D. J., Hoyt, S. C., and Tanigoshi, L. K. (1976). An analytic model
#' for description of temperature dependent rate phenomena in arthropods. Environmental
#' Entomology, 5(6), 1133-1140.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/ee/5.6.1133}
"logan6_76"

#' @title Logan et al. equation 10 of development rate as a function of temperature.
#'
#' @description Logan, J. A., Wollkind, D. J., Hoyt, S. C., and Tanigoshi, L. K. (1976). An analytic model
#' for description of temperature dependent rate phenomena in arthropods. Environmental
#' Entomology, 5(6), 1133-1140.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/ee/5.6.1133}
"logan10_76"

#' @title Sharpe and DeMichele equation of development rate as a function of temperature.
#'
#' @description Sharpe, P.J. & DeMichele, D.W. (1977) Reaction kinetics of poikilotherm development.
#' Journal of Theoretical Biology, 64, 649-670.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1016/0022-5193(77)90265-X}
"sharpeDeMichele_77"

#' @title Analytis equation of development rate as a function of temperature.
#'
#' @description Analytis, S. (1977) Uber die Relation zwischen biologischer Entwicklung und Temperatur bei
#' phytopathogenen Pilzen. Journal of Phytopathology 90(1): 64-76.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1111/j.1439-0434.1977.tb02886.x}
"analytis_77"

#' @title Schoolfield et al. equation of development rate as a function of temperature.
#'
#' @description Schoolfield, R., Sharpe, P. & Magnuson, C. (1981) Non-linear regression of biological
#' temperature-dependent rate models based on absolute reaction-rate theory.
#' Journal of theoretical biology, 88, 719-731.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1016/0022-5193(81)90246-0}
"schoolfield_81"

#' @title Schoolfield et al. equation of development rate as a function of temperature for
#' intermediate to high temperatures only.
#'
#' @description Schoolfield, R., Sharpe, P. & Magnuson, C. (1981) Non-linear regression of biological
#' temperature-dependent rate models based on absolute reaction-rate theory.
#' Journal of theoretical biology, 88, 719-731.
#' Wagner, T.L., Wu, H.I., Sharpe, P.S.H., Schoolfield, R.M., Coulson, R.N. (1984) Modeling
#' insect development rates: a literature review and application of a biophysical model.
#' Annals of the Entomological Society of America 77(2): 208-20.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1016/0022-5193(81)90246-0}
"schoolfieldHigh_81"

#' @title Schoolfield et al. equation of development rate as a function of temperature for
#' intermediqte to low temperatures only.
#'
#' @description Schoolfield, R., Sharpe, P. & Magnuson, C. (1981) Non-linear regression of biological
#' temperature-dependent rate models based on absolute reaction-rate theory.
#' Journal of theoretical biology, 88, 719-731.
#' Wagner, T.L., Wu, H.I., Sharpe, P.S.H., Schoolfield, R.M., Coulson, R.N. (1984) Modeling
#' insect development rates: a literature review and application of a biophysical model.
#' Annals of the Entomological Society of America 77(2): 208-20.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1016/0022-5193(81)90246-0}
"schoolfieldLow_81"

#' @title Taylor equation of development rate as a function of temperature.
#'
#' @description Taylor, F. (1981) Ecology and evolution of physiological time in insects.
#' American Naturalist, 1-23.
#' Lamb, RJ. (1992) Developmental rate of Acyrthosiphon pisum (Homoptera: Aphididae) at low
#' temperatures: implications for estimating rate parameters for insects.
#' Environmental Entomology 21(1): 10-19.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://www.jstor.org/stable/2460694}
"taylor_81"

#' @title Second-order polynomial equation of development rate as a function of temperature.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
"poly2"

#' @title Harcourt and Yee equation of development rate as a function of temperature.
#'
#' @description Harcourt, D. and Yee, J. (1982) Polynomial algorithm for predicting the duration of insect
#' life stages. Environmental Entomology, 11, 581-584.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/ee/11.3.581}
"harcourtYee_82"

#' @title Fourth-order polynomial equation of development rate as a function of temperature.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
"poly4"

#' @title Holling type III equation of development rate as a function of temperature.
#'
#' @description Hilbert, DW, y JA Logan (1983) Empirical model of nymphal development for the migratory
#' grasshopper, Melanoplus sanguinipes (Orthoptera: Acrididae).
#' Environmental Entomology 12(1): 1-5.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/ee/12.1.1}
"hilbertLogan_83"

#' @title Lamb equation of development rate as a function of temperature.
#'
#' @description Lamb, R. J., Gerber, G. H., & Atkinson, G. F. (1984). Comparison of developmental rate curves
#' applied to egg hatching data of Entomoscelis americana Brown (Coleoptera: Chrysomelidae).
#' Environmental entomology, 13(3), 868-872.
#' Lamb, RJ. (1992) Developmental rate of Acyrthosiphon pisum (Homoptera: Aphididae) at low
#' temperatures: implications for estimating rate parameters for insects.
#' Environmental Entomology 21(1): 10-19.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/ee/21.1.10}
"lamb_92"

#' @title Lactin et al. equation 1 of development rate as a function of temperature.
#'
#' @description Lactin, Derek J, NJ Holliday, DL Johnson, y R Craigen (995) Improved rate model of
#' temperature-dependent development by arthropods. Environmental Entomology 24(1): 68-75.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/ee/24.1.68}
"lactin1_95"

#' @title Lactin et al. equation 2 of development rate as a function of temperature.
#'
#' @description Lactin, Derek J, NJ Holliday, DL Johnson, y R Craigen (995) Improved rate model of
#' temperature-dependent development by arthropods. Environmental Entomology 24(1): 68-75.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/ee/24.1.68}
"lactin2_95"

#' @title Briere et al equation 1 of development rate as a function of temperature.
#'
#' @description Briere, J.F., Pracros, P., le Roux, A.Y. and Pierre, S. (1999) A novel rate model of
#' temperature-dependent development for arthropods. Environmental Entomology, 28, 22-29.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/ee/28.1.22 }
"briere1_99"

#' @title Briere et al equation 2 of development rate as a function of temperature.
#'
#' @description Briere, J.F., Pracros, P., le Roux, A.Y. and Pierre, S. (1999) A novel rate model of
#' temperature-dependent development for arthropods. Environmental Entomology, 28, 22-29.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/ee/28.1.22 }
"briere2_99"

#' @title Kontodimas et al. equation of development rate as a function of temperature.
#'
#' @description Kontodimas, D.C., Eliopoulos, P.A., Stathas, G.J. and Economou, L.P. (2004) Comparative
#' temperature-dependent development of Nephus includens (Kirsch) and Nephus bisignatus
#' (Boheman)(Coleoptera: Coccinellidae) preying on Planococcus citri
#' (Risso)(Homoptera: Pseudococcidae): evaluation of a linear and various nonlinear models
#' using specific criteria. Environmental Entomology 33(1): 1-11.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://ee.oxfordjournals.org/content/33/1/1}
"kontodimas_04"

#' @title Simplified beta type equation of development rate as a function of temperature.
#'
#' @description Damos, P.T., and Savopoulou-Soultani, M. (2008). Temperature-dependent bionomics and modeling
#' of Anarsia lineatella (Lepidoptera: Gelechiidae) in the laboratory.
#' Journal of economic entomology, 101(5), 1557-1567.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/jee/101.5.1557}
"damos_08"

#' @title Inverse second-order polynomial equation of development rate as a function of temperature.
#'
#' @description Damos, P., and Savopoulou-Soultani, M. (2011) Temperature-driven models for insect
#' development and vital thermal requirements. Psyche: A Journal of Entomology, 2012.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1155/2012/123405}
"damos_11"

#' @title Wang et al. equation of development rate as a function of temperature.
#'
#' @description Wang, R., Lan, Z. and Ding, Y. (1982) Studies on mathematical models of the relationship
#' between insect development and temperature. Acta Ecol. Sin, 2, 47-57.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://en.cnki.com.cn}
"wang_82"

#' @title Bayoh and Lindsay equation of development rate as a function of temperature.
#'
#' @description Bayoh, M.N., Lindsay, S.W. (2003) Effect of temperature on the development of the aquatic
#' stages of Anopheles gambiae sensu stricto (Diptera: Culicidae). Bulletin of entomological
#' research 93(5): 375-81.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1079/BER2003259}
"bayoh_03"

#' @title Hagstrum et Milliken equation of development rate as a function of temperature retrieved
#' from Wagner 1984.
#'
#' @description Hagstrum, D.W., Milliken, G.A. (1988) Quantitative analysis of temperature, moisture, and
#' diet factors affecting insect development. Annals of the Entomological Society of America
#' 81(4): 539-46.
#' Wagner, T.L., Wu, H.I., Sharpe, P.S.H., Schoolfield, R.M., Coulson, R.N. (1984) Modeling
#' insect development rates: a literature review and application of a biophysical model.
#' Annals of the Entomological Society of America 77(2): 208-20.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://dx.doi.org/10.1093/aesa/81.4.539}
"wagner_88"

#' @title Bieri equation 1 of development rate as a function of temperature.
#'
#' @description Bieri, M., Baumgartner, J., Bianchi, G., Delucchi, V., Arx, R. von. (1983)
#' Developmentand fecundity of pea aphid (Acyrthosiphon pisum Harris) as
#' affected by constant temperatures and by pea varieties. Mitteilungen der
#' Schweizerischen Entomologischen Gesellschaft, 56, 163-171.
#'
#' @details Equation:
#' rT ~
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation as a formula object.}
#'   \item{eqAlt}{The equation as a string.}
#'   \item{name}{The equation name.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameter values found in the literature with their references.}
#'   \item{com}{An otional comment about the equation use.}
#'   \item{id}{An id to identificate the equation.}
#' }
#' @source \url{http://www.e-periodica.ch}
"bieri1_83"

#' The list of all available equations of development rate as a function of temperature.
"devRateEqList"
