#' Bartekova and Praslicka linear thermal performance curve for the
#'   development performance curve of Helicoverpa armigera
#'
#' @description Linear development performance curve for eggs, larvae and
#'   pupae from three experimental temperatures (20, 25, and 30 degrees
#'   Celsius).
#' @seealso Bartekova, A., and Praslicka, J. (2006). The effect of ambient
#'   temperature on the development of cotton bollworm (Helicoverpa armigera
#'   Hubner, 1808). Plant Protection Science, 42(4), 135.
#'   https://doi.org/10.17221/2768-PPS
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of fitted models for the
#'   different life stages considered in the article. Each element of the
#'   list of models is an object of class nls.
#' @examples
#'   mymodel <- ha_bartekova2006(plotfig = FALSE)
#' @export
ha_bartekova2006 <- function(plotfig = TRUE){
  campbell_74 <- campbell_74
  LDT <- c(14.8, 11.3, 8.2, 11.5)
  SET <- c(64.1, 344.8, 222.2, 625.0)
  bb <- 1/SET
  aa <- -LDT*bb
  if(plotfig){
    temp <- c(5, 20, 25, 30)
    DR_egg <- aa[1] + bb[1]*temp
    DR_larva <- aa[2] + bb[2]*temp
    DR_pupa <- aa[3] + bb[3]*temp
    graphics::plot(
      x = temp, y = DR_egg, ylim = c(0, 0.25), type = "o", pch = 15,
      main = "Figure 1 ; DOI: 10.17221/2768-PPS ; Bartekova et al. 2006",
      xlab = "Ambient temperature (degrees Celsius)",
      ylab = "Development rate (1/DT)",
      xlim = c(0, 40)
    )
    graphics::points(x = temp, y = DR_larva, type = "o", pch = 16)
    graphics::points(x = temp, y = DR_pupa, type = "o", pch = 17)
    graphics::legend(
      "topleft", pch = c(15, 16, 17), legend = c("egg", "larva", "pupa"),
      bty = "n"
    )
  }
  eq <- campbell_74
  dr_egg <- list(aa = aa[1], bb = bb[1])
  dr_larva <- list(aa = aa[2], bb = bb[2])
  dr_pupa <- list(aa = aa[3], bb = bb[3])
  return(list(
    equation = eq,
    model = list(egg = dr_egg, larva = dr_larva, pupa = dr_pupa)
  ))
}

#' Jallow and Matsumura linear thermal performance curve for the
#'   development performance curve of Helicoverpa armigera
#'
#' @description Linear development performance curve for eggs, larvae and
#'   pupae from nine experimental temperatures (10, 13.3, 16.4, 20, 22.5, 25,
#'   27.9, 30.5, and 32.5 degrees Celsius).
#' @seealso Jallow, M. F., and Matsumura, M. (2001). Influence of temperature
#'   on the rate of development of Helicoverpa armigera (Hubner)(Lepidoptera:
#'   Noctuidae). Applied Entomology and Zoology, 36(4), 427-430.
#'   https://doi.org/10.1303/aez.2001.427
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of fitted models for the
#'   different life stages considered in the article. Each element of the
#'   list of models is an object of class nls.
#' @examples
#'   mymodel <- ha_jallow2001(plotfig = FALSE)
#' @export
ha_jallow2001 <- function(plotfig = TRUE){
  campbell_74 <- campbell_74
  if(plotfig){
    temp <- c(10, 13.3, 16.4, 20, 22.5, 25, 27.9, 30.5, 32.5)
    DR_egg <- -0.21+0.02*temp
    DR_larva <- -0.045+0.004*temp
    DR_pupa <- -0.083+0.006*temp
    graphics::plot(
      x = temp, y = DR_egg, ylim = c(0, 0.4), type = "o", pch = 15,
      main = "Figure 1 ; DOI: 10.1303/aez.2001.427 ; Jallow et al. 2001",
      xlab = "Ambient temperature (degrees Celsius)",
      ylab = "Development rate (1/DT)",
      xlim = c(0, 40)
    )
    graphics::points(x = temp, y = DR_larva, type = "o", pch = 16)
    graphics::points(x = temp, y = DR_pupa, type = "o", pch = 17)
    graphics::legend(
      "topleft", pch = c(15:17), legend = c("egg", "larva", "pupa"),
      bty = "n"
    )
  }
  eq <- campbell_74
  dr_egg <- list(aa = -0.21, bb = 0.02)
  dr_larva <- list(aa = -0.045, bb = 0.004)
  dr_pupa <- list(aa = -0.083, bb = 0.006)
  return(list(
    equation = eq,
    model = list(egg = dr_egg, larva = dr_larva, pupa = dr_pupa)
  ))
}

#' Mironidis and Savopoulou-Soultani linear thermal performance curve for the
#'   development performance curve of Helicoverpa armigera
#'
#' @description Linear development performance curve for eggs, larvae and
#'   pupae from twelve experimental temperatures (10, 12.5, 15, 17.5, 20,
#'   25, 27.5, 30, 32.5, 35, 37.5, and 40 degrees Celsius).
#' @seealso Mironidis, G. K., and Savopoulou-Soultani, M. (2008). Development,
#'   survivorship, and reproduction of Helicoverpa armigera (Lepidoptera:
#'   Noctuidae) under constant and alternating temperatures. Environmental
#'   Entomology, 37(1), 16-28. https://doi.org/10.1093/ee/37.1.16
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of fitted models for the
#'   different life stages considered in the article. Each element of the
#'   list of models is an object of class nls.
#' @examples
#'   mymodel <- ha_mironidis2008_ls(plotfig = FALSE)
#' @export
ha_mironidis2008_ls <- function(plotfig = TRUE){
  campbell_74 <- campbell_74
  if(plotfig){
    temp <- c(10, 12.5, 15, 17.5, 20, 25, 27.5, 30, 32.5, 35, 37.5, 40)
    DR_egg <- -0.3013+0.0252*temp
    DR_larva <- -0.0442+0.0042*temp
    DR_pupa <- -0.0529+0.0052*temp
    graphics::plot(
      x = temp, y = DR_egg, ylim = c(0, 0.4), type = "o", pch = 15,
      main = "Figure X ; DOI: 10.1093/ee/37.1.16 ; Mironidis et al. 2008",
      xlab = "Ambient temperature (degrees Celsius)",
      ylab = "Development rate (1/DT)",
      xlim = c(0, 40)
    )
    graphics::points(x = temp, y = DR_larva, type = "o", pch = 16)
    graphics::points(x = temp, y = DR_pupa, type = "o", pch = 17)
    graphics::legend(
      "topleft", pch = c(15:17), legend = c("egg", "larva", "pupa"),
      bty = "n"
    )
  }
  eq <- campbell_74
  dr_egg <- list(aa = -0.3013, bb = 0.0252)
  dr_larva <- list(aa = -0.0442, bb = 0.0042)
  dr_pupa <- list(aa = -0.0529, bb = 0.0052)
  return(list(
    equation = eq,
    model = list(egg = dr_egg, larva = dr_larva, pupa = dr_pupa)
  ))
}

#' Mironidis and Savopoulou-Soultani non-linear thermal performance curve for
#'   the development performance curve of Helicoverpa armigera
#'
#' @description Non-linear development performance curve for eggs, larvae and
#'   pupae from twelve experimental temperatures (10, 12.5, 15, 17.5, 20,
#'   25, 27.5, 30, 32.5, 35, 37.5, and 40 degrees Celsius), using Lactin2
#'   model (Lactin, 1995).
#' @seealso Mironidis, G. K., and Savopoulou-Soultani, M. (2008). Development,
#'   survivorship, and reproduction of Helicoverpa armigera (Lepidoptera:
#'   Noctuidae) under constant and alternating temperatures. Environmental
#'   Entomology, 37(1), 16-28. https://doi.org/10.1093/ee/37.1.16
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of fitted models for the
#'   different life stages considered in the article. Each element of the
#'   list of models is an object of class nls.
#' @examples
#'   mymodel <- ha_mironidis2008_nls(plotfig = FALSE)
#' @export
ha_mironidis2008_nls <- function(plotfig = TRUE){
  lactin2_95 <- lactin2_95
  if(plotfig){
    temp <- c(10, 12.5, 15, 17.5, 20, 25, 27.5, 30, 32.5, 35, 37.5, 40)
    temp2 <- 5:40
    aa <- c(0.0165, 0.0042, 0.0053)
    Tmax <- c(42.0240, 43.1521, 43.3886)
    deltaT <- c(2.0503, 1.8197, 1.6854)
    bb <- c(-1.1906, -1.0480, -1.0674)
    DR_egg2 <- exp(aa[1] * temp2) - exp(aa[1] * Tmax[1] - (Tmax[1] - temp2)/deltaT[1]) + bb[1]
    DR_larva2 <- exp(aa[2] * temp2) - exp(aa[2] * Tmax[2] - (Tmax[2] - temp2)/deltaT[2]) + bb[2]
    DR_pupa2 <- exp(aa[3] * temp2) - exp(aa[3] * Tmax[3] - (Tmax[3] - temp2)/deltaT[3]) + bb[3]
    DR_egg <- exp(aa[1] * temp) - exp(aa[1] * Tmax[1] - (Tmax[1] - temp)/deltaT[1]) + bb[1]
    DR_larva <- exp(aa[2] * temp) - exp(aa[2] * Tmax[2] - (Tmax[2] - temp)/deltaT[2]) + bb[2]
    DR_pupa <- exp(aa[3] * temp) - exp(aa[3] * Tmax[3] - (Tmax[3] - temp)/deltaT[3]) + bb[3]
    graphics::plot(
      x = temp2, y = DR_egg2, ylim = c(0, 0.6), type = "l", pch = 15,
      main = "Figure 2 ; DOI: 10.1093/ee/37.1.16 ; Mironidis et al. 2008",
      xlab = "Temperature (degrees Celsius)",
      ylab = "Development rate (Day^(-1))",
      xlim = c(0, 40)
    )
    graphics::points(x = temp2, y = DR_larva2, type = "l", pch = 16)
    graphics::points(x = temp2, y = DR_pupa2, type = "l", pch = 17)
    graphics::points(x = temp, y = DR_egg, type = "p", pch = 15)
    graphics::points(x = temp, y = DR_larva, type = "p", pch = 16)
    graphics::points(x = temp, y = DR_pupa, type = "p", pch = 17)
    graphics::legend(
      "topleft", pch = c(15:17), legend = c("egg", "larva", "pupa"),
      bty = "n"
    )
  }
  eq <- lactin2_95
  dr_egg <- list(aa = 0.0165, Tmax = 42.0240, deltaT = 2.0503, bb = -1.1906)
  dr_larva <- list(aa = 0.0042, Tmax = 43.1521, deltaT = 1.8197, bb = -1.0480)
  dr_pupa <- list(aa = 0.0053, Tmax = 43.3886, deltaT = 1.6854, bb = -1.0674)
  return(list(
    equation = eq,
    model = list(egg = dr_egg, larva = dr_larva, pupa = dr_pupa)
  ))
}

# devtools::document()
# devtools::check()
# devtools::check_win_devel()
# devtools::build()

# TO DO
## P1- add new models adjusted for H. armigera from the literature
## P2- modify devRateIBMparam to choose when to insert the timeLayEggs value
## P3- modify devRateIBMparam to use different models for each life stage
##     this would be a version change to 0.3.
## P4- add a vignette to explain the different H. armigera models ans how to use them


