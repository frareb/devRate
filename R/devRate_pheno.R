devRateIBM <- function(tempTS, timeStepTS, models, numInd = 1000, stocha){

}


tempTS <- rnorm(n = 50, mean = 25, sd = 1)
timeStepTS <- 2
models <- list(mEggs, mLarva, mPupa)




tx <- 0
G01S01 <- cumsum(stats::predict(models[[1]], newdata = list(T = tempTS)) * timeStepTS)
G01S01 <- G01S01[G01S01 <= 1]
tx <- tx + length(G01S01) + 1
tG01S01 <- tx

G01S02 <- cumsum(stats::predict(models[[2]], newdata = list(T = tempTS[tx:length(tempTS)])) * timeStepTS)
G01S02 <- G01S02[G01S02 <= 1]
tx <- tx + length(G01S02) + 1
tG01S02 <- tx

G01S03 <- cumsum(stats::predict(models[[3]], newdata = list(T = tempTS[tx:length(tempTS)])) * timeStepTS)
G01S03 <- G01S03[G01S03 <= 1]
tx <- tx + length(G01S03) + 1
tG01S03 <- tx

plot(x = c(tG01S01, tG01S02,tG01S03), y = c(1,1,1), xlim = c(0, 50))
