devRateIBM <- function(tempTS, timeStepTS, models, numInd = 100, stocha, timeLayEggs = 1){

}


tempTS <- rnorm(n = 100, mean = 25, sd = 1)
timeStepTS <- 1
models <- list(mEggs, mLarva, mPupa)
timeLayEggs <- 1
stocha <- 0.05
numInd <- 100

for(ind in 1:numInd){
  g <- 0
  tx <- 0
  while(tx < length(tempTS)){
    g <- g + 1
    for(i in seq_along(models)){
      currentDev <- 0
      while(currentDev < 1){
        tx <- tx + 1
        if(tx > length(tempTS)){break}
        currentDev <- currentDev + rnorm(n = 1, mean = stats::predict(models[[i]], newdata = list(T = tempTS[tx])), sd = stocha) * timeStepTS
      }
      # if(g < 10){gg <- paste0("0", g)}else{gg <- as.character(g)}
      # if(i < 10){ii <- paste0("0", i)}else{ii <- as.character(i)}
      assign(paste0("g", g, "s", i), tx)
    }
    tx <- tx + as.integer(timeLayEggs)
  }
  stages <- paste0(rep(paste0("g",1:(g + 3)), each = length(models)),"s", 1:length(models))
  if(exists(x = "communityInd")){
    # stages <- c(stages[1:((g - 1) * length(models) + (i - 1))], rep(NA, (ncol(communityInd) - ((g - 1) * length(models) + (i - 1)))))
    stages <- c(stages[1:(((g - 1) * length(models) + (i - 1)) - 1)], rep(NA, ((ncol(communityInd) - ((g - 1) * length(models) + (i - 1))) + 1)))
  }else{
    # stages <- c(stages[1:((g - 1) * length(models) + (i - 1))], rep(NA, (length(stages) - ((g - 1) * length(models) + (i - 1)))))
    stages <- c(stages[1:(((g - 1) * length(models) + (i - 1)) - 1)], rep(NA, ((length(stages) - ((g - 1) * length(models) + (i - 1))) + 1)))
  }
  currentInd <- sapply(stages, function(k){
    if(!is.na(k)){return(get(k))}else{return(NA)}
  })
  if(exists(x = "communityInd")){
    communityInd <- rbind(communityInd, currentInd)
  }else{
    communityInd <- matrix(currentInd, ncol = length(currentInd))
  }
}
bdd <- communityInd
rm(communityInd)

plot(0, type = 'n', xlim = c(0, length(tempTS)), ylim = c(0, 0.5))
colG <- rep(1:8, each = length(models))
ltyG <- rep(1:length(models), 8)
for(j in 1:ncol(bdd)){
  points(density(bdd[,j], na.rm = TRUE, adjust = 2), type ='l', col = colG[j], lty = ltyG[j], lwd = 2)
}

numGen <- sum(!is.na(apply(bdd, MARGIN = 2, FUN = mean, na.rm = TRUE))) / length(models)

