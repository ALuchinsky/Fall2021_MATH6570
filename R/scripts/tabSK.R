library(e1071)


genFCN <- function(n, p = 0.5, mu2 = 1, sigma2 = 1) {
  u <- runif(n)
  x1 <- rnorm(n, mean = 0, sd = 1)
  x2 <- rnorm(n, mean = mu2, sd = sigma2)
  ifelse(u>=p, x1, x2)
}



calcSKtable <- function(p, mu2, sigma2, nSample = 1e4, nEv = 1e3) {
  T <- replicate(nEv, {
    x <- genFCN(nSample, p = p, mu2 = mu2, sigma2 = sigma2)
    c(skewness(x), 3+kurtosis(x))
  })
  t(T)
}

saveSKtable <- function(p, mu2, sigma2, nSample = 1e4, nEv = 1e3, nLoop = 10, file_name = "../data/processed/tabSK.rds") {
  if( !file.exists( file_name)) {
    df <- data.frame()
  }
  else {
    df <- readRDS(file_name)
  }
  pb <- knitrProgressBar::progress_estimated(nLoop)
  for(i in 1:nLoop) {
    knitrProgressBar::update_progress(pb)
    T <- calcSKtable(p, mu2, sigma2, nSample = nSample, nEv = nEv)
    df <- rbind(df, 
                data.frame( 
                  p = p, mu2 = mu2, sigma2 = sigma2, nSample = nSample, nEv = nEv, 
                  Sk = mean(T[,1]), sdSk = sd(T[,1]),
                  Kurt = mean(T[,2]), sdKurt = sd(T[,2])
                )
    )
  }
  saveRDS(df, file_name)
  df
}
