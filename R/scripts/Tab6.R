calcTab6 <- function(n, nV = 3, ne = 1e4) {
  T <- replicate(ne, {
    data <- data.frame(matrix( rnorm(n*nV), ncol = nV))
    data$Y <- rnorm(n)
    model <- lm(Y ~ ., data = data)
    tseries::jarque.bera.test(model$residuals)$statistic
  })
  T <- as.numeric(T)
  T <- T[!is.na(T)]
  T
}

saveTab6 <- function(n, nV = 3, ne = 1e4, 
                     alpha.list = c(0.01, 0.02, 0.05, 0.1, 0.2) , 
                     file_name = "../data/processed/tab6.rds", overwrite = FALSE) {
  if( !file.exists( file_name) || overwrite) {
    df <- data.frame()
  }
  else {
    df <- readRDS(file_name)
  }
  TT <- calcTab6(n, nV = nV, ne = ne)
  cv <- quantile(TT, 1-alpha.list)
  df <- rbind(df, 
              data.frame(n = n, alpha = alpha.list, cv = as.numeric(cv))
  );
  saveRDS(df, file_name)
  df
}
