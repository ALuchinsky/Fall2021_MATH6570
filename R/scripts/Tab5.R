library(knitrProgressBar)


getJBCV <- function(n=10, N=1e5, alpha = 0.05) {
  #' Calculates the Jarque-Bera test critical value on the standard norm 
  #' sample
  #' 
  #' @param n Number of columns in sample
  #' @param N Number of rows in sample
  #' @param alpha level value (the vector can be provided)
  #' @returns critical value
  data <- matrix(rnorm(n*N), ncol = n)
  T <- apply(data, 1,function(x)tseries::jarque.bera.test(x)$statistic)
  quantile(T, 1-alpha)
}



updateJBCVfile <- function(n=10, 
                           N=1e5, 
                           nLoop = 1,
                           alpha = c(0.01, 0.02, 0.05, 0.10, 0.20), 
                           file_name = "../data/processed/tab5.rds", 
                           seed = NA,
                           overwrite = FALSE) {
  #' updates the data file with the result of the JBCV test
  #' 
  #' @param n Number of columns in sample
  #' @param N Number of rows in sample
  #' @param nLoop number of CV's to be calculated
  #' @param alpha level value (the vector can be provided)
  #' @param file_name name of the file to be updated. File will be reated if does not exist
  #' @param seed Random seed. If NA (default) current time will be used
  #' @param overwrite If true even existing file will be overwritten
  #' @return the updated dataframe
  if(is.na(seed)) {
    seed <- as.numeric( Sys.time())
  }
  set.seed(seed)
  if( file.exists(file_name) && !overwrite) {
    df <- readRDS(file_name)
  } else {
    df <- data.frame()
  }
  pb <- knitrProgressBar::progress_estimated( nLoop )
  for(i in 1:nLoop) {
    update_progress(pb)
    cv <- as.numeric(getJBCV(n = n, N = N, alpha = alpha))
    df <- rbind(df, data.frame(n = n, N = N, alpha = alpha, seed = seed, cv = cv))
    saveRDS(df, file_name)
  }
  df
}

