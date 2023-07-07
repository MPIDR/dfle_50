# This document contains functios neccessary to run the main analysis

# life table simulation
sim_lt <- function(eqx,eta,n){
  
# period life table starting quantities  
  dat <- data.frame(
    age = eta,
    qx = eqx,
    nr = length(eqx),
    ax = c(rep(n/2,length(eta)-1), NA),
    lx = c(500000,rep(0,(length(eta)-1))),
    px = 1 - eqx,
    Lx = NA)
  
# simulate lifetimes (elimination by death)  
  for (i in 1:(nrow(dat)-1)) {
    ## draw random prob for each individual from the cohort
    threshold <- runif(dat$lx[i])
    ## compare with age-spec prob from the models
    lx.test <- as.numeric(dat$qx[i] > threshold)
    ## survived to the next age interval is the subtraction of deaths
    dat$lx[i + 1] <- dat$lx[i] - sum(lx.test)
    ## Number of person-years lived between ages x and x+n
    dat$Lx[i] <- n*(dat$lx[i + 1]) + dat$ax[i] * sum(lx.test)
    
  }  
  
  dat$Lx[nrow(dat)] <- 0
  dat$Tx <-  rev(cumsum(rev(dat$Lx)))
  dat$ex <-  dat$Tx/dat$lx
  
  
  return(dat)
  
}


# DFLE formula, given Sullivan method
exDF <- function(lx, wx, Lx){
  dfle <- 1/lx[1]*sum((1-wx)*Lx)
  return(dfle)
}


# re-sampling for bootstraps
## data needs to be in data.table format
long_resample <- function(dat, id, time) {
  
  id = substitute(id)
  time = substitute(time)
  
  tmp <- split(dat, by = id)
  
  idx <- unique(dat[[id]])
  
  resample_id <- sample(idx, length(idx), replace = TRUE)
  
  resample_dat <- lapply(seq_along(resample_id), function(x) {
    res_dat <- tmp[[as.character(resample_id[x])]]
    res_dat$new_id <- x
    return(res_dat)
  })
  
  resample_dat <- do.call(rbind, resample_dat)
  
  resample_dat
}

# confidence intervals 
confidence_interval <- function(vector) {
  estimate <- mean(vector)
  ci <- quantile(vector, probs = c(.025,.975),type=8)
  result <- c(estimate, ci)
  return(result)
}
