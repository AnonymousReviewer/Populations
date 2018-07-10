
library(fitdistrplus)


getDistribType <- function(x,distrnames = c("norm", "lnorm", "exp", "gamma", "unif","logis"),criteria="ks"){
  # other potential distribs (need additional parameters depending on the estimation method)
  #,"weibull")
  fits = list()
  for(distrname in distrnames){
    fits[[distrname]] = fitdistrplus::fitdist(x,distr = distrname,method = "mme")
  }
  
  # available goodness of fit statistics
  #gofstat(fits)
  #gofstat(fits)$kstest
  #gofstat(fits)$cvmtest
  #gofstat(fits)$adtest
  
  gof = gofstat(fits)[[criteria]]
  bestdistr= distrnames[gof==min(gof)]
  return(list(
    name=bestdistr,
    fit = fits[[bestdistr]]
    )
  )
  
}

