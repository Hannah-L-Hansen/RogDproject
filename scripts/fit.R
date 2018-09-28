# Fits models

library(minpack.lm)


# Fits one-pool first-order model to data from single bottle
fit1o <- function(
  dat,
  t,
  y = 'cvCH4',
  modtype = 'fo1p'        # Number of degradable pools
  #ytype = 'cum'
  ) {
  
  # Pull out columns that are needed
  d <- dat[, c(t, y)]

  d$t <- d[, t]

  # Drop any zero time
  d <- d[d$t != 0, ]

  # Calculate average production rates
  d <- d[order(d[, t]), ]
  d$ra <- diff(c(0, d[, y]))/diff(c(0, d[, t]))
  
  # Default values in case of nls error
  cc <- c(a = NA, k = NA)
  mssg <- 0
  rss <- NA

  # Fit model
  if(modtype == 'fo1p') {
    try(
        {
        # Starting values
        sval <- c(a = max(d[, y]), k = -1)
        
        mod <- nlsLM(ra ~ (
                           a*(1 - exp(-10^k*t)) - 
                           c(0, a*(1 - exp(-10^k*t[-nrow(d)])))
                          )/(diff(c(0, t))), 
                   data = d, start = sval)
        }
    )
    
  } else if(modtype == 'fo2p') {
    try(
        {
        sval <- c(a = max(d[, y]), k1 = -0.5, k2 = -2, f1 = 0)
        
        mod <- nlsLM(ra ~ (
                           logistic(f1)*a*(1 - exp(-10^k1*t)) - 
                           c(0, logistic(f1)*a*(1 - exp(-10^k1*t[-nrow(d)]))) + 
                           (1 - logistic(f1))*a*(1 - exp(-10^k2*t)) - 
                           c(0, (1 - logistic(f1))*a*(1 - exp(-10^k2*t[-nrow(d)])))
                          )/(diff(c(0, t))), 
                   data = d, start = sval)
        }
    )
 
  } else if(modtype == 'monod') {
    try(
        {
        sval <- c(a = max(d[, y]), k = -1)
        
        mod <- nlsLM(ra ~ (
                           a*10^k*t/(1 + 10^k*t) - 
                           c(0, a*10^k*t[-nrow(d)]/(1 + 10^k*t[-nrow(d)]))
                          )/(diff(c(0, t))), 
                   data = d, start = sval)
        }
    )
 
  } else if(modtype == 'gompertz') {
    try(
        {
        sval <- c(a = max(d[, y]), k = -0.5, ti = 0)

        d$weight <- 1/d$ra
        d$weight[!is.finite(d$weight)] <- max(d$weight[is.finite(d$weight)])
        d$weight[is.na(d$weight)] <- 0
        
        mod <- nlsLM(ra ~ (
                           a*exp(-exp(-2.17828*10^k * (t - ti))) -
                           c(0, a*exp(-exp(-2.17828*10^k * (t[-nrow(d)] - ti))))
                          )/(diff(c(0, t))), 
                   data = d, start = sval) #, weights = weight)
        }
    )
 
  } else {
    stop(paste0('modtype argument must xxx but is ', modtype, '.'))
  }

  return(mod)

}

fitMods <- function(
  dat,
  id.name,
  time.name,
  vol.name,
  modtype = 'fo1p' 
) {
  
  coefs <- data.frame()
  preds <- dat[, c(id.name, time.name)]
  preds$pred <- NA
  
  for(i in unique(dat[, id.name]))  {
  
    d <- dat[dat[, id.name] == i, ]   
   
    mod <- fit1o(d, t = time.name, y = vol.name, modtype = modtype)
    cc <- data.frame(t(coef(mod)))
    mssg <- mod$convInfo$stopCode

    coefs <- rbind(coefs, data.frame(id = i, cc, mssg = mssg))

    if(modtype == 'fo1p') {
      preds[preds[, id.name] == i, 'pred'] <- 
          cc$a*(1 - exp(-10^cc$k*preds[preds[, id.name] == i, time.name])) 
    } else if(modtype == 'fo2p') {
      preds[preds[, id.name] == i, 'pred'] <- 
          logistic(cc$f1)*cc$a*(1 - exp(-10^cc$k1*preds[preds[, id.name] == i, time.name])) + 
          (1 - logistic(cc$f1))*cc$a*(1 - exp(-10^cc$k2*preds[preds[, id.name] == i, time.name]))
    } else if(modtype == 'monod') {
      preds[preds[, id.name] == i, 'pred'] <- 
          cc$a*10^cc$k*preds[preds[, id.name] == i, time.name]/
          (1 + 10^cc$k*preds[preds[, id.name] == i, time.name]) 
    } else if(modtype == 'gompertz') {
      preds[preds[, id.name] == i, 'pred'] <- 
          cc$a*exp(-exp(-2.17828*10^cc$k * (preds[preds[, id.name] == i, time.name] - cc$ti))) 
    } 
   
  }

  
  names(coefs)[names(coefs) == 'id'] <- id.name
  #return(coefs)
  return(list(coefs = coefs, preds = preds))
}
