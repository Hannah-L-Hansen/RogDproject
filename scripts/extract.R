# Extract results from biogas calcs

# Extract
cbg <- bmp1p <- bmp1pg <- bmp0.5p <- bmp20 <- bmp30 <- yldmn <- yld <- inocyld <- inocyldmn <- inocyldend <- inocrate <- inocrate1dmn <- ratesg <- rates <- foc <- data.frame()

for(i in names(dat)) {
  #cat('\n', i, i, i, i, '\n')
  cat(i, i, i)

  test <- substr(i, 5, 5)
  lab <- substr(i, 2, 3)
  #lab <- dat[[i]]$args$id
  method <- dat[[i]]$args$method

  d <- dat[[i]]$cbg
  d <- cbind(test = test, lab = lab, file = i, d)
  cbg <- rbindf(cbg, d)
  
  d <- dat[[i]]$bmp1p
  d <- cbind(test = test, lab = lab, file = i, method = method, d)
  d$BMP <- d$mean
  bmp1p <- rbindf(bmp1p, d)
   
  d <- dat[[i]]$bmp1pg
  d <- cbind(test = test, lab = lab, file = i, method = method, d)
  d$BMP <- d$mean
  bmp1pg <- rbindf(bmp1pg, d)
   
  d <- dat[[i]]$bmp0.5p
  d <- cbind(test = test, lab = lab, file = i, method = method, d)
  d$BMP <- d$mean
  bmp0.5p <- rbindf(bmp0.5p, d)

  d <- dat[[i]]$bmp20
  d <- cbind(test = test, lab = lab, file = i, method = method, d)
  d$BMP <- d$mean
  bmp20 <- rbindf(bmp20, d)
   
  d <- dat[[i]]$bmp30
  d <- cbind(test = test, lab = lab, file = i, method = method, d)
  d$BMP <- d$mean
  bmp30 <- rbindf(bmp30, d)
  
  d <- dat[[i]]$yldmn
  d <- cbind(test = test, lab = lab, file = i, d)
  yldmn <- rbindf(yldmn, d)
  
  d <- dat[[i]]$yld
  d <- cbind(test = test, lab = lab, file = i, method = method, d)
  yld <- rbindf(yld, d)
   
  d <- dat[[i]]$inocyld
  d <- cbind(test = test, lab = lab, file = i, d)
  inocyld <- rbindf(inocyld, d)
   
  d <- dat[[i]]$inocyldmn
  d <- cbind(test = test, lab = lab, file = i, d)
  inocyldmn <- rbindf(inocyldmn, d)
   
  d <- dat[[i]]$inocyldend
  d <- cbind(test = test, lab = lab, file = i, d)
  inocyldend <- rbindf(inocyldend, d)
    
  d <- dat[[i]]$inocrate
  d <- cbind(test = test, lab = lab, file = i, d)
  inocrate <- rbindf(inocrate, d)
    
  d <- dat[[i]]$inocrate1dmn
  d <- cbind(test = test, lab = lab, file = i, d)
  inocrate1dmn <- rbindf(inocrate1dmn, d)
  
  d <- dat[[i]]$ratesg
  d <- cbind(test = test, lab = lab, file = i, d)
  ratesg <- rbindf(ratesg, d)
  
  d <- dat[[i]]$rates
  d <- cbind(test = test, lab = lab, file = i, d)
  rates <- rbindf(rates, d)
  
  #d <- dat[[i]]$foc
}


#bmp$lwr <- bmp$mean - bmp$sd
#bmp$upr <- bmp$mean + bmp$sd

# Set rates < 0 to 0
#rates$rrvCH4[rates$rrvCH4 < 0] <- 0
