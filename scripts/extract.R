# Extract results from biogas calcs

# Extract
cbg <- bmp1p <- bmp0.5p <- bmp20 <- bmp30<- inocyld <- inocyldmn <- inocyldend <- data.frame()

for(i in names(dat)) {
  cat(i, i, i)

  test <- substr(i, 5, 5)
  lab <- substr(i, 2, 3)
  method <- dat[[i]]$args$method

  d <- dat[[i]]$cbg
  d <- cbind(test = test, lab = lab, file = i, d)
  cbg <- rbindf(cbg, d)
  
  d <- dat[[i]]$bmp1p
  d <- cbind(test = test, lab = lab, file = i, method = method, d)
  d$BMP <- d$mean
  bmp1p <- rbindf(bmp1p, d)
  
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
  
  d <- dat[[i]]$inocyld
  d <- cbind(test = test, lab = lab, file = i, d)
  inocyld <- rbindf(inocyld, d)
  
  d <- dat[[i]]$inocyldmn
  d <- cbind(test = test, lab = lab, file = i, d)
  inocyldmn <- rbindf(inocyldmn, d)
  
  d <- dat[[i]]$inocyldend
  d <- cbind(test = test, lab = lab, file = i, d)
  inocyldend <- rbindf(inocyldend, d)
}




