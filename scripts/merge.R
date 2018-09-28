# Combine files for Soeren

comb <- merge(cbg[, c('test', 'lab', 'file', 'id', 'substrate', 'm.inoc', 'm.sub', 'c.vs2.subf',
                      'm.inoc.vs', 'm.sub.vs', 'ISR',
                      'time.d', 'vBg', 'vCH4', 'cvCH4', 'cvBg')], 
              yld[, c('test', 'lab', 'file', 'time.d', 'id', 'substrate', 'method', 'cvCH4')],
              by = c('test', 'lab', 'file', 'time.d', 'id', 'substrate'), suffixes = c('', '.yld'),
              all = TRUE)

comb <- comb[order(comb$test, comb$lab, comb$substrate, comb$id, comb$time.d), ]

comb$m.sub.1
cbg$m.sub.1
yld$m.sub.1

head(cbg)
options(width = 70)

head(comb)
dim(comb)
table(comb$substrate)

# BMP results
bmpm <- rbindf(bmp20, bmp30, bmp1p, bmp0.5p, bmp1pg)
bmpmp <- rbindf(bmp1p, bmp0.5p)
#bmpmnotmet <- subset(bmpm, !rate.met)
#bmpm <- subset(bmpm, is.na(rate.met) | rate.met)

# Merge 1p and 0.5p for comparison
bmpwm <- merge(bmp1p, bmp0.5p, by = c('test', 'lab', 'file', 'method', 'substrate'), 
              suffixes = c('.1p', '.0.5p'))

bmpwm$dt <- bmpwm$time.d.0.5p - bmpwm$time.d.1p
bmpwm$dBMP <- bmpwm$BMP.0.5p - bmpwm$BMP.1p
bmpwm$rdBMP <- 100*bmpwm$dBMP/bmpwm$BMP.0.5p

bmpwm2 <- merge(bmp1p, bmp20, by = c('test', 'lab', 'file', 'method', 'substrate'), 
              suffixes = c('.1p', '.20d'))

bmpwm2$dt <- bmpwm2$time.d.20d - bmpwm2$time.d.1p
bmpwm2$dBMP <- bmpwm2$BMP.20d - bmpwm2$BMP.1p
bmpwm2$rdBMP <- 100*bmpwm2$dBMP/bmpwm2$BMP.20d

bmpwm3 <- merge(bmp1p, bmp1pg, by = c('test', 'lab', 'file', 'method', 'substrate'), 
              suffixes = c('.1p', '.1pg'))

bmpwm3$dt <- bmpwm3$time.d.1pg - bmpwm3$time.d.1p
bmpwm3$dBMP <- bmpwm3$BMP.1pg - bmpwm3$BMP.1p
bmpwm3$rdBMP <- 100*bmpwm3$dBMP/bmpwm3$BMP.1p


# Inoculum stuff (endogenous production)
inocrate1dmn$rate.1d <- inocrate1dmn$mean
inocyldend$rate.ave <- inocyldend$mean/inocyldend$time.d

inoc.cor <- merge(inocyldend, inocrate1dmn, by = c('test', 'lab'), suffixes = c('.end', '.1d'))
inoc.cor <- subset(inoc.cor, lab != 15 | time.d.end == 20)
