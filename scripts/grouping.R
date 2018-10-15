#grouping biogas calc for plotting


#merging dataframes bmp0.5p and bmp1% to create new dataframe bmpPlot to use for plotting

bmpMerge <- merge(x = bmp0.5p, y = bmp1p, by = c('test','lab','substrate'), all = TRUE, suffixes = c('.bmp0.5p','.bmp1p'))

bmpPlot <- bmpMerge[,c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p','rate.met.bmp1p','rate.met.bmp0.5p')]

#add bmp20 and bmp30


#add bmp 20
bmpMerge20 <- merge(x = bmpPlot, y = bmp20, by = c('test','lab','substrate'), all = TRUE, suffixes = c('.bmpPlot','.bmp20'))

bmpPlot20 <- bmpMerge20[,c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p','rate.met.bmp1p','rate.met.bmp0.5p','time.d','BMP')]

#naming bmpPlot20

names(bmpPlot20) <- c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p','rate.met.bmp1p','rate.met.bmp0.5p','time.d.BMP20','BMP20')

#Add bmp30

bmpMerge30 <- merge(x = bmpPlot20, y = bmp30, by = c('test','lab','substrate'), all = TRUE)

bmpPlot30 <- 
  bmpMerge30[,c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p','rate.met.bmp1p','rate.met.bmp0.5p','time.d.BMP20','BMP20','time.d','BMP')]

#naming bmpPlot30

names(bmpPlot30) <- 
  c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p','rate.met.bmp1p','rate.met.bmp0.5p','time.d.BMP20','BMP20','time.d.BMP30','BMP30')


#reshapeing data frame for comparrison of BMP0.5p and BMP1p

reshapeDataFrame <- function(x){
  return(
    rbind(
      x %>% 
        select( 
        test=test, 
        lab=lab, 
        substrate=substrate, 
        time.d=time.d.bmp0.5p, 
        rate.met=rate.met.bmp0.5p,
        BMP=BMP.bmp0.5p) %>%
        mutate(calcMethod='0.5p'),
      x %>% 
        select( 
        test=test,
        lab=lab, 
        substrate=substrate, 
        time.d=time.d.bmp1p,
        rate.met=rate.met.bmp1p,           
        BMP=BMP.bmp1p) %>%
        mutate(calcMethod ='1p')
    ))
}

dfPlot0.5And1 <- reshapeDataFrame(bmpPlot30)

#reshapeing data frame for comparrison of BMP20 and BMP30

reshapeDataFrame2 <- function(x){
  return(
    rbind(
      x %>% 
        select
      ( test=test, 
        lab=lab, 
        substrate=substrate, 
        time.d=time.d.BMP20, 
        BMP=BMP20) %>%
        mutate(endTime='20 days'),
      x %>% 
        select( 
          test=test,
          lab=lab, 
          substrate=substrate, 
          time.d=time.d.BMP30,
          BMP=BMP30) %>%
        mutate(endTime='30 days')
    ))
}

dfPlot20_30 <- reshapeDataFrame2(bmpPlot30)

#Removing labs were rate.met = FALSE

bmpRate.met0.5p1p<- filter(dfPlot0.5And1, rate.met == TRUE)


