#grouping biogas calc for plotting


#merging dataframes bmp0.5p and bmp1% to create new dataframe bmpPlot to use for plotting

bmpMerge <- merge(x = bmp0.5p, y = bmp1p, by = c('test','lab','substrate'), all = TRUE, suffixes = c('.bmp0.5p','.bmp1p'))

bmpPlot <- bmpMerge[,c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p')]

#add bmp20 and bmp30


#add bmp 20
bmpMerge20 <- merge(x = bmpPlot, y = bmp20, by = c('test','lab','substrate'), all = TRUE, suffixes = c('.bmpPlot','.bmp20'))

bmpPlot20 <- bmpMerge20[,c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p','time.d','BMP')]

#naming bmpPlot20

names(bmpPlot20) <- c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p','time.d.BMP20','BMP20')

#Add bmp30

bmpMerge30 <- merge(x = bmpPlot20, y = bmp30, by = c('test','lab','substrate'), all = TRUE)

bmpPlot30 <- 
  bmpMerge30[,c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p','time.d.BMP20','BMP20','time.d','BMP')]

#naming bmpPlot30

names(bmpPlot30) <- 
  c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p','time.d.BMP20','BMP20','time.d.BMP30','BMP30')


#add gross vs net --> figure out if bmp0.5pg is possible to add first!

