#grouping biogas calc for plotting


# BMP1p <- bmp1p %>%
#   group_by(lab) %>%
#   summarise(
#   )
# 
# BMP0.5p <- bmp0.5p %>%
#   group_by(lab) %>%
#   summarise(
#     BMP0.5p= mean(BMP, na.rm = TRUE)                 
#   )

#Creating data frame with testno, time, lab no, substrate, bmp1p and Bmp0.5p

# bmpCompare <- bmp0.5p[,c('test','lab','substrate','time.d','BMP')]
# 
# #extracting BMP column from bmp1p:
# 
# bmp1percent <- bmp1p[,c('BMP')]
# 
# #addition of bmp1percent to bmpCompare
# 
# bmpCompare <- as.data.frame(cbind(bmpCompare,bmp1percent))
# 
# #Naming columns in bmpCompare
# 
# names(bmpCompare) <- c('test','lab','substrate','time','bmp0.5p','bmp1p')

#merging dataframes bmp0.5p and bmp1%

bmpMerge <- merge(x = bmp0.5p, y = bmp1p, by = c('test','lab','substrate'), all = TRUE, suffixes = c('.bmp0.5p','.bmp1p'))


bmpPlot <- bmpMerge[,c('test','lab','substrate','time.d.bmp0.5p','time.d.bmp1p', 'BMP.bmp0.5p', 'BMP.bmp1p')]

#navngiv kolonner
#names(bmpPlot) <- c('test')

#add bmp20 and bmp30
#add gross vs net

