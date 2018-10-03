#grouping biogas calc for plotting

# BMP1p <- bmp1p %>%
#   group_by(lab) %>%
#   summarise(
#     time = mean(time.d, na.rm = TRUE),
#     BMP1p = mean(BMP, na.rm = TRUE)
#    
#   )
# 
# BMP0.5p <- bmp0.5p %>%
#   group_by(lab) %>%
#   summarise(
#     BMP0.5p= mean(BMP, na.rm = TRUE)                 
#   )

#Creating data frame with testno, time, lab no, substrate, bmp1p and Bmp0.5p

bmpCompare <- bmp0.5p[,c('test','lab','substrate','time.d','BMP')]

#extracting BMP column from bmp1p:

bmp1percent <- bmp1p[,c('BMP')]

#addition of bmp1percent to bmpCompare

bmpCompare <- as.data.frame(cbind(bmpCompare,bmp1percent))

#Naming columns in bmpCompare

names(bmpCompare) <- c('test','lab','substrate','time','bmp0.5p','bmp1p')


