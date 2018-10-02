#grouping biogas calc for plotting

BMP1p <- bmp1p %>%
  group_by(lab) %>%
  summarise(
    time = mean(time.d, na.rm = TRUE),
    BMP1p = mean(BMP, na.rm = TRUE)
   
  )

BMP0.5p <- bmp0.5p %>%
  group_by(lab) %>%
  summarise(
    BMP0.5p= mean(BMP, na.rm = TRUE)                 
  )

#creating bmpCompare - data frame with time, lab no, BMP1p and BMp0.5p

bmpCompare <- as.data.frame(mutate(BMP1p, BMP0.5p$BMP0.5p))
bmpCompare

#quick plot of time vs. BMP1p
qplot(lab, BMP1p, BMP0.5p$BMP0.5p, data = bmpCompare, xlab='Time (days)', ylab='BMP', colour = lab)
