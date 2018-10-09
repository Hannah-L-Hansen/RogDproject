#Summarising data for tables

#Table with substrate, duration (in days) and BMP value, separated by method (bmp1p and bmp 0.5p)


#method = 0.5p
 durAndBmp_Table_0.5p <- bmpPlot30 %>% 
  group_by(substrate) %>%
  summarise(time.d.bmp0.5pmn = mean(time.d.bmp0.5p, na.rm = TRUE), 
            sdtime.d.bmp0.5p = sd(time.d.bmp0.5p, na.rm = TRUE),
            Bmp.0.5pmn = mean(BMP.bmp0.5p, na.rm = TRUE),
            sdBmp.0.5p = sd(BMP.bmp0.5p, na.rm = TRUE))
           
#method = 1p
 
 durAndBmp_Table_1p <- bmpPlot30 %>% 
   group_by(substrate) %>%
   summarise( time.d.bmp1pmn = mean(time.d.bmp1p, na.rm = TRUE),
              sdtime.d.bmp1p = sd(time.d.bmp1p, na.rm = TRUE),
              Bmp.1pmn = mean(BMP.bmp1p, na.rm = TRUE),
              sdBmp.1p = sd(BMP.bmp1p, na.rm = TRUE))
 