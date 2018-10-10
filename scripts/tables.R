#Summarising data for tables

#Table with substrate, duration (in days) and BMP value, separated by method (bmp1p and bmp 0.5p)

#creating 2 separate dataframes for the 2 tests:

bmp_test1 <- filter(bmpPlot30, test == 1)

bmp_test2 <- filter(bmpPlot30, test==2)

#method = 0.5p test 1
 durBmp_0.5p_t1 <- bmp_test1 %>% 
  group_by(substrate) %>%
  summarise(time.d.bmp0.5pmn = mean(time.d.bmp0.5p, na.rm = TRUE), 
            sdtime.d.bmp0.5p = sd(time.d.bmp0.5p, na.rm = TRUE),
            Bmp.0.5pmn = mean(BMP.bmp0.5p, na.rm = TRUE),
            sdBmp.0.5p = sd(BMP.bmp0.5p, na.rm = TRUE))
           
#method = 1p test1
 
 durBmp_1p_t1 <- bmp_test1 %>% 
   group_by(substrate) %>%
   summarise( time.d.bmp1pmn = mean(time.d.bmp1p, na.rm = TRUE),
              sdtime.d.bmp1p = sd(time.d.bmp1p, na.rm = TRUE),
              Bmp.1pmn = mean(BMP.bmp1p, na.rm = TRUE),
              sdBmp.1p = sd(BMP.bmp1p, na.rm = TRUE))
 
 
 #method = 0.5p test 2
 durBmp_0.5p_t2 <- bmp_test2 %>% 
   group_by(substrate) %>%
   summarise(time.d.bmp0.5pmn = mean(time.d.bmp0.5p, na.rm = TRUE), 
             sdtime.d.bmp0.5p = sd(time.d.bmp0.5p, na.rm = TRUE),
             Bmp.0.5pmn = mean(BMP.bmp0.5p, na.rm = TRUE),
             sdBmp.0.5p = sd(BMP.bmp0.5p, na.rm = TRUE))
 
 #method = 1p test2
 
 durBmp_1p_t2 <- bmp_test2 %>% 
   group_by(substrate) %>%
   summarise( time.d.bmp1pmn = mean(time.d.bmp1p, na.rm = TRUE),
              sdtime.d.bmp1p = sd(time.d.bmp1p, na.rm = TRUE),
              Bmp.1pmn = mean(BMP.bmp1p, na.rm = TRUE),
              sdBmp.1p = sd(BMP.bmp1p, na.rm = TRUE))
 
 
 #fixed 20 and 30 days end:
 #method = 1p test1
 
 bmp2030_t1 <- bmp_test1 %>% 
   group_by(substrate) %>%
   summarise( BMP20mn = mean(BMP20, na.rm = TRUE),
              sdBMP20 = sd(BMP20, na.rm = TRUE),
              BMP30mn = mean(BMP30, na.rm = TRUE),
              sdBMP30 = sd(BMP30, na.rm = TRUE))
 
 
bmp2030_t2 <- bmp_test2 %>% 
   group_by(substrate) %>%
   summarise( BMP20mn = mean(BMP20, na.rm = TRUE),
              sdBMP20 = sd(BMP20, na.rm = TRUE),
              BMP30mn = mean(BMP30, na.rm = TRUE),
              sdBMP30 = sd(BMP30, na.rm = TRUE))
 