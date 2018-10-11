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


#creating dataframes to count from:

rate.met_test1_0.5p <- filter(dfPlot0.5And1, test == 1 & calcMethod == '0.5p')
rate.met_test1_1p <- filter(dfPlot0.5And1, test == 1 & calcMethod == '1p')

rate.met_test2_0.5p <- filter(dfPlot0.5And1, test==2 & calcMethod == '0.5p')
rate.met_test2_1p <- filter(dfPlot0.5And1, test == 2 & calcMethod == '1p')

#counting rate.met:

is.rate.met_t1_0.5<-rate.met_test1_0.5p%>%
  group_by(substrate)%>%
  count(rate.met)

is.rate.met_t1_1<-rate.met_test1_1p%>%
  group_by(substrate)%>%
  count(rate.met)

is.rate.met_t2_0.5p<-rate.met_test2_0.5p%>%
  group_by(substrate)%>%
  count(rate.met)

is.rate.met_t2_1p<-rate.met_test2_1p%>%
  group_by(substrate)%>%
  count(rate.met)


# performing t tests:

#separating by substrate

#Test1:

bmp_test1_cell <- filter(bmp_test1, substrate == 'CEL')
bmp_test1_SA <- filter(bmp_test1, substrate == 'SA')
bmp_test1_SB <- filter(bmp_test1, substrate == 'SB')
bmp_test1_SC <- filter(bmp_test1, substrate == 'SC')

#Test2:
bmp_test2_cell <- filter(bmp_test2, substrate == 'CEL')
bmp_test2_SA <- filter(bmp_test2, substrate == 'SA')
bmp_test2_SB <- filter(bmp_test2, substrate == 'SB')
bmp_test2_SC <- filter(bmp_test2, substrate == 'SC')


#t tests:

tTest0.5p1p <- function(x){
  return(t.test(x$BMP.bmp0.5p,x$BMP.bmp1p))
}
 

t_test_t1cell <- tTest0.5p1p(bmp_test1_cell)

t_test_t1cell

t_test_t1SA <- tTest0.5p1p(bmp_test1_SA)

t_test_t1SA

t_test_t1SB <- tTest0.5p1p(bmp_test1_SB)
t_test_t1SB

t_test_t1SC <- tTest0.5p1p(bmp_test1_SC)
t_test_t1SC

t_test_t2cell <- tTest0.5p1p(bmp_test2_cell)
t_test_t2cell

t_test_t2SA <- tTest0.5p1p(bmp_test2_SA)
t_test_t2SA 

t_test_t2SB <- tTest0.5p1p(bmp_test2_SB)
t_test_t2SB

t_test_t2SC <- tTest0.5p1p(bmp_test2_SC)
t_test_t2SC

#ANOVA:

ANOVA_test1 <- aov(BMP.bmp0.5p ~ BMP.bmp1p, data = bmp_test1)

