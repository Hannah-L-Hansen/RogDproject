#Summarising data for tables

#Table with substrate, duration (in days) and BMP value, separated by method (bmp1p and bmp 0.5p)

#creating 2 separate dataframes for the 2 tests:

##Test 1, only data where rate is met

test1_1p <- filter(bmpRate.met0.5p1p, test == 1 & calcMethod == '1p')

test1_0.5p <- filter(bmpRate.met0.5p1p, test == 1 & calcMethod =='0.5p')

##test2, only data where rate is met

test2_1p <- filter(bmpRate.met0.5p1p, test==2 & calcMethod == '1p')

test2_0.5p <- filter(bmpRate.met0.5p1p, test==2 & calcMethod == '0.5p')


#summary tests
 Summarytest1_1p <- test1_1p %>% 
  group_by(substrate) %>%
  summarise(mntime = mean(time.d, na.rm = TRUE), 
            sdtime = sd(time.d, na.rm = TRUE),
            mnBmp = mean(BMP, na.rm = TRUE),
            sdBmp = sd(BMP, na.rm = TRUE))
 
 
 Summarytest1_0.5p <- test1_0.5p %>% 
   group_by(substrate) %>%
   summarise(mntime = mean(time.d, na.rm = TRUE), 
             sdtime = sd(time.d, na.rm = TRUE),
             mnBmp = mean(BMP, na.rm = TRUE),
             sdBmp = sd(BMP, na.rm = TRUE))
 
  Summarytest2_1p <- test2_1p %>% 
   group_by(substrate) %>%
   summarise(mntime = mean(time.d, na.rm = TRUE), 
             sdtime = sd(time.d, na.rm = TRUE),
             mnBmp = mean(BMP, na.rm = TRUE),
             sdBmp = sd(BMP, na.rm = TRUE))


 Summarytest2_0.5p <- test2_0.5p %>% 
   group_by(substrate) %>%
   summarise(mntime = mean(time.d, na.rm = TRUE), 
             sdtime = sd(time.d, na.rm = TRUE),
             mnBmp = mean(BMP, na.rm = TRUE),
             sdBmp = sd(BMP, na.rm = TRUE))
 
 
 ##Summary time run:
 
 SummaryTime <- dfPlot0.5And1 %>% 
   group_by(test) %>%
   summarise(mntime = mean(time.d, na.rm = TRUE), 
             sdtime = sd(time.d, na.rm = TRUE))
             
CountLabs <- dfPlot0.5And1 %>%
  group_by(test)%>%
  count(test)

#How many times was rate.met=TRUE
 
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
summary(ANOVA_test1)
#Effekten af at skrifte fra lab til lab er signifikant = signifikant forskel

