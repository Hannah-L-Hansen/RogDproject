#testing assumptions


#visual check
qqnorm(bmp_test1_cell$BMP.bmp0.5p)

ggplot(bmp_test1_cell,aes(BMP.bmp0.5p))+
  geom_histogram(aes(y=..density..))+
  stat_function(fun=dnorm, 
                args = list(mean=mean(bmp_test1_cell$BMP.bmp0.5p),
                            sd=sd(bmp_test1_cell$BMP.bmp0.5p)))
#numeric check
library(pastecs)

#test1 cell 0.5
stat.desc(bmp_test1_cell$BMP.bmp0.5p, norm=TRUE)
#test 1 SA 0.5
stat.desc(bmp_test1_SA$BMP.bmp0.5p, norm=TRUE)
#test 1 SB 0.5
stat.desc(bmp_test1_SB$BMP.bmp0.5p, norm=TRUE)
#test 1 SC 0.5
stat.desc(bmp_test1_SC$BMP.bmp0.5p, norm=TRUE)

#test2 cell 0.5
stat.desc(bmp_test2_cell$BMP.bmp0.5p, norm=TRUE)
#test 2 SA 0.5
stat.desc(bmp_test2_SA$BMP.bmp0.5p, norm=TRUE)
#test 2 SB 0.5
stat.desc(bmp_test2_SB$BMP.bmp0.5p, norm=TRUE)
#test 2 SC 0.5
stat.desc(bmp_test2_SC$BMP.bmp0.5p, norm=TRUE)



#testing 1p:

#test1 cell 1
stat.desc(bmp_test1_cell$BMP.bmp1p, norm=TRUE)
#test 1 SA 1
stat.desc(bmp_test1_SA$BMP.bmp1p, norm=TRUE)
#test 1 SB 1
stat.desc(bmp_test1_SB$BMP.bmp1p, norm=TRUE)
#test 1 SC 1
stat.desc(bmp_test1_SC$BMP.bmp1p, norm=TRUE)

#test2 cell 1
stat.desc(bmp_test2_cell$BMP.bmp1p, norm=TRUE)
#test 2 SA 1
stat.desc(bmp_test2_SA$BMP.bmp1p, norm=TRUE)
#test 2 SB 1
stat.desc(bmp_test2_SB$BMP.bmp1p, norm=TRUE)
#test 2 SC 1
stat.desc(bmp_test2_SC$BMP.bmp1p, norm=TRUE)




#visual check --> bmp_test1 (alle lab, ikke separeret efter substrat)
qqnorm(bmp_test1$BMP.bmp0.5p)

ggplot(bmp_test1,aes(BMP.bmp0.5p))+
  geom_histogram(aes(y=..density..))+
  stat_function(fun=dnorm, 
                args = list(mean=mean(bmp_test1$BMP.bmp0.5p),
                            sd=sd(bmp_test1$BMP.bmp0.5p)))
      #numeric check test1 0.5 
stat.desc(bmp_test1$BMP.bmp.0.5p, norm=TRUE)

#sqrt transformed (still not normally destributed)
stat.desc(bmp_test1$sqrtBMP.0.5p, norm=TRUE)

#log transformed
stat.desc(bmp_test1$logBMP.0.5p, norm=TRUE)

#reciprocal transformation
stat.desc(bmp_test1$recipBMP.0.5p, norm=TRUE)

       #numeric check test1 1p
stat.desc(bmp_test1$BMP.bmp1p, norm=TRUE)

#sqrt transformed (still not normally destributed)
stat.desc(bmp_test1$sqrtBMP.1p, norm=TRUE)

#log transformed
stat.desc(bmp_test1$logBMP.1p, norm=TRUE)

#reciprocal transformation
stat.desc(bmp_test1$recipBMP.1p, norm=TRUE)




#visual check --> bmp_test2 (alle lab, ikke separeret efter substrat)
qqnorm(bmp_test2$BMP.bmp0.5p)

ggplot(bmp_test2,aes(BMP.bmp0.5p))+
  geom_histogram(aes(y=..density..))+
  stat_function(fun=dnorm, 
                args = list(mean=mean(bmp_test2$BMP.bmp0.5p),
                            sd=sd(bmp_test2$BMP.bmp0.5p)))

#numeric check
stat.desc(bmp_test2$BMP.bmp0.5p, norm=TRUE)
