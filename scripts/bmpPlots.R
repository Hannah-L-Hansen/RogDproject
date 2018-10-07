#plottong bmp1p and bmp0.5p

#ggplot template:

# ggplot(data = bmpPlot30, aes(time.d.bmp0.5p, BMP.bmp0.5p, colour = lab, group = lab) + 
#   geom_point() + facet_grid(~ substrate) + 
#   labs(x = 'Time (d)', y = 'BMP', colour = 'lab'))
 

#Create sorting function

#sortedBmpPlot30 <- bmpPlot30[with(bmpPlot30, order(BMP.bmp0.5p+BMP.bmp1p)),] 

#reshapeing data frame for comparrison of BMP0.5p and BMP1p


#creating and adding rank column

labRankDataFrame <- bmpPlot30 %>% 
  group_by(lab) %>%
  summarise() %>%
  arrange(lab)%>%
  mutate(labPlotRank=row_number())

#merging labRank onto bmpPlot30

bmpPlot30 <- merge(x = bmpPlot30, y = labRankDataFrame, by = c('lab'), all.x = TRUE)
  

reshapeBMP0.5pBMP1p <- function(x){
return(
  rbind(
   x %>% 
   select
        ( 
          labPlotRank=labPlotRank,
            test=test, 
            lab=lab, 
            substrate=substrate, 
            time.d=time.d.bmp0.5p, 
            BMP=BMP.bmp0.5p) %>%
  mutate(yIndex=0),
    x %>% 
    select(   
      labPlotRank=labPlotRank,
            test=test, 
            lab=lab, 
            substrate=substrate, 
            time.d=time.d.bmp1p, 
            BMP=BMP.bmp1p) %>%
            mutate(yIndex=1)
            ))
}

dfPlot0.5And1 <- reshapeBMP0.5pBMP1p(bmpPlot30)


#Bar plot 

ggplot(
  data = dfPlot0.5And1, 
  aes(
    x=1+((labPlotRank-1)*2+yIndex),
    y=BMP,
    color= 000,
    fill=lab
    )) +
 geom_col()+
  facet_wrap(facets = 'substrate')



