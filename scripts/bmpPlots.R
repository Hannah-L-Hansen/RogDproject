
#reshapeing data frame for comparrison of BMP0.5p and BMP1p


#creating and adding rank columns

labRankDataFrame <- bmpPlot30 %>%
  group_by(lab) %>%
  summarise() %>%
  arrange(lab)%>%
  mutate(labPlotRank=row_number())

substrateRankDataFrame <- bmpPlot30 %>%
  group_by(substrate) %>%
  summarise() %>%
  arrange(substrate)%>%
  mutate(substratePlotRank=row_number())


#merging labRank and substrateRank onto bmpPlot30

bmpPlot30 <- merge(x = bmpPlot30, y = labRankDataFrame, by = c('lab'), all.x = TRUE)

bmpPlot30 <- merge(x = bmpPlot30, y = substrateRankDataFrame, by = c('substrate'), all.x = TRUE)


reshapeBMP0.5pBMP1p <- function(x){
return(
  rbind(
   x %>% 
   select
        ( 
          substratePlotRank=substratePlotRank,
          labPlotRank=labPlotRank,
            test=test, 
            lab=lab, 
            substrate=substrate, 
            time.d=time.d.bmp0.5p, 
            BMP=BMP.bmp0.5p) %>%
  mutate(calcMethod='0.5p'),
    x %>% 
    select( 
      substratePlotRank=substratePlotRank,
      labPlotRank=labPlotRank,
            test=test,
            lab=lab, 
            substrate=substrate, 
            time.d=time.d.bmp1p, 
            BMP=BMP.bmp1p) %>%
            mutate(calcMethod ='1p')
            ))
}

dfPlot0.5And1 <- reshapeBMP0.5pBMP1p(bmpPlot30)


#Bar plot 

# ggplot(
#   data = dfPlot0.5And1, 
#   aes(
#     x=1+((labPlotRank-1)*2+yIndex),
#     y=BMP,
#     fill=lab
#   )) +
#   geom_col()+
#   facet_wrap(facets = 'substrate')+
#   labs(x='lab')

#boxplot by substrate

# ggplot(
#   data = dfPlot0.5And1, 
#   aes(
#     x=substrate,
#     y=BMP,
#     group=substrate)) +
#   facet_grid(yIndex~.)+
#   geom_boxplot(width = 0.3)
 
#boxplot
ggplot(data = dfPlot0.5And1, aes(substrate, BMP, fill = factor(calcMethod))) +
  geom_boxplot()+
  #facet_grid(.~ substrate) +
  labs(x='substrate',fill = 'method')

 

#bar chart
ggplot(data = dfPlot0.5And1, aes(lab, BMP, fill = factor(calcMethod))) +
       geom_col(position = 'dodge')+
       facet_wrap(~ substrate) +
       labs(x='lab',fill = 'method')
