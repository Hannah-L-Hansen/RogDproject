
#reshapeing data frame for comparrison of BMP0.5p and BMP1p


#creating and adding rank columns --> ikke nødvendigt alligevel!

# labRankDataFrame <- bmpPlot30 %>%
#   group_by(lab) %>%
#   summarise() %>%
#   arrange(lab)%>%
#   mutate(labPlotRank=row_number())
# 
# substrateRankDataFrame <- bmpPlot30 %>%
#   group_by(substrate) %>%
#   summarise() %>%
#   arrange(substrate)%>%
#   mutate(substratePlotRank=row_number())


#merging labRank and substrateRank onto bmpPlot30

# bmpPlot30 <- merge(x = bmpPlot30, y = labRankDataFrame, by = c('lab'), all.x = TRUE)
# 
# bmpPlot30 <- merge(x = bmpPlot30, y = substrateRankDataFrame, by = c('substrate'), all.x = TRUE)


reshapeBMP0.5pBMP1p <- function(x){
return(
  rbind(
   x %>% 
   select
        (   test=test, 
            lab=lab, 
            substrate=substrate, 
            time.d=time.d.bmp0.5p, 
            rate.met=rate.met.bmp0.5p,
            BMP=BMP.bmp0.5p) %>%
  mutate(calcMethod='0.5p'),
    x %>% 
    select( 
      # substratePlotRank=substratePlotRank,
      # labPlotRank=labPlotRank,
            test=test,
            lab=lab, 
            substrate=substrate, 
            time.d=time.d.bmp1p,
            rate.met=rate.met.bmp1p, 
            BMP=BMP.bmp1p) %>%
            mutate(calcMethod ='1p')
            ))
}

dfPlot0.5And1 <- reshapeBMP0.5pBMP1p(bmpPlot30)

#separating dfPlot0.5And1 in the two tests

dfPlot0.5And1 <- group_by(dfPlot0.5And1, test)

##Creating plots

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
