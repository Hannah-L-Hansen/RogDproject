
##Creating plots for comparison of 0.5p and 1p method:

#boxplot with rate.met,test and method
ggplot(data = dfPlot0.5And1, aes(substrate, BMP, fill = calcMethod)) +
  geom_boxplot()+
  facet_grid(test ~ rate.met)  +
  labs(x='substrate',fill = 'method', facet_grid='test')

#boxplot with test and method
ggplot(data = dfPlot0.5And1, aes(substrate, BMP, fill = calcMethod)) +
  geom_boxplot()+
  facet_grid(test ~ .)  +
  labs(x='substrate',fill = 'method', facet_grid='test')


#bar chart with rate.met as colours , have to change colours to be able to see data better
ggplot(data = dfPlot0.5And1, aes(lab, BMP, fill = calcMethod, colour = rate.met)) +
       geom_col(position = 'dodge')+
       facet_grid(test ~ substrate) +
       labs(x='lab',fill = 'method')

#bar chart with test, method and substrate   --> adding sd bars?
ggplot(data = dfPlot0.5And1, aes(lab, BMP, fill = calcMethod)) +
  geom_col(position = 'dodge')+
  facet_grid(test ~ substrate) +
  labs(x='lab',fill = 'method')

##Creating plots for comparison of 20 and 30 days end time method:

#boxplot 
ggplot(data = dfPlot20_30, aes(substrate, BMP, fill = endTime)) +
  geom_boxplot()+
  facet_grid(test ~ .)  +
  labs(x='substrate',fill = 'End time', facet_grid='test')


#bar chart
ggplot(data = dfPlot20_30, aes(lab, BMP, fill = endTime)) +
  geom_col(position = 'dodge')+
  facet_grid(test ~ substrate) +
  labs(x='lab',fill = 'End time')

