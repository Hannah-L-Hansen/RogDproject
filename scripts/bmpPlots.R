
##Creating plots

#boxplot
ggplot(data = dfPlot0.5And1, aes(substrate, BMP, fill = calcMethod)) +
  geom_boxplot()+
  facet_grid(test ~ substrate)  +
  labs(x='substrate',fill = 'method', facet_grid='test')

 

#bar chart
ggplot(data = dfPlot0.5And1, aes(lab, BMP, fill = calcMethod)) +
       geom_col(position = 'dodge')+
       facet_grid(test ~ substrate) +
       labs(x='lab',fill = 'method')
