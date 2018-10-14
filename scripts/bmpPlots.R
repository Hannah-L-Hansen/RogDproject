
##bmp Plots compariing 0.5p and 1p method:

#boxplot with test and method
box_0.5p_1p <- ggplot(data = bmp_rate.met, aes(substrate, BMP, fill = calcMethod)) +
  geom_boxplot()+
  facet_grid(test ~ .)  +
  labs(x='substrate',y='BMP (mg/L)', fill = 'method', facet_grid='test')

ggsave('boxplot0.5p_1p.png', plot = box_0.5p_1p, width = 250, height = 150, units = 'mm', path = '../plots/')

#bar chart with test, method and substrate   
bar_0.5p_1p <- ggplot(data = bmp_rate.met, aes(lab, BMP, fill = calcMethod)) +
  geom_col(position = 'dodge')+
  facet_grid(test ~ substrate) +
  labs(x='lab',y='BMP (mg/L)', fill = 'method')

ggsave('barchart 0.5p_1p.png', plot = bar_0.5p_1p, width = 275, height = 120, units = 'mm', path = '../plots/') 


## Duration plots comparing 0.5p and 1p method

#boxplot 
box_0.5p_1p <- ggplot(data = bmp_rate.met, aes(substrate, time.d, fill = calcMethod)) +
  geom_boxplot()+
  facet_grid(test ~ .)  +
  labs(x='substrate', y='time (days)',fill = 'method', facet_grid='test')

ggsave('Duration_boxplot0.5p_1p.png', plot = box_0.5p_1p, width = 250, height = 150, units = 'mm', path = '../plots/')

#bar chart   
bar_0.5p_1p <- ggplot(data = bmp_rate.met, aes(lab, time.d, fill = calcMethod)) +
  geom_col(position = 'dodge')+
  facet_grid(test ~ substrate) +
  labs(x='lab',y='time (days)', fill = 'method')

ggsave('Duration_barchart 0.5p_1p.png', plot = bar_0.5p_1p, width = 275, height = 120, units = 'mm', path = '../plots/') 

##Plots comparing 20 and 30 days end time method:

#boxplot 
box_20_30 <- ggplot(data = dfPlot20_30, aes(substrate, BMP, fill = endTime)) +
  geom_boxplot()+
  facet_grid(test ~ .)  +
  labs(x='substrate',y='BMP (mg/L)',fill = 'End time', facet_grid='test')
  

ggsave('boxplot20_30.png', plot = box_20_30, width = 250, height = 150, units = 'mm', path = '../plots/')

#bar chart
bar_20_30 <- ggplot(data = dfPlot20_30, aes(lab, BMP, fill = endTime)) +
  geom_col(position = 'dodge')+
  facet_grid(test ~ substrate) +
  labs(x='lab',y='BMP (mg/L)',fill = 'End time')


ggsave('barchart 20_30.png', plot = bar_20_30, width = 275, height = 120, units = 'mm', path = '../plots/')


