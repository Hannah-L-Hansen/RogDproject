

labpos <- as.data.frame(summarise(group_by(yldmn, test, lab, substrate, bid), time.d = max(time.d), 
                                  ypos = max(mean)))
ggplot(data = yldmn, aes(time.d, mean, colour = lab, group = bid)) + 
geom_line() +
geom_text(data = labpos, aes(time.d, ypos, label = lab, colour = lab), nudge_x = 2, size = 3) +
facet_wrap(~ substrate, scales = 'free_y', ncol = 4) + 
labs(x = 'Time (d)', y = 'Methane yield (mL/g)', colour = 'Substrate') +
theme(legend.position = 'none')
ggsave('../pres plots/yield_grouped.png', height = 3, width = 8, scale = 1.0)

ggplot(bmpmp) + geom_histogram(aes(time.d, fill = rate.met)) +
    facet_grid(end ~ substrate) +
    labs(x = 'Duration (d)', y = 'Count', fill = 'Met rate crit.')
ggsave('../pres plots/rate_counts.png', height = 3, width = 8, scale = 1.0)

ggplot(bmpwm, aes(dt, rdBMP, colour = rate.met.0.5p)) + 
    geom_point() + 
    facet_grid(~ substrate) +
    labs(x = 'Increase in duration (d)', y = 'Increase in BMP (%)', colour = 'Met rate crit.')
ggsave('../pres plots/increase_1_0.5.png', height = 3, width = 8, scale = 1.0)

ggplot(bmpwm2, aes(dt, rdBMP, colour = rate.met)) + 
    geom_point() + 
    facet_grid(~ substrate) +
    labs(x = 'Increase in duration (d)', y = 'Increase in BMP (%)', colour = 'Met rate crit.')
ggsave('../pres plots/increase_1_20.png', height = 3, width = 8, scale = 1.0)

ggplot(bmpm, aes(time.d, BMP, colour = end)) + 
  geom_line(aes(group = lab), colour = 'gray80') +
  geom_point() + 
  facet_grid(test ~ substrate) +
  ylim(275, 500) +
  labs(x = 'Duration (d)', y = 'BMP (mL/g)', colour = 'Duration') 
ggsave('../pres plots/BMP_v_end.png', height = 4, width = 8, scale = 1.0)

labpos <- as.data.frame(summarise(group_by(inocyldmn, test, lab), time.d = max(time.d), 
                                  ypos = max(mean)))
ggplot(data = inocyldmn, aes(time.d, mean, colour = lab)) + 
geom_line() +
geom_text(data = labpos, aes(time.d, ypos, label = lab, colour = lab), nudge_x = 1, size = 3) +
facet_grid(. ~ test) + 
labs(x = 'Time (d)', y = 'Methane yield (mL/g)') +
theme(legend.position = 'none')
ggsave('../pres plots/inoc_yld.png', height = 3, width = 8, scale = 1.0)

ggplot(inoc.cor) +  
geom_smooth(aes(rate.1d, rate.ave), method = 'lm') +
geom_point(aes(rate.1d, rate.ave), colour = 'white', size = 5) +
geom_text(aes(rate.1d, rate.ave, label = lab, colour = lab), size = 3) +
#geom_smooth(aes(rate.1d, rate.ave), method = 'lm', formula = y ~ x - 1) +
labs(x = 'First day specific methane activity (mL/g-d)', y = 'Average specific methane activity (mL/g-d)') +
theme(legend.position = 'none')
ggsave('../pres plots/inoc_cor.png', height = 3, width = 3, scale = 1.3)

ggplot(ecomp) + 
geom_point(aes(days, time.d), colour = 'white', size = 5) +
geom_text(aes(days, time.d, label = lab, colour = substrate), size = 3) +
facet_wrap(~ substrate, ncol = 4) + 
labs(x = 'Submitted BMP duration (d)', y = '1% duration (d)', colour = 'Substrate') +
geom_abline(intercept = 0, slope = 1, colour = 'gray55') +
theme(legend.position = 'none')
ggsave('../pres plots/dur_actual.png', height = 3, width = 8, scale = 1.0)
    
ggplot(bmpwm3) + 
geom_point(aes(time.d.1p, time.d.1pg, colour = rate.met.1p), size = 5, shape = 21) +
geom_text(aes(time.d.1p, time.d.1pg, label = lab, colour = rate.met.1pg), size = 3) +
facet_wrap(~ substrate, ncol = 4) + 
labs(x = '1% net duration (d)', y = '1% gross (total) duration (d)', colour = 'Substrate') +
geom_abline(intercept = 0, slope = 1, colour = 'gray55') +
theme(legend.position = 'none')
ggsave('../pres plots/dur_gn_comp.png', height = 3, width = 8, scale = 1.0)
    
png('../pres plots/BMP_gn_diff.png', height = 4, width = 4, units = 'in', res = 600)
hist(bmpwm3$rdBMP, breaks = 30, col = 'skyblue', xlab = 'Rel. diff. in BMP (% of 1% net BMP)', ylab = 'Number of obs. (mean BMPs)', main = '')
box()
dev.off()
