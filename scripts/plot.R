## Plots
#ggplot(data = cbg, aes(time.d, cvCH4, colour = substrate, group = id)) + 
#  geom_point() + geom_line() + facet_grid(~ lab) + 
#  labs(x = 'Time (d)', y = 'Cumulative methane (mL)', colour = 'Substrate')

dim(cbg)

pdf('../plots/cum_CH4.pdf', height = 10, width = 11)
  p <- ggplot(data = cbg, aes(time.d, cvCH4.L, colour = substrate, group = id)) + 
    geom_point(shape = 1) + 
    geom_line(aes(time.d, cvCH4.L, group = bid), colour = 'gray45', size = 0.5) + 
    #geom_line(aes(time.d, cvCH4.L, group = bid), size = 0.5) + 
    facet_wrap(~ ltid, scales = 'free_y', ncol = 5) + 
    labs(x = 'Time (d)', y = 'Methane volume (L)', colour = 'Substrate', title = 'Cumulative total methane production by bottle')
  print(p)
dev.off()

pdf('../plots/cum_yield.pdf', height = 10, width = 11)
  p <- ggplot(data = yld, aes(time.d, cvCH4, colour = substrate, group = id)) + 
    geom_point(shape = 1) + geom_line() + facet_wrap(~ ltid, scales = 'fixed', ncol = 5) + 
    xlim(c(0, 36)) +
    labs(x = 'Time (d)', y = 'Methane yield (mL/g)', colour = 'Substrate', title = 'Cumulative methane yield (VS basis) by bottle')
  print(p)
dev.off()

pdf('../plots/inoc_rate.pdf', height = 10, width = 11)
  p <- ggplot(data = inocrate, aes(time.d, rvCH4, group = bid)) + 
    geom_point(shape = 1) + 
    geom_line() +
    facet_wrap(~ ltid, scales = 'free_y', ncol = 5) + 
    #facet_wrap(~ ltid, ncol = 5) + 
    labs(x = 'Time (d)', y = 'Methane production rate (mL/g-d)', colour = 'Substrate', 
         title = 'Methane production rate (VS basis) by bottle')
  print(p)
dev.off()

pdf('../plots/inoc_yield.pdf', height = 10, width = 11)
  p <- ggplot(data = inocyld, aes(time.d, cvCH4, group = bid)) + 
    geom_point(shape = 1) + 
    geom_line() +
    #facet_wrap(~ ltid, scales = 'free_y', ncol = 5) + 
    facet_wrap(~ ltid, ncol = 5) + 
    labs(x = 'Time (d)', y = 'Methane yield (mL/g)', colour = 'Substrate', 
         title = 'Cumulative methane yield (VS basis) by bottle')
  print(p)
dev.off()

pdf('../plots/yield_grouped.pdf', height = 10, width = 11)
  labpos <- as.data.frame(summarise(group_by(yldmn, test, lab, substrate, bid), time.d = max(time.d), 
                                    ypos = max(mean)))
  p <- ggplot(data = yldmn, aes(time.d, mean, colour = substrate, group = bid)) + 
    geom_line() +
    geom_text(data = labpos, aes(time.d, ypos, label = lab), colour = 'black') +
    facet_wrap(~ substrate, scales = 'free_y', ncol = 2) + 
    labs(x = 'Time (d)', y = 'Methane yield (mL/g)', colour = 'Substrate', 
         title = 'Cumulative methane yield (VS basis) by substrate (mean of 3 bottles)')
  print(p)
dev.off()


pdf('../plots/rates_grouped.pdf', height = 10, width = 11)
  labpos <- as.data.frame(summarise(group_by(cbg, test, lab, substrate, bid), time.d = max(time.d), 
                                    ypos = min(log10(na.omit(rrvCH4t)))))
  p <- ggplot(data = cbg, aes(time.d, log10(rrvCH4t), colour = as.numeric(lab), group = bid)) + 
    geom_line() +
    geom_text(data = labpos, aes(time.d, ypos, label = lab), colour = 'black') +
    facet_wrap(~ substrate, scales = 'free_y', ncol = 2) + 
    ylim(-3, 0) +
    labs(x = 'Time (d)', y = 'Methane yield (mL/g)', colour = 'Substrate', 
         title = 'Cumulative methane yield (VS basis) by substrate (mean of 3 bottles)')
  print(p)
dev.off()

pdf('../plots/yield_diff.pdf', height = 18, width = 11)
  p <- ggplot(data = yldmn, aes(time.d, mean, colour = test, group = bid)) + 
    geom_line() +
    geom_errorbar(aes(time.d, ymin = lwr, ymax = upr)) +
    facet_grid(lab ~ substrate, scales = 'fixed') + 
    labs(x = 'Time (d)', y = 'Methane yield (mL/g)', colour = 'Test', 
         title = 'Cumulative methane yield (VS basis) by substrate (mean of 3 bottles)')
  print(p)
dev.off()

pdf('../plots/CH4_rates.pdf', height = 11, width = 11)
  p <- ggplot(data = cbg, aes(time.d, log10(rrvCH4t), colour = substrate, group = id)) + 
    geom_point(shape = 1, size = 0.4) + geom_line() + facet_wrap(~ ltid, scales = 'fixed', ncol = 5) + 
    xlim(c(0, 36)) +
    ylim(c(-3, 0)) +
    labs(x = 'Time (d)', y = 'Log rel. methane prod. rate (frac. of total/d)', colour = 'Substrate', title = 'Methane production rate by bottle')
  print(p)
dev.off()

pdf('../plots/CH4_rates_lin.pdf', height = 11, width = 11)
  p <- ggplot(data = cbg, aes(time.d, rrvCH4t, colour = substrate, group = id)) + 
    geom_point(shape = 1, size = 0.4) + geom_line() + facet_wrap(~ ltid, scales = 'fixed', ncol = 5) + 
    xlim(c(0, 36)) +
    ylim(c(0, 0.6)) +
    labs(x = 'Time (d)', y = 'Relative methane prod. rate (frac. of total/d)', colour = 'Substrate', title = 'Methane production rate by bottle')
  print(p)
dev.off()

pdf('../plots/yield_rates.pdf', height = 11, width = 11)
  p <- ggplot(data = yld, aes(time.d, log10(rvCH4), colour = substrate, group = id)) + 
    geom_point(shape = 1, size = 0.4) + geom_line() + facet_wrap(~ ltid, scales = 'fixed', ncol = 5) + 
    xlim(c(0, 36)) +
    ylim(c(-2, 2.5)) +
    labs(x = 'Time (d)', y = 'Methane yield rate (mL/g-d)', colour = 'Substrate', title = 'Methane yield rate by bottle')
  print(p)
dev.off()

pdf('../plots/yield_rates_lin.pdf', height = 11, width = 11)
  p <- ggplot(data = yld, aes(time.d, rvCH4, colour = substrate, group = id)) + 
    geom_point(shape = 1, size = 0.4) + geom_line() + facet_wrap(~ ltid, scales = 'fixed', ncol = 5) + 
    xlim(c(0, 36)) +
    labs(x = 'Time (d)', y = 'Methane yield rate (mL/g-d)', colour = 'Substrate', title = 'Methane yield rate by bottle')
  print(p)
dev.off()

pdf('../plots/1p_crit.pdf', height = 11, width = 11)
  p <- ggplot(data = rates, aes(time.d, log10(rrvCH4), colour = substrate, group = id)) + 
    geom_hline(yintercept = 0, colour = 'gray45') +
    geom_point(shape = 1) + geom_line() + facet_wrap(~ ltid, scales = 'fixed', ncol = 5) + 
    xlim(c(0, 36)) +
    ylim(c(-2, 1)) +
    labs(x = 'Time (d)', y = 'Log relative methane prod. rate (%/d)', colour = 'Substrate', title = 'Relative methane production rate (yield rate divided by cumulative yield at that time) (note: off-scale points omitted)')
  print(p)
dev.off()

pdf('../plots/1pg_crit.pdf', height = 11, width = 11)
  p <- ggplot(data = ratesg, aes(time.d, log10(rrvCH4), colour = substrate, group = id)) + 
    geom_hline(yintercept = 0, colour = 'gray45') +
    geom_point(shape = 1) + geom_line() + facet_wrap(~ ltid, scales = 'fixed', ncol = 5) + 
    xlim(c(0, 36)) +
    ylim(c(-2, 1)) +
    labs(x = 'Time (d)', y = 'Log relative methane prod. rate (%/d)', colour = 'Substrate', title = 'Relative methane production rate (yield rate divided by cumulative yield at that time) (note: off-scale points omitted)')
  print(p)
dev.off()

ggplot(bmpwm3) + 
geom_point(aes(time.d.1p, time.d.1pg, colour = rate.met.1p), size = 5, shape = 21) +
geom_text(aes(time.d.1p, time.d.1pg, label = lab, colour = rate.met.1pg), size = 3) +
facet_wrap(~ substrate, ncol = 4) + 
labs(x = '1% net duration (d)', y = '1% gross (total) duration (d)', colour = 'Substrate') +
geom_abline(intercept = 0, slope = 1, colour = 'gray55') +
theme(legend.position = 'none')
ggsave('../plots/dur_gn_comp.pdf', height = 3, width = 8, scale = 1.0)
    
pdf('../pres plots/BMP_gn_diff.png', height = 4, width = 4)
hist(bmpwm3$rdBMP, breaks = 30, col = 'skyblue', xlab = 'Rel. diff. in BMP (% of 1% net BMP)', ylab = 'Number of obs. (mean BMPs)', main = '')
box()
dev.off()
