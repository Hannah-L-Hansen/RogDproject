
head(bmpwm3)


head(bmpm)
table(bmpm$rate.met, exclude = NULL)

for(i in list(bmp20, bmp30, bmp1p, bmp0.5p)) {
  x <- as.data.frame(summarise(group_by(i, substrate), 
  			n = length(unique(lab)),
  			n2 = length(lab),
  			mean = mean(BMP),
  			sd = sd(BMP),
  			rsd = 100*sd(BMP)/mean(BMP),
  			min = min(BMP),
  			max = max(BMP),
  			rel.range = 100*(max(BMP) - min(BMP))/mean(BMP)
  			))
  print(kable(x, digits = 1))
}


ggplot(bmpm) + geom_point(aes(reorder(lab, BMP), BMP, colour = rate.met, shape = end)) +
    facet_wrap(~ substrate)

ggplot(bmpm, aes(time.d, BMP)) + 
    geom_point() + 
    geom_point(data = bmpmnotmet, colour = 'red') + 
#    geom_smooth() +
    facet_wrap(~ substrate + end)



bmpm <- merge(bmp20, bmp30, by = c('test', 'lab', 'file', 'method', 'substrate'), 
              suffixes = c('.20d', '.30d'))

bmpm2 <- merge(bmp1p, bmp0.5p, by = c('test', 'lab', 'file', 'method', 'substrate'), 
              suffixes = c('.20d', '.30d'))
head(bmpm)
head(bmp20)
head(bmp1p)
table(bmp20$lab, exclude = NULL)


plot(vCH4 ~ vBg, data = comb)

ls()

head(cbg)
head(yld)
yld$mthd
names(cbg)

options(width = 65)

source('plot.R')
ggsave('BMP_comp.pdf')

dim(cbg)

head(yld)

head(setup)

head(inocyld)

subset(cbg, is.na(ltid))$lab
subset(cbg, is.na(ltid))$test

x <- cbind(x = 1, b = 2, cbg)
head(x)

## For Christof
#bmpex <- bmp[, c('exp', 'inst.num', 'inst', 'method', 'days', 'id', 'substrate', 'ISR', 'cvCH4', 'end')]
#bmpex[is.na(bmpex$ISR), 'ISR'] <- ''
#bmpex$days <- round(bmpex$days, 2)
#names(bmpex) <- c('Batch', 'Institute code', 'Institute abbrev.', 'Method', 'Time (d)', 'Bottle ID', 'Substrate', 'ISR', 'BMP', 'End selection')
#bmpex
#write.csv(bmpex, '../output/BMP.csv', row.names = FALSE)
#
#bmp30ex <- bmp30[, c('exp', 'inst.num', 'inst', 'method', 'days', 'id', 'substrate', 'ISR', 'cvCH4')]
#bmp30ex[is.na(bmp30ex$ISR), 'ISR'] <- ''
#bmp30ex$days <- round(bmp30ex$days, 2)
#names(bmp30ex) <- c('Batch', 'Institute code', 'Institute abbrev.', 'Method', 'Time (d)', 'Bottle ID', 'Substrate', 'ISR', 'bmp30')
#bmp30ex
#write.csv(bmp30ex, '../output/BMP30.csv', row.names = FALSE)

head(cbg, 2)

head(setup)
names(cbg)
cbg$lab
cbg$lab.name


ggplot(data = bmp, aes(substrate, mean, fill = substrate)) + 
  geom_col() + facet_wrap(~ lab) + 
  geom_errorbar(aes(substrate, ymin = lwr, ymax = upr, colour = substrate)) + 
  labs(x = 'Substrate', y = 'Cumulative methane (mL)', fill = 'Substrate')

pdf('../plots/BMP.pdf', height = 7, width = 11)
  p <- ggplot(data = bmp, aes(reorder(interaction(lab, exp), cvCH4), cvCH4, fill = substrate)) + 
    geom_point() + facet_wrap(~ substrate) + 
    labs(x = 'Institute', y = 'BMP (mL/g)', fill = 'Substrate') 
    guides(colour = FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #geom_hline(yintercept = 0.85*413)
  print(p)
dev.off()


pdf('../plots/BMP2.pdf', height = 7, width = 16)
  p <- ggplot(data = bmp, aes(interaction(exp, lab), cvCH4, colour = lab, fill = substrate)) + 
    geom_point() + facet_wrap(~ substrate) + 
    labs(x = 'Institute', y = 'BMP (mL/g)', fill = 'Substrate') 
    guides(colour = FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #geom_hline(yintercept = 0.85*413)
  print(p)
dev.off()

#pdf('../plots/BMP.pdf', height = 7, width = 11)
#  p <- ggplot(data = bmp, aes(reorder(interaction(lab, exp), mean), mean, fill = substrate)) + 
#    geom_col() + facet_wrap(~ substrate) + 
#    geom_errorbar(aes(interaction(lab, exp), ymin = lwr, ymax = upr, colour = substrate)) + 
#    labs(x = 'Institute', y = 'BMP (mL/g)', fill = 'Substrate') +
#    guides(colour = FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#    #geom_hline(yintercept = 0.85*413)
#  print(p)
#dev.off()


#pdf('../plots/first-order_k.pdf', height = 7, width = 11)
#  p <- ggplot(data = foc, aes(lab, log10(-k), colour = substrate)) + 
#    geom_point(shape = 1) + facet_wrap(~ substrate) + 
#    labs(x = 'Institute', y = 'Log first-order rate constant (1/d)', fill = 'Substrate') +
#    guides(colour = FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#    #geom_hline(yintercept = 0.85*413)
#  print(p)
#dev.off()

pdf('../plots/cum_CH4_inoc.pdf', height = 14, width = 4)
  dd <- cbg[cbg$substrate == 'Inoc', ]
  ggplot(data = dd, aes(days, cvCH4, group = id)) + 
    #geom_point(shape = 1) + geom_line() + facet_wrap(~ interaction(exp, inst), scales = 'free') + 
    geom_point(shape = 1, colour = 'gray45', size = 0.5) + geom_line(colour = 'blue') + facet_grid(lab ~ exp, scales = 'free') + 
    labs(x = 'Time (d)', y = 'Cumulative methane (mL)', colour = 'Substrate')
dev.off()

i
Q
i
args$interval 

debug(summBg)


summ.inoc
n
n
n
n
n
n
n
n
n
n
n
n
n
inoc.m.name
head(summ.inoc[, vol.name])
head(summ.inoc[, inoc.m.name])
head(summ.inoc)

Q














#dd$substrate
#bmp.cel <- bmp[bmp$substrate == 'CEL', ]
#
#foc.inoc <- foc[foc$substrate == 'Inoc', ]
#head(foc)
#foc.cel <- foc[grepl('cel', foc$id, ignore.case = TRUE), ]
#foc.cel <- foc[grepl('cel', foc$id, ignore.case = TRUE), ]
#plot(log10(-foc.cel$k))
#plot(log10(-k) ~ inst.num, data = foc.inoc)
#par(new = TRUE)
#plot(cvCH4 ~ inst.num, data = bmp.cel, col = 'red', pch = 19)
#
#     head(foc)
#
#head(bmp)
#head(bmp30)
#bmp$exper
#
#names(bmp) == names(bmp30)
#names(bmp30)
#names(bmp)
#bmp$end
#
#bmp$end2 <- '1%'
#bmp30$end2 <- '30'
#
#bmp.comb <- rbindf(bmp, bmp30)
#head(bmp.comb)
#table(bmp.comb$end2)
#
#bmp.cel <- bmp[bmp$substrate == 'CEL', ]
#plot(cvCH4 ~ vol.mi.mn, data = bmp.cel)
#plot(cvCH4 ~ days, data = bmp.cel)
#
#pairs(bmp.cel[, c('m.sub', 'm.inoc', 'v.head', 'vol.mi.mn', 'fv.inoc', 'days', 'cvCH4')])
#
#subset(bmp.cel, m.sub.vs > 2)$inst
#
##bmp.comb <- merge(bmp, bmp30, by = c('inst.num', 'id', 'exp'), suffixes = c('', '.30'))
##head(bmp.comb)
##table(bmp.comb$inst.num, bmp.comb$exp)
#
#ggplot(data = bmp.comb) + geom_point(aes(inst.num, cvCH4, colour = end2)) +
#    facet_grid(exp ~ substrate)
#
#head(subset(bmp.comb, inst.num == 9), 10)
#head(subset(bmp, inst.num == 9), 10)
#head(subset(bmp30, inst.num == 9), 10)
#
#
