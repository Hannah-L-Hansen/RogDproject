
source('fit.R')

table(cbg$bid)

mfo1 <- fitMods(subset(cbg, days <= 10), 'bid', 'days', 'cvCH4', modtype = 'fo1p')
#mfo2 <- fitMods(cbg, 'bid', 'days', 'cvCH4', modtype = 'fo2p')
#mmonod <- fitMods(cbg, 'bid', 'days', 'cvCH4', modtype = 'monod')
#mgom <- fitMods(cbg, 'bid', 'days', 'cvCH4', modtype = 'gompertz')

cbg <- merge(cbg, mfo1$preds, all.x = TRUE)
cbg$pred.L <- cbg$pred/1000

#dim(mfo1$coefs)
#
#foc <- merge(unique(cbg[, c('test', 'lab', 'substrate', 'bid')]), mfo1$coefs)
#foc <- subset(foc, mssg == 1)
#dim(foc)
#
#write.csv(foc, '../output/foc.csv', row.names = FALSE)
#
#ggplot(foc) + geom_jitter(aes(substrate, k, colour = substrate)) 
#ggsave('../plots/foc.png')


