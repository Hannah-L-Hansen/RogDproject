# Exports results 

## Cumulative Bg
#cbg <- cbg[order(comb$test, comb$lab, comb$substrate, comb$id, comb$days), ]
#write.csv(cbg, '../output/cum_biogas.csv', row.names = FALSE)
#
#yld <- yld[order(yld$test, yld$lab, yld$substrate, yld$id, yld$days), ]
#write.csv(yld, '../output/cum_yield.csv', row.names = FALSE)

write.csv(comb, '../output/cum_bg_yld.csv', row.names = FALSE)
