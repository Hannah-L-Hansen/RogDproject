# Compare duration results

dsub <- read.csv('../../lab analysis 1/data/sub_results_v4.csv', skip = 5, stringsAsFactors = FALSE, 
                 na.strings = c('---', '', '-'))

dsub$substrate <- dsub$sub


ecomp <- merge(dsub, bmp1p, by = c('test', 'lab', 'substrate'))

head(ecomp)
head(dsub)
head(bmp1p)
ls()
head(bmp1p)
