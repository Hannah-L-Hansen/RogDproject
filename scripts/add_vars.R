# Add some things to data frames

# Add lab/test id for plotting
cbg$ltid <- interaction(cbg$lab, cbg$test)
cbg$ltid <- factor(cbg$ltid, levels = sort(unique(as.character(cbg$ltid))))
# rates$ltid <- interaction(rates$lab, rates$test)
# rates$ltid <- factor(rates$ltid, levels = sort(unique(as.character(rates$ltid))))
# ratesg$ltid <- interaction(ratesg$lab, ratesg$test)
# # ratesg$ltid <- factor(ratesg$ltid, levels = sort(unique(as.character(ratesg$ltid))))
# yldmn$ltid <- interaction(yldmn$lab, yldmn$test)
# yldmn$ltid <- factor(yldmn$ltid, levels = sort(unique(as.character(yldmn$ltid))))
# yld$ltid <- interaction(yld$lab, yld$test)
# yld$ltid <- factor(yld$ltid, levels = sort(unique(as.character(yld$ltid))))
inocyld$ltid <- interaction(inocyld$lab, inocyld$test)
inocyld$ltid <- factor(inocyld$ltid, levels = sort(unique(as.character(inocyld$ltid))))
# inocrate$ltid <- interaction(inocrate$lab, inocrate$test)
# inocrate$ltid <- factor(inocrate$ltid, levels = sort(unique(as.character(inocrate$ltid))))

# Add L units for plotting
cbg$cvCH4.L <- cbg$cvCH4/1000

# Add bottle id for plotting
cbg$bid <- paste(cbg$lab, cbg$test, cbg$id, sep = '.')
#rates$bid <- paste(rates$lab, rates$test, rates$id, sep = '.')
#ratesg$bid <- paste(ratesg$lab, ratesg$test, ratesg$id, sep = '.')
#yldmn$bid <- paste(yldmn$lab, yldmn$test, yldmn$substrate, sep = '.')
#yld$bid <- paste(yld$lab, yld$test, yld$id, sep = '.')
inocyld$bid <- paste(inocyld$lab, inocyld$test, inocyld$id, sep = '.')
#inocrate$bid <- paste(inocrate$lab, inocrate$test, inocrate$id, sep = '.')

# Error bar info
# yldmn$lwr <- yldmn$mean - yldmn$sd
# yldmn$upr <- yldmn$mean + yldmn$sd

# Calculate relative CH4 production rate normalized by total cumulative production
cbg <- as.data.frame(mutate(group_by(cbg, lab, test, id), rrvCH4t = rvCH4/max(cvCH4)))

#yld <- as.data.frame(mutate(group_by(yld, lab, test, id), rvCH4 = diff(c(0, cvCH4))/diff(c(0, time.d))))
