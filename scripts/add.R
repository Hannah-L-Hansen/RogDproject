# Add some things to data frames

# Add lab/test id for plotting
cbg$ltid <- interaction(cbg$lab, cbg$test)
cbg$ltid <- factor(cbg$ltid, levels = sort(unique(as.numeric(as.character(cbg$ltid)))))
rates$ltid <- interaction(rates$lab, rates$test)
rates$ltid <- factor(rates$ltid, levels = sort(unique(as.numeric(as.character(rates$ltid)))))
yld$ltid <- interaction(yld$lab, yld$test)
yld$ltid <- factor(yld$ltid, levels = sort(unique(as.numeric(as.character(yld$ltid)))))

# Add L units for plotting
cbg$cvCH4.L <- cbg$cvCH4/1000
cbg$pred.L <- cbg$pred/1000

# Add bottle id for plotting
cbg$bid <- paste(cbg$lab, cbg$test, cbg$id, sep = '.')
rates$bid <- paste(rates$lab, rates$test, rates$id, sep = '.')
yld$bid <- paste(yld$lab, yld$test, yld$id, sep = '.')

# Calculate relative CH4 production rate normalized by total cumulative production
cbg <- as.data.frame(mutate(group_by(cbg, lab, exp, id), rrvCH4t = rvCH4/max(cvCH4)))

yld <- as.data.frame(mutate(group_by(yld, lab, exp, id), rvCH4 = diff(c(0, cvCH4))/diff(c(0, days))))


