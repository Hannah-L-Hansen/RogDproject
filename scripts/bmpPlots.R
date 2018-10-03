#plottong bmp1p and bmp0.5p

#ggplot template:

# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

ggplot(data=bmpCompare) +
    geom_point(mapping = aes(x = bmpCompare$time, y = bmpCompare$bmp0.5p, color = bmpCompare$substrate))