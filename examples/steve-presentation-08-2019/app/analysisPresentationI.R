indir <- "C:\\projects\\bscActivity\\outputs\\20190829\\"

library(data.table); library(raster); library(leaflet)

ff <- list.files(indir, recursive = TRUE, pattern = 'RData', full = TRUE)

fun <- function(x) { cat('loading ', x, '\n');load(x); EX$out}
d <- lapply(ff, fun)

D <- do.call(rbind, d)

# vis
library(maps)
xy <- unique(D[, list(x,y,z, name, id)])
coordinates(xy) <- apply(xy[, list(x,y)], 2, as.numeric)

mp <- map('state', c('colorado', 'utah', 'nevada', 'arizona', 'new mexico'))
mp <- map('state', c('utah'))
points(xy)

usa <- rgdal::readOGR("C:\\data\\gz_2010_us_040_00_500k.json")
west <- usa[which(tolower(usa$NAME) %in% c('colorado', 'utah', 'arizona', 'new mexico', 'Nevada')),]

##
## all stations
##

leaflet(data = xy) %>%  addTiles(urlTemplate = '//server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
  #addMarkers( popup = ~as.character(name), label = ~as.character(name)) %>% 
  addCircleMarkers(
    radius = 5,
    label = ~as.character(name),
    color = 'red',
    stroke = FALSE, fillOpacity = 0.5
  ) %>% 
  addPolygons(data = west, color = "white", weight = 1, smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = 0.0)


##
## Clim-Met stations
##

seug <- new("Extent", xmin = -109.99417701873, xmax = -109.410102469672, 
    ymin = 37.9504496808815, ymax = 38.4207426424739)

plot(seug, add = TRUE)    
seu <- crop(xy, seug)
seu$x <- as.numeric(seu$x)
seu$y <- as.numeric(seu$y)

leaflet(data = seu) %>%  addTiles(urlTemplate = '//server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
  #addMarkers( popup = ~as.character(name), label = ~as.character(name)) %>% 
  addCircleMarkers(
    radius = 10,
    label = ~as.character(name),
    color = 'blue',
    stroke = FALSE, fillOpacity = 0.5,
    labelOptions = labelOptions(noHide = F)
  )

i <- seu$id

# make graphs of each stations
d <- D[id %in% i,]
#d <- d[-which(d$prec == 106.0 & d$month == 10 & d$name == 'Virginia Park'),]

  b <- d[, list( mean = mean(nd, na.rm = TRUE), se = sd(nd, na.rm = TRUE) / sqrt(sum(!is.na(nd)))), by = list(mo = month,month = month.abb[month], name, id)]

  b$month <- factor(b$month, levels = month.abb)

for(i in unique(d$name)){
  x11(9.625*.6, 5.750*.6)
  p <- ggplot(b[name == i,], aes(x=month, y=mean, fill=month)) + 
    geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                 position=position_dodge(.9)) +  scale_fill_manual(guide=FALSE, values=rev(terrain.colors(12)))+ 
  theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=14))+
  xlab("") + ylab("Potential Active Hours") + 
  ggtitle(i)+ ylim(0, 125)
  print(p)
}


ggplot(b, aes(x = mo, y = mean, color = name)) + geom_line(lwd = 1, alpha = .6, lty = 2) + theme_bw()+scale_x_continuous("Month", (1:12), labels = month.abb) +
geom_ribbon(aes(ymin=mean - se, ymax=mean + se, fill = name), alpha=0.1, colour = NA) + ylab('Potential Hours of Activity')

# look at climatologies
x <- d[, list(prec = mean(prec, na.rm = TRUE), tmin = mean(Tmin, na.rm=TRUE), tmax = mean(Tmax, na.rm=TRUE), ET.pm = mean(ET.pm, na.rm = TRUE), uz = mean(uz, na.rm = TRUE), fracOK = mean(fracOK, na.rm = TRUE), Rs = mean(Rs, na.rm = TRUE), Rh = mean( (RHmax + RHmin)/2, na.rm =TRUE)), by = list(month, name)] 


 ggplot(x, aes(x = as.factor(month), y = prec, group = name, color = name)) + geom_line(lwd = 3)+  scale_fill_manual( values=rev(terrain.colors(4)))+ 
  theme_bw() + xlab('month') + ggtitle('Preciptation')

 ggplot(x, aes(x = as.factor(month), y = ET.pm, group = name, color = name)) + geom_line(lwd = 3)+  scale_fill_manual( values=rev(terrain.colors(4)))+ 
  theme_bw() + ylab('mm') + ggtitle('ETo') + xlab('Month')

 ggplot(x, aes(x = as.factor(month), y = Rs, group = name, color = name)) + geom_line(lwd = 3, alpha = .6)+  scale_fill_manual( values=rev(terrain.colors(4)))+ 
  theme_bw() + ylab('MJ/m2/d') + ggtitle('Radiation')

 ggplot(x, aes(x = as.factor(month), y = tmin, group = name, color = name)) + geom_line(lwd = 3)+  scale_fill_manual( values=rev(terrain.colors(4)))+ 
  theme_bw() + ylab('C') + ggtitle('tmin')

 ggplot(x, aes(x = as.factor(month), y = tmax, group = name, color = name)) + geom_line(lwd = 3)+  scale_fill_manual( values=rev(terrain.colors(4)))+ 
  theme_bw() + ylab('C') + ggtitle('tmax')

 ggplot(x, aes(x = as.factor(month), y = uz, group = name, color = name)) + geom_line(lwd = 3)+  scale_fill_manual( values=rev(terrain.colors(4)))+ 
  theme_bw() + ylab('m/s') + ggtitle('Wind')

 ggplot(x, aes(x = as.factor(month), y = Rh, group = name, color = name)) + geom_line(lwd = 3)+  scale_fill_manual( values=rev(terrain.colors(4)))+ 
  theme_bw() + ylab('%') + ggtitle('Humidity')

# check precip in VP

  pvp <- d[name== 'Virginia Park' & month == 10, list( prec, dt)]
  pvp <- d[ month == 10, list( prec, dt, name)]

# Interannual variation

  #monthly averages
  xmu <- d[flag == '', list( nd = mean(nd, na.rm = TRUE), prec = mean(prec, na.rm=TRUE)), by = list(month, name)]
  
  # fill missing with monthly averages
  a <- d
  a[flag != '', nd :=NA]
  a[flag != '', prec :=NA]
  b <- merge(a, xmu, by = c('month', 'name'), all = TRUE)
  b$nd <- ifelse(is.na(b$nd.x), b$nd.y, b$nd.x)
  b$prec <- ifelse(is.na(b$prec.x), b$prec.y, b$prec.x)
  
  iav <- b[, list( hours = sum(nd), prec = sum(prec)), by = list(year, name)]
  ggplot(iav, aes(x = as.factor(year), y = hours, group = name, color = name)) + geom_line(lwd = 2, alpha = .8)+  scale_fill_manual( values=rev(terrain.colors(4)))+ 
  theme_bw() + ylab('hours') + ggtitle('Interannual Variation Activity') + ylim(0,1200)

  ppt <- b[, list( prec = sum(prec)), by = list(year, name)]
  ggplot(ppt, aes(x = as.factor(year), y = prec, group = name, color = name)) + geom_line(lwd = 2, alpha = .8)+  scale_fill_manual( values=rev(terrain.colors(4)))+ 
  theme_bw() + ylab('mm') + ggtitle('Interannual Variation Precip') 

  # correlations with precip annual vs intra-annual
  
  cor(b[flag == '', list(prec, nd)])
  cor(iav[, list(prec, hours)])
  iav[, cor(prec, hours), by = list(name)]
  b[flag == '', cor(prec, nd), by = list(name)]
  ggplot(b[flag == '' & prec != 0, ], aes(x = prec, y = nd, color = name)) + geom_point()+ geom_smooth(method="lm", se = FALSE) 
  
  b$Month <- month.abb[b$month]
  ggplot(a[flag == '' & prec != 0, ], aes(x = prec, y = nd, color = as.factor(month))) + geom_point(alpha = .3)+ geom_smooth(method="lm", se = FALSE) + ylab('Potential Active Hours')+  xlab('precip') + theme_bw() +labs(color = 'Month')
  
  ggplot(a[flag == '' & prec != 0, ], aes(x = prec, y = nd, color = name)) + geom_point(alpha = .3)+ geom_smooth(method="lm", se = FALSE) + ylab('Potential Active Hours')+  xlab('precip') + theme_bw() +labs(color = 'Site')
  
  knitr::kable(t(b[flag == '' & prec != 0, list(R2 = round(cor(prec, nd)**2,2)), by = list(name)]))
  knitr::kable(t(b[flag == '' & prec != 0, list(R2 = round(cor(prec, nd)**2,2)), by = list(Month)]))
  
  
  # CV values
  
  # get complete years
  d[, completeYear := sum(is.na(nd)) == 0, by = list(name, year)]
  d[completeYear == TRUE, list(length(unique(year))), by = name]
  yy <- d[completeYear == TRUE, list(hours = sum(nd)), by = list(name, year)]
  y0 <- d[ , list(hours = sum(nd, na.rm = TRUE)), by = list(name, year)]
  gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
  
  boxplot(main = 'Interannual Variation\nPotential Active Hours', hours ~ name, y0,  width = c(7, 12, 9,8), lwd = 2, col = gg_color_hue(4))
  

## Overall CV

  D[, completeYear := sum(is.na(nd)) == 0, by = list(name, year)]
  V <- D[completeYear == TRUE, list(hours = sum(nd)), by = list(name, year, x, y, z)]
  V <- V[, list(CV = mean(hours) / sd(hours),MN = mean(hours), .N), by= list( name, x, y, z)]
  V <- na.omit(V)
  V <- V[N > 6,]
  plot(CV ~ N, V)
  plot(MN ~ N, V)
  plot(MN ~ z, V)
  plot(west)
  points(V$x, V$y, cex = (1 + (V$CV)/max(V$CV))**3)
  points(V$x, V$y, cex = -.5 + (1 + (V$MN)/max(V$MN))**3)
  