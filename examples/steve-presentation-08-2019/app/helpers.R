
fixExtraction <- function(extraction){
    
    target <- extraction$extractedTarget
    controls <- extraction$extractedReference

    w <- which(!apply(target,2, function(x)(all(is.na(x)) | all(x==0))))
    controls <- controls[,w]
    target <- target[,w]
    
    w <- which(colnames(target)== 'satvi_median_ls8_2018_MarNov1')
    if(length(w) > 0){
        target[,w] <- target[,w] * 1000 
        controls[,w] <- controls[,w] * 1000 
    }

    if(is.null(ncol(target))){
        target <- matrix(target)
        if(dim(target)[1] > dim(target)[2]){
            target <- t(target)
        }
    }

    
    
    extraction$extractedTarget <- target
    extraction$extractedReference <- controls
    
    extraction

}



makeAgs <- function(extraction, padpixels, allpixels){
    cat('aggregating\n', file =stderr())
    target <- extraction$extractedTarget
    controls <- extraction$extractedReference
    cat('in makeAgs\n', dim(target), file = stderr()) 
    if(is.null(ncol(target))){
        target <- matrix(target)
        if(dim(target)[1] > dim(target)[2]){
            target <- t(target)
        }
    }

    tarM <- apply(target,2, mean, na.rm=TRUE)
    tarSD <- apply(target,2, sd, na.rm=TRUE)
    if(all(is.na(tarSD))){ tarSD <- rep(0, length(tarM))}
    tarU <- tarM + tarSD
    tarD <- tarM - tarSD
    
    conM <- apply(controls,2, mean, na.rm=TRUE)
    conSD <- apply(controls,2, sd, na.rm=TRUE)
    conU <- conM + conSD
    conD <- conM - conSD
    
    trend <- do.call(rbind, list(tarM, tarU, tarD, conM, conU, conD))
    

    # Thin / aggregate if there are too many
    n = 300
    f <- round(nrow(target)/n)
    if( f > 1){
       cl <- kmeans(coordinates(padpixels), n)$cluster
       target = apply( target, 2, function(x) {
                    tapply( x,cl, FUN = mean)})
    }
    
    f2 <- round(nrow(controls)/n)
    if( f2 > 1){
          cl <- kmeans(coordinates(allpixels), n)$cluster
          controls = apply( controls, 2, function(x) {
                    tapply( x,cl, FUN = mean)})

    }
    
    w <- which(! apply(target, 2, function(x)(all(is.na(x)))))
    controls <- controls[,w]
    target <- target[,w]
    trend <- trend[,w]
   
   
    yl = range(c(target[], controls[]), na.rm = TRUE)
    if(is.null(ncol(target))) target <- t(target)
    x <- 1:ncol(target)
    
      
    xnames <- 1983 + 1:ncol(target)
    
    return( list( trend = trend, target = target, controls = controls, yl = yl, x = x , xnames = xnames))
}

tsPlot <- function(ags, year){

    plot(ags$x, ags$x, ylim = ags$yl, type = 'n', ylab = '', xaxt = 'n')
    axis(1,at = ags$x, labels = ags$xnames)
    abline(v = year, col = 'red')

    apply(ags$controls, 1, lines, col = rgb(0,0,0,.15))
    apply(ags$target, 1, lines, col = rgb(1,0,0, .15))
    legend('topleft', col = c('red', 'black'), lty = 1, legend = c('treated', 'control'), bty = 'n')
    #dev.off()
    
}

trendPlot <- function(ags, year){
    
    yl <- range(ags$trend[])
    plot(ags$x, ags$x, ylim = yl, type = 'n', ylab = '', xaxt = 'n')
    axis(1,at = ags$x, labels = ags$xnames)
    abline(v = year, col = 'red')

    lines(ags$trend[1,], col = 'red')
    polygon(c(ags$x, rev(ags$x)),c(ags$trend[2,], rev(ags$trend[3,])), col = rgb(1,0,0,.1), border = NA)

    lines(ags$trend[4,], col = 'black')
    polygon(c(ags$x, rev(ags$x)),c(ags$trend[5,], rev(ags$trend[6,])), col = rgb(0,0,0,.1), border = NA)

    legend('topleft', col = c('red', 'black'), lty = 1, legend = c('treated', 'control'), bty = 'n')
    #dev.off()
    
}



trendPlot2 <- function(ags, year, padpoly){

#    clim <- getClimAnom(padpoly)
    yl <- range(ags$trend[])

    y0 <- mean(yl)
#    clim2 <- apply(clim,2,function(x) y0 + x * abs(diff(yl))/6)
#    clim2 <- clim2[ match(ags$xnames, 1970:2018),]
    
    plot(ags$x, ags$x, ylim = yl, type = 'n', ylab = '', xaxt = 'n')
    axis(1,at = ags$x, labels = ags$xnames)
 #   arrows(ags$x,y0, ags$x, clim2[,2], col =rgb(0,0,1,.2), length=0, lwd = 10)
 #   arrows(ags$x + .2,y0, ags$x+.2, clim2[,1], col =rgb(1,0,0,.2),lty=1, length=0, lwd = 10)


    abline(v = year, col = 'red')

    lines(ags$trend[1,], col = 'red')
    polygon(c(ags$x, rev(ags$x)),c(ags$trend[2,], rev(ags$trend[3,])), col = rgb(1,0,0,.1), border = NA)

    lines(ags$trend[4,], col = 'black')
    polygon(c(ags$x, rev(ags$x)),c(ags$trend[5,], rev(ags$trend[6,])), col = rgb(0,0,0,.1), border = NA)

    
    legend('topleft', col = c('red', 'black', rgb(1,0,0,.4), rgb(0,0,1,.4)), lty = 1, lwd = c(1,1,10,10), legend = c('treated', 'control', 'PET anomaly', 'PPT anomaly'), bty = 'n')
                                        #dev.off()
    axis(4, at = c(seq(yl[1], yl[2], length.out = 7)), labels = c(3,-2, -1, 0, 1, 2, 3))
    
}




synthPlot <- function(ev, year){
    y = ev$gs$est.att[,'ATT']
    yu <-  ev$gs$est.att[, 'CI.upper']
    yd <-  ev$gs$est.att[, 'CI.lower']
    xl = range(ev$effect$time)
    xlabs = xl[1]:xl[2]
    x = 1:length(y)
    
    plot(x, y, ylim = range(c(y, yu, yd)), type = 'line', col = 'red', xaxt = 'n')
    axis(1, at = x, labels = xlabs)
#    points(x,y, pch = ifelse(ev$gs$est.att[,'p.value'] < 0.05, 8,26), lwd = 3, col = 'red')
    polygon(c(x, rev(x)), c(yu, rev(yd)), col = rgb(1,0,0,0.1), border = NA)
    abline(h = 0, lty = 3)
    abline(v = year, col = 'red')
    abline( v = x[max(which(ev$gs$est.att[,'n.Treated'] ==0))], lty = 3, lwd = 3)
    legend('topleft', pch = 8, col = 'red', legend = 'significant')
    }

synthPlot2 <- function(ev, year){

   # y = rowMeans(ev$gs$est.ind[,'EFF',])
    y = ev$gs$est.att[,'ATT']
    if(dim(ev$gs$est.ind)[3] > 1){
        yu <-  rowMeans(ev$gs$est.ind[, 'CI.upper',])
        yd <-  rowMeans(ev$gs$est.ind[, 'CI.lower',])
    } else {
        yu <-  ev$gs$est.att[, 'CI.upper']
        yd <-  ev$gs$est.att[, 'CI.lower']
    }
    xl = range(ev$effect$time)
    xlabs = xl[1]:xl[2]
    x = 1:length(y)
    
    plot(x, y, ylim = range(c(y, yu, yd)), type = 'line', col = 'red', xaxt = 'n')
    axis(1, at = x, labels = xlabs)
#    points(x,y, pch = ifelse(ev$gs$est.att[,'p.value'] < 0.05, 8,26), lwd = 3, col = 'red')
    polygon(c(x, rev(x)), c(yu, rev(yd)), col = rgb(1,0,0,0.1), border = NA)
    abline(h = 0, lty = 3)
    abline(v = year, col = 'red')
    abline( v = x[max(which(ev$gs$est.att[,'n.Treated'] ==0))], lty = 3, lwd = 3)
    legend('topleft', pch = 8, col = 'red', legend = 'significant')
    }




plotEV <- function(ev) {

  
  columns <- names(ev)[  (grep('effect', names(ev)) + 1 ) : length(names(ev))]
  
  delta <- ev[,columns] - ev$effect
  yl <- range(delta, na.rm = TRUE) 
  plot(ev$effect ~ ev$time, ylim = yl, type = 'n', ylab = 'error (predicted - actual)')
    
  for(n in columns ){
  
    lines(ev$time, delta[,n], col = grep(n, columns), lty = 2)
  }
  abline(h = 0, lty = 2, col = rgb(0,0,0,.3))
  legend('topleft', legend = columns,pch = 17, col = 1:length(columns))
    

}


getClimAnom <- function(pp){
    PET <- brick("/home/steve/data/GIS_ARCHIVE/CRU/anomalies/PET_MarNov.tif")   
    PPT <- brick("/home/steve/data/GIS_ARCHIVE/CRU/anomalies/PPT_MarNov.tif")

    xy = colMeans(coordinates(pp))
    pet <- extract(PET, pp)
    ppt <- extract(PPT, pp)
    return(cbind(pet[[1]][1,], ppt[[1]][1,]))
}

## get gSynth predictions
getGsRast <- function(ev, year){

    j <- ev$gs$est.ind
    dn <- dimnames(j)[3][[1]]
    eff <- j[year,1,]
    ps <- j[year,5,]
    eff[ps >.05] <- 0
    eff[ order( as.numeric( gsub('trt', '', dn) ) ) ]
}

plotRel <- function(ev,year){


    y <- ev$rel$effect$effect
    x <- ev$rel$effect$time
    SD <- ev$rel$effect$sd
    up <- y +SD
    dn <- y - SD
    yl <- range(c(up, dn))
    plot( x, y, type= 'line', col = 'red' , ylim = yl)
    polygon(c(x, rev(x)), c(up, rev(dn)), col = rgb(1,0,0,0.1), border = NA)
    abline(h = 0, lty = 3)
    abline(v = year, col = 'red')
    abline( v = x[max(which(ev$gs$est.att[,'n.Treated'] ==0))], lty = 3, lwd = 3)
    
}

plotDID <- function(ev, year){

    z <- ev$malmDiD
    y <-z$effect
    x <- z$time
    SD <- z$sd
    up <- y +SD
    dn <- y - SD
    yl <- range(c(up, dn))
    plot( x, y, type= 'line', col = 'red' , ylim = yl)
    polygon(c(x, rev(x)), c(up, rev(dn)), col = rgb(1,0,0,0.1), border = NA)
    abline(h = 0, lty = 3)
    abline(v = year, col = 'red')
    abline( v = x[max(which(ev$gs$est.att[,'n.Treated'] ==0))], lty = 3, lwd = 3)
    

    }

## some default colors
  cols <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")

  tcols <- rev(c("#000000", "#808080", "#FFFFFF"))


