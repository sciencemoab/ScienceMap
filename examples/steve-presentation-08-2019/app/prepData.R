
    # processing
    pp <- spTransform(padpoly, projection(raster()))
    pp$meta <- paste(paste0(names(unlist(pp@data)), ' : ', unlist(pp@data)), collapse = '</br>')


    extraction <- fixExtraction(extraction)
   
   
    # buffer raster
    aindex <- unique(c(toposim$index))
    tb <- table(toposim$index[])
    allpixels$freq <- NA
    allpixels$freq[ match(as.numeric( names(tb) ) , aindex) ] <- tb
    padpixels$freq <- 0
     
    master <- rbind(padpixels , allpixels)
    master$SYNTH <- 0   

    # timeseries 1
    ags <- makeAgs(extraction, padpixels, allpixels)

    # quantile of satvi values
    quants <- quantile( extraction$extractedTarget[], na.rm = TRUE, c(0, 1))
    
    # save
    
