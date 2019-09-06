   # list of data files
    indir <- "../data/"
    ff <- list.files(indir, full = TRUE, pattern = 'RData')
    
   # choose starting location
   infile = file.path(indir, 'ID_7.RData')

   # metadata
   metadata = read.csv( file.path(indir, 'metadata.csv'))
   
