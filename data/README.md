# ScienceMap Data Ingestion Folder

Initially, this folder will be an upload point for our georeferenced study data for initial building of the application framework. Eventually, a more scalable solution will be needed (something to think about).

## Instructions

To get us started, I'm going to ask that all incoming data be standardized as RDS files of SpatialPolygonsDataFrame objects. RDS files store actual R objects and all of their properties in one clean file. This obviously has advantages over using GIS files like shapefiles that have many actual files associated with one feature class. In order to be able to merge all of our files, we will also need to standardize the fields and coordinate system we use. Since most data is originally collected in WGS84, I'm going to ask that all data in these initial stages be in that system. Let's all start with the following attribute table fields:

| StudyName | URL | Description | Contributor | Authors | ID |
| --------- | --- | ----------- | ----------- | ------- | -- |
| Biocrust magic | www.blahbla.blah | Biocrust study | Travis Nauman | Some peoples | TWN1 |

## Examples
Below are a vignettes with some of the more common scenarios I'd expect folks to run into when getting studies into SpatialPolygons and exporting as an RDS.

### Using a bounding box or polygon coordinates

```
## Import packages
required.packages <- c("sp", "rgdal")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


## Get coordinates from a sciencebase bounding box
# https://www.sciencebase.gov/catalog/item/imap/5aba8a43e4b081f61abb4c8c
poly <- c(-111.879476326, 39.426374016, -107.931508792, 39.426374016, -107.931508792, 37.734191073, -111.879476326, 37.734191073)
ID <- "TWN1"
name <- "Elevated aeolian sediment transport on the Colorado Plateau, USA: the role of grazing, vehicle disturbance, and increasing aridity"
URL <- "https://docs.google.com/a/naumangeospatial.com/viewer?a=v&pid=sites&srcid=bmF1bWFuZ2Vvc3BhdGlhbC5jb218d3d3fGd4OjE0ZmIwYjY2YjhiMjY1MzY"
Description <- "Wind erosion study in the Moab area showing how drought, grazing and off-pavement vehicle use can increase blowing sediment."
Contributor <- "Travis Nauman"
Authors <- "Nauman, Travis W.; Duniway, Michael C.; Webb, Nicholas P.; Belnap, Jayne"

## Create SptialPolygons object
polysp <- SpatialPolygons(list(Polygons(list(Polygon(matrix(poly, ncol=2, byrow = T))),ID)))
## Now attach attributes to get a SpatialPolygonsDataFrame
polysp.df <- SpatialPolygonsDataFrame(polysp, data.frame(StudyName=name,row.names=ID,URL=URL,Description=Description,Contributor=Contributor,Authors=Authors, stringsAsFactors = F))

## Can edit attributes by modifying the ...@data slot
polysp.df@data$Contributor[1] <- "Travis W Nauman"

## Now attach projection
wgs84.proj <- CRS("+proj=longlat +datum=WGS84")
projection(polysp.df) <- wgs84.proj

## Can check by plotting
plot(polysp.df)

## Now save to an RDS file
saveRDS(polysp.df, "C:/Users/tnauman/Desktop/Nauman.rds") # please title files by your name to keep track of them

```

