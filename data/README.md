# ScienceMap Data Ingestion Folder

Initially, this folder will be an upload point for our georeferenced study data for initial building of the application framework. Eventually, a more scalable solution will be needed (something to think about). Perhaps some kind of cloud storage...

## Instructions

To get us started, I'm going to ask that all incoming data be standardized as RDS files of SpatialPolygonsDataFrame objects. RDS files store actual R objects and all of their properties in one clean file. This obviously has advantages over using GIS files like shapefiles that have many actual files associated with one feature class. In order to be able to merge all of our files, we will also need to standardize the fields and coordinate system we use. Since most data is originally collected in WGS84, I'm going to ask that all data in these initial stages be in that system. If the coordinates you single points rather than shapes, use google earth to find four different sets of coordinates that loosely represent the area that the research has occured in. Be very concious of not disclosing specific locations of research sites that are not already widely known by the public. Let's all start with the following attribute table fields, we need an ID field for the datatype, so I'm proposing we use our initials, birthday month, plus a number unique to each study:

| StudyName | URL | Description | Contributor | Authors | ID |
| --------- | --- | ----------- | ----------- | ------- | -- |
| Biocrust magic | www.blahbla.blah | Biocrust study | Travis Nauman | Some peoples | TWN041 |

## Examples
Below are a vignettes with some of the more common scenarios I'd expect folks to run into when getting studies into SpatialPolygonsDataFrame objects and exporting as an RDS.

### Using a bounding box or polygon coordinates

```
## Import packages
required.packages <- c("sp", "rgdal","raster")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


## Get coordinates from a sciencebase bounding box
# Example: https://www.sciencebase.gov/catalog/item/imap/5aba8a43e4b081f61abb4c8c
poly <- c(-111.879476326, 39.426374016, -107.931508792, 39.426374016, -107.931508792, 37.734191073, -111.879476326, 37.734191073)

## Now create the attribute fields
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

## If you want load an existed RDS file into R
polys.ex <- readRDS("C:/Users/tnauman/Desktop/Nauman.rds")

```
The SpatialPolygonsDataFrame() class function also acts as an rbind type function if you want to put multiple polygons with the same set of attributes together. So if you have multiple bounding boxes in SpatialPolygonsDataFrame objects, this is one way to put them together, although there are other ways to accomplish this.
```
newpolys <- SpatialPolygonsDataFrame(1stpoly, 2ndpoly)
```


### Using a pre-editted shapefile
```
required.packages <- c("sp", "rgdal", "raster")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

## Load file
setwd("C:/Models_active_work") ## FOlder with file
shp.polys <-readOGR(".", "study_file") # don't include .shp extention

## Now switch projection if needed
wgs84.proj <- CRS("+proj=longlat +datum=WGS84")
shp.polys <- spTransform(shp.polys, wgs84.proj)

## Unfortunately, shp fieldnames are too short (max of 10 char) for our fields and thus have to be editted
shp.polys@data$Description <- shp.polys@data$Descriptio
shp.polys@data$Contributor <- shp.polys@data$Contributo
# Remove old fields
shp.polys@data$Descriptio <- NULL
shp.polys@data$Contributo <- NULL
 
## Can plot to see if it still looks ok
plot(shp.polys)

## Now save to an RDS file
saveRDS(shp.polys, "C:/Users/tnauman/Desktop/Nauman.rds") # please title files by your name to keep track of them

```
## Upload your file

From inside the [data](https://github.com/sciencemoab/ScienceMap/tree/master/data) folder you can upload your rds file. Just click the "Upload files" button on the upper right of the page just under the settings tab.
