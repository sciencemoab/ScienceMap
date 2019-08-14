# ScienceMap Data Ingestion Folder

Initially, this folder will be an upload point for our georeferenced study data for initial building of the application framework. Eventually, a more scalable solution will be needed (something to think about).

## Instructions

To get us started, I'm going to ask that all incoming data be standardized as RDS files of SpatialPolygons objects. RDS files store actual R objects and all of their properties in one clean file. This obviously has advantages over using GIS files like shapefiles that have many actual files associated with one feature class. In order to be able to merge all of our files, we will also need to standardize the fields and coordinate system we use. Since most data is originally collected in WGS84, I'm going to ask that all data in these initial stages be in that system. Let's all start with the following attribute table fields:

| StudyName | URL | Description | Contributor | Authors |
| --------- | --- | ----------- | ----------- | ------- |
| Elevated aeolian sediment transport on the Colorado Plateau, USA: the role of grazing, vehicle disturbance, and increasing aridity | https://docs.google.com/a/naumangeospatial.com/viewer?a=v&pid=sites&srcid=bmF1bWFuZ2Vvc3BhdGlhbC5jb218d3d3fGd4OjE0ZmIwYjY2YjhiMjY1MzY | Wind erosion study in Moab area | Travis Nauman | Nauman, Travis W.; Duniway, Michael C.; Webb, Nicholas P.; Belnap, Jayne |


## Examples
Below are a vignettes with some of the more common scenarios I'd expect folks to run into when getting studies into the 
