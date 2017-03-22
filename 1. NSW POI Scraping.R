library(RCurl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(maptools)
library(maps)
library(rgdal)
library(rgeos)
library(ggplot2)


######### Script to scrape NSW POI’s ##########
NSW_POI_DF <- data.frame(OBJECTID = character(),
                         topoid = character(),
                         poigroup = character(),
                         poitype = character(),
                         poiname = character(),
                         poilabel = character(),
                         poilabeltype = character(),
                         poialtlabel = character(),
                         x = numeric(),
                         y = numeric())

for (i in 1:137391) 
{
  
  NSW_POI_Query <- paste0("http://maps.six.nsw.gov.au/arcgis/rest/services/public/NSW_POI/MapServer/find?searchText=", i, "&contains=true&searchFields=objectid&sr=&layers=Points_Of_Interest+%280%29&layerDefs=&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&dynamicLayers=&returnZ=false&returnM=false&gdbVersion=&f=pjson")
  Single_nswPOI <- tryCatch( {
      #message("This is the 'try' part")
      fromJSON(getURL(NSW_POI_Query))
      #print(i)
      },
    error=function(cond){
      ht <- NULL
      Sys.sleep(60)},
    warning=function(cond){
      ht <- NULL
      Sys.sleep(60)
    }
  )
  NSW_POI_DF <- rbind(NSW_POI_DF, cbind(Single_nswPOI$results$attributes, Single_nswPOI$results$geometry[,c(1,2)]))
}

write.csv(NSW_POI_DF,"Output Files/NSW POI.csv")

POI_sum <- NSW_POI_DF %>% group_by(poitype) %>% tally()
write.csv(POI_sum,"Output Files/NSW POI Summary.csv")
