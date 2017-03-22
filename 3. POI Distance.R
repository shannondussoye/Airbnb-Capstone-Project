#Read POI category counter, POI aggregated by count
POI_sum <-  read.csv("/home/shannon/R/Projects/SpringboardProject/Output Files/NSW POI Summary.csv")

#filter POI categories that will be used
POI_sum_top <- POI_sum %>%
  filter(
    poitype %in% c(
      "Zoo",
      "City",
      "Lighthouse",
      "Observatory",
      "Transport Interchange",
      "Art Gallery",
      "Tourist Attraction",
      "Museum",
      "Waterfall",
      "Historic Site",
      "Town,Shopping Centre",
      "Railway Station",
      "Beach"
    )
  ) %>% select(poitype)

#Create vector to filter categories
POI_sum_top <- as.character(POI_sum_top[, 1])

#load NSW POI and filter
POI <-fread("/home/shannon/R/Projects/SpringboardProject/Output Files/NSW POI.csv") %>%
      filter(poitype %in% POI_sum_top)

#add unique id for counter
listings$lid <- rownames(listings) %>% as.numeric()

#calculate distance by looping through each listing_id and all POI, then filter <10 km
df <- data.frame()
for (i in 1:nrow(listings))
{
  listings_dist <- listings %>% filter(lid == i)
  listings_dist <- merge(listings_dist, POI, all = TRUE)
  #calculate distance
  setDT(listings_dist)[, dist_km := distGeo(matrix(c(longitude, latitude), ncol = 2), matrix(c(x, y), ncol = 2)) /
                         1000]
  listings_dist <- listings_dist %>% filter(dist_km <= 10)
  df <- rbind(df, listings_dist)
  listings_dist <- data.frame()
  print(i)
}

#save file
fwrite(df, "/home/shannon/R/Projects/SpringboardProject/Output Files/Listings Distance POI.csv")
