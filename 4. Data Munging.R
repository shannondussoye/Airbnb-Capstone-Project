library(dplyr)
library(data.table)
library(tidyr)
library(mice)

#dataset, read files
setwd("/home/shannon/R/Projects/Springboard Project Files/Output Files/")
data <- fread("Listings Distance POI.csv", stringsAsFactors = F)
data <- data[, countid := .N, by = id]

#separate train stations, order train station by distance, use first one
data_train <- data %>% filter(poitype == "Railway Station")
data <- data %>% filter(poitype != "Railway Station")
#rank distance to train station
data_train <- data_train %>%
  group_by(id) %>%
  mutate(distrank = rank(dist_km, ties.method = "first"))
#get closest station only, within 1km
data_train <- data_train %>% filter(distrank == 1, dist_km <= 1.0)
data_train <- mutate(data_train, transport1km = 1)


##doing same thing with City, a place might be close to multiple city, surrounded by city is an adv
#separate train stations, order train station by distance, use first one
data_city <- data %>% filter(poitype == "City")
data <- data %>% filter(poitype != "City")

#rank distance to city
data_city <- data_city %>%
  group_by(id) %>%
  mutate(distrank = rank(dist_km, ties.method = "first"))

#get closest city
data_city <- data_city %>% filter(distrank == 1)

#add distance field
setDT(data_city)
data_city <- data_city[dist_km <= 1, city1km := 1]
data_city <- data_city[dist_km > 1 & dist_km <= 5, city5km := 1]
data_city <- data_city[dist_km > 5 & dist_km <= 10, city10km := 1]


##Data
#add distance field
setDT(data)
data <- data[dist_km <= 1, poi1km := 1]
data <- data[dist_km > 1 & dist_km <= 5, poi5km := 1]
data <- data[dist_km > 5 & dist_km <= 10, poi10km := 1]
data <- data %>% as.data.frame()

#replace NA
data[c("poi1km", "poi5km", "poi10km")][is.na(data[c("poi1km", "poi5km", "poi10km")])] <-  0

#select fields
data <- data %>% select(1:30, 42:46)
data <- data %>% select(-c(dist_km, dist_km, countid))

#sum number of poi within each distance
data <- data %>%
  group_by(
    id,
    listing_url,
    host_id,
    host_since,
    city,
    latitude,
    longitude,
    property_type,
    room_type,
    accommodates,
    bathrooms,
    bedrooms,
    beds,
    bed_type,
    price,
    security_deposit,
    cleaning_fee,
    guests_included,
    extra_people,
    minimum_nights,
    number_of_reviews,
    review_scores_rating,
    review_scores_accuracy,
    review_scores_cleanliness,
    review_scores_checkin,
    review_scores_communication,
    review_scores_location,
    review_scores_value,
    cancellation_policy,
    lid
  ) %>%
  summarise(
    spoi1km = max(poi1km),
    spoi5km = max(poi5km),
    spoi10km = max(poi10km)
  )

data$price <- gsub("\\$", "", data$price)
data$price <- as.numeric(data$price)

#adding weights to spoi distance
data$spoi1km <- data$spoi1km*10
data$spoi5km <- data$spoi5km*4


######left join data and data train
data_train <- data_train %>% select(id,dist_km) %>% rename(train_dist=dist_km)
data <- left_join(data,data_train,by="id")
data <- data %>% as.data.frame() %>% select(-c(host_id,host_since,review_scores_accuracy,review_scores_cleanliness,review_scores_checkin,
                                               review_scores_communication,review_scores_location,review_scores_value))

data["train_dist"][is.na(data["train_dist"])] <-  0
data$dist_train <- data$train_dist*1000000 %>% as.integer()
for(i in 1:nrow(data))
{
  if(data$dist_train[i] > 0) {
    data$dist_train[i] <- 1
  }
}
data <- data %>% select(-train_dist)



####left join city with data
data_city <- data_city %>% select(id,city1km,city5km,city10km)
data <- left_join(data,data_city,by="id")
data <- data %>% 
  select(-c(security_deposit,cleaning_fee,guests_included,extra_people,minimum_nights,number_of_reviews,cancellation_policy))
data <- data %>% rename(listing_id=id)


#read renting - price summarised file
# cal_renting <- read.csv("/home/shannon/R/Projects/SpringboardProject/Output Files/Calendar Summarised Income.csv")
# data <- left_join(data,cal_renting,by="listing_id")
# data <- data %>% select(-c(Max,Min,Std))
# data <- data %>% mutate(mean_price=Mean*price,median_price=Median*price)
# data <- data %>% filter(!is.na(Mean))
# data$median_price <- round(data$median_price,-2)
# data$mean_price <-  round(data$mean_price,-2)

rm(data_train,data_city,cal_renting)

# data$mean_price <- round(data$mean_price,-3)
# data$median_price <- round(data$median_price,-3)
# data$min <- NA
# data$max <- NA
# data <- data %>% filter(!is.na(mean_price),!is.na(median_price))
# for(i in 1:nrow(data)){
#   if(data$mean_price[i] > data$median_price[i]) 
#   {
#      data$max[i] <-  data$mean_price[i]
#      data$min[i]  <-  data$median_price[i]
#   }
#   else if(data$mean_price[i] < data$median_price[i]){
#     
#     data$min[i] <- data$mean_price[i]
#     data$max[i] <- data$median_price[i]
#   }
#   else{
#     data$min[i] <- data$mean_price[i]
#     data$max[i] <- data$median_price[i]
#   }
# 
# }
# 
# paste(data$min,data$max) %>% unique()
# 
# for (i in 1:nrow(data)) {
# if (data$min[i] == data$max[i]) {
#   data$min[i] <- data$max[i] - 1000
#   }
# }
# 
# data$response <- paste(data$min,'-',data$max)
# data$response %>% unique()
# 
# 
# #some cases where people made no money at all. we'll remove these
# data <- filter(data,max > 0)
# 
# #create bins for response
# band <- data$response %>% unique() %>% as.data.frame()
# band <- rename(band,response = .)
# band$response <- as.character(band$response)
# band$response_band <- rownames(band) %>% as.numeric()
# 
# #left join response-band
# data <- left_join(data,band,by = "response")
# data <- select(data,-c(X,Mean,Median,city,mean_price,median_price,min,max,response))
data[c("city1km","city5km","city10km")][is.na(data[c("city1km","city5km","city10km")])] <-  0

#Spread
data <- spread(data,room_type,room_type)
colnames(data)[22:24] <- c("home_apt","private_room","shared_room")
data[c("home_apt","private_room","shared_room")][is.na(data[c("home_apt","private_room","shared_room")])] <-  0
data$home_apt <- gsub("Entire home\\/apt",1,data$home_apt)
data$private_room <- gsub("Private room",1,data$private_room)
data$shared_room <- gsub("Shared room",1,data$shared_room)

#too many property types, filtered based on %share
property_vector <- data %>% 
                    group_by(property_type) %>% 
                      tally() %>% 
                        arrange(-n) %>% 
                          mutate(per=(n/sum(n)*100)) %>% 
                            filter(per>2) %>% 
                              select(property_type) %>% 
                                as.vector()

data <- filter(data,property_type=="Apartment"|property_type=="House"|property_type=="Townhouse")
data <- spread(data,property_type,property_type)
data[c("Apartment","House","Townhouse" )][is.na(data[c("Apartment","House","Townhouse")])] <-  0
data$Apartment <- gsub("Apartment",1,data$Apartment)
data$House <- gsub("House",1,data$House)
data$Townhouse <- gsub("Townhouse",1,data$Townhouse)

databk <- data
data <- data %>% select(-c(listing_id,listing_url,latitude,longitude,accommodates,lid,bed_type))

# ### First 4 columns has NAs - need to treat NAs
# > summary(data)
# bathrooms        bedrooms           beds        review_scores_rating    spoi1km          spoi5km         spoi10km     
# Min.   :0.000   Min.   : 0.000   Min.   : 1.000   Min.   : 20.00       Min.   : 0.000   Min.   :0.000   Min.   :0.0000  
# 1st Qu.:1.000   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 90.00       1st Qu.: 0.000   1st Qu.:4.000   1st Qu.:1.0000  
# Median :1.000   Median : 1.000   Median : 1.000   Median : 96.00       Median :10.000   Median :4.000   Median :1.0000  
# Mean   :1.319   Mean   : 1.582   Mean   : 1.936   Mean   : 92.92       Mean   : 7.125   Mean   :3.992   Mean   :0.9999  
# 3rd Qu.:1.500   3rd Qu.: 2.000   3rd Qu.: 2.000   3rd Qu.:100.00       3rd Qu.:10.000   3rd Qu.:4.000   3rd Qu.:1.0000  
# Max.   :7.000   Max.   :10.000   Max.   :16.000   Max.   :100.00       Max.   :10.000   Max.   :4.000   Max.   :1.0000  
# NA's   :21      NA's   :9        NA's   :33       NA's   :8336                                                          
#
# dist_train        city1km           city5km          city10km      response_band       home_apt         private_room      
# Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :  1.000   Length:21023       Length:21023      
# 1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:  2.000   Class :character   Class :character  
# Median :0.0000   Median :0.00000   Median :0.0000   Median :0.0000   Median :  6.000   Mode  :character   Mode  :character  
# Mean   :0.4308   Mean   :0.03962   Mean   :0.3754   Mean   :0.4059   Mean   :  9.833                                        
# 3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:  9.000                                        
# Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :140.000   
# 
# shared_room         Apartment            House            Townhouse        
# Length:21023       Length:21023       Length:21023       Length:21023      
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
data <- data %>% filter(!is.na(price))
write.csv(data,"pre-completion-dataset.csv")

#complete datasets
dataMod <- mice(data, method="rf")  # perform mice imputation, based on random forests.
completedata <- complete(dataMod)  # generate the completed data.
anyNA(completedata)
write.csv(completedata,"completedata.csv")

#There are 140 different types of response. ~too many?
#possible solutions: clustering or create larger buckets

# library(stringr)
# band[,c(3,4)] <- str_split_fixed(band$response,"-",2)
# band$V3 <- band$V3 %>% as.numeric()
# band$V4 <- band$V4 %>% as.numeric()
# 
# #group greater than min (25k) together, max is 30k
# band$newband <- NA
# for(i in 1:nrow(band))
# {
#   if (band$V3[i] >= 25000)
#     band$newband[i] <- "25000 - 30000"
#   
# }



