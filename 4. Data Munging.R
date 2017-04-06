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

##beach
data_beach <- data %>% filter(poitype == "Beach")
data <- data %>% filter(poitype != "Beach")

#rank distance to beach
data_beach <- data_beach %>%
  group_by(id) %>%
  mutate(distrank = rank(dist_km, ties.method = "first"))

#get closest beach
data_beach <- data_beach %>% filter(distrank == 1)

#add distance field
setDT(data_beach)
data_beach <- data_beach[dist_km <= 1, beach1km := 1]
data_beach <- data_beach[dist_km > 1 & dist_km <= 5, beach5km := 1]

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
    spoi1km = sum(poi1km),
    spoi5km = sum(poi5km),
    spoi10km = sum(poi10km)
  )

data$price <- gsub("\\$", "", data$price)
data$price <- as.numeric(data$price)

#adding weights to spoi distance
data$spoi1km <- data$spoi1km*20
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

##leftjoin beach
data_beach <- data_beach %>% select(id,beach1km,beach5km)
data <- left_join(data,data_beach,by="id")
data <- data %>% 
  select(-c(security_deposit,cleaning_fee,guests_included,extra_people,minimum_nights,number_of_reviews,cancellation_policy))

####left join city with data
data_city <- data_city %>% select(id,city1km,city5km,city10km)
data <- left_join(data,data_city,by="id")
data <- data %>% rename(listing_id=id)
rm(data_train,data_city,data_beach)

data[c("city1km","city5km","city10km")][is.na(data[c("city1km","city5km","city10km")])] <-  0
data[c("beach1km","beach5km")][is.na(data[c("beach1km","beach5km")])] <-  0


#Spread
data <- spread(data,room_type,room_type)
colnames(data)[24:26] <- c("home_apt","private_room","shared_room")
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

data <- data %>% select(-c(listing_id,listing_url,latitude,longitude,accommodates,lid,bed_type))

# > summary(data)
# city             bathrooms        bedrooms           beds            price       review_scores_rating    spoi1km      
# Length:22583       Min.   :0.000   Min.   : 0.000   Min.   : 1.000   Min.   : 13.0   Min.   : 20.00       Min.   :  0.00  
# Class :character   1st Qu.:1.000   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 80.0   1st Qu.: 90.00       1st Qu.:  0.00  
# Mode  :character   Median :1.000   Median : 1.000   Median : 1.000   Median :139.0   Median : 96.00       Median : 20.00  
# Mean   :1.351   Mean   : 1.605   Mean   : 1.969   Mean   :188.4   Mean   : 92.88       Mean   : 35.22  
# 3rd Qu.:1.500   3rd Qu.: 2.000   3rd Qu.: 2.000   3rd Qu.:231.0   3rd Qu.:100.00       3rd Qu.: 40.00  
# Max.   :8.000   Max.   :10.000   Max.   :16.000   Max.   :999.0   Max.   :100.00       Max.   :480.00  
# NA's   :37      NA's   :9        NA's   :42       NA's   :404     NA's   :9217                         
#     spoi5km         spoi10km        dist_train        beach1km         beach5km         city1km           city5km      
#  Min.   :  0.0   Min.   :  0.00   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000  
#  1st Qu.: 36.0   1st Qu.: 51.00   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000  
#  Median : 84.0   Median : 62.00   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.00000   Median :0.0000  
#  Mean   :109.2   Mean   : 58.13   Mean   :0.4232   Mean   :0.3073   Mean   :0.4508   Mean   :0.03804   Mean   :0.3647  
#  3rd Qu.:196.0   3rd Qu.: 70.00   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:1.0000  
#  Max.   :284.0   Max.   :122.00   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000  
#                                                                                                                        
#     city10km        home_apt         private_room       shared_room         Apartment            House            Townhouse        
#  Min.   :0.0000   Length:22583       Length:22583       Length:22583       Length:22583       Length:22583       Length:22583      
#  1st Qu.:0.0000   Class :character   Class :character   Class :character   Class :character   Class :character   Class :character  
#  Median :0.0000   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character  
#  Mean   :0.4096                                                                                                                    
#  3rd Qu.:1.0000                                                                                                                    
#  Max.   :1.0000  
data <- data %>% filter(!is.na(price))


#complete datasets
dataMod <- mice(data, method="rf")  # perform mice imputation, based on random forests.
completedata <- complete(dataMod)  # generate the completed data.
anyNA(completedata)
setwd("/home/shannon/R/Projects/Airbnb-Capstone-Project/Output Files/")
write.csv(completedata,"completedata.csv")
