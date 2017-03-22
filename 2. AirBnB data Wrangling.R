library(dplyr)
library(zoo)
library(ggplot2)
library(data.table)
library(geosphere)

setwd("/home/shannon/R/Projects/Springboard Project Files/Input Files/Airbnb Files")

#Read listings and select fields
listings <- fread("listings.csv",stringsAsFactors = F)
listings <- listings %>% select(-c(scrape_id,last_scraped,name,summary,space,description,experiences_offered,
                                   neighborhood_overview,notes,transit,access,interaction,house_rules,thumbnail_url,
                                   medium_url,picture_url,xl_picture_url,host_url,host_name,host_location,host_about,
                                   host_response_time,host_response_rate,host_acceptance_rate,host_is_superhost,host_thumbnail_url,
                                   host_picture_url,host_neighbourhood,host_listings_count,host_total_listings_count,host_verifications,
                                   host_has_profile_pic,host_identity_verified,street,neighbourhood,neighbourhood_cleansed,
                                   neighbourhood_group_cleansed,state,zipcode,market,smart_location,country_code,country,
                                   is_location_exact,amenities,square_feet,weekly_price,monthly_price,maximum_nights,
                                   calendar_updated,has_availability,availability_30,availability_60,availability_90,
                                   availability_365,calendar_last_scraped,first_review,last_review,requires_license,license,
                                   jurisdiction_names,instant_bookable,require_guest_profile_picture,require_guest_phone_verification,
                                   calculated_host_listings_count,reviews_per_month))

#read all calendars
cal_sep15 <- fread("Calendars/calendar_sept2015.csv",stringsAsFactors = F) %>% filter(available != 't')
cal_oct15 <- fread("Calendars/calendar_oct2015.csv",stringsAsFactors = F) %>% filter(available != 't')
cal_dec15 <- fread("Calendars/calendar_dec2015.csv",stringsAsFactors = F) %>% filter(available != 't')
cal_jan16 <- fread("Calendars/calendar_jan2016.csv",stringsAsFactors = F) %>% filter(available != 't')
cal_dec16 <- fread("Calendars/calendar_dec2016.csv",stringsAsFactors = F) %>% filter(available != 't')

#bind all calendar listings
calendar <- rbind(cal_sep15,cal_oct15,cal_dec15,cal_jan16,cal_dec16)

#get unique listings, there are some overlappings
calendar <- unique(calendar)

#format date and ad new columns
calendar$date <- as.Date(calendar$date)
calendar$year <- as.numeric(format(calendar$date, "%Y"))
calendar$mon <- as.numeric(format(calendar$date, "%m"))
calendar$ym <- as.yearmon(calendar$date, "%m %Y")

#summarise per month
cal_mon <- calendar %>% group_by(listing_id,ym) %>% tally() %>% as.data.frame()

#summarise per listings
cal_renting <- cal_mon %>% 
  group_by(listing_id) %>% 
  summarise(Mean=mean(n), Max=max(n), Min=min(n), Median=median(n), Std=sd(n))

#round numbers
cal_renting$Mean <- round(cal_renting$Mean)
cal_renting$Max <- round(cal_renting$Max)
cal_renting$Min <- round(cal_renting$Min)
cal_renting$Median <- round(cal_renting$Median)
cal_renting$Std <- round(cal_renting$Std)

#remove unused data frames
rm(cal_dec15,cal_dec16,cal_jan16,cal_oct15,cal_sep15,calendar,cal_mon)

write.csv(cal_renting,"/home/shannon/R/Projects/SpringboardProject/Output Files/Calendar Summarised Income.csv")
