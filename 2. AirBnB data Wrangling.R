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

