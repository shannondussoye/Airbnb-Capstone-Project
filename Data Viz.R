plot_ly(data, x = ~log(price)) %>%
  add_histogram()
plot_ly(data, x = ~price, y = ~bedrooms, name = "default")


plot_ly(data, x = ~jitter(round(price,-1),1), y = ~log(bedrooms, name = "default",color = ~factor(dist_train),size = ~city1km))

plot_ly(data, x = ~price, y = ~home_apt, name = "default")

cor(data, method="kendall", na.rm = TRUE)


library("leaflet")

dataleaflet <- listings %>% as.data.frame()
dataleaflet$price <- gsub("\\$", "", dataleaflet$price) %>% as.integer()
dataleaflet$price <- round(dataleaflet$price,-1)
dataleaflet <- dataleaflet %>% filter(!is.na(price))
dataleaflet$price <-  round(dataleaflet$price,-1)

# Show first 20 rows from the `quakes` dataset
leaflet(data = dataleaflet[1:20,]) %>% addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(price), label = ~as.character(price))


leaflet(dataleaflet) %>% addTiles() %>%
  addCircleMarkers(
    radius = ~(price/100),
    color = ~price,
    stroke = FALSE, fillOpacity = 0.5,
    popup = ~as.character(price)
  ) 



