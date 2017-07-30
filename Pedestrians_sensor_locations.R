
u_sensor = as.data.frame(unique(pedestrians2$sensor_name))
names(u_sensor) = c("sensor_name")
u_sensor$sensor_name = as.character(u_sensor$sensor_name)

for(i in seq(1:nrow(u_sensor)))
{
    
    txt = paste('u_sensor$LAT[', i, '] = google_geocode(address = u_sensor$sensor_name[', i, '], key = apiKey)$results$geometry$location$lat' ,  sep="" )
    eval(parse(text=txt))
    txt = paste('u_sensor$LNG[', i, '] = google_geocode(address = u_sensor$sensor_name[', i, '], key = apiKey)$results$geometry$location$lng' ,  sep="" )
    eval(parse(text=txt))
}

write.csv(u_sensor, file = "u_sensor.csv", row.names=FALSE, na="", sep = ';')

