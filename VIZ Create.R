
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


u_traffic = as.data.frame(unique(traffic2$road_name))
names(u_traffic) = c("road_name")
u_traffic$road_name = as.character(u_traffic$road_name)

u_traffic

write.csv(u_traffic, file = "u_traffic.csv", row.names=FALSE, na="", sep = ';')


u_sensor = read.csv('u_sensor.csv')
pedestrians3 <- pedestrians2

u_sensor$sensor_name = gsub("\\. "," ",u_sensor$sensor_name)
pedestrians3$sensor_name = gsub(", "," ",pedestrians3$sensor_name)

u_sensor

pedestrians2 <- sqldf("
select  pedestrians3.*, u_sensor.LAT, u_sensor.LNG
from pedestrians3 left join u_sensor on pedestrians3.sensor_name = u_sensor.sensor_name;")

pedestrians2 = filter(pedestrians2, month == "05" & as.numeric(day) <8)


u_traffic = read.csv('u_traffic.csv')
traffic3 <- traffic2

u_traffic$road_name = gsub("\\. "," ",u_traffic$road_name)
traffic3$road_name = gsub(", "," ",traffic3$road_name)

traffic2 <- sqldf("
select  traffic3.*, u_traffic.LAT, u_traffic.LNG
from traffic3 left join u_traffic on traffic3.road_name = u_traffic.road_name;")

#accidents2 <- get_address(accidents2)
#accidents2 <- get_geocode(accidents2, "address")

#traffic2 <- get_geocode(traffic2, "road_name")

pedestrians2
traffic2
accidents2



# JOIN pedestrians with traffic
pedestrians2$speed_limit = 0
pedestrians2$maximum_speed = 0
pedestrians2$vehicles_count = 0
traffic2$hourly_count = 0

viz = select(pedestrians2, c(day, month, year,time, speed_limit, maximum_speed, vehicles_count, hourly_count, LAT, LNG ))
viz = rbind(viz, select(traffic2, c(day, month, year,time, speed_limit, maximum_speed, vehicles_count, hourly_count, LAT, LNG )))

viz$day = as.numeric(viz$day)
viz$month = as.numeric(viz$month)
viz$year = as.numeric(viz$year)
viz$time = as.numeric(viz$time)
viz$speed_limit = as.numeric(viz$speed_limit)
viz$vehicles_count = as.numeric(viz$vehicles_count)
viz$maximum_speed = as.numeric(viz$maximum_speed)
viz$hourly_count = as.numeric(viz$hourly_count)


write.csv(viz, file = "viz.csv", row.names=FALSE, na="", sep = ';')

