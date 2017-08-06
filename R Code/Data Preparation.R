
library(tidyr)
library(rmarkdown)
library(dplyr)
library(ggplot2)
library(data.table)
library(RSocrata)
library(googleway)
library(jsonlite)
library(rgdal)
library(rgeos)
library(geojsonio)
library(stringr)
library(sqldf)
library(ggmap)
library(ggthemes)
library(scales)
library(grid)
library(gridExtra)



# Load Data 
token <- "add token from data.melbourne.vic.gov"
df <- read.socrata("https://data.melbourne.vic.gov.au/resource/u2n7-9zb7.json", app_token = token)
df

url <- 'https://opendata.arcgis.com/datasets/c2a69622ebad42e7baaa8167daa72127_0.geojson'
df2 <- geojsonio::geojson_read(url,  what = "sp")
df2

df3 = read.csv('pedestrian.csv')
df3


traffic <- df
traffic$vehicles_count = as.numeric(traffic$vehicle_class_1) + as.numeric(traffic$vehicle_class_2) + as.numeric(traffic$vehicle_class_3) + as.numeric(traffic$vehicle_class_4) + as.numeric(traffic$vehicle_class_5) + as.numeric(traffic$vehicle_class_6) + as.numeric(traffic$vehicle_class_7) + as.numeric(traffic$vehicle_class_8) + as.numeric(traffic$vehicle_class_9) + as.numeric(traffic$vehicle_class_10) + as.numeric(traffic$vehicle_class_11) + as.numeric(traffic$vehicle_class_12) + as.numeric(traffic$vehicle_class_13)

traffic = select(traffic, c(date, time, location, speed_limit, maximum_speed, road_name, suburb, vehicles_count ))

#traffic = filter(traffic, str_sub(traffic$date,-4,-1) == '2016')

traffic$location = gsub("South of ","",traffic$location)
traffic$location = gsub("North of ","",traffic$location)
traffic$location = gsub("Just North of ","",traffic$location)
traffic$location = gsub("Between ","",traffic$location)
traffic$location = gsub("At Entrance to ","",traffic$location)
traffic$location = gsub("and .*","",traffic$location)
traffic$location = gsub("& .*","",traffic$location)
traffic$road_name = paste(traffic$road_name, ", ", traffic$suburb, ", ", "Melbourne")

traffic$year = str_sub(traffic$date,-4,-1)
traffic$month = str_sub(traffic$date,-7,-6)
traffic$day= str_sub(traffic$date,-10,-8)
traffic$day = gsub("\\/", "", traffic$day)
traffic$time = gsub(":.*", "", traffic$time)

accidents <- df2
accidents <- as.data.frame(accidents)
#accidents = filter(accidents, str_sub(accidents$ACCIDENT_DATE,-4,-1) == '2016')
accidents = filter(accidents, DAY_OF_WEEK == 'Monday' | DAY_OF_WEEK == 'Tuesday' | DAY_OF_WEEK == 'Wednesday' | DAY_OF_WEEK == 'Thursday' | DAY_OF_WEEK == 'Friday' )
accidents$time = str_sub(accidents$ACCIDENT_TIME,0,2)
accidents$ACCIDENT_DATE = gsub("\\/", "  -    ", accidents$ACCIDENT_DATE)
accidents$day = str_sub(accidents$ACCIDENT_DATE,1,3)
accidents$month = str_sub(accidents$ACCIDENT_DATE,6,11)
accidents$year = str_sub(accidents$ACCIDENT_DATE,16,22)

accidents <- accidents[grep("METROPOLITAN", accidents$REGION_NAME_ALL), ]

accidents = select(accidents, c(REGION_NAME_ALL, day, month, year, LGA_NAME, LONGITUDE, LATITUDE, ACCIDENT_DATE, DAY_OF_WEEK, time, ACCIDENT_TIME, LIGHT_CONDITION, SPEED_ZONE, SEVERITY, FATALITY, PEDESTRIAN, ACCIDENT_TYPE  ) )


pedestrians <- df3
#pedestrians <- filter(pedestrians, year == 2016)
pedestrians <- select(pedestrians, c(date_time, year, mdate, time, sensor_name, hourly_count ))
pedestrians$day = str_sub(pedestrians$date_time,1,3)
pedestrians$month = str_sub(pedestrians$date_time,5,7)
pedestrians$year = str_sub(pedestrians$date_time,9,12)
pedestrians$month[grepl("JAN",pedestrians$month)]<-"01"
pedestrians$month[grepl("FEB",pedestrians$month)]<-"02"
pedestrians$month[grepl("MAR",pedestrians$month)]<-"03"
pedestrians$month[grepl("APR",pedestrians$month)]<-"04"
pedestrians$month[grepl("MAY",pedestrians$month)]<-"05"
pedestrians$month[grepl("JUN",pedestrians$month)]<-"06"
pedestrians$month[grepl("JUL",pedestrians$month)]<-"07"
pedestrians$month[grepl("AUG",pedestrians$month)]<-"08"
pedestrians$month[grepl("SEP",pedestrians$month)]<-"09"
pedestrians$month[grepl("OCT",pedestrians$month)]<-"10"
pedestrians$month[grepl("NOV",pedestrians$month)]<-"11"
pedestrians$month[grepl("DEC",pedestrians$month)]<-"12"

pedestrians$sensor_name = as.character(pedestrians$sensor_name)

pedestrians$sensor_name = gsub("\\(", "", pedestrians$sensor_name)
pedestrians$sensor_name = gsub("\\)", "", pedestrians$sensor_name)
pedestrians$sensor_name[grepl("Town Hall",pedestrians$sensor_name)]<-"100 Swanston St, VIC 3000"
pedestrians$sensor_name[grepl("Collins Place South",pedestrians$sensor_name)]<-"66/20 Flinders Ln VIC 3000"
pedestrians$sensor_name[grepl("Collins Place North",pedestrians$sensor_name)]<-"45-55 Collins St, VIC 3000"
pedestrians$sensor_name[grepl("Bourke Street Mall South",pedestrians$sensor_name)]<-"Bourke Street Mall VIC 3000"
pedestrians$sensor_name[grepl("Bourke Street Mall North",pedestrians$sensor_name)]<-"311 Little Bourke St VIC 3000"
pedestrians$sensor_name[grepl("Spencer St-Collins St South",pedestrians$sensor_name)]<-"73 State Route 50 VIC 3004"
pedestrians$sensor_name[grepl("Lygon St West",pedestrians$sensor_name)]<-"Cemetery/Lygon St"
pedestrians$sensor_name[grepl("Lygon St East",pedestrians$sensor_name)]<-"Cemetery East - Stop 115"
pedestrians$sensor_name[grepl("Flinders St-Elizabeth St East",pedestrians$sensor_name)]<-"223/291 Flinders St, VIC 3000"
pedestrians$sensor_name[grepl("Chinatown-Lt Bourke St South",pedestrians$sensor_name)]<-"Chinatown Melbourne"
pedestrians$sensor_name[grepl("Chinatown-Swanston St North",pedestrians$sensor_name)]<-"224 Swanston St, VIC 3000"
pedestrians$sensor_name[grepl("Flinders St-Spring St West",pedestrians$sensor_name)]<-"2 Spring St East VIC 3002"
pedestrians$sensor_name[grepl("Flinders St-Spark L",pedestrians$sensor_name)]<-"32 Flinders St VIC 3000"
pedestrians$sensor_name[grepl("Flinders St-Swanston St West",pedestrians$sensor_name)]<-"1 Swanston St VIC 3004"
pedestrians$sensor_name[grepl("Lonsdale St-Spring St West",pedestrians$sensor_name)]<-"1 Lonsdale St East VIC 3000"
pedestrians$sensor_name[grepl("Grattan St-Swanston St West",pedestrians$sensor_name)]<-"143/151 Grattan St Carlton VIC 3053"
pedestrians$sensor_name[grepl("Monash Rd-Swanston St West",pedestrians$sensor_name)]<-"Sidney Myer Asia Centre, Swanston St & Monash Rd, Parkville VIC 3010"
pedestrians$sensor_name[grepl("Tin Alley-Swanston St West",pedestrians$sensor_name)]<-"834 Swanston St Carlton VIC 3053"
pedestrians$sensor_name[grepl("St Kilda Rd-Alexandra Garden",pedestrians$sensor_name)]<-"128/2-128 St Kilda Rd Southbank VIC 3006"
pedestrians$sensor_name[grepl("Bourke St-Russell St West",pedestrians$sensor_name)]<-"153 Russell St VIC 3000"
pedestrians$sensor_name[grepl("Spencer St-Collins St North",pedestrians$sensor_name)]<-"Level 2 Cnr Collins & Spencer Sts, Docklands VIC 3008"



traffic2 <- sqldf("
                  select  date, time, day, month, year, location, speed_limit, road_name, suburb, max(maximum_speed) as maximum_speed, sum(vehicles_count) as vehicles_count
                  from traffic
                  group by date, time, day, month, year, location, speed_limit, road_name, suburb
                  HAVING(sum(vehicles_count)>0);")



accidents2 <- sqldf("
                    select  ACCIDENT_DATE as date, time, day, month, year,  LGA_NAME as location, LONGITUDE, LATITUDE, sum(PEDESTRIAN) as pedestrians_involved
                    from accidents
                    group by ACCIDENT_DATE, time , day, month, year, LGA_NAME, LONGITUDE, LATITUDE;")




pedestrians2 <- sqldf("
                      select  date_time as date , time, day, month, year, sensor_name || ', Melbourne'  as sensor_name , sum(hourly_count) as hourly_count
                      from pedestrians
                      group by date_time , time, sensor_name, day, month, year;")




# Googleway geocoding
apiKey = 'add google key'

get_geocode <- function(df, var)
{
    for(i in seq(1:nrow(df)))
    {
        
        txt = paste('df$LAT[', i, '] = google_geocode(address = df$', var, '[', i, '], key = apiKey)$results$geometry$location$lat' ,  sep="" )
        eval(parse(text=txt))
        txt = paste('df$LNG[', i, '] = google_geocode(address = df$', var, '[', i, '], key = apiKey)$results$geometry$location$lng' ,  sep="" )
        eval(parse(text=txt))
    }
    return(df)
}


get_address <- function(df)
{
    for(i in seq(1:nrow(df)))
    {
        
        txt = paste('df$address[', i, '] = google_reverse_geocode(location = c(accidents2$LATITUDE[',i, '], accidents2$LONGITUDE[',i,']), key = apiKey)$results$formatted_address[1]' ,  sep="" )
        eval(parse(text=txt))
        
    }
    return(df)
}


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



apiKey = 'add google key'

#x = google_geocode(address = u_traffic$road_name[90], key = apiKey)$results$geometry$location$lat

u_traffic = as.data.frame(unique(traffic2$road_name))
names(u_traffic) = c("road_name")
u_traffic$road_name = as.character(u_traffic$road_name)

u_traffic

for(i in seq(1:nrow(u_traffic)))
{
    
    txt = paste('xcode = google_geocode(address = u_traffic$road_name[', i, '], key = apiKey)' ,  sep="" )
    eval(parse(text=txt))
    txt = paste('longitude = ifelse(is.null(xcode$results$geometry$location$lng),0,xcode$results$geometry$location$lng)' ,  sep="" )
    eval(parse(text=txt))
    txt = paste('latitude = ifelse(is.null(xcode$results$geometry$location$lat),0,xcode$results$geometry$location$lat)' ,  sep="" )
    eval(parse(text=txt))
    
    txt = paste('u_traffic$LAT[', i, '] = latitude' ,  sep="" )
    eval(parse(text=txt))
    txt = paste('u_traffic$LNG[', i, '] = longitude' ,  sep="" )
    eval(parse(text=txt))
}

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


u_traffic = read.csv('u_traffic.csv')
traffic3 <- traffic2

u_traffic$road_name = gsub("\\. "," ",u_traffic$road_name)
traffic3$road_name = gsub(", "," ",traffic3$road_name)

traffic2 <- sqldf("
                  select  traffic3.*, u_traffic.LAT, u_traffic.LNG
                  from traffic3 left join u_traffic on traffic3.road_name = u_traffic.road_name;")



pedestrians2$day_week <- weekdays(as.Date(paste(pedestrians2$year, "-", pedestrians2$month, "-", pedestrians2$day, sep="")))

pedestrians2$day_week_id <- wday(as.Date(paste(pedestrians2$year, "-", pedestrians2$month, "-", pedestrians2$day, sep="")))

pedestrians2$day_type = ifelse(pedestrians2$day_week_id == 1 | pedestrians2$day_week_id == 7, "weekend", "workday")

traffic2$day_week <- weekdays(as.Date(paste(traffic2$year, "-", traffic2$month, "-", traffic2$day, sep="")))

traffic2$day_week_id <- wday(as.Date(paste(traffic2$year, "-", traffic2$month, "-", traffic2$day, sep="")))

traffic2$day_type = ifelse(traffic2$day_week_id == 1 | traffic2$day_week_id == 7, "weekend", "workday")



qnt_threshold <- quantile(pedestrians2$hourly_count, probs=.9, na.rm = T)[1]
pedestrians2$hourly_count[pedestrians2$hourly_count > qnt_threshold] <- qnt_threshold

qnt_threshold <- quantile(traffic2$vehicles_count, probs=.9, na.rm = T)[1]
traffic2$vehicles_count[traffic2$vehicles_count > qnt_threshold] <- qnt_threshold


pedestrians2$hourly_count_norm = (pedestrians2$hourly_count - min(pedestrians2$hourly_count) ) / ( max(pedestrians2$hourly_count) - min(pedestrians2$hourly_count) ) 

traffic2$vehicles_count_norm = (traffic2$vehicles_count - min(traffic2$vehicles_count) ) / ( max(traffic2$vehicles_count) - min(traffic2$vehicles_count) )

pedestrians2$hourly_count = round(10*pedestrians2$hourly_count_norm,0)
traffic2$vehicles_count = round(10*traffic2$vehicles_count_norm,0)



# JOIN pedestrians with traffic
pedestrians2$speed_limit = 0
pedestrians2$maximum_speed = 0
pedestrians2$vehicles_count = 0
traffic2$hourly_count = 0

viz = select(pedestrians2, c(day, month, year,time, day_week_id, day_type, speed_limit, maximum_speed, vehicles_count, hourly_count, LAT, LNG ))
viz = rbind(viz, select(traffic2, c(day, month, year,time, day_week_id, day_type, speed_limit, maximum_speed, vehicles_count, hourly_count, LAT, LNG )))

viz$day = as.numeric(viz$day)
viz$month = as.numeric(viz$month)
viz$year = as.numeric(viz$year)
viz$time = as.numeric(viz$time)
viz$speed_limit = as.numeric(viz$speed_limit)
viz$vehicles_count = as.numeric(viz$vehicles_count)
viz$maximum_speed = as.numeric(viz$maximum_speed)
viz$hourly_count = as.numeric(viz$hourly_count)
viz$day_week_id = as.numeric(viz$day_week_id)


new_viz <- sqldf("
                 select  year, day_type, time, LAT, LNG, avg(vehicles_count) as vehicles_count, avg(hourly_count) as hourly_count
                 from viz 
                 group by year, day_type, time, LAT, LNG;")


new_viz = viz %>% gather(measure, value, c(hourly_count,vehicles_count))
new_viz = select(new_viz, c(year, day_type, time, LAT, LNG, measure, value) )
new_viz = filter(new_viz, value > 0)

new_viz$risk = ifelse( new_viz$value > quantile(new_viz$value, probs=.75, na.rm = T)[1], 5,  
                       ifelse( new_viz$value > quantile(new_viz$value, probs=.6, na.rm = T)[1], 4, 
                               ifelse( new_viz$value > quantile(new_viz$value, probs=.45, na.rm = T)[1], 3, 
                                       ifelse( new_viz$value > quantile(new_viz$value, probs=.3, na.rm = T)[1], 2, 
                                               ifelse( new_viz$value > quantile(new_viz$value, probs=.15, na.rm = T)[1], 1, 0)))))



new_viz = filter(new_viz, value > 0)
new_viz = filter(new_viz, year >= 2017)
new_viz = filter(new_viz, time >7 & time <9)


n = nrow(new_viz)

# create first row
row = select(new_viz, c(year, day_type, time, LAT, LNG, risk, measure, value))[1,]
row$value = 1
tidy_viz = row

tic()
# create following rows
for(i in seq(1:n)){
    
    row = select(new_viz, c(year, day_type, time, LAT, LNG, risk, measure, value))[i,]
    rows = row$value
    row$value = 1
    
    for(j in seq(1:rows))
    {
        tidy_viz = rbind(tidy_viz, row)
    }
    
    if(i%%1000 == 0 ) { print(paste(round(i/n,3)*100, " %"))}
    
    
    
}
toc()

tidy_viz <- tidy_viz[2:nrow(tidy_viz),]
tidy_viz

write.csv(tidy_viz, file = "tidy_viz.csv", row.names=FALSE, na="")


