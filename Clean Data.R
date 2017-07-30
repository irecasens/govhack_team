
traffic <- df
traffic$vehicles_count = as.numeric(traffic$vehicle_class_1) + as.numeric(traffic$vehicle_class_2) + as.numeric(traffic$vehicle_class_3) + as.numeric(traffic$vehicle_class_4) + as.numeric(traffic$vehicle_class_5) + as.numeric(traffic$vehicle_class_6) + as.numeric(traffic$vehicle_class_7) + as.numeric(traffic$vehicle_class_8) + as.numeric(traffic$vehicle_class_9) + as.numeric(traffic$vehicle_class_10) + as.numeric(traffic$vehicle_class_11) + as.numeric(traffic$vehicle_class_12) + as.numeric(traffic$vehicle_class_13)

traffic = select(traffic, c(date, time, location, speed_limit, maximum_speed, road_name, suburb, vehicles_count ))

traffic = filter(traffic, str_sub(traffic$date,-4,-1) == '2016')

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
accidents = filter(accidents, str_sub(accidents$ACCIDENT_DATE,-4,-1) == '2016')
accidents = filter(accidents, DAY_OF_WEEK == 'Monday' | DAY_OF_WEEK == 'Tuesday' | DAY_OF_WEEK == 'Wednesday' | DAY_OF_WEEK == 'Thursday' | DAY_OF_WEEK == 'Friday' )
accidents$time = str_sub(accidents$ACCIDENT_TIME,0,2)
accidents$ACCIDENT_DATE = gsub("\\/", "  -    ", accidents$ACCIDENT_DATE)
accidents$day = str_sub(accidents$ACCIDENT_DATE,1,3)
accidents$month = str_sub(accidents$ACCIDENT_DATE,6,11)
accidents$year = str_sub(accidents$ACCIDENT_DATE,16,22)

accidents <- accidents[grep("METROPOLITAN", accidents$REGION_NAME_ALL), ]

accidents = select(accidents, c(REGION_NAME_ALL, day, month, year, LGA_NAME, LONGITUDE, LATITUDE, ACCIDENT_DATE, DAY_OF_WEEK, time, ACCIDENT_TIME, LIGHT_CONDITION, SPEED_ZONE, SEVERITY, FATALITY, PEDESTRIAN, ACCIDENT_TYPE  ) )



pedestrians <- df3
pedestrians <- filter(pedestrians, year == 2016)
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

