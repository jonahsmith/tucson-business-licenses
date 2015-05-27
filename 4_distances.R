data <- read.table("datasets/tucson_area.tsv", sep="\t", head=TRUE)

# Let's map it.
library('ggmap')
CenterOfMap <- geocode("Tucson, AZ")
tucson <- get_googlemap(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat), zoom=11, maptype="roadmap", source="google", color="bw", style="element:labels|visibility:off&style=feature:road|visibility:off")

tucson_full <- get_googlemap(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat), zoom=11, maptype="roadmap", source="google")

activities <- read.table("reference_data/activity-codes.txt", sep="\t", header=TRUE, comment.char = "")

merged_data <- merge(data, activities, by.x = "Activity_Code_1", by.y = "code", all.x = TRUE)

data$Activity_Code_1 <- as.factor(data$Activity_Code_1)
ggmap(tucson) + geom_point(data=merged_data, aes(x = lon, y = lat, color = activity_category), alpha = .5)

#Restaurants, Amuseuments, Retail Sales, Peddler-Food, Peddlar-Variety
interesting_activities <- c(11,12,17,31,37)

relevant_companies_data <- subset(merged_data, Activity_Code_1 %in% interesting_activities)
ggmap(tucson_full) + geom_point(data=relevant_companies_data, aes(x = lon, y = lat, color = activity_category), alpha = .5)

deg2rad <- function(deg) return(deg*pi/180)

gcd.hf <- function(long1, lat1, long2, lat2) {
  long1 <- deg2rad(long1) 
  lat1 <- deg2rad(lat2)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
	
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d*0.621371) # Distance in miles
}

gcd.eu <- function(long1, lat1, long2, lat2){
	return(sqrt((long1-long2)^2 + (lat1-lat2)^2))
}

stations <- read.table("reference_data/sunlink-stops.txt", sep=",", header=TRUE)

merged_data$dist_to_closest_stop <- Inf
merged_data$closest_stop <- NA
for (i in 1:nrow(stations)){
	for (j in 1:nrow(merged_data)){
		dist <- gcd.eu(stations$stop_lon[i], stations$stop_lat[i], merged_data$lon[j], merged_data$lat[j])
		if (merged_data$dist_to_closest_stop[j] > dist) {
			merged_data$dist_to_closest_stop[j] <- dist
			merged_data$closest_stop[j] <- as.character(stations$stop_code[i])
		}
	}
}

write.table(merged_data, "datasets/with_closest_stop.csv", sep=",", row.names=FALSE)