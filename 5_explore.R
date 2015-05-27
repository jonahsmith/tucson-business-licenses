data <- read.csv("datasets/with_closest_stop.csv")
data$Accept_Date <- as.Date(data$Accept_Date)
interesting_activities <- c(11,12,17,31,37)

# Let's define the corridor to be a radius of .003 from the stops. Looks reasonable on the maps.
corridor <- subset(data, dist_to_closest_stop < .003)
non_corridor <- subset(data, !(dist_to_closest_stop < .003))

# Just for future reference
write.table(corridor, "datasets/corridor.csv", sep=",", row.names=FALSE)
write.table(non_corridor, "datasets/non-corridor.csv", sep=",", row.names=FALSE)

library("ggmap")
CenterOfMap <- geocode("Tucson City Hall, AZ")
tucson_full <- get_googlemap(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat), zoom=14, maptype="roadmap", source="google")
ggmap(tucson_full) + geom_point(data=corridor, aes(x = lon, y = lat, color = activity_category), alpha = .5)

#type over time
mosaicplot(as.factor(format(corridor$Accept_Date, "%Y"))~corridor$activity_category, las=2)

#interesting categories over time
c_interesting <- subset(corridor, Activity_Code_1 %in% interesting_activities)
nc_interesting <- subset(non_corridor, Activity_Code_1 %in% interesting_activities)
c_interesting$activity_category <- factor(c_interesting$activity_category)
nc_interesting$activity_category <- factor(nc_interesting$activity_category)

mosaicplot(as.factor(format(c_interesting$Accept_Date, "%Y"))~c_interesting$activity_category, las=1)

#############################################
# 			MEAN WAIT TIMES					#
#############################################

#all
mean(corridor$start_to_approved)
mean(non_corridor$start_to_approved)

#interesting
mean(corridor$start_to_approved)
mean(non_corridor$start_to_approved)

#############################################
# 			GROWTH RATE						#
#############################################

#volume over time
plot(as.factor(format(corridor$Accept_Date, "%Y")))
plot(as.factor(format(non_corridor$Accept_Date, "%Y")))

plot(as.factor(format(c_interesting$Accept_Date, "%Y")))
plot(as.factor(format(nc_interesting$Accept_Date, "%Y")))

# Only interesting categories, by year.
nc_table <- table(as.factor(format(nc_interesting$Accept_Date, "%Y")))
c_table <- table(as.factor(format(c_interesting$Accept_Date, "%Y")))

# http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/
plot(nc_table, type="l", col="blue")
par(new=TRUE)
plot(c_table, type="l", col="red", xaxt="n", yaxt="n", xlab="", ylab="")
line(c_table)
axis(4)

# Just interesting categories by month (mostly noise)
nc_table <- table(as.factor(format(nc_interesting$Accept_Date, "%Y-%m")))
c_table <- table(as.factor(format(c_interesting$Accept_Date, "%Y-%m")))

plot(nc_table, type="l", col="blue")
par(new=TRUE)
plot(c_table, type="l", col="red", xaxt="n", yaxt="n", xlab="", ylab="")
line(c_table)
axis(4)

# All categories by year
nc_table <- table(as.factor(format(non_corridor$Accept_Date, "%Y")))
c_table <- table(as.factor(format(corridor$Accept_Date, "%Y")))

plot(nc_table, type="l", col="blue")
par(new=TRUE)
plot(c_table, type="l", col="red", xaxt="n", yaxt="n", xlab="", ylab="")
line(c_table)
axis(4)

# Interestingly, here we see some growth in the corridor compared to outside in 2013.
# What industries will those be?

# NAICS categories won't really help--the data are even sparser.


#############################################
# 			Categories over time				#
#############################################
corridor$activity_category <- factor(corridor$activity_category)
non_corridor$activity_category <- factor(non_corridor$activity_category)

c_cats <- table(corridor$activity_category, format(corridor$Accept_Date, "%Y"))
nc_cats <- table(non_corridor$activity_category, format(non_corridor$Accept_Date, "%Y"))

library("reshape")
ggplot(data=melt(c_cats, varnames=c("activity", "year")), aes(x=year, y=value, color=activity)) + geom_line()
ggplot(data=melt(nc_cats, varnames=c("activity", "year")), aes(x=year, y=value, color=activity)) + geom_line()

## We can also try year-to-year changes
c_cats_per <- c_cats
for (i in 1:nrow(c_cats)){
	for (j in 1:ncol(c_cats)){
		if (j==1){
			c_cats_per[i,j] <- 0
		} else {
			c_cats_per[i,j] <- (c_cats[i,j] - c_cats[i,j-1])
		}
	}
}

nc_cats_per <- nc_cats
for (i in 1:nrow(nc_cats)){
	for (j in 1:ncol(nc_cats)){
		if (j==1){
			nc_cats_per[i,j] <- 0
		} else {
			nc_cats_per[i,j] <- (nc_cats[i,j] - nc_cats[i,j-1])
		}
	}
}

ggplot(data=melt(c_cats_per[,-5], varnames=c("activity", "year")), aes(x=year, y=value, color=activity)) + geom_line()
ggplot(data=melt(nc_cats_per[,-5], varnames=c("activity", "year")), aes(x=year, y=value, color=activity)) + geom_line()

## Attempt SVG export
g <- ggplot(data=melt(c_cats, varnames=c("activity", "year")), aes(x=year, y=value, color=activity)) + geom_line()
ggsave(plot=g, "test.svg")

library("RSvgDevice")

plot_chart <- function(tab, cat) {
	if (max(tab$value) > 10) {
		bks = c(0,5,10,15,20)
		lims = c(0,20)
	} else if (max(tab$value) > 1) {
		bks = c(0,5,10)
		lims = c(0,10)
	} else {
		bks = 0:2
		lims = c(0,2)
	}
	g <- ggplot(tab, aes(x=year, y=as.integer(value))) + geom_line(color="#B13F00", size=1.25) + labs(x=element_blank(), y=element_blank()) + scale_y_continuous(limits=lims, breaks=bks)
	s <- style()
	return(g+s)
}

style <- function(){
	s <- theme_bw() + theme(panel.border = element_blank(), panel.grid.major.y=element_line(color="gray", size=1), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), axis.line=element_blank(), axis.title=element_blank(), axis.ticks.y=element_blank())
	return(s)
}

# This function takes a table with category rows and year 
svgify <- function(table, directory=""){
	names <- row.names(table)
	for (i in 1:nrow(table)){
		cat <- names[i]
		tab <- melt(table[i,])
		tab <- cbind("year"=row.names(tab), tab)
		row.names(tab) <- NULL
		tab$year <- as.integer(as.character(tab$year))
		g <- plot_chart(tab, cat)
		devSVG(paste(directory,"/",cat,".svg", sep=""), height=6, width=9)
		plot(g)
		dev.off()
	}
}

svgify(c_cats[,-5], "c_charts")