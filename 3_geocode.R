library(ggmap)

data <- read.table("data.txt", sep="\t", header=TRUE)
activitytypes <- read.table("reference_data/activity-codes.txt", sep="\t", header=TRUE, quote="\"")
licensetype <- read.table("reference_data/license-type.txt", sep="\t", header=TRUE, quote="\"")
ownertype <- read.table("reference_data/owner-type.txt", sep="\t", header=TRUE, quote="\"")

data$Accept_Date <- as.Date(data$Accept_Date)
data$Start_Date <- as.Date(data$Start_Date)

data$License_Type <- as.factor(data$License_Type)

data$Owner_Type <- as.factor(data$Owner_Type)
data$Activity_Code_1 <- as.factor(data$Activity_Code_1)
data$Activity_Code_2 <- as.factor(data$Activity_Code_2)
data$Activity_Code_3 <- as.factor(data$Activity_Code_3)

data$start_to_approved <- data$Accept_Date - data$Start_Date

data$lon <- NA
data$lat <- NA

# Modified from http://stackoverflow.com/questions/13673894/suppress-nas-in-paste
# We need NAs that are pasted to be empty, not NA
paste2 <- function(...,sep=" ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}

for (i in 1:nrow(data)) {
  tryCatch({
    loc <- NA
    if (is.na(data$lon[i]) || is.na(data$lat[i])){
      address <- paste2(as.character(data$Street_Number[i]), as.character(data$Street_Direction[i]),
                        as.character(data$Street_Name[i]), as.character(data$Street_Suffix[i]),
                        as.character(data$Suite.Apt_No.[i]), as.character(data$City[i]),
                        as.character(data$State[i]), as.character(data$Zip_Code[i]))
      loc <- geocode(address, override_limit=TRUE)
      data$lon[i] <- loc[[1]]
      data$lat[i] <- loc[[2]]
      print(i)
      print(loc)}
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

count = 0
for (i in 12200:nrow(data)) {
  if (is.na(data$lon[i])){
    print(i)
    count = count + 1
    if (count > 100) { break }
  }
}

lon_backup <- data$lon
data$lon <- as.numeric(lon_backup)

write.table(data, "with_location_info.txt", sep="\t")

stations <- read.table("sunlink-stops.txt", sep=",", header=TRUE)

data <- read.table("with_location_info.txt", sep="\t")

data <- read.table("with_location_info.txt", header=TRUE, sep="\t")

# Eliminate duplicate rows
dup_indices <- c()
for (i in 2:nrow(data)){
  if (data[i,1] == data[i-1,1] && data[i,2] == data[i-1,2] 
      && data[i,3] == data[i-1,3] && data[i,4] == data[i-1,4]){
    dup_indices <- append(dup_indices, i-1)
  }
}

data <- data[-dup_indices,]

# Create the box
indices <- c()
for (i in 1:nrow(data)){
  if (i%%10 == 0) {print(i)}
  if (!is.na(data$lon[i]) && !is.na(data$lat[i])
      && (data$lon[i] > -111.0337624) && (data$lon[i] < -110.7415947)
      && (data$lat[i] > 32.0810678) && (data$lat[i] < 32.293393)){
    indices <- append(indices, i)
  }
}

tucson_area <- data[indices,]

write.table(data, "tucson_area.tsv", sep="\t", row.names=FALSE)