setwd('C:/Users/Danley/Documents/PSU/IST 557 - Data Mining/Final Project') 	# set the working directory
.libPaths("C:/Users/Danley/Documents/R/win-library/3.1")  # set personal library directory

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

# Calculates azimuth towards point two from point one specified by radian latitude/longitude and distance
azm.slc <- function(long1, lat1, long2, lat2) {
#  b <- acos( cos(90.0 - lat2) * cos (90.0 - lat1) + sin(90.0 - lat2) * sin (90.0 - lat1) * cos(long2 - long1) )
#  azm <- asin( sin(90.0 - lat2) * sin(long2 - long1) / sin(b) )
   y <- cos(lat2)*sin(long2-long1)
   x <- cos(lat1)*sin(lat2) - sin(lat1)*cos(lat2)*cos(long2-long1)
   return(atan2(y, x) %% (2*pi))
}

# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180.0)

# Convert radians to degrees
rad2deg <- function(rad) return(rad*180.0/pi)

car_file <- c("point1.csv", "point2.csv", "point3.csv", "point4.csv")
car_lat <- {}
car_long <- {}
car_emis <- {}

for (i in 1:length(car_file))
{
tmp_loc <- read.csv(car_file[i], header=TRUE)
car_lat <- c(car_lat, tmp_loc$Lat[length(tmp_loc$FID)/2])
car_long <- c(car_long, tmp_loc$Long[length(tmp_loc$FID)/2])
car_emis <- c(car_emis, median(tmp_loc$CH4_CCPPB))
}

conv_df <- read.csv("Conventional_Emissions.csv", header=TRUE)
uncv_df <- read.csv("Unconventional_Emissions.csv", header=TRUE)
coal_df <- read.csv("coal.csv", header=TRUE)
lndf_df <- read.csv("landfills.csv", header=TRUE)
inds_df <- read.csv("other_IndustryType.csv", header=TRUE)

for (i in 1:length(car_emis))
{
tmp_d <- gcd.slc(deg2rad(rep_len(car_long[i], length(conv_df$Long))), deg2rad(rep_len(car_lat[i], length(conv_df$Lat))),
  deg2rad(conv_df$Long), deg2rad(conv_df$Lat))
tmp_a <- rad2deg(azm.slc(deg2rad(rep_len(car_long[i], length(conv_df$Long))), deg2rad(rep_len(car_lat[i], length(conv_df$Lat))),
  deg2rad(conv_df$Long), deg2rad(conv_df$Lat)))
tmp_name <- paste("car_",i, sep="")
conv_df[,paste(tmp_name,"_dist",sep="")] = tmp_d
conv_df[,paste(tmp_name,"_azm",sep="")] = tmp_a

tmp_d <- gcd.slc(deg2rad(rep_len(car_long[i], length(uncv_df$long))), deg2rad(rep_len(car_lat[i], length(uncv_df$lat))),
  deg2rad(uncv_df$long), deg2rad(uncv_df$lat))
tmp_a <- rad2deg(azm.slc(deg2rad(rep_len(car_long[i], length(uncv_df$long))), deg2rad(rep_len(car_lat[i], length(uncv_df$lat))),
  deg2rad(uncv_df$long), deg2rad(uncv_df$lat)))
tmp_name <- paste("car_",i, sep="")
uncv_df[,paste(tmp_name,"_dist",sep="")] = tmp_d
uncv_df[,paste(tmp_name,"_azm",sep="")] = tmp_a

tmp_d <- gcd.slc(deg2rad(rep_len(car_long[i], length(coal_df$Longitude))), deg2rad(rep_len(car_lat[i], length(coal_df$Latitude))),
  deg2rad(coal_df$Longitude), deg2rad(coal_df$Latitude))
tmp_a <- rad2deg(azm.slc(deg2rad(rep_len(car_long[i], length(coal_df$Longitude))), deg2rad(rep_len(car_lat[i], length(coal_df$Latitude))),
  deg2rad(coal_df$Longitude), deg2rad(coal_df$Latitude)))
tmp_name <- paste("car_",i, sep="")
coal_df[,paste(tmp_name,"_dist",sep="")] = tmp_d
coal_df[,paste(tmp_name,"_azm",sep="")] = tmp_a

tmp_d <- gcd.slc(deg2rad(rep_len(car_long[i], length(lndf_df$Longitude))), deg2rad(rep_len(car_lat[i], length(lndf_df$Latitude))),
  deg2rad(lndf_df$Longitude), deg2rad(lndf_df$Latitude))
tmp_a <- rad2deg(azm.slc(deg2rad(rep_len(car_long[i], length(lndf_df$Longitude))), deg2rad(rep_len(car_lat[i], length(lndf_df$Latitude))),
  deg2rad(lndf_df$Longitude), deg2rad(lndf_df$Latitude)))
tmp_name <- paste("car_",i, sep="")
lndf_df[,paste(tmp_name,"_dist",sep="")] = tmp_d
lndf_df[,paste(tmp_name,"_azm",sep="")] = tmp_a

tmp_d <- gcd.slc(deg2rad(rep_len(car_long[i], length(inds_df$Longitude))), deg2rad(rep_len(car_lat[i], length(inds_df$Latitude))),
  deg2rad(inds_df$Longitude), deg2rad(inds_df$Latitude))
tmp_a <- rad2deg(azm.slc(deg2rad(rep_len(car_long[i], length(inds_df$Longitude))), deg2rad(rep_len(car_lat[i], length(inds_df$Latitude))),
  deg2rad(inds_df$Longitude), deg2rad(inds_df$Latitude)))
tmp_name <- paste("car_",i, sep="")
inds_df[,paste(tmp_name,"_dist",sep="")] = tmp_d
inds_df[,paste(tmp_name,"_azm",sep="")] = tmp_a
}

