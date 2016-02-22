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

# READ IN CAR DATA--------------------------------------------------------------------------------------

num_points = 50

#car_file <- c("point1.csv", "point2.csv", "point3.csv", "point4.csv")
#car_time <- {}
#car_lat <- {}
#car_long <- {}
#car_emis <- {}

#for (i in 1:length(car_file))
#{
#tmp_loc <- read.csv(car_file[i], header=TRUE)
#car_time <- c(car_time, (tmp_loc$Date[length(tmp_loc$FID)/2]*24.0)+tmp_loc$Time[length(tmp_loc$FID)/2])
#car_lat <- c(car_lat, tmp_loc$Lat[length(tmp_loc$FID)/2])
#car_long <- c(car_long, tmp_loc$Long[length(tmp_loc$FID)/2])
#car_emis <- c(car_emis, median(tmp_loc$CH4_CCPPB))
#}

car_file <- "./Yanni Data/car/Car Data 2015/May132015.txt"

car_data <- read.table(car_file, sep="\t", header=FALSE)
car_time_full <- car_data[,3]*24.0+car_data[,4]+car_data[,5]/60.0+car_data[,6]/3600.0
car_lat_full <- car_data[,7]
car_long_full <- car_data[,8]
car_emis_full <- car_data[,9]*1000.0

sel_ind <- sample(1:length(car_data[,1]), num_points)

car_time <- car_time_full[sel_ind]
car_lat <- car_lat_full[sel_ind]
car_long <- car_long_full[sel_ind]
car_emis <- car_emis_full[sel_ind]

# READ IN TOWER DATA------------------------------------------------------------

towdata_path <- "./Yanni Data/tower/east"
#file_list <- list.files(path=towdata_path, pattern="\\.csv$")
file_list <- "FCDS2047-20150513_level0.csv"

tow_time <- vector()
tow_emis <- vector()

for (i in 1:length(file_list))
{
in_file <- paste(towdata_path,'/',file_list[i], sep="")
tow_in <- read.csv(file=in_file,head=TRUE,sep=",")
tow_day <- rep_len(as.integer(substr(file_list[i], 16, 17)), length(tow_in[,1]))
tow_hour <- as.integer(substr(tow_in$time, 1,2))
tow_minsec <- as.integer(substr(tow_in$time, 4,5))*60+as.numeric(substr(tow_in$time, 7,12))
tow_time <- c(tow_time, (tow_day*24.0)+(tow_hour)+(tow_minsec/3600.0))
tow_emis <- c(tow_emis, tow_in$ch4)
}

# NORMALIZE CAR EMISSION---------------------------------------------------------------------------

tow_fit <- smooth.spline(tow_time, tow_emis)
tow_out <- predict(tow_fit, car_time)$y
car_emis <- car_emis-tow_out

# READ IN EMISSION SOURCES AND CALCULATE DISTANCES/AZIMUTHS------------------------------------------

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

# CLASSIFY SOURCES BASED ON RADIAL DISTANCE AND AZIMUTH---------------------------------------

rad1 <- 50.0
rad2 <- 100.0
rad3 <- 200.0

src_list <- list(conv_df, uncv_df, coal_df, lndf_df, inds_df)

for (i in 1:length(src_list))
{
  tmp_df <- data.frame(src_list[i])
  for (k in 1:length(car_emis))
  {
    name_dist <- paste("car_",k,"_dist",sep="")
    name_azm <- paste("car_",k,"_azm",sep="")
    name_clsd <- paste("car_",k,"_clsd",sep="")
    name_clsa <- paste("car_",k,"_clsa",sep="")
    name_cls <- paste("car_",k,sep="")

    tmp_df[which(tmp_df[[name_dist]] < rad1), name_clsd] <- as.numeric(1)
    tmp_df[which(tmp_df[[name_dist]] > rad1 & tmp_df[[name_dist]] < rad2), name_clsd] <- as.numeric(2)
    tmp_df[which(tmp_df[[name_dist]] > rad2 & tmp_df[[name_dist]] < rad3), name_clsd] <- as.numeric(3)
    tmp_df[which(tmp_df[[name_dist]] > rad3), name_clsd] <- as.numeric(0)

    tmp_df[which(tmp_df[[name_azm]] < 45.0 | tmp_df[[name_azm]] > 315.0),name_clsa] <- "n"
    tmp_df[which(tmp_df[[name_azm]] > 45.0 & tmp_df[[name_azm]] < 135.0),name_clsa] <- "e"
    tmp_df[which(tmp_df[[name_azm]] > 135.0 & tmp_df[[name_azm]] < 225.0),name_clsa] <- "s"
    tmp_df[which(tmp_df[[name_azm]] > 225.0 & tmp_df[[name_azm]] < 315.0),name_clsa] <- "w"

    tmp_df[[name_dist]] <- NULL
    tmp_df[[name_azm]] <- NULL
  }

src_list[[i]] <- tmp_df
}

#write.csv(data.frame(src_list[1]), file="conv_cls.csv")
#write.csv(data.frame(src_list[2]), file="unconv_cls.csv")
#write.csv(data.frame(src_list[3]), file="coal_cls.csv")
#write.csv(data.frame(src_list[4]), file="landfill_cls.csv")
#write.csv(data.frame(src_list[5]), file="otherind_cls.csv")

for (i in 1:length(src_list))
{
  tmp_df <- data.frame(src_list[i])
  flg_list <- rep_len(0, length(tmp_df[,1]))
  for (k in 1:length(car_emis))
  {
    name_clsd <- paste("car_",k,"_clsd",sep="")
    flg_list <- flg_list + tmp_df[[name_clsd]]     
  }
src_list[[i]] <- tmp_df[which(flg_list > 0),]
}

sel_conv <- data.frame(src_list[1])
sel_unconv <- data.frame(src_list[2])
sel_coal <- data.frame(src_list[3])
sel_landfill <- data.frame(src_list[4])
sel_otherind <- data.frame(src_list[5])

cls_types <- c("1n","1e","1s","1w","2n","2e","2s","2w","3n","3e","3s","3w")

mod_inp <- array(NA, c(length(car_emis), 5*length(cls_types)))

for (k in 1:length(car_emis))
{
  name_clsd <- paste("car_",k,"_clsd",sep="")
  name_clsa <- paste("car_",k,"_clsa",sep="")
  for (j in 1:length(cls_types))
  {
    mod_inp[k, 0*length(cls_types)+j] = sum(sel_conv$Emissions_mol.hour[which(paste(sel_conv[[name_clsd]], sel_conv[[name_clsa]], sep="") == cls_types[j])])
  }
}

for (k in 1:length(car_emis))
{
  name_clsd <- paste("car_",k,"_clsd",sep="")
  name_clsa <- paste("car_",k,"_clsa",sep="")
  for (j in 1:length(cls_types))
  {
    mod_inp[k, 1*length(cls_types)+j] = sum(sel_unconv$emissions_mol.hour[which(paste(sel_unconv[[name_clsd]], sel_unconv[[name_clsa]], sep="") == cls_types[j])])
  }
}

for (k in 1:length(car_emis))
{
  name_clsd <- paste("car_",k,"_clsd",sep="")
  name_clsa <- paste("car_",k,"_clsa",sep="")
  for (j in 1:length(cls_types))
  {
    mod_inp[k, 2*length(cls_types)+j] = sum(sel_coal$CH4_em_molh[which(paste(sel_coal[[name_clsd]], sel_coal[[name_clsa]], sep="") == cls_types[j])])
  }
}

for (k in 1:length(car_emis))
{
  name_clsd <- paste("car_",k,"_clsd",sep="")
  name_clsa <- paste("car_",k,"_clsa",sep="")
  for (j in 1:length(cls_types))
  {
    mod_inp[k, 3*length(cls_types)+j] = sum(sel_landfill$CH4_em_molh[which(paste(sel_landfill[[name_clsd]], sel_landfill[[name_clsa]], sep="") == cls_types[j])])
  }
}

for (k in 1:length(car_emis))
{
  name_clsd <- paste("car_",k,"_clsd",sep="")
  name_clsa <- paste("car_",k,"_clsa",sep="")
  for (j in 1:length(cls_types))
  {
    mod_inp[k, 4*length(cls_types)+j] = sum(sel_otherind$CH4_em_molh[which(paste(sel_otherind[[name_clsd]], sel_otherind[[name_clsa]], sep="") == cls_types[j])])
  }
}

mod_inp <- data.frame(mod_inp)

mod_inp_names <- list()
for (i in 1:length(cls_types))
{
  mod_inp_names <- c(mod_inp_names, paste("conv_", cls_types[i], sep=""))
}
for (i in 1:length(cls_types))
{
  mod_inp_names <- c(mod_inp_names, paste("unconv_", cls_types[i], sep=""))
}
for (i in 1:length(cls_types))
{
  mod_inp_names <- c(mod_inp_names, paste("coal_", cls_types[i], sep=""))
}
for (i in 1:length(cls_types))
{
  mod_inp_names <- c(mod_inp_names, paste("landfill_", cls_types[i], sep=""))
}
for (i in 1:length(cls_types))
{
  mod_inp_names <- c(mod_inp_names, paste("otherind_", cls_types[i], sep=""))
}

colnames(mod_inp) <- mod_inp_names

mod_inp[["car_emis"]] <- car_emis
#mod_inp[["car_time"]] <- car_time

# MODEL FITTING-----------------------------------------------------------------

train_ind <- 1:40
train <- mod_inp[train_ind,]
test <- mod_inp[-train_ind,]

lin_fit <- lm(car_emis ~ ., data=train)
summary(lin_fit)
fit_out <- predict(lin_fit, test)
mean(abs((test$car_emis-fit_out)/test$car_emis))
sd(abs((test$car_emis-fit_out)/test$car_emis))/sqrt(length(fit_out))

library(rpart)
regtree_fit <- rpart(car_emis ~ ., method="anova", data=train)
summary(regtree_fit)
printcp(regtree_fit)
post(regtree_fit, file="regtree_plot.eps")
fit_out <- predict(regtree_fit, test)
mean(abs((test$car_emis-fit_out)/test$car_emis))
sd(abs((test$car_emis-fit_out)/test$car_emis))/sqrt(length(fit_out))

library(neuralnet)
n <- names(train)
f <- as.formula(paste("car_emis ~", paste(n[!n %in% "car_emis"], collapse = " + ")))
acc_list = list()
for (i in 2:10)
{
nn <- neuralnet(f,data=train,hidden=c(5,i),linear.output=T)
if (i == 3)
{
plot(nn)
dev.copy2eps(file='nn_plot.eps')
dev.off()
}
fit_out <- compute(nn, test[,1:(length(test[1,])-1)])
acc_list <- c(acc_list, mean(abs((test$car_emis-fit_out$net.result)/test$car_emis)))
#sd(abs((test$car_emis-fit_out$net.result)/test$car_emis))/sqrt(length(fit_out))
}
plot(2:10, acc_list, type='o', xlab='k', ylab='Relative Error')
dev.copy2eps(file='nn_plot_acc.eps')
dev.off()


library(FNN)
acc_list = list()
for (i in 2:10)
{
knn_fit <- knn.reg(train, test, k=i, car_emis)
acc_list <- c(acc_list, mean(abs((test$car_emis-knn_fit$pred)/test$car_emis)))
#sd(abs((test$car_emis-knn_fit$pred)/test$car_emis))/sqrt(length(test$car_emis))
}
plot(2:10, acc_list, type='o', xlab='k', ylab='Relative Error')
dev.copy2eps(file='knn_plot.eps)
dev.off()