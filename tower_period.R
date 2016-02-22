# Set up R session
setwd('C:/Users/Danley/Documents/PSU/IST 557 - Data Mining/Final Project') 	# set the working directory
.libPaths("C:/Users/Danley/Documents/R/win-library/3.1")  # set personal library directory

towdata_path <- "./Yanni Data/tower/east"
file_list <- list.files(path=towdata_path, pattern="\\.csv$")

day <- vector()
hour <- vector()
minsec <- vector()
tow_emis <- vector()

for (i in 1:length(file_list))
{
in_file <- paste(towdata_path,'/',file_list[i], sep="")
tow_in <- read.csv(file=in_file,head=TRUE,sep=",")
day_vec <- rep_len(as.integer(substr(file_list[i], 16, 17)), length(tow_in[,1]))
day <- c(day, day_vec)
hour <- c(hour, as.integer(substr(tow_in$time, 1,2)))
minsec <- c(minsec, as.integer(substr(tow_in$time, 4,5))*60+as.numeric(substr(tow_in$time, 7,12)))
tow_emis <- c(tow_emis, tow_in$ch4)
}

day <- day[-which(is.na(tow_emis))]
hour <- hour[-which(is.na(tow_emis))]
minsec <- minsec[-which(is.na(tow_emis))]
tow_emis <- tow_emis[-which(is.na(tow_emis))]
tow_time <- day*24.0+hour+minsec/3600.0

rang_ind <- 800000:1600000
plot(tow_time[rang_ind], tow_emis[rang_ind], type='l')

tow_fit <- smooth.spline(tow_time[rang_ind], tow_emis[rang_ind])
inp_pred <- seq(550,650,0.1)
out_pred <- predict(tow_fit, inp_pred)$y
pacf(out_pred)
plot(inp_pred, out_pred, type='l')
abline(v=c(565, 565+24, 565+48, 565+72), col="red")
# 1PM on Saturday (5/23) -> Sunday (5/24) -> Monday (5/25) -> Tuesday (5/26)
























# EXTRAS------------------------------------------------------------

pred_vec <- vector()
e1 <- vector()
e2 <- vector()
e3 <- vector()
e4 <- vector()
e5 <- vector()
e6 <- vector()

for (d in 15:20) {
for (h in 5:20) {
pred_day = d
pred_hr = h
pred_minsec = 1500.00

near_ind = which((abs(day-pred_day)+abs(hour-pred_hr)+abs(minsec-pred_minsec))==min(abs(day-pred_day)+abs(hour-pred_hr)+abs(minsec-pred_minsec)))
if (pred_hr == 0) {
prev_hr <- 23
} else {
prev_hr <- pred_hr-1 }
if (pred_hr == 23) {
next_hr <- 0
} else {
next_hr <- pred_hr+1 }

pred_vec <- c(pred_vec, tow_emis[near_ind])
#e1 <- c(e1, tow_emis[near_ind-1])
#e2 <- c(e2, tow_emis[near_ind-2])
#e3 <- c(e3, tow_emis[near_ind-3])
#e6 <- c(e6, mean(tow_emis[(near_ind-100):(near_ind-1)]))

e1 <- c(e1, mean(tow_emis[which(hour==pred_hr & day < pred_day)]))
e2 <- c(e2, mean(tow_emis[which(hour==prev_hr & day < pred_day)]))
e3 <- c(e3, mean(tow_emis[which(hour==next_hr & day < pred_day)]))
e4 <- c(e4, mean(tow_emis[which(hour==pred_hr & day == (pred_day-1))]))
e5 <- c(e5, mean(tow_emis[which(hour==prev_hr & day == (pred_day-1))]))
e6 <- c(e6, mean(tow_emis[which(hour==next_hr & day == (pred_day-1))]))
}}

fit <- lm(pred_vec ~ e1 + e2 + e3 + e4 + e5 + e6)
summary(fit)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

rang_ind <- 1692800:1693100
plot(day[rang_ind]*24*3600+hour[rang_ind]*3600+minsec[rang_ind], tow_emis[rang_ind])

time_tot <- day*24*3600 + hour*3600 + minsec




