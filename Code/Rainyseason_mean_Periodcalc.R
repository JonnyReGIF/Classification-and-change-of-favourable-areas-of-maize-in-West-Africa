################################################################
#### Script to derive different effect on maize cultivation ####
################################################################


## required packages
library(terra)
library(tmap)
library(dplyr)
library(lubridate)
library(PCICt)
library(parallel)
library(doParallel)
memory.limit(size=56000)


#################################
#### Variables to be defined ####
#################################


## set working directory
setwd("~/Meine Dokumente/6_Studium_Halle/01_Module/13_LSS_II/")

## path to the folder to store the data in
path <- "~/Meine Dokumente/6_Studium_Halle/01_Module/13_LSS_II/02_Daten/Perioden_Data/Mean_RS_2061_2080"

## first day of data stack (yyyy-mm-dd)
start_data <- "2021-01-01"

## last day of data stack (yyyy-mm-dd)
end_data <- "2100-12-31"

## first day of the period to be calculated (yyyy-mm-dd)
start_period <- "2061-01-01"

## last day of the period to be calculated (yyyy-mm-dd)
end_period <- "2080-12-31"


##############################################################################################################


#################
#### Content ####
#################

# PART I:   Preprocessing Cordex Data 
# PART II:  Calculation Start of Rainy Season and Growing Season 
# PART III: Subset the data to Growing Season
# PART IV: Calculation of mean of defined periods
# PART V: Identification and Classification of Temperature/Heat Days threshold values during defined periods 
# PART VI: Calculation and Classification of Precipitation total during defined periods 
# PART VII: Calculation and Classification of Dry Spells during defined periods
# PART VIII:  Final Classification to evaluate possible Maize Cultivation areas


##############################################################################################################


###########################################
#### PART I: Preprocessing Cordex Data ####
###########################################

## NOTE: 
# we are using following Data: Precipitation, Mean Temperature, Max Temperature (from 2021 till 2100) (CORDEX, daily resolution, rcp 45, Africa 22x22, CCCMA institute)
# preproecessing means: cropping the Data to the extent of the Area of interest, resampling the data, mask the pixels where no maize is cultivated right now


## load or access necessary data
files_Tmean <- list.files(paste0(getwd(), "/02_Daten/CordexData/Temp_Mean")) #lists mean Temperature nc-file names
files_P <- list.files(paste0(getwd(), "/02_Daten/CordexData/Precipitation")) #lists Precipitation nc-file names
files_Tmax <- list.files(paste0(getwd(), "/02_Daten/CordexData/Temp_Max")) #lists max Temperature nc-file names
UG <- vect("./02_Daten/Extent_FC/Extent_FC.shp") # shapefile --> area of interest
maize <- rast("./02_Daten/Maize_hektar_pro_pixel.tif") # raster image of reasoned areas of maize cultivation after Monfreda et al. 2008 


## due run time issues when single functions were applied to the hole raster stack (Data from 2021 til 2100) we applied a loop running through each 5-years stack individually

## loop to preprocess Mean Temperature Data
tmean_v <- c(paste0("r_Tmean_", 1:length(files_Tmean)))  # defines a vector to name the raster stacks

for (i in 1:length(files_Tmean)) {
  r <- rast(c(paste0(getwd(), "/02_Daten/CordexData/Temp_Mean/", files_Tmean[i]))) # loads first data set
  # renames layers: new names = dates
  crs(r) <- crs(UG) # defines crs 
  c <- crop(r, ext(UG)) # crops the data to the extent of the area of interest
  t <- c - 273.15 # the temperature units are in Kelvin (K)  --> converting them into degree Celcius 
  m <- terra::resample(maize, t) # resamples pixelsize of the "maize" data (Monfreda)
  e <- terra::mask(t, m, maskvalues = 0, updatevalue = NA) # masks the data, so all pixels where no maize is cultivated gets set to "NA"
  assign(n[i], e) # preprocesed dataset gets assigned to a unique name 
}


## loop to preprocess Max Temperature Data
tmax_v <- c(paste0("r_Tmax_", 1:length(files_Tmax))) # defines a vector to name the raster stacks

for (i in 1:length(files_Tmax)) {
  r <- rast(c(paste0(getwd(), "/02_Daten/CordexData/Temp_Max/", files_Tmax[i]))) # loads first data set
  crs(r) <- crs(UG) # defines crs
  c <- crop(r, ext(UG)) # crops the data to the extent of the area of interest
  t <- c - 273.15 # the temperature units are in Kelvin (K)  --> converting them into degree Celcius 
  m <- terra::resample(maize, t) # resamples pixelsize of the "maize" data (Monfreda)
  e <- terra::mask(t, m, maskvalues = 0, updatevalue = NA) # masks the data, so all pixels where no maize is cultivated gets set to "NA"
  assign(n[i], e) # preprocesed dataset gets assigned to a unique name 
}


## loop to preprocess Precipitation Data
prec_v <- c(paste0("r_P_", 1:length(files_P))) # defines a vector to name the raster stacks

for (i in 1:length(files_P)) { 
  r <- rast(c(paste0(getwd(), "/02_Daten/CordexData/Precipitation/", files_P[i]))) # loads first data set
  crs(r) <- crs(UG) # defines crs 
  c <- crop(r, ext(UG))  # crops the data to the extent of the area of interest
  ## since the units of the data is in 	kg m-2 s-1 we need to convert them into something more imaginable than that eg mm/day
  ## to convert from mass to volume we need the density. But we can assume that (for reasonable temperatures) that 1L of water 
  ## weighs 1kg. And also 1L of water spread over 1m^2 is 1mm deep. So by that we got from kg m-2 s-1 to mm/s.
  ## one day has 86400 seconds. So we can multiply the raster units with that number. assuming that it rains equally during the day. 
  t <- c * 86400
  m <- terra::resample(maize, t) # resamples pixelsize of the "maize" data (Monfreda)
  e <- terra::mask(t, m, maskvalues = 0, updatevalue = NA)  # masks the data, so all pixels where no maize is cultivated gets set to "NA"
  assign(n[i], e) # preprocesed dataset gets assigned to a unique name 
}


## stacking preprocessed Data
T_mean <- rast(mget(as.vector(tmean_v)))
T_max <- rast(mget(as.vector(tmax_v)))
prec <- rast(mget(as.vector(prec_v)))

## change layernames to dates
names(ras_Tmean) <- seq(as.PCICt(start_data, cal = "365_day"), as.PCICt(end_data, cal = "365_day"), by = "day")
names(ras_Tmax) <- seq(as.PCICt(start_data, cal = "365_day"), as.PCICt(end_data, cal = "365_day"), by = "day")
names(ras_P) <- seq(as.PCICt(start_data, cal = "365_day"), as.PCICt(end_data, cal = "365_day"), by = "day")


## saving Data as nc-file
writeCDF(ras_Tmean,"./02_Daten/Temp_Mean_2021_2100.nc", overwrite = TRUE)
writeCDF(ras_Tmax,"./02_Daten/Temp_Max_2021_2100.nc", overwrite = TRUE)
writeCDF(ras_P,"./02_Daten/Precipitation_2021_2100.nc", overwrite = TRUE)

##############################################################################################################


## load required data
prec <- rast("./02_Daten/Yearly_Data_2021_2100/Precipitation_2021_2100.grd")
names(prec) <- seq(as.PCICt(start_data, cal = "365_day"), as.PCICt(end_data, cal = "365_day"), by = "day")
prec <- terra::subset(prec, as.character(seq(as.PCICt(start_period, cal = "365_day"), as.PCICt(end_period, cal = "365_day"), by = "day"))) 


T_max <- rast("./02_Daten/Yearly_Data_2021_2100/Temp_Max_2021_2100.grd")
names(T_max) <- seq(as.PCICt(start_data, cal = "365_day"), as.PCICt(end_data, cal = "365_day"), by = "day")
T_max <- terra::subset(T_max, as.character(seq(as.PCICt(start_period, cal = "365_day"), as.PCICt(end_period, cal = "365_day"), by = "day"))) 

T_mean <- rast("./02_Daten/Yearly_Data_2021_2100/Temp_Mean_2021_2100.grd")
names(T_mean) <- seq(as.PCICt(start_data, cal = "365_day"), as.PCICt(end_data, cal = "365_day"), by = "day")
T_mean <- terra::subset(T_mean, as.character(seq(as.PCICt(start_period, cal = "365_day"), as.PCICt(end_period, cal = "365_day"), by = "day")))



#########################################################################
#### PART II: Calculation Start of Rainy Season and Growing Season ######
#########################################################################

## Note:
# Calculation of rainy season onset according to Dunning et al. 2016


## in case not loaded, load necessary Precipitation data and rename layer with the date
# prec <- terra::rast("./02_Daten/Yearly_Data_2021_2100/Precipitation_2021_2100.grd")


## creating empty raster to store output of the following loops in it
prec_q_quer <- rast(extent = ext(prec), resolution = res(prec), crs = crs(prec))            # Q_quer = Mean precipitation of one year for each pixel
precipitation_anomaly <- rast(extent = ext(prec), resolution = res(prec), crs = crs(prec))  # daily precipitation anomaly data for one year
storage_cumulative <- rast(extent = ext(prec), resolution = res(prec), crs = crs(prec))     # cumulative sum of the precipitation anomaly for each year
table_start_end_day <- as.data.frame(prec, xy = TRUE)[, 1:2]                                # extract coordinates of center points for each pixel and puts them into a dataframe


## create additional data sets/sequences etc.
# two sequences to indicate the layer who are representing the first/last day for each year
l <- seq(1, nlyr(prec), 365)
m <- seq(365, nlyr(prec), 365)

  
# sequence that indicates the layer that represents the first/last day of the static rainy season of each year (required to subset dataset)
# "60" represents the day of the year we are postulating to be the earliest the growing season could start 
# "212" represents the day of the year we are postulating to be the latest the growing season could start
l2 <- seq(60, nlyr(prec), 365) 
m2 <- seq(212, nlyr(prec), 365) 

# vector of numbers to name each year of time period
n <- c(substr(names(prec[[1]]), 1, 4):substr(names(prec[[nlyr(prec)]]), 1, 4))              


### loop to calculate Q_quer (Mean precipitation of one year for each pixel)
# just in case: sets all run variables to zero 
i <- 0
result <-0 
subset <-  0


## the loop to calculate Q_quer
for (i in 1:(nlyr(prec)/365)){
  
  # subset for each year
  subset <- prec[[l[i]:m[i]]]
  
  # calculate mean prec for each pixel over one year
  result <- app(subset, fun = mean) 
  
  # filling new raster stack with calculated data
  add(prec_q_quer) <- result

}


### loop to calculate daily precipitation anomaly for each day and pixel of the year (actual daily precipitation minus Q_quer) 
# just in case: sets all run variables to zero 
i <- 0
j <- 0
subset <- 0
result <- 0


## the loop to calculate the precipitation anomaly
for (i in 1:nlyr(prec_q_quer)) {
  
  # subset for each year
  subset <- prec[[l[i]:m[i]]]
  
  #calcs the anomaly -> the actual precipitation value minus the mean precipitation value (Q_quer) for each pixel
  result <- subset - prec_q_quer[[i]] # app() applies cell based calculations
  
  # filling new raster stack with calculated data
  add(precipitation_anomaly) <- result
}



### the loop that calculates the cumulative anomaly for each day of a year
# just in case: sets all run variables to zero 
j <- 0 
i <- 0
sub_anomaly <- 0
cumulativ <- 0


## the loop to calculate the cumulative anomaly
for (i in 1:(nlyr(precipitation_anomaly)/365)) {
  # subset for each year
  sub_anomaly <- precipitation_anomaly[[l[i]:m[i]]]
  
  # calculate cumulative anomaly 
  cumulativ <- app(sub_anomaly, fun = cumsum)
  
  # filling new raster stack with calculated data
  add(storage_cumulative) <- cumulativ
}



### Loop to identify the layer (=day) of each year where the pixel value is the the minimum (=start of the rainy season)
## just in case: sets all run variables to zero 
i <- 0
t <- 0


## the loop to find the onset of the rainy season and calculate growing season (start growing season = start rainy season, growing season of maize ends after 120 d (Nnoli et al. 2019)
for (i in 1:(nlyr(storage_cumulative)/365)) {

  table_cumulative <- as.data.frame(storage_cumulative[[l2[i]:m2[i]]]) # subsetting the data year wise and converting it into a dataframe
  min_index <- apply(table_cumulative, 1, which.min) # identifies for each pixel the column (=day of selected year) where the value is the minimum
  
  t <- cbind(start_day_ = (min_index + 59), # creating a new dataframe with the start day of the rainy season for the selected day (we need to add 59 days since we cutted out the first 60 days of the year) 
             end_day_ = (min_index+120+59)) # the end day is defined as the day the growing period of maize (120 days after sowing) is reached
  colnames(t)[1] <- paste0("start_day_", n[i]) # renaming the columns (adding the selected year)
  colnames(t)[2] <- paste0("end_day_", n[i])  # renaming the columns (adding the selected year)
  table_start_end_day <- cbind(table_start_end_day, t) # adding the dataframe we just created to the dataframe containing x-/y-coordinates
}


## saving datatable 
write.table(table_start_end_day, paste0(path, "/Rainy_Season_", substr(names(prec[[1]]), 1, 4), "_", substr(names(prec[[nlyr(prec)]]), 1, 4), ".txt"))

##############################################################################################################



#######################################################
#### PART III: Subset the data to growing season ######
#######################################################

## converting rasterstacks to dataframes
table_Tmean <- as.data.frame(T_mean, xy = TRUE)
table_Tmax <- as.data.frame(T_max, xy = TRUE)
table_prec <- as.data.frame(prec, xy = TRUE)


## creation of different data sets we are going to need
ts <- seq(3,ncol(table_start_end_day), 2) # vector we need to specify the column for the start day of rainy season for each year
te <- seq(4,ncol(table_start_end_day), 2) # vector we need to specify the column for the end day of growing season for each year
tabelle_RS_Tmean <- table_Tmean[, 1:2]
tabelle_RS_Tmax <- table_Tmax[, 1:2]
tabelle_RS_Prec <- table_Tmean[, 1:2]
#tabelle_RS_Tmax <- table_Tmax[, 1:2]
#tabelle_RS_Prec <- table_prec[, 1:2]

## additional data 
# vector of numbers to name each year of time period
n <- c(substr(names(prec[[1]]), 1, 4):substr(names(prec[[nlyr(prec)]]), 1, 4))

# two sequences to indicate the layer who are representing the first/last day for each year 
l <- seq(1, ncol(table_Tmean), 365)
m <- seq(365, ncol(table_Tmean), 365)


## just in case: sets all run variables to zero
j <- 0
i <- 0
q <- 0
u <- 0
b <- 0


## creating subsets of the data containing just the data from the onset of rainy season +120 days. for each pixel (=growing season)
for (j in 1:(ncol(table_Tmean)/365)) {
  
  sub_Tmean <- data.frame()
  sub_Tmax <- data.frame()
  sub_Prec <- data.frame()
  
  sub_mean <- table_Tmean[l[j]:m[j]]   # subsetting the mean Temperature dataframe to the right year
  sub_max <- table_Tmax[l[j]:m[j]]     # subsetting the max Temperature dataframe to the right year
  sub_pr <- table_prec[l[j]:m[j]]      # subsetting the precipitation dataframe to the right year
  
  for (i in 1:nrow(sub_mean)) {
    u <- sub_mean[i,  table_start_end_day[i, ts[j]]:table_start_end_day[i, te[j]]]   # this loop subsets the selected year data (t mean) to the dynamic growing season for each pixel
    b <- sub_max[i,  table_start_end_day[i, ts[j]]:table_start_end_day[i, te[j]]]    # this loop subsets the selected year data (t max) to the dynamic growing season for each pixel
    q <- sub_pr[i,  table_start_end_day[i, ts[j]]:table_start_end_day[i, te[j]]]     # this loop subsets the selected year data (prec) to the dynamic growing season for each pixel
    colnames(u) <- paste0(c(1:121), "_Day_", n[j])
    colnames(b) <- paste0(c(1:121), "_Day_", n[j])
    colnames(q) <- paste0(c(1:121), "_Day_", n[j])
    sub_Tmean <- rbind(sub_Tmean, u)
    sub_Tmax <- rbind(sub_Tmax, b)
    sub_Prec <- rbind(sub_Prec, q)
  }
  
  tabelle_RS_Tmean <- cbind(tabelle_RS_Tmean, sub_Tmean)
  tabelle_RS_Tmax <- cbind(tabelle_RS_Tmax, sub_Tmax)
  tabelle_RS_Prec <- cbind(tabelle_RS_Prec, sub_Prec)
}


## convert dataframes to rasterstacks
Tmean_RS_rast <- rast(tabelle_RS_Tmean, type = "xyz", crs = "EPSG:4326")
Tmax_RS_rast <- rast(tabelle_RS_Tmax, type = "xyz", crs = "EPSG:4326")
Prec_RS_rast <- rast(tabelle_RS_Prec, type = "xyz", crs = "EPSG:4326")


## saving raster stacks
writeRaster(Tmean_RS_rast,paste0(path,"/Tmean_RS_", substr(names(T_mean[[1]]), 1, 4), "_", substr(names(T_mean[[nlyr(T_mean)]]), 1, 4), ".grd"), filetype = "RRASTER")
writeRaster(Tmax_RS_rast,paste0(path,"/Tmax_RS_", substr(names(T_max[[1]]), 1, 4), "_", substr(names(T_max[[nlyr(T_max)]]), 1, 4), ".grd"), filetype = "RRASTER")
writeRaster(Prec_RS_rast,paste0(path,"/Prec_RS_", substr(names(prec[[1]]), 1, 4), "_", substr(names(prec[[nlyr(prec)]]), 1, 4), ".grd"), filetype = "RRASTER")


##############################################################################################################



############################################################
#### PART IV:  Calculation of mean of defined periods ######
############################################################


## create function to calculate mean-year representing the period
## the mean is calculated for indice values (T_mean, T_max, prec) within growing season of every year throughout selected amount of years, e.g. 20 y periods 
mean_clim <- function(data, type){
  # create a new Index containing only the day-mon information of the time attribute 
  index <- c(1:strsplit(names(data[[nlyr(data)]]), "_")[[1]][1])
  # Uses the above created index as a new index for the whole stack 
  # Subsets the stack based on the index
  # Applies a function (mean) to the subsetted Data 
  mean_ <- tapp(data,index, type)
  return(mean_)
}


## apply function to data stacks
Mean_RS_Tmax <- mean_clim(Tmax_RS_rast, type = "mean")
Mean_RS_Tmean <- mean_clim(Tmean_RS_rast, type = "mean")
Mean_RS_Prec <- mean_clim(Prec_RS_rast, type = "mean")


## save raster stacks
writeRaster(Mean_RS_Tmax,paste0(path,"/Tmax_mean_RS_", substr(names(T_mean[[1]]), 1, 4), "_", substr(names(T_mean[[nlyr(T_mean)]]), 1, 4), ".grd"), filetype = "RRASTER")
writeRaster(Mean_RS_Tmean,paste0(path,"/Tmean_mean_RS_", substr(names(T_max[[1]]), 1, 4), "_", substr(names(T_max[[nlyr(T_max)]]), 1, 4), ".grd"), filetype = "RRASTER")
writeRaster(Mean_RS_Prec,paste0(path,"/Prec_mean_RS_", substr(names(prec[[1]]), 1, 4), "_", substr(names(prec[[nlyr(prec)]]), 1, 4), ".grd"), filetype = "RRASTER")


########################################################################


#############################################################################################################
#### PART V: Identification and classification Temperature/Heat Days threshold values for defined periods####
#############################################################################################################

## loop to identify Heat days during the dynamic growing period for the defined periods 
## furthermore the loop classifies the data. 
## Classifiation rules: 
# 0: criteria have not been met
# 1: optimum (15°C > T mean > 32°C) 
# 2: risk of yield loss (T mean > 32°C) 
# 3: risk of crop failure (T max > 46°C)


## classify temperature data
ge_r <- ifel(Mean_RS_Tmax >= 46, 3,  # risk of crop failure 
             ifel(Mean_RS_Tmean > 32, 2, # risk of yield loss 
                  ifel(Mean_RS_Tmean >= 15 & Mean_RS_Tmean <= 32, 1, 0))) # optimum, otherwise criteria not met 


## creating new single easterlayer with only the max values for each pixel out of the stack because this is final classification value for each year 
heat_days_class_raster <- app(ge_r, fun = max)


## saveing raster stacks
writeRaster(heat_days_class_raster, paste0(path,"/Temp_Classification_Periode_", substr(names(T_mean[[1]]), 1, 4), "_", substr(names(T_mean[[nlyr(T_mean)]]), 1, 4), ".grd"), filetype = "RRASTER", overwrite = TRUE)


##############################################################################################################


###########################################################################################
#### PART VI: Calculation and classification of Precipitation sums for defined periods ####
###########################################################################################

## loop to calculate the precipitation sum for the dynamic growing period for the defined periods 
## furthermore the loop classifies the data. 
# Classification rules
# 0: criteria have not been met = risk of crop failure 
# 1: risk of yield loss because too dry (400 mm < prec < 600 mm) 
# 2: Optima (600 mm < prec < 1200 mm) 
# 3: risk of yield loss because too wet (1200 mm < prec < 1800 mm)


## sum up the Precipitation for each pixel
sum_rs <- app(Mean_RS_Prec, fun = sum)


## classify precipitation data
prec_possible_raster <- ifel(sum_rs >= 400 & sum_rs <= 600, 2, # too dry, risk of yield loss
                              ifel(sum_rs > 600 & sum_rs <= 1200, 1, # optimum
                                    ifel(sum_rs > 1200 & sum_rs <= 1800, 3, 0))) # too wet, risk of yield loss, otherwise risk of crop failure 


## saving raster
writeRaster(prec_possible_raster, paste0(path,"/Prec_Classification_", substr(names(T_mean[[1]]), 1, 4), "_", substr(names(T_mean[[nlyr(T_mean)]]), 1, 4), ".grd"), filetype = "RRASTER")


##############################################################################################################


####################################################################################
#### PART VII: Calculation and classification of dry spells for defined periods ####
####################################################################################

## NOTE: 
# the dry Spells Analysis identifies consecutive dry days (Dry day = prec < 0.85, Barron et al. 2003) during the growing season for each year 
# other than with precipitation sums and temperature, we cannot simply use the mean dry spells values for each selected period because the average distorts the results
# instead, we calculate dry spell lengths for each year and then average the values to 20 y periods and use these results for classification

# Classification rules 
# 1: Optimum (DS <5 d) 
# 2: risk of yield loss (5 d < DS < 13 d)  
# 3: risk of crop failure (DS >13 d)


## additional data transformation
table_prec <- as.data.frame(prec)


## creation of different data sets we are going to need
dry_spells <- c()             # creating dataframe with x and y column
n <- c(substr(names(prec[[1]]), 1, 4):substr(names(prec[[nlyr(prec)]]), 1, 4)) # vector with years, used to name new data
ts <- seq(3,ncol(table_start_end_day), 2) # vector we need to specify the column for the start day of rainy season for each year
te <- seq(4,ncol(table_start_end_day), 2) # vector we need to specify the column for the end day of growing season for each year

# two sequences to indicate the layer who are representing the first/last day for each year 
l <- seq(1, ncol(table_prec),365)
m <- seq(365, ncol(table_prec),365)


# just in case: sets all run variables to zero 
j <- 0
i <- 0
t <- 0
u <- 0


## the loop to calculate dry spells 
for (j in 1:(ncol(table_prec)/365)) {
  
  sub_rs <- c()
  
  sub <- table_prec[l[j]:m[j]] # subset to each year 
  
  for (i in 1:nrow(sub)) {
    u <- sub[i,  table_start_end_day[i, ts[j]]:table_start_end_day[i, te[j]]] # subset to growing season for each year 
    colnames(u) <- c(1:121)
    sub_rs <- rbind(sub_rs, u)
  }
  
  table_sub_rs <- as.data.frame(sub_rs)
  t <- ifelse(table_sub_rs < 0.85, 1, 0) # classify day as dry day if prec < 0.85 mm (Barron et al. 2003)
  
  fe <- apply(t, 1, FUN = function(x=t) {
    rle <- rle(x)   #  Compute the lengths and values of runs of equal values in a vector (rle())
    max_p <- max(rle$lengths[rle$values!=0]) # find the highest number of consecutive dry days within each growing season -> this will be the number for classification for each year
  })
  
  dry_spells <- cbind(dry_spells, fe)
}


## mean of dry spells length over selected period, these values are used for classification 
sum_dry <- apply(dry_spells, 1, FUN = mean)


## classify the averaged dry spells
dry_class <- ifelse(sum_dry > 13, 3, # risk crop failure 
                        ifelse(sum_dry < 13 & sum_dry > 5, 2, 1)) # risk yield loss, otherwise optimum 


## add coordinates to table
dry_class <- cbind(as.data.frame(prec, xy = TRUE)[, 1:2], dry_class)


## convert table into raster
dry_spell_mean_rast <- rast(dry_class, type = "xyz", crs = "EPSG:4326")


## saving raster
writeRaster(dry_spell_mean_rast, paste0(path,"/dry_Spell_Classes_", substr(names(T_mean[[1]]), 1, 4), "_", substr(names(T_mean[[nlyr(T_mean)]]), 1, 4), ".grd"), filetype = "RRASTER")


##############################################################################################################


####################################################################################äää
#### PART VIII:  Final Classification to evaluate possible Maize Cultivation areas ####
####################################################################################äää

prec <-  prec_possible_raster
temp <- heat_days_class_raster
DS <- dry_spell_mean_rast


# create crop growth classification on the basis of precipitation and temperature (heat day) classes
TP <- ifel(prec == 1 & temp == 1, 1, # optimum
            ifel(prec ==2 & temp == 1 | prec == 3 & temp == 1 | prec == 1 & temp ==2 | prec == 3 & temp == 2, 2, # risk of slight yield loss
                  ifel(prec == 2 & temp == 2, 3,  # risk of severe yield loss
                       ifel(prec >= 0 & temp == 3 | prec == 0 & temp >= 1, 4, NA)))) # risk of crop failure 
 



# do the final crop prediction classes on the basis of precipitation, temperature and dry spells
TPD_Final <- ifel(TP == 1 & DS == 1, 1, # optimum 
                  ifel(TP == 1 & DS == 2 | TP == 2 & DS == 1, 2, # risk slight yield loss
                       ifel(TP == 2 & DS == 2 | TP == 3 & DS == 1 | TP == 3 & DS ==2 , 3, # risk severe yield loss
                            ifel(TP == 4 & DS >= 1 | TP >= 1 & DS ==3, 4, NA)))) # risk of crop failure 


## saving raster
writeRaster(TP, paste0(path,"/TP_Final_Periode_", substr(names(T_mean[[1]]), 1, 4), "_", substr(names(T_mean[[nlyr(T_mean)]]), 1, 4), ".grd"), filetype = "RRASTER", overwrite = TRUE)
writeRaster(TPD_Final, paste0(path,"/TPD_Final_Periode_", substr(names(T_mean[[1]]), 1, 4), "_", substr(names(T_mean[[nlyr(T_mean)]]), 1, 4), ".grd"), filetype = "RRASTER", overwrite = TRUE)



