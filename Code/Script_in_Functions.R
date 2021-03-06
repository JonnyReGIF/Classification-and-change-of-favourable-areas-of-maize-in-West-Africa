###########################################################################################
#### Functions to calculate change of cop cultivation areas based on climatic Indices #####
###########################################################################################

# required packages 
require(terra)      # Handling raster data
require(PCICt)      # 365 d calender
require(lubridate)  # Handling dates 
library(tidyverse)  
require(stringr)


files_prec <- list.files("your path", full.names = TRUE)
files_TMax  <- list.files("your path", full.names = TRUE)
files_TMean <- list.files("your path", full.names = TRUE)


#################
#### Content ####
#################

# PART I:   Function for Preprocessing Cordex Data and Calculate Rainy Season Onset and Growing Period 
# PART II:  Functions to Calculate and Classify three climatic indices: temperature (heat days), precipitation and dry spells within growing period
# PART III: Functions to combine the classifications of all climatic indices 
# PART IV:  Function to calculate mean within defined periods 
# PART V:   Function to calculate area 



##########################################################################################################################
####### PART I:   Function for Preprocessing Cordex Data and Calculate Rainy Season Onset and Growing Period   ###########
##########################################################################################################################

# Preprocessing includes stacking all raters, masking the desired study area, 
# naming all raster Layers by date optional the user can supply a raster with crop cultivation area which is then used to mask the input data
prep_cordex      <- function (file_list, studyarea, crop_cultivation_area, unit, start_data, end_data) {
  
  if(missing(crop_cultivation_area) == T){
    
    # Create raster stack from all files in file_list
    rasterstack <- rast(file_list)
    
    # define stack crs and crop to the extent of study area 
    crs(rasterstack) <- crs(studyarea)
    rasterstack_crop <- crop(rasterstack, ext(studyarea))
    
    
    if(unit == "mm/d"){
      rasterstack_mask_unit <- rasterstack_crop * 86400
    }else if(unit == "°C"){
      rasterstack_mask_unit <- rasterstack_crop - 273.15
    }
    
    # Renaming Data 
    names(rasterstack_mask_unit) <- seq(as.PCICt(start_data, cal = "365_day"), as.PCICt(end_data, cal = "365_day"), by = "day")
    
    return(rasterstack_mask_unit)
    
  }else{
    
    # Create raster stack from all files in file_list
    rasterstack <- rast(file_list)
    
    # define stack crs and crop to the extent of study area 
    crs(rasterstack) <- crs(studyarea)
    rasterstack_crop <- crop(rasterstack, ext(studyarea))
    
    # Mask climate data to maize cultivation areas from Monfreda et. al 2008
    # Resample maize cultivation data (Monfreda) to rasterstack res 
    maize_resampled <- terra::resample(crop_cultivation_area, rasterstack_crop)
    rasterstack_mask <- terra::mask(rasterstack_crop, maize_resampled)
    
    if(unit == "mm/d"){
      rasterstack_mask_unit <- rasterstack_mask * 86400 # convert units of precipitation from kg m-2 s-1 to mm per day 
    }else if(unit == "°C"){
      rasterstack_mask_unit <- rasterstack_mask - 273.15 # convert units of temperature from Kelvin to °C
    }
    
    # Renaming Data 
    names(rasterstack_mask_unit) <- seq(as.PCICt(start_data, cal = "365_day"), as.PCICt(end_data, cal = "365_day"), by = "day")
    
    return(rasterstack_mask_unit)
  }
}


# Helper function to subset climate data by year, only works with in conjunction prep_cordex 
# Create a yearly subset of any data indexed by a Date
subset_data_year <- function(year, data){
  
  # Create a yearly subset of any data indexed  by a Date
  
  #define start of the year
  #y_start <- unique(year(ymd(c(names(data)))))
  start <- paste0(year,"-01-01")
  
  #define lasst day of year
  #y_end <- unique(year(ymd(c(names(data)))))[i]
  end <- paste0(year,"-12-31")
  
  #subset one year by start and end of year
  #Sub creates a T (True == in time range) or F (not in time range) binary mask
  #The mask is uses to subset the actual Data
  
  if(class(data) == "SpatRaster"){
    
    sub <- names(data) >= as.POSIXct(start) & names(data) <= as.POSIXct(end)
    subset <- data[[sub]]
    
    return(subset)
    
  } else if(class(data) == "data.frame" ){
    print("I am class df")
    #sub <- colnames(data) >= as.POSIXct(start) & names(data) <= as.POSIXct(end)
    subset <- data[,c(substring(colnames(data), 1,4) == year)]
    
    return(subset)
  }
}


# Function to calculate the rainy season onset after Dunning et al. 2016
calc_rainyseason <- function(prec){
  
  ## creating empty raster to store output of the following loops in it
  prec_q_quer <- rast(extent = ext(prec), resolution = res(prec), crs = crs(prec))  # Q_quer = Mean precipitation of one year for each pixel
  precipitation_anomaly <- rast(extent = ext(prec), resolution = res(prec), crs = crs(prec))  # Daily precipitation anomaly data for one year
  storage_cumulative <- rast(extent = ext(prec), resolution = res(prec), crs = crs(prec))  # Cumulative sum of the precipitation anomaly for each year
  table_start_end_day <- as.data.frame(prec, xy = TRUE)[, 1:2]  # empty table to store results of growing season dates
  
  l <- seq(1, nlyr(prec), 365)
  m <- seq(365, nlyr(prec), 365)
  
  # sequence that indicates the layer that represents the first/last day of the static rainy season of each year (required to subset dataset)
  # "60" represents the day of the year we are postulating to be the earliest the rainy season could start 
  # "212" represents the day of the year we are postulating to be the latest the rainy season could start
  l2 <- seq(60, nlyr(prec), 365) 
  m2 <- seq(212, nlyr(prec), 365) 
  
  # vector of numbers to name each year of time period
  n <- c(substr(names(prec[[1]]), 1, 4):substr(names(prec[[nlyr(prec)]]), 1, 4))  
  
  
  ### loop to calculate Q_quer = Mean precipitation of one year for each pixel
  # just in case: sets all run variables to zero 
  i <- y_start <- j_start <- y_end <- j_end <- 0
  
  ## the loop to calculate Q_quer
  for (i in 1:length(unique(year(ymd(c(names(prec))))))){
    
    # Create a yearly subset from the input data to calc the mean precipitation for each pixel
    subset <- subset_data_year(as.character(n[i]), prec)
    
    #calc Q_quer 
    result <- app(subset, fun = mean) # app() applies cell based calculations
    add(prec_q_quer) <- result
    names(prec_q_quer)[i] <- unique(year(ymd(c(names(prec)))))[i]
  }
  
  ### loop to calculate daily precipitation anomaly for each day and pixel of the year
  # just in case: sets all run variables to zero 
  i <- y_start <- j_start <- y_end <- j_end <- 0
  
  ## the loop precipitation anomaly
  for (i in 1:nlyr(prec_q_quer)) {
    
    subset <- subset_data_year(as.character(n[i]), prec)
    
    #calcs the anomaly -> the actual precepetation value of each day minus the mean precipitation value (Q_quer) for each pixel
    result <- subset - prec_q_quer[[i]] 
    add(precipitation_anomaly) <- result
  }
  
  ### the loop that calculates the cumulative anomaly for each day of a year
  # just in case: sets all run variables to zero 
  j <- i <- 0
  
  ## the loop cumulative anomaly
  for (i in 1:(nlyr(precipitation_anomaly)/365)) {
    cumulativ <- 0
    sub_anomaly <- 0
    # s <- 1  
    # e <- 365  
    # l <- seq(s, nlyr(precipitation_anomaly), 365)
    # m <- seq(e, nlyr(precipitation_anomaly), 365)
    # sub_anomaly <- c(precipitation_anomaly[[l[i]:m[i]]])
    sub_anomaly <- subset_data_year(n[i], precipitation_anomaly)
    
    cumulativ <- app(sub_anomaly, fun = cumsum)
    
    add(storage_cumulative) <- cumulativ
  }
  
  ### Loop to identify the layer (=day) of each year where the pixel value is the the minimum (=start of the rainy season)
  # just in case: sets all run variables to zero 
  i <- t <- 0
  
  ## the loop to find minimum
  for (i in 1:(nlyr(storage_cumulative)/365)) {
    
    table_cumulative <- as.data.frame(storage_cumulative[[l2[i]:m2[i]]]) # subsetting the data year wise and converting it into a dataframe
    min_index <- apply(table_cumulative, 1, which.min) # identifies for each pixel the column (=day of selected year) where the value is minimum
    
    t <- cbind(start_day_ = (min_index + 59), # creating a new dataframe with the start day of the rainy season for the selected day (we need to add 59 days since we cutted out the first 60 days of the year; rainy season onset = start of growing season) 
               end_day_ = (min_index+120+59)) #  the end day is defined as the day the growing period of maize (120 days after rainy season onset, Nnoli et al. 2019) is reached
    colnames(t)[1] <- paste0("s_day_", n[i]) # renaming the columns (adding the selected year)
    colnames(t)[2] <- paste0("e_day_", n[i])  # renaming the columns (adding the selected year)
    table_start_end_day <- cbind(table_start_end_day, t) # adding the dataframe we just created to the dataframe containing x-/y-coordinates
  }
  
  results_rs <- list(prec_q_quer = prec_q_quer, precipitation_anomaly = precipitation_anomaly,
                     storage_cumulative = storage_cumulative, table_start_end_day = table_start_end_day)
  
  return(results_rs)
}


##############################################################################################################################################################
### PART II:  Functions to Calculate and Classify three climatic indices: temperature (heat days), precipitation sums and dry spells within growing period ###
##############################################################################################################################################################


# Calculate climatic indices 
# If start the rainy season onset table is not supplied as an argument the function calc_rainyseason is called  
# Classification thresholds can be set by the user, otherwise default Class thresholds are used 


# The thresholds are used to calculate three classes: 1. Optimum Growth 2. Risk of Yield reduction 3. risk of Crop Failure
# To change the classification thresholds, a named list with upper, lower and optimum thresholds can be passed to the function by the user
# The example below explains how to set classification thresholds 
# Default Parameters are synthesized from Literature, for the crop maize and can be viewed in climate indices functions (For References: https://github.com/JonnyReGIF/Classification-and-change-of-favourable-areas-of-maize-in-West-Africa)

# params_temperature_thresh <- list(risk_crop_failure  = 46,
#                                   risk_yield_red = 32,
#                                   optimum_min_Tmean = 15,
#                                   optimum_max_Tmean = 32)

# params_precipitation      <- list(max = c(1200, 1800),
#                                   min = c(400,   600), 
#                                   optimum = c(600,  1200))

# params_dry_spells     <- list(risk_crop_failure = 13, 
#                                   risk_yield_red = c(13, 5)


# Function to classify the temperature thresholds based on daily mean and maximum near air temperature 
temperatur_thresh <- function(T_max, T_mean, table_start_end_day, params_temperature_thresh){
  
  # Classification rules 
  # 0: criteria have not been met
  # 1: optimum (15°C > T mean > 32°C) 
  # 2: risk of yield reduction (T mean > 32°C) 
  # 3: risk of crop failure (T max > 46°C)
  
  # If Parameter for the temperature thresh are not defined default parameters are taken into account
  # else user defined parameters are used 
  
  if(missing(params_temperature_thresh) == T){
    params_temperature_thresh <- list(risk_crop_failure  = 46,
                                      risk_yield_red = 32,
                                      optimum_min_Tmean = 15,
                                      optimum_max_Tmean = 32)
  } 
  
  
  # If the table_start_end_day is not supplied the function calls the calc_rainyseason function and
  # asks the user to supply the path to the preprocessed precipitation dataset 
  
  if(missing(table_start_end_day) == T){
    
    cat("Table start end day not supplied \n Function calc_rainyseason will be called \n pls provide windows path to precipitation dataset: \n Pay attention to slashes, pls provide in an R interpretable way")
    prec_path = readline()
    #prec = str_replace_all(prec, "\", "[\\]")
    print(prec_path)
    prec = rast(prec_path)
    table_start_end_day <- calc_rainyseason(prec)
    table_start_end_day <- table_start_end_day$table_start_end_day
    
  }else{
    
    # Calc a Vector containing all unique Years in one input stack (naming data by data function prep)
    n <- c(substr(names(T_max[[1]]), 1, 4):substr(names(T_max[[nlyr(T_max)]]), 1, 4))  
    
    # Stack to df 
    table_Tmean <- as.data.frame(T_max)
    table_Tmax  <- as.data.frame(T_mean)
    
    # Create empty data.frames to store results 
    heat_days_growing_season <- as.data.frame(T_mean, xy = TRUE)[, 1:2]   # creating dataframe with x and y column 
    heat_days_possible <- as.data.frame(T_mean, xy = TRUE)[, 1:2]   # creating dataframe with x and y column 
    heat_days_class_raster <- rast(extent = ext(T_mean), resolution = res(T_mean), crs = crs(T_mean)) #empty raster to store loop output data
    n <- c(substr(names(T_mean[[1]]), 1, 4):substr(names(T_mean[[nlyr(T_mean)]]), 1, 4))# vector with years used to name new data
    
    ts <- seq(3,ncol(table_start_end_day), 2) # ts = time_start, vector containing index values pointing to the start day of rainy season for each year
    te <- seq(4,ncol(table_start_end_day), 2) # te = time_end, vector containing index values pointing to the end of growing season for each year
    
    
    # Initialize all variabels used inside the loop with zero 
    j <- i <- s <- e <- l <- m <- u <- b <- t <- 0
    
    for (j in 1:(ncol(table_Tmean)/365)) {
      sub_Tmean <- data.frame()
      sub_Tmax <- data.frame()
      # s <- 1  
      # e <- 365  
      # l <- seq(s, ncol(table_Tmean), 365)
      # m <- seq(e, ncol(table_Tmean), 365)
      
      # sub_mean <- table_Tmean[l[j]:m[j]]           
      # sub_max <- table_Tmax[l[j]:m[j]]
      
      sub_mean <- subset_data_year(n[j], table_Tmean)   # subsetting the mean Temperature dataframe to the right year
      sub_max <-  subset_data_year(n[j], table_Tmax)    # subsetting the max Temperature dataframe to the right year
      print(sub_mean)
      print(sub_max)
      for (i in 1:nrow(sub_mean)) {
        u <- sub_mean[i,  table_start_end_day[i, ts[j]]:table_start_end_day[i, te[j]]]   # this loop subsets the selected year data to the dynamic growing season for each pixel
        b <- sub_max[i,  table_start_end_day[i, ts[j]]:table_start_end_day[i, te[j]]]    # this loop subsets the selected year data to the dynamic growing season for each pixel
        colnames(u) <- c(1:121)
        colnames(b) <- c(1:121)
        sub_Tmean <- rbind(sub_Tmean, u)
        sub_Tmax <- rbind(sub_Tmax, b)
      }
  
      
      t <- ifelse(sub_Tmax >= params_temperature_thresh$risk_crop_failure, 3, # risk of crop failure   
                  ifelse(sub_Tmean > params_temperature_thresh$risk_yield_red, 2,# risk of yield reduction 
                         ifelse(sub_Tmean >= params_temperature_thresh$optimum_min_Tmean & sub_Tmean <= params_temperature_thresh$optimum_max_Tmean, 1, 0))) # optimum, otherwise 0 
      #heat_days_possible <- cbind(heat_days_possible, t)
      
      # name all days in the growing season 
      colnames(t) <- paste0("day_", c(1:121))
      
      # combine coordinates from rasterstack and classified values to one df
      ge <- cbind(as.data.frame(T_mean, xy = TRUE)[, 1:2], t)
      
      # Create raster from df
      ge_r <- rast(ge, type = "xyz", crs = "EPSG:4326")
      
      # get maximum Value from each pixel because this is used for classification of the pixel for each year (layer)
      p_2 <- app(ge_r, fun = max)  
      
      names(p_2) <- n[j]
      add(heat_days_class_raster) <- p_2
    }
    
    list_results <- list()
    
    return(heat_days_class_raster)
  }
}


# Function to classify precipitation sums 
prec_sums         <- function(prec, table_start_end_day, params_precipitation){
  
  # Classification rules
  # 0: criteria have not been met = risk of crop failure 
  # 1: risk of yield reduction because too dry (400 mm < prec < 600 mm) 
  # 2: Optima (600 mm < prec < 1200 mm) 
  # 3: risk of yield reduction because too wet (1200 mm < prec < 1800 mm)
  
  # If Parameter for the temperature thresh are not defined default parameters are taken into account
  # else user defined parameters are used 
  
  if(missing(params_precipitation) == T){
    params_precipitation      <- list(max = c(1200, 1800),
                                      min = c(400,   600), 
                                      optimum     = c(600,  1200))
  }
  
  # If the table_start_end_day is not supplied the function calls the calc_rainyseason function and
  # asks the user to supply the path to the preprocessed precipitation dataset 
  
  if(missing(table_start_end_day) == T){
    
    cat("Table start end day not supplied \n Function calc_rainyseason will be called \n pls provide windows path to precipitation dataset: \n Pay attention to slashes, pls provide in an R interpretable way")
    prec_path = readline()
    #prec = str_replace_all(prec, "\", "[\\]")
    print(prec_path)
    prec = rast(prec_path)
    table_start_end_day <- calc_rainyseason(prec)
    table_start_end_day <- table_start_end_day$table_start_end_day
    
  }else{
    
    # Create storage Variabels 
    prec_possible <- as.data.frame(prec, xy = TRUE)[, 1:2]          # creating dataframe with x and y column
    n <- c(substr(names(prec[[1]]), 1, 4):substr(names(prec[[nlyr(prec)]]), 1, 4))  # vector with years. used to name new data
    ts <- seq(3,ncol(table_start_end_day), 2) # vector we need to specify the column for the start day of rainy season for each year
    te <- seq(4,ncol(table_start_end_day), 2) # vector we need to specify the column for the end day of growing season for each year
    
    # from rasterstack to df 
    table_prec <- as.data.frame(prec)
    
    # Initialize all Variables used in the loop with 0 
    j <- i <- s <- e <- l <- m <- t <- u <- 0
    
    for (j in 1:(ncol(table_prec)/365)) {
      
      sub_rs <- data.frame()
      
      sub <- subset_data_year(n[i], table_prec) # subsetting precipitation dataframe to the right year
      
      for (i in 1:nrow(sub)) {
        u <- sub[i,  table_start_end_day[i, ts[j]]:table_start_end_day[i, te[j]]] # this loop subsets the selected year data to the dynamic growing season for each pixel
        colnames(u) <- c(1:121)
        sub_rs <- rbind(sub_rs, u)
      }
      
      sum_rs <- as.data.frame(apply(sub_rs, 1, FUN = sum))  # sums up daily precipitation values within growing season
      colnames(sum_rs)[1] <- paste0("mm_", n[j])
      
      
      t <- ifelse(sum_rs >= params_precipitation$min[1] & sum_rs <= params_precipitation$min[2], 2, # too dry = risk yield reduction
                  ifelse(sum_rs > params_precipitation$optimum[1] & sum_rs <= params_precipitation$optimum[2], 1, # optimum 
                         ifelse(sum_rs > params_precipitation$max[1] & sum_rs <= params_precipitation$max[2], 3, 0))) # too wet = risk yield reduction, if no criteria matches = risk crop failure
      
      prec_possible <- cbind(prec_possible, t)
      
    }
    
    prec_possible_raster <- rast(prec_possible, type = "xyz", crs = "EPSG:4326")
    
    return(prec_possible_raster)
  }
}


# Function to calculate and classify dry spells (DS)
dry_spells    <- function(prec, table_start_end_day, params_dry_spells){
  
  # Classification rules 
  # 1: Optimum (DS <5 d) 
  # 2: risk of yield reduction (5 d < DS < 13 d)  
  # 3: risk of crop failure (DS >13 d) 
  
  # If user defined parameters are missing 
  # Default Parameters are used 
  
  if(missing(params_dry_spells) == T){
    params_dry_spells     <- list(risk_crop_failure = 13, 
                                      risk_yield_red = c(13, 5))
  }
  # If the table_start_end_day is not supplied the function calls the calc_rainyseason function and
  # asks the user to supply the path to the preprocessed precipitation dataset 
  
  if(missing(table_start_end_day) == T){
    
    cat("Table start end day not supplied \n Function calc_rainyseason will be called \n pls provide windows path to precipitation dataset: \n Pay attention to slashes, pls provide in an R interpretable way")
    prec_path = readline()
    #prec = str_replace_all(prec, "\", "[\\]")
    print(prec_path)
    prec = rast(prec_path)
    table_start_end_day <- calc_rainyseason(prec)
    table_start_end_day <- table_start_end_day$table_start_end_day
    
  }else{
    
    # Storage
    dry_spells <- as.data.frame(prec, xy = T)[, 1:2]  # creating dataframe with x and y column
    n <- c(substr(names(T_mean[[1]]), 1, 4):substr(names(T_mean[[nlyr(T_mean)]]), 1, 4))  # vector with years, used to name new data
    ts <- seq(3,ncol(table_start_end_day), 2) # vector we need to specify the column for the start day of rainy season for each year
    te <- seq(4,ncol(table_start_end_day), 2) # vector we need to specify the column for the end day of growing season for each year
    
    
    # Rasterstack to df 
    table_prec <- as.data.frame(prec)
    
    # set all run variables to zero initally
    j <- i <- s <- e <- l <- m <- t <- u <- 0
    
    # loop to calculate dry spells 
    for (j in 1:(ncol(table_prec)/365)) {
      
      sub_rs <- data.frame()
      
      sub <- subset_data_year(n[i], table_prec) # subsetting precipitation dataframe to the right year
      
      for (i in 1:nrow(sub)) {
        
        # subset (year) the subset to only rainy season per year 
        u <- sub[i,  table_start_end_day[i, ts[j]]:table_start_end_day[i, te[j]]]  # subset to only growing season per year 
        colnames(u) <- c(1:121)
        sub_rs <- rbind(sub_rs, u)
      }
      
      table_sub_rs <- as.data.frame(sub_rs)
      
      # If a day during the growing season has less than 0.85 mm of precipitation, it is classified as a dry day, Barron et al. 2003
      t <- ifelse(table_sub_rs < 0.85, 1, 0)
      
      fe <- apply(t, 1, FUN = function(x=t) {
        
        # Compute the lengths and values of runs of equal values in a vector (rle())
        rle <- rle(x)
        
        max_p <- max(rle$lengths[rle$values!=0]) # find the highest number of consecutive dry days within each growing season -> this will be the number for classification for each year
        op <- ifelse(max_p > params_dry_spells$risk_crop_failure, 3, # risk crop failure
                     ifelse(max_p < params_dry_spells$risk_yield_red[1] & max_p > params_dry_spells$risk_yield_red[2], 2, 1)) # risk yield reduction, otherwise optimum 
      })
      
      dry_spells <- cbind(dry_spells, fe)
    }
    
    names(dry_spells)[3:ncol(dry_spells)] <- n # assign years to the data 
    
    
    ## convert result dataframe into raster stack
    dry_spells_rast <- rast(dry_spells, type = "xyz", crs = "EPSG:4326")
    
    return(dry_spells_rast)
  }
}


##################################################################################
### PART III: Functions to combine the classifications of all climatic indices ###
##################################################################################

# Function to combine all three climate indices and classify to final (un)favourable Maize cultivation areas 
combined_classification <- function(prec_possible_raster, heat_days_class_raster, dry_spells_rast){
  
  
  # Storage
  TP_Final <- rast(extent = ext(prec_possible_raster), resolution = res(prec_possible_raster), crs = crs(prec_possible_raster))
  TPD_Final <- rast(extent = ext(prec_possible_raster), resolution = res(prec_possible_raster), crs = crs(prec_possible_raster))
  
  # first loop combining temperature (heat days) and precipitation sums 
  for (i in 1:nlyr(prec_possible_raster)) {
    prec <- prec_possible_raster[[i]]
    temp <- heat_days_class_raster[[i]]
    
    s <- ifel(prec == 1 & temp == 1, 1, # optimum
              ifel(prec ==2 & temp == 1 | prec == 3 & temp == 1 | prec == 1 & temp ==2 | prec == 3 & temp == 2, 2, # risk slight yield reduction
                   ifel(prec == 2 & temp == 2, 3, # risk severe yield reduction
                        ifel(prec >= 0 & temp == 3 | prec == 0 & temp >= 1, 4, NA)))) # risk crop failure, otherwise NA
    
    add(TP_Final) <- s
  }
  
  
  ## the loop to do the final crop prediction classes on the basis of precipitation sums, temperature (heatdays) and dry spell lenghts 
  for (i in 1:nlyr(TP_Final)) {
    TP <- TP_Final[[i]]
    DS <- dry_spells_rast[[i]]
    
    l <- ifel(TP == 1 & DS == 1, 1, # optimum
              ifel(TP == 1 & DS == 2 | TP == 2 & DS == 1, 2, # risk slight yield reduction
                   ifel(TP == 2 & DS == 2 | TP == 3 & DS == 1 | TP == 3 & DS ==2 , 3, # risk severe yield reduction
                        ifel(TP == 4 & DS >= 1 | TP >= 1 & DS ==3, 4, NA)))) # optimum 
    
    add(TPD_Final) <- l
  }
  
  return(TPD_Final)
  
}


#######################################################################
#### PART IV: Function to calculate mean within defined periods #######
#######################################################################


# Function to Calc mean values over a defined period 
mean_period_calc        <- function(time_start, time_end, data, type){
  
  #subset into 20 yr periodes 
  data_2021_2040 <- data[[ names(data) >= as.character(time_start) & names(data) <= as.character(time_end)]]
  #data_2021_2040 <- data[[ names(data) >= "2021-01-01" & names(data) <= "2040-12-31"]]
  
  #create a new Index containing only the day-mon information of the time attribute 
  index <- as.Date(names(data))%>%
    format(format = "%m-%d")
  
  # Uses the above created index as a new index for the whole stack 
  # Subsets the stack based on the index
  # Applys a function (mean) to the subseted Data 
  mean_ <- tapp(data_2021_2040,index, mean, type)
  
  return(mean_)
}


############################################
### PART V: Function to calculate Area #####
############################################


area_fun <- function(data, crs, unit = "sqkm") {
  
  # project data in metric system 
  data_proj <- project(data, crs, method = "near")
  
  name_unit <- paste0("area_", unit)
  
  # delete nan/na values and select unique values
  e <- na.omit(c(unique(values(data_proj))))|>sort()
  
  # Selected unit can be "ha" = Hektar, "sqkm" = Sqare kilometers, or "sqm" = square meter 
  if (unit=="sqkm") {
    unit_value <- 100000
  } else if (unit=="ha") {
    unit_value <- 10000
  } else if(unit=="sqm"){
    unit_value <- 1
  }
  
  #empty vector to store data in
  area <- c()
  prozent <- c()
  
  # loop to calculate area for each unique value 
  for (i in 1:length(e)) {
    a <- length(data_proj[data_proj%in%e[i]])*((res(data_proj)[1]^2)/unit_value)
    area[i] <- a
  }
  
  # loop to calculate percentage
  for (j in 1:length(area)) {
    f <- (100*area[j])/sum(area)
    prozent[j] <- f
  }
  
  # create data table 
  area_tab <- cbind(class=e, area = area, prozent=prozent)|>as.data.frame()
  names(area_tab)[2] <- name_unit
  
  
  return(area_tab)
}
