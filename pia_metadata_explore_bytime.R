# User defined variables 

# How many images to keep
# e.g. a cutting factor of 25 would keep 1 in 25 images
cutting_factor = 5

# How much time to split the bins into
user_bin_time  = "1 hour"
# Must be a number and then a peroid
# week
# hour
# month

# ~ # ~ # ~ # Reading in data ####

# Set working directory
setwd("D:\\OneDrive\\OneDrive - University of East Anglia\\PIA_meta_explorer")

# Reading CSV from python script
joined <- read.csv("Metadata_allimages_2018.csv")

# ~ # ~ # ~ # Tidying and truncating data ####

library(dplyr)

# Renaming vars to standard format
joined <- joined %>%
  rename(lat = img_lattitude,
         lon = img_longtitude)

# Clipping the dataset by X, entered before last )
joined_cut     = joined[seq(1, nrow(joined), cutting_factor), ]

# Writing cut dataframe for ships transect
cut_file_name <- paste0("cut_data_by_1in",cutting_factor,".csv")

write.csv(joined_cut, cut_file_name,
          row.names = F)

# Extracting just hour
# Function to select right most characters
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Removing hour and converting time to POSIX
# and extracting date
joined_cut <- joined_cut %>% 
  mutate(hour = substrRight(img_time, 8),
         img_time = as.POSIXct(img_time),
         date = as.Date(img_time))

# ~ # ~ # ~ # Binning time by n hours ####

library(hms)
library(dplyr)
library(tibbletime)

# Creating a dummy variable not to loose time
joined_cut$bintime_start <- joined_cut$img_time

# Binning by time
binned_time <- joined_cut %>% 
  arrange(bintime_start) %>%                                 
  as_tbl_time(index = bintime_start) %>%   
  # Time choice below
  collapse_by(period = user_bin_time,                       
              start_date = first(joined_cut$bintime_start),  
              side = "start",
              # Choosing if to round up
              clean = F) 


# Summarising the needed variables for the plot
library(geosphere)

binned_time <- binned_time %>% 
  group_by(bintime_start) %>% 
  # Creating a lagged variable to calculate distance
  mutate(lat_prev = lag(lat,1), lon_prev = lag(lon,1) ) %>%
  # Using Geosphere to calculate a distance between two points
  mutate(dist = distHaversine(matrix(c(lon_prev, lat_prev), ncol = 2),
                              matrix(c(lon, lat),   ncol = 2))) %>%
  # Summarising variables 
  summarise(dist_m = sum(dist,na.rm=T),
            count  = length(filename),
            #
            avglat = mean(lat),
            avglon = mean(lon),
            maxlat = max(lat),
            minlat = min(lat),
            maxlon = max(lon),
            minlon = min(lon),
            #
            min_time = min(img_time),
            max_time = max(img_time),
            avg_time = mean(img_time),
            date     = mean(date)) %>% 
  rename(lat = avglat,
         lon = avglon)


# ~ # ~ # ~ # Calculating sunrise and sunset ####

library(suncalc)

# Getting sunlight times based on average date 
bin_time_sun <- getSunlightTimes(data = binned_time,
                                 keep = c("sunrise", "sunset"))

# Pulling those variables neede across
# Meta data
bin_time_sun$bintime_start  <- binned_time$bintime_start
bin_time_sun$count     <- binned_time$count
# Min lats/lons
bin_time_sun$maxlat    <- binned_time$maxlat
bin_time_sun$minlat    <- binned_time$minlat
bin_time_sun$maxlon    <- binned_time$maxlon
bin_time_sun$minlon    <- binned_time$minlon
# Distance 
bin_time_sun$dist_m    <- binned_time$dist_m
# Mean time
bin_time_sun$avg_time  <- binned_time$avg_time
bin_time_sun$min_time  <- binned_time$min_time
bin_time_sun$max_time  <- binned_time$max_time

# Calculating if a bin is mainly in day or in the night
bin_time_sun <- bin_time_sun %>% 
  mutate(day  = ifelse(avg_time < sunset & avg_time > sunrise, "Day", "Night")) %>% 
  mutate(day  = as.factor(day))


# ~ # ~ # ~ # Naming the polygons for future reference ####

# Simple name based on first to last time
bin_time_sun$poly_name <- paste0("Poly_", 1:as.numeric(count(bin_time_sun)))

# ~ # ~ # ~ # Saving he grouped data and cleaning the workplace ####

# Writing the data

user_time_bin_no_space <- sub(" ", "", user_bin_time)

time_bin_csv_name <- paste0("timebin_", user_time_bin_no_space,"_cuttingfactor_",cutting_factor,".csv")
  
write.csv(bin_time_sun, time_bin_csv_name,
          row.names = F)

# Removing all but that nasty csv 
rm(list=ls()[! ls() %in% c("joined", "cutting_factor","user_bin_time","time_bin_csv_name","cut_file_name")])

# Reading data back in 
plotdata <- read.csv(time_bin_csv_name)

# Changing time to factor
plotdata$day <- as.factor(plotdata$day)


# ~ # ~ # ~ # Creating the polygons ####

library(sf)

lst <- lapply(1:nrow(plotdata), function(x){
  # Creating the polygons using the min and max lats and lons
  # for each hour (need FIX here - better method?)
  res <- matrix(c(plotdata[x, 'maxlon'], plotdata[x, 'maxlat'],
                  plotdata[x, 'maxlon'], plotdata[x, 'minlat'],
                  plotdata[x, 'minlon'], plotdata[x, 'minlat'],
                  plotdata[x, 'minlon'], plotdata[x, 'maxlat'],
                  plotdata[x, 'maxlon'], plotdata[x, 'maxlat'])  
                , ncol =2, byrow = T
  )
  st_polygon(list(res))})

# Extracting the geometry per polygon
# Create simple feature geometry list column
plotdata$geomtry <- st_sfc(lst)
str(plotdata)
data_assf <- st_sf(plotdata)



# ~ # ~ # ~ # Splitting the shapefile data frame by day and night ####

# night
night_only <- data_assf[data_assf$day == 'Night',]
# day
day_only   <- data_assf[data_assf$day == 'Day',]


# ~ # ~ # ~ # Getting the boat transect and changing to sp ####

# Reading cut file
transect <- read.csv(cut_file_name)

# Changing image time to indexing
transect <- transect %>% 
  mutate(img_time_num = as.numeric(as.POSIXct(img_time, tz = "GMT")))

# ~ # ~ # ~ # Plotting in Leaflet ####

library(leaflet)
library(leaflet.extras)
library(htmltools)

# Title
rr <- tags$div(
  (HTML("<b>Notes:</b><br>",
        "- 2018 Peltic Survey PI Metadata <br>",
        "- Each polygon is created using the max/min lat/lon<br>",
        "&nbsp;&nbsp;of a two hour time bin <br>",
        "- Data is clipped for rendering speed <br>",
        "<b>User defined parameters:</b><br>",
    "Only 1 in every", as.character(cutting_factor), "images kept", "<br>",
        "Time bins are", as.character(user_bin_time), "<br>"
    )))
  
map <- leaflet() %>%
  #
  addTiles() %>%
  #
  addPolylines(data = transect,
               lng = ~lon,
               lat = ~lat,
               group = "Vessel track",
               weight = 1,
               color = "black") %>% 
  #
  addPolygons(data = night_only,
              group = "Night",
              color = "Grey",
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 4,
                                                  bringToFront = TRUE),
              popup = ~paste0("<h3>Station Info: &#128674;</h3>",
                              "<b>Mean Bin Time &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b>",
                              avg_time,
                              "<br>",
                              "<b>No. Images&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; </b>",
                              count*cutting_factor,
                              "<br>",
                              "<b>Boat distance (km) &nbsp;&nbsp;&nbsp;</b>",
                              round(dist_m/1000, digits = 2),
                              "<br>",
                              "<b>Litres Sampled:  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b>",
                              "&#128542; ",
                              "<br>",
                              "<b>Polygon Code:  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b>",
                              poly_name,
                              "<br>",
                              "<br>",
                              "<h3>Subsample parameters: &#128187;</h3>",
                              "<b>Start time &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b>",
                              min_time,
                              "<br>",
                              "<b>End time &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b>",
                              max_time)) %>%  
  #
  addPolygons(data = day_only,
              group = "Day",
              color = "White",
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 4,
                                                  bringToFront = TRUE),
              popup = ~paste0("<h3>Station Info: &#128674;</h3>",
                              "<b>Mean Bin Time &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b>",
                              avg_time,
                              "<br>",
                              "<b>No. Images&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; </b>",
                              count*cutting_factor,
                              "<br>",
                              "<b>Boat distance (km) &nbsp;&nbsp;&nbsp;</b>",
                              round(dist_m/1000, digits = 2),
                              "<br>",
                              "<b>Litres Sampled:  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b>",
                              "&#128542; ",
                              "<br>",
                              "<b>Polygon Code:  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b>",
                              poly_name,
                              "<br>",
                              "<br>",
                              "<h3>Subsample parameters: &#128187;</h3>",
                              "<b>Start time &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b>",
                              min_time,
                              "<br>",
                              "<b>End time &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b>",
                              max_time)) %>%  
  addLayersControl(
    overlayGroups = c("Day", "Night", "Vessel track"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addControl(rr, position = "bottomleft")

map


# ~ # ~ # ~ # Saving map ####

# library(htmlwidgets)
# 
# user_time_bin_no_space <- sub(" ", "", user_bin_time)
# mapname <- paste0("timebin_", user_time_bin_no_space,"_cuttingfactor_",cutting_factor,"_map.html")
# 
# saveWidget(map, file=mapname)
