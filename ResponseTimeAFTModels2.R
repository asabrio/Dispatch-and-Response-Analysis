# -----------------------------------------------------------------------------
# Code for: "Dispatch Delays and Geographic Disparities in Police Response Times in New Orleans"
# Authors: Alexandra Sabrio, Debashis Mondal
# Description: This script cleans the 2018 Calls for Service data and runs Travel Time model
# analyses after additional data cleaning is done.
# -----------------------------------------------------------------------------

# -----------------------------
# 1. Libraries
# -----------------------------
library(dplyr)
library(readxl)
library(sf)
library(ggplot2)
library(spdep)
library(data.table)
library(effects)
library(car)
library(cowplot)
library(igraph)
library(ggridges)

# -----------------------------
# 2. File Paths
# -----------------------------
data_dir <- "~/"       # Put your Excel and shapefiles here
fig_dir <- "~/"     # Folder to save plots

# -----------------------------
# 3. Load Data
# -----------------------------
my_data <- read_excel(file.path(data_dir, "Calls_for_Service_2018.xlsx"))

# -----------------------------
# 4. Time Variables and Data Cleaning
# -----------------------------
my_data <- subset(my_data, my_data$SelfInitiated == "N")
my_data <- subset(my_data, my_data$Location != "(0.0, 0.0)")

time_dispatch <- (my_data$TimeDispatch)
time_arrive <- (my_data$TimeArrive)
time_create <- (my_data$TimeCreate)
time_close <- (my_data$TimeClosed)
time = data.frame()
time <- data.frame(
  time_arrive = time_arrive,
  time_create = time_create,
  time_dispatch = time_dispatch,
  time_close = time_close
)

time$time_create <- as.POSIXct(time$time_create, format = "%Y-%m-%dT%H:%M:%S")
time$time_arrive <- as.POSIXct(time$time_arrive, format = "%Y-%m-%dT%H:%M:%S")
time$time_dispatch <- as.POSIXct(time$time_dispatch, format = "%Y-%m-%dT%H:%M:%S")
time$time_close <- as.POSIXct(time$time_close, format = "%Y-%m-%dT%H:%M:%S")

my_data$ResponseTime <- as.numeric(difftime(time$time_arrive, time$time_create, 
                                            units = "mins"))
my_data$WaitTime <- as.numeric(difftime(time$time_dispatch, time$time_create, 
                                        units = "mins"))
my_data$HandlingTime <- as.numeric(difftime(time$time_arrive, time$time_dispatch, 
                                            units = "mins"))
# -----------------------------
# 5. Data Cleaning
# -----------------------------
my_data <- subset(my_data, my_data$WaitTime >= 0)

my_data <- na.omit(my_data)

my_data <- subset(my_data, my_data$ResponseTime >= 0)

my_data_NoZero <- subset(my_data, my_data$ResponseTime > 0)

# additional cleaning here
my_data_NoZero <- subset(my_data_NoZero, my_data_NoZero$HandlingTime > 0)

my_data_NoZero <- setDT(my_data_NoZero) # needs to be a data table to reformat time

my_data_NoZero[, TimeCreate := as.POSIXct(TimeCreate, format="%Y-%m-%dT%H:%M:%S")]

my_data_NoZero[, TimeDispatch := as.POSIXct(TimeDispatch, format="%Y-%m-%dT%H:%M:%S")]

# -----------------------------
# 6. Extract Latitude/Longitude
# -----------------------------
my_data_NoZero$Latitude <- as.numeric(sub("\\((.*?),.*", "\\1", my_data_NoZero$Location))

my_data_NoZero$Longitude <- as.numeric(sub(".*,\\s*(.*?)\\)", "\\1", my_data_NoZero$Location))

# Keep only rows with valid coordinates
my_data_NoZero <- my_data_NoZero[!is.na(my_data_NoZero$Latitude) & !is.na(my_data_NoZero$Longitude), ]

crime_sf <- st_as_sf(my_data_NoZero, coords = c("Longitude", "Latitude"), crs = 4326)

# -----------------------------
# 7. Load Neighborhood Shapefile
# -----------------------------
no.B <- st_read(file.path(data_dir, "harvard_NOLA.shp")) %>%
  st_transform(crs = 4326)

no.B<-st_transform(no.B,crs=4326)
no.B <- st_make_valid(no.B)
no.mat<-nb2mat(poly2nb(no.B), style = "B")

g0 <- graph_from_adjacency_matrix((no.mat+t(no.mat))/2, mode="undirected")

# -----------------------------
# 8. Assign Neighborhood Labels
# -----------------------------
lst <- st_intersects(crime_sf$geometry, no.B$geometry)
n <- nrow(crime_sf)
crime_sf$nbhd <- as.numeric(lst[1:n])
crime_sf$label <- no.B$gnocdc_lab[as.numeric(lst)]

crime_sf_final <- subset(crime_sf, crime_sf$nbhd> 0)

# -----------------------------
# 9. Recode Priority
# -----------------------------
zeros <- c("0B", "0C", "0E", "0H", "0Z")
ones <- c("1Z")
twos <- c("2G", "2H", "2J", "2Q")
threes <- c("3A", "3C")

crime_sf_final <- crime_sf_final %>%
  mutate(InitialPriority = case_when(
    InitialPriority %in% zeros ~ "0Other",
    InitialPriority %in% ones  ~ "1C",
    InitialPriority %in% twos  ~ "2Other",
    InitialPriority %in% threes ~ "3",
    TRUE ~ InitialPriority
  ))

# -----------------------------
# 10. Extract Hour
# -----------------------------
crime_sf_final$Hour <- 0
crime_sf_final$Hour <- format(crime_sf_final$TimeCreate, "%H")
crime_sf_final$Hour <- as.numeric(crime_sf_final$Hour)
class(crime_sf_final$Hour) = "character"

crime_sf_final$HandlingHour <- 0
crime_sf_final$HandlingHour <- format(crime_sf_final$TimeDispatch, "%H")
crime_sf_final$HandlingHour <- as.numeric(crime_sf_final$HandlingHour)
class(crime_sf_final$HandlingHour) = "character"

# -----------------------------
# 11. Prepare Variables for Analysis
# -----------------------------
yRT= log(crime_sf_final$ResponseTime)
yWT= log(crime_sf_final$WaitTime)
yHT= log(crime_sf_final$HandlingTime)

dispatch=crime_sf_final$WaitTime
dispatch= as.numeric(dispatch>0)
dispatch0=which(dispatch==0)
dispatch1=which(dispatch==1)

y0= yRT[dispatch0]
y00= yRT[dispatch1]
y1= yWT[dispatch1]
y2= yHT[dispatch1]

priority0=factor(crime_sf_final$InitialPriority[dispatch0])
priority1=factor(crime_sf_final$InitialPriority[dispatch1])

type0=crime_sf_final$InitialTypeText[dispatch0]
type1=crime_sf_final$InitialTypeText[dispatch1]

nbhd0=factor(crime_sf_final$nbhd[dispatch0])
nbhd1=factor(crime_sf_final$nbhd[dispatch1])

label0=factor(crime_sf_final$label[dispatch0])
label1=factor(crime_sf_final$label[dispatch1])

police0=factor(crime_sf_final$PoliceDistrict[dispatch0])
police1=factor(crime_sf_final$PoliceDistrict[dispatch1])

hour0=factor(crime_sf_final$Hour[dispatch0])
hour1=factor(crime_sf_final$Hour[dispatch1])

# Need different Hour for handling time because we calculate not at hour of call, but at hour of dispatch
handlinghour=factor(crime_sf_final$HandlingHour[dispatch1])

label0 <- factor(label0)
label0 <- relevel(label0, ref = 'LAKEVIEW')

police0 <- factor(police0)
police0 <- relevel(police0, ref = 4)

label1 <- factor(label1)
label1 <- relevel(label1, ref = 'LAKEVIEW')

# -----------------------------
# 16. Handling Time AFT Model
# -----------------------------
# Handling Time
RespTimeANOVA2 <- lm(y2 ~priority1+ police1+ label1+ label1:police1 + handlinghour)

# -----------------------------
# 17. Handling Time AFT Model ANOVA Table
# -----------------------------
Anova(RespTimeANOVA2)
