# -----------------------------------------------------------------------------
# Code for: "Dispatch Delays and Geographic Disparities in Police Response Times in New Orleans"
# Authors: Alexandra Sabrio, Debashis Mondal
# Description: This script creates plots for all AFT models in the paper.
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
# 3. Load AFT Model Data and 2018 Call Data
# -----------------------------
RespTimeANOVA0  <- readRDS(file.path(data_dir, "RespTimeANOVA0.rds"))
RespTimeANOVA00 <- readRDS(file.path(data_dir, "RespTimeANOVA00.rds"))
RespTimeANOVA1  <- readRDS(file.path(data_dir, "RespTimeANOVA1.rds"))
RespTimeANOVA2 <- readRDS(file.path(data_dir, "RespTimeANOVA2.rds"))

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
# 8. Ridge Plot Creation 
# -----------------------------
my_data_NoZero$DispatchTimeCategory <- 0

my_data_NoZero <- my_data_NoZero %>%
  mutate(DispatchTimeCategory = case_when(
    WaitTime == 0 ~ '0',
    WaitTime > 0 & WaitTime <= 10 ~ '0 to 10',
    WaitTime > 10 & WaitTime <= 20 ~ '10 to 20',
    WaitTime > 20 & WaitTime <= 30 ~ '20 to 30',
    WaitTime > 30 ~ '30 up'
  ))

my_data_NoZero$DispatchTimeCategory <- factor(my_data_NoZero$DispatchTimeCategory, levels = c("0", "0 to 10", "10 to 20", "20 to 30", "30 up"))

gg_ridge <- ggplot(my_data_NoZero, aes(x = log(ResponseTime),
                                       y = DispatchTimeCategory,
                                       fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 0.9) +
  scale_fill_viridis_c(option = "C") +
  labs(x = "log(ResponseTime) in minutes", y = "Dispatch Time Grouping in minutes", fill = '') +
  theme_minimal() +
  theme(#legend.justification = c(1, 0), legend.position = c(1, 0),
    legend.background = element_rect(colour = NA, fill = "white"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 10),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.7, "cm"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_line(linewidth = 0.3)
  )
gg_ridge

# -----------------------------
# 9. HoneyComb Plot of Police Response Time
# -----------------------------
mapping <- no.B 
city_combined <- st_union(mapping)
city_outline <- st_boundary(city_combined)

no.B_projected <- st_transform(city_combined, crs = 26915)

hexgrid <- st_make_grid(no.B_projected, cellsize = 200, what = 'polygons',
                        square = FALSE) |> st_as_sf()

hexgrid_NO <- hexgrid[st_intersects(hexgrid, no.B_projected, sparse = FALSE),]
hexgrid_NO$CallCount 

my_data <- st_as_sf(my_data_NoZero, coords = c("Longitude", "Latitude"), crs = 4326)

calls_sf <- st_transform(my_data, crs = st_crs(no.B_projected))
calls_sf$x <- calls_sf$geometry

hexgrid_with_calls <- st_join(hexgrid_NO, calls_sf, join = st_intersects)

hexgrid_with_counts <- hexgrid_with_calls %>%
  group_by(x.x) %>%
  summarize(avg_response_time = mean(log(ResponseTime), na.rm = FALSE))

hexgrid_with_counts_df <- as.data.frame(hexgrid_with_counts)

gg_honey_time <- ggplot() +
  geom_sf(data = hexgrid_with_counts,
          aes(fill = avg_response_time),
          color = NA,
          size = 0.1) +
  geom_sf(data = no.B_projected, fill = NA, color = 'black', size = 0.1) +
  scale_fill_viridis_c(option = "C") +
  labs(fill = "", size = 5) +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position.inside = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(0.7, "cm"),
        axis.text.x = element_blank(), #element_text(size = 5),
        axis.text.y = element_blank(), #element_text(size = 5),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )
gg_honey_time




# -----------------------------
# 10. Plot of AFT Model Priority Coefficients 
# -----------------------------
# Immediate ANOVA
coef_names0 <- rownames(summary(RespTimeANOVA0)$coefficients)

coef_priority0 <- data.frame(
  Variable = names(RespTimeANOVA0$coefficients[
    grep("^priority0", names(RespTimeANOVA0$coefficients))]),
  Coefficient = RespTimeANOVA0$coefficients[
    grep("^priority0", names(RespTimeANOVA0$coefficients)) %>%
      intersect(grep(":", names(RespTimeANOVA0$coefficients), invert = TRUE))],
  SE = summary(RespTimeANOVA0)$coefficients[grep("^priority0", names(RespTimeANOVA0$coefficients)) %>%
                                              intersect(grep(":", names(RespTimeANOVA0$coefficients), invert = TRUE)), "Std. Error"]
)

coef_priority0$CleanVariable <- gsub("^priority0", "", coef_priority0$Variable)

coef_priority0$CI_Lower <- coef_priority0$Coefficient - 1.96 * coef_priority0$SE
coef_priority0$CI_Upper <- coef_priority0$Coefficient + 1.96 * coef_priority0$SE

# NonImmediate ANOVA
coef_names00 <- rownames(summary(RespTimeANOVA00)$coefficients)

coef_priority00 <- data.frame(
  Variable = names(RespTimeANOVA00$coefficients[
    grep("^priority1", names(RespTimeANOVA00$coefficients))]),
  Coefficient = RespTimeANOVA00$coefficients[
    grep("^priority1", names(RespTimeANOVA00$coefficients)) %>%
      intersect(grep(":", names(RespTimeANOVA00$coefficients), invert = TRUE))],
  SE = summary(RespTimeANOVA00)$coefficients[grep("^priority1", names(RespTimeANOVA00$coefficients)) %>%
                                               intersect(grep(":", names(RespTimeANOVA00$coefficients), invert = TRUE)), "Std. Error"]
)

coef_priority00$CleanVariable <- gsub("^priority1", "", coef_priority00$Variable)

coef_priority00$CI_Lower <- coef_priority00$Coefficient - 1.96 * coef_priority00$SE
coef_priority00$CI_Upper <- coef_priority00$Coefficient + 1.96 * coef_priority00$SE

# Wait Time ANOVA
coef_names1 <- rownames(summary(RespTimeANOVA1)$coefficients)

coef_priority1 <- data.frame(
  Variable = names(RespTimeANOVA1$coefficients[
    grep("^priority1", names(RespTimeANOVA1$coefficients))]),
  Coefficient = RespTimeANOVA1$coefficients[
    grep("^priority1", names(RespTimeANOVA1$coefficients)) %>%
      intersect(grep(":", names(RespTimeANOVA1$coefficients), invert = TRUE))],
  SE = summary(RespTimeANOVA1)$coefficients[grep("^priority1", names(RespTimeANOVA1$coefficients)) %>%
                                              intersect(grep(":", names(RespTimeANOVA1$coefficients), invert = TRUE)), "Std. Error"]
)

coef_priority1$CleanVariable <- gsub("^priority1", "", coef_priority1$Variable)

coef_priority1$CI_Lower <- coef_priority1$Coefficient - 1.96 * coef_priority1$SE
coef_priority1$CI_Upper <- coef_priority1$Coefficient + 1.96 * coef_priority1$SE

# Handling Time ANOVA
coef_names2 <- rownames(summary(RespTimeANOVA2)$coefficients)

coef_priority2 <- data.frame(
  Variable = names(RespTimeANOVA2$coefficients[
    grep("^priority1", names(RespTimeANOVA2$coefficients))]),
  Coefficient = RespTimeANOVA2$coefficients[
    grep("^priority1", names(RespTimeANOVA2$coefficients)) %>%
      intersect(grep(":", names(RespTimeANOVA2$coefficients), invert = TRUE))],
  SE = summary(RespTimeANOVA2)$coefficients[grep("^priority1", names(RespTimeANOVA2$coefficients)) %>%
                                              intersect(grep(":", names(RespTimeANOVA2$coefficients), invert = TRUE)), "Std. Error"]
)

coef_priority2$CleanVariable <- gsub("^priority1", "", coef_priority2$Variable)

coef_priority2$CI_Lower <- coef_priority2$Coefficient - 1.96 * coef_priority2$SE
coef_priority2$CI_Upper <- coef_priority2$Coefficient + 1.96 * coef_priority2$SE

coef_priority0$Model <- "Immediate"
coef_priority00$Model <- "Non-Immediate"
coef_priority1$Model <- "Wait"
coef_priority2$Model <- "Travel"

combined_priority <- bind_rows(
  coef_priority0,
  coef_priority00,
  coef_priority1,
  coef_priority2
)

combined_priority_plot <- ggplot(combined_priority, 
                                 aes(x = CleanVariable, 
                                     y = Coefficient,
                                     color = Model, shape = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  scale_shape_manual(values = c("Immediate" = 16,
                                "Non-Immediate" = 17,
                                "Wait" = 15,
                                "Travel" = 18)) +  # customize as needed
  labs(x = "Priority Level", y = "Coefficient Estimate") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank()
  )
combined_priority_plot



# -----------------------------
# 11. Plot of AFT Model Neighborhood Coefficients 
# -----------------------------
# Immediate
coef_names0 <- rownames(summary(RespTimeANOVA0)$coefficients)

coef_nbhds0 <- RespTimeANOVA0$coefficients[
  grep("^label0", names(RespTimeANOVA0$coefficients)) %>%
    intersect(grep(":", names(RespTimeANOVA0$coefficients), invert = TRUE))]

coef_df0 <- data.frame(
  gnocdc_lab = names(coef_nbhds0),
  Estimate = coef_nbhds0
)

coef_df0$gnocdc_lab <- sub("label0", "", coef_df0$gnocdc_lab)

coef_sf0 <- no.B %>%
  left_join(coef_df0, by = c("gnocdc_lab"))

# NonImmediate
coef_names00 <- rownames(summary(RespTimeANOVA00)$coefficients)

coef_nbhds00 <- RespTimeANOVA00$coefficients[
  grep("^label1", names(RespTimeANOVA00$coefficients)) %>%
    intersect(grep(":", names(RespTimeANOVA00$coefficients), invert = TRUE))]

coef_df00 <- data.frame(
  gnocdc_lab = names(coef_nbhds00),
  Estimate = coef_nbhds00
)

coef_df00$gnocdc_lab <- sub("label1", "", coef_df00$gnocdc_lab)

coef_sf00 <- no.B %>%
  left_join(coef_df00, by = c("gnocdc_lab"))

# Wait Time
coef_names1 <- rownames(summary(RespTimeANOVA1)$coefficients)

coef_nbhds1 <- RespTimeANOVA1$coefficients[
  grep("^label1", names(RespTimeANOVA1$coefficients)) %>%
    intersect(grep(":", names(RespTimeANOVA1$coefficients), invert = TRUE))]

coef_df1 <- data.frame(
  gnocdc_lab = names(coef_nbhds1),
  Estimate = coef_nbhds1
)

coef_df1$gnocdc_lab <- sub("label1", "", coef_df1$gnocdc_lab)

coef_sf1 <- no.B %>%
  left_join(coef_df1, by = c("gnocdc_lab"))

# Handling Time
coef_names2 <- rownames(summary(RespTimeANOVA2)$coefficients)

coef_nbhds2 <- RespTimeANOVA2$coefficients[
  grep("^label1", names(RespTimeANOVA2$coefficients)) %>%
    intersect(grep(":", names(RespTimeANOVA2$coefficients), invert = TRUE))]

coef_df2 <- data.frame(
  gnocdc_lab = names(coef_nbhds2),
  Estimate = coef_nbhds2
)

coef_df2$gnocdc_lab <- sub("label1", "", coef_df2$gnocdc_lab)

coef_sf2 <- no.B %>%
  left_join(coef_df2, by = c("gnocdc_lab"))

# change inputs for different models
T0_nbhd_estimate_plot <- ggplot(data = coef_sf0) +
  geom_sf(aes(fill = Estimate)) +
  scale_fill_viridis_c(option = "C") +
  labs(fill = "", size = 5) +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.width = unit(0.4, "cm"),   
        legend.key.height = unit(0.7, "cm"),
        axis.text.x = element_blank(), #element_text(size = 8),
        axis.text.y = element_blank(), #element_text(size = 8),
        axis.title.x = element_blank(), #element_text(size = 9),
        axis.title.y = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )

T00_nbhd_estimate_plot <- ggplot(data = coef_sf00) +
  geom_sf(aes(fill = Estimate)) +
  scale_fill_viridis_c(option = "C") +
  labs(fill = "", size = 5) +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.width = unit(0.4, "cm"),   
        legend.key.height = unit(0.7, "cm"),
        axis.text.x = element_blank(), #element_text(size = 8),
        axis.text.y = element_blank(), #element_text(size = 8),
        axis.title.x = element_blank(), #element_text(size = 9),
        axis.title.y = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )

T1_nbhd_estimate_plot <- ggplot(data = coef_sf1) +
  geom_sf(aes(fill = Estimate)) +
  scale_fill_viridis_c(option = "C") +
  labs(fill = "", size = 5) +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.width = unit(0.4, "cm"),   
        legend.key.height = unit(0.7, "cm"),
        axis.text.x = element_blank(), #element_text(size = 8),
        axis.text.y = element_blank(), #element_text(size = 8),
        axis.title.x = element_blank(), #element_text(size = 9),
        axis.title.y = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )

T2_nbhd_estimate_plot <- ggplot(data = coef_sf2) +
  geom_sf(aes(fill = Estimate)) +
  scale_fill_viridis_c(option = "C") +
  labs(fill = "", size = 5) +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.width = unit(0.4, "cm"),   
        legend.key.height = unit(0.7, "cm"),
        axis.text.x = element_blank(), #element_text(size = 8),
        axis.text.y = element_blank(), #element_text(size = 8),
        axis.title.x = element_blank(), #element_text(size = 9),
        axis.title.y = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )


# Combine Immediate and Non-Immediate spatial coefficient plots
T0_nbhd_estimate_plot <- T0_nbhd_estimate_plot + coord_sf(expand = FALSE)
T00_nbhd_estimate_plot <- T00_nbhd_estimate_plot + coord_sf(expand = FALSE)

T0_nbhd_estimate_plot <- T0_nbhd_estimate_plot + theme(plot.margin = margin(0, 0, 0, 0))
T00_nbhd_estimate_plot <- T00_nbhd_estimate_plot + theme(plot.margin = margin(0, 0, 0, 0))

T0_nbhd_estimate_plot <- T0_nbhd_estimate_plot +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = alpha('white', 0.7), color = NA)
  )

T00_nbhd_estimate_plot <- T00_nbhd_estimate_plot + theme(legend.position = "none")

combined_plot <- plot_grid(
  T0_nbhd_estimate_plot,
  T00_nbhd_estimate_plot,
  ncol = 2,
  rel_widths = c(1, 1)
) 
combined_plot


# Combine Wait and Travel Time coefficient plots
T1_nbhd_estimate_plot <- T1_nbhd_estimate_plot + coord_sf(expand = FALSE)
T2_nbhd_estimate_plot <- T2_nbhd_estimate_plot + coord_sf(expand = FALSE)

T1_nbhd_estimate_plot <- T1_nbhd_estimate_plot + theme(plot.margin = margin(0, 0, 0, 0))
T2_nbhd_estimate_plot <- T2_nbhd_estimate_plot + theme(plot.margin = margin(0, 0, 0, 0))

T1_nbhd_estimate_plot <- T1_nbhd_estimate_plot +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = alpha('white', 0.7), color = NA)
  )

T2_nbhd_estimate_plot <- T2_nbhd_estimate_plot + theme(legend.position = "none")

combined_plotWT <- plot_grid(
  T1_nbhd_estimate_plot,
  T2_nbhd_estimate_plot,
  ncol = 2,
  rel_widths = c(1, 1)
)
combined_plotWT



# -----------------------------
# 12. Plots of Nested Police and Neighborhood Effects 
# -----------------------------
E0=effect("police0:label0",RespTimeANOVA0)

E1=effect("police1:label1",RespTimeANOVA1)

E2=effect("police1:label1",RespTimeANOVA2)

E3=effect("police1:label1",RespTimeANOVA00)

plot_df0 <- as.data.frame(E0)
plot_df00 <- as.data.frame(E1)
plot_df1 <- as.data.frame(E2)
plot_df2 <- as.data.frame(E3)

# immediate AFT effects
policenbhd0 <- ggplot(plot_df0, aes(x = label0, y = fit, group = police0, color = police0)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 9), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(
    x = "Neighborhood",
    y = "Effect",
    color = ""
  )
policenbhd0

# non-immediate AFT effects
policenbhd00 <- ggplot(plot_df00, aes(x = label1, y = fit, group = police1, color = police1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 9), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(
    x = "Neighborhood",
    y = "Effect",
    color = ""
  )
policenbhd00

# wait time AFT effects
policenbhd1 <- ggplot(plot_df1, aes(x = label1, y = fit, group = police1, color = police1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 9), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(
    x = "Neighborhood",
    y = "Effect",
    color = ""
  )
policenbhd1

# handling time AFT effects 
policenbhd2 <- ggplot(plot_df2, aes(x = label1, y = fit, group = police1, color = police1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 9), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(
    x = "Neighborhood",
    y = "Effect",
    color = ""
  )
policenbhd2

