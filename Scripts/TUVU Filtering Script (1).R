#Author: Alec Iskenderian, aiskenderian@cpp.edu
#date created: May 2022
#date modified: May 2022
###adapted from: Data Cleaning with R and the Tidyverse: Detecting Missing Values, How to Remove Outliers in R, and How to Remove Rows in R(With Examples)
#https://towardsdatascience.com/data-cleaning-with-r-and-the-tidyverse-detecting-missing-values-ea23c519bc62
#https://www.programmingr.com/content/remove-outliers-in-r/
#https://www.statology.org/remove-rows-in-r/

library(tidyverse)
df <- TUVU.Lead_0412_2022
glimpse(df)
#View missing values from GPS error
is.na((df$location.long))

#Omit rows with missing geographical coordinates 
new_df <- na.omit(df)
view(new_df)
write.csv(new_df, "Modified_TUVU_data.csv")

#Create a boxplot to view outliers
boxplot(new_df$argos.altitude)


install.packages("ggstatsplot")
library(ggstatsplot)
data("new_df")

#Identify and visualize argos.altitude outliers in df
boxplot(new_df$argos.altitude)$out

#Find outlliers using statistical methods
Q <- quantile(new_df$argos.altitude, probs = c(.25, .75), na.rm = FALSE)
iqr <- IQR(new_df$argos.altitude)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???

#Elimate OUtliers using subset function 
eliminated<- subset(new_df, new_df$argos.altitude > (Q[1] - 1.5*iqr) & warpbreaks$breaks < (Q[2]+1.5*iqr))

#Visualize any remaining outliers
boxplot(eliminated)$out
#Subset until outliers no longer remain
new_new_df <- subset(eliminated, argos.altitude<3157)
#Final view of boxplot without outliers
boxplot(new_new_df$argos.altitude)$out
write.csv(new_new_df, "New_Modified_TUVU_data.csv")

attach(New_Modified_TUVU_data)

install.packages("scatterplot3d")
library(scatterplot3d)
attach(New_Modified_TUVU_data)
scatterplot3d(location.long, location.lat, argos.altitude,
              highlight.3d = TRUE, angle = -100,
              type = "h", main = "3D Scatterplot Example"
)

library(dplyr)
arrange(New_Modified_TUVU_data, argos.altitude)
arrange(New_Modified_TUVU_data, argos.altitude)
arrange(New_Modified_TUVU_data, desc(argos.altitude))

persp(New_Modified_TUVU_data$location.lat, New_Modified_TUVU_data$location.long, New_Modified_TUVU_data$argos.altitude)
install.packages("plotly")
library(plotly)
library(ggplot2)
?ggplot2

ggplot(New_Modified_TUVU_data, aes(location.lat, location.long)) + 
  geom_path()

New_Modified_TUVU_data >%> 
  identify_outliers(argos.altitude)
?identify_outliers
??identify_outliers
plot(New_Modified_TUVU_data, type = "l")
geom_path(data = New_Modified_TUVU_data, 
          aes(x=location.long,y=location.lat,group=id,color=spd), 
          alpha = 0.3)+
  geom_point(data = New_Modified_TUVU_data, 
             aes(x=location.long,y=location.lat,group=id,fill=spd),
             alpha = 0.7, shape=21, size = 2)+
  # formatting
  scale_fill_viridis_c(option = "inferno")+
  scale_color_viridis_c(option = "inferno")+
  scale_size_continuous(range = c(0.1,10))+
  labs(x=NULL, y=NULL, 
       fill = 'Speed (m/s)', 
       color = 'Speed (m/s)')+
  theme_dark()+
  theme(panel.grid = element_blank())

install.packages("leaflet")
library(leaflet)

df_lonlat <- data.frame(latitude = New_Modified_TUVU_data$location.lat,
                        longitude = New_Modified_TUVU_data$location.long)

sf_lonlat <- sf::st_as_sf(x = df_lonlat,
                          coords = c("longitude", "latitude"),
                          crs = 4326) # https://epsg.io/4326

# switch coordinates to Cartesian coordinate system (i.e., x,y format)
# for other countries target coordinate reference system (crs) will be different
# this transformation is used because you can't use DBSCAN for Long/Lat numbers
sf_lonlat <- sf::st_transform(x = sf_lonlat,
                              crs = 3059) # https://epsg.io/3059

install.packages("fpc")
library(fpc)

install.packages("usethis")
library(usethis) 
usethis::edit_r_environ()
dist(df_lonlat, method = "euclidean")
# Calculate DBSCAN clusters (and find outliers)                 
dbscan_result <- fpc::dbscan(sf::st_coordinates(sf_lonlat), eps = 10, MinPts = 4)
                                eps = 10, # 10 meter distance. Points with larger distance will be classified in different cluster or will be classified as outlier
                                minPts = 2) # here used 2 points, because df_lonlat has only 10 rows - small sample.
??minPts
# Add DBSCAN clusters to df_lonlat and plot results
df_lonlat$cluster <- dbscan_result$cluster
# "0 cluster" are outliers, all other cluster numbers can be ignored 


ggplot(df_lonlat, mapping = aes(x = latitude, y = longitude,
                                color = as.factor(cluster))) +
  geom_point() +
  labs(x = 'Latitude',
       y = 'Longitude',
       title = 'Long/Lat coordinates and outliers',
       color = 'Cluster') +
  theme_light()




new_df_lonlat <- filter(df_lonlat, cluster > 0)       

ggplot(new_df_lonlat, aes(latitude, longitude)) + 
  geom_path()