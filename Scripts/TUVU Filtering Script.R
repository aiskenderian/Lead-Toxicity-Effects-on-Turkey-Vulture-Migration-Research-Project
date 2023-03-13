#Author: Alec Iskenderian, aiskenderian@cpp.edu
#date created: May 2022
#date modified: August 2022
###adapted from: Data Cleaning with R and the Tidyverse: Detecting Missing Values, How to Remove Outliers in R, and How to Remove Rows in R(With Examples)
#https://towardsdatascience.com/data-cleaning-with-r-and-the-tidyverse-detecting-missing-values-ea23c519bc62
#https://www.programmingr.com/content/remove-outliers-in-r/
#https://www.statology.org/remove-rows-in-r/

#Convert datasets to data frame
library(tidyverse)
df_7783 <- data.frame(`7783`)
df_8096 <- data.frame(`89460800120113348096`)
df_0134 <- data.frame(`89460800120113350134`)
df_44665 <- data.frame(`89460800120136624465.2`)
glimpse(df)

#Add column to idenitify bird in DynamoVis
df_0134$Name <- c(0134)
df_44665$Name <- c(665)
df_7783$Name <- c(783)
df_8096$Name <- c(096)


#Remove GPS errors where lat/lon = 0
library(dplyr)
tuvu_filter_96 = filter(df_8096, lon != "0")
tuvu_filter_7783 = filter(df_7783, lon!= "0")
tuvu_filter_4665 = filter(df_4465, lon!= "0" )
tuvu_filter_0134 = filter(df_0134, lon!= "0")

#View GPS lat/lon where GPS errors are 
##Repeat for each bird set
library(ggplot2)
library(plotly)

p <- ggplot(data = tuvu_filter_7783, aes(x = lat, y = lon)) +
  geom_line(colour = "grey", aes(lat, lon)) +
  geom_line(colour = "#408FA6")
ggplotly(p)


#Filter out GPS errors 
tuvu_filter_0134_Final =filter(tuvu_filter_0134, lon != "-120.381241", lon != "-121.391991")
tuvu_filter_4665_Final =filter(tuvu_filter_4665, lon != "-117.824532")
tuvu_filter_96_Final =filter(tuvu_filter_96, GPS_YYYY.MM.DD_HH.MM.SS != "2021-05-11 21:51:57")
tuvu_filter_7783_Final =filter(tuvu_filter_7783, GPS_YYYY.MM.DD_HH.MM.SS != "2021-06-12 19:44:59", GPS_YYYY.MM.DD_HH.MM.SS != "2021-07-01 20:18:40", GPS_YYYY.MM.DD_HH.MM.SS != "2021-06-16 00:58:07")

#View plot to see if remaining GPS errors & fix if necessary
library(ggplot2)
library(plotly)

p <- ggplot(data = tuvu_filter_7783_Final, aes(x = lat, y = lon)) +
  geom_line(colour = "grey", aes(lat, lon)) +
  geom_line(colour = "#408FA6")
ggplotly(p)

#Remove extra columns if "filter" function adds X column 
tuvu_filter_4665_Final <- tuvu_filter_4665_Final %>% select(-X)
tuvu_filter_7783_Final <- tuvu_filter_7783_Final %>% select(-X)

#Bind data frames
df_list <- rbind(tuvu_filter_0134_Final, tuvu_filter_4665_Final, tuvu_filter_7783_Final, tuvu_filter_96_Final)

#Export combined data for DynamoVis
write.csv(df_list, "tuvu_combined_final.csv")
