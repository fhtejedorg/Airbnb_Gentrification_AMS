##########################################################################
# # R version:
# # File Name: 
# # Author:
# # Process:
# # Inputs:
# # Outputs:
# # File history:
##########################################################################
setwd("/Users/fabiotejedor/Documents/TU_Delft_University/Thesis_Project/Thesis\ Preparation/Thesis_Airbnb_Disruption/Methodology/Data")

##########################################################################
## loading packages
##########################################################################
library(tidyverse)
library(rgdal)
library(xlsx)
library(readxl)
library(dplyr)
library(tigris)
library(ggplot2)
library(mapproj)
library(maps)
library(ggthemes)
library(gridExtra)
library(ggpubr)
library(tidyr)
library(rlang)
library(rgeos)
library(EnvStats)
library(sf)
library(tmap)
library(sf)
library(mapview)
library(geojsonio)
library(spatialEco)
library(FRK)
library(FactoMineR)
library(psych)
library(mice)
library(reshape2)
library(hrbrthemes)
library(viridis)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# #  functions 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
rm(list = ls())
fun_cut <- function(x, nCut=6) {
  z <- cut(x,quantile(x, seq(0, 1, len = nCut), na.rm = T), 
           include.lowest = TRUE, dig.lab = 10, ordered_result = T)
  z
}

fun_sta <- function(x){
  z <- (x- mean(x, na.rm = T))/sd(x, na.rm = T)
  z
}
# # # Loading information steps 3 and 4 

load("../Output/Chapter_1/Step3/dataList_airbnb.Rdata")
load("../Output/Chapter_1/Step3/data_airbnb_4.Rdata")
load("../Output/Chapter_1/Step3/dataList_airbnb_sf.Rdata")
load("../Output/Chapter_1/Step3/dataList_airbnb_4sf.Rdata")
load("../Output/Chapter_1/Step3/map_AMS_wijken_sf.Rdata")

load("../Output/Chapter_1/Step4/data_IND1_02I.Rdata")
load("../Output/Chapter_1/Step4/data_IND2_02I.Rdata")
colIntOrRd <- c('#fef0d9','#fdcc8a','#fc8d59','#e34a33','#b30000') #  quintiles
colIntRdBu1 <- c("#fdae61", "#67a9cf") # colors gentrified 
colIntAccent <- c('#7fc97f','#beaed4','#fdc086') # colors gentrified combined 
colIntCatSES <- c("#fc8d59", "#ffffbf", "#91bfdb")
map_AMS_district <- readOGR(dsn = "./GIS/geojson_district", layer = "geojson_districts-polygon")
list_year = 2015:2018
# comparision neighborhoods already nentrified 

table(map_AMS_wijken_sf$gentryABnB_opDef,
map_AMS_wijken_sf$gentry_opDef)
map_AMS_wijken_sf$gentryABnBSES <- with(map_AMS_wijken_sf, 
                                        ifelse(gentry_opDef == "Gentrified" & gentryABnB_opDef == "Gentrified", "Income+ABnB",
                                               ifelse(gentry_opDef == "Gentrified" & gentryABnB_opDef == "Other Neighborhoods", "Only Income",
                                                      ifelse(gentry_opDef == "Other Neighborhoods" & gentryABnB_opDef == "Gentrified", "Only ABnB",
                                                             ifelse(gentry_opDef == "Other Neighborhoods" & gentryABnB_opDef == "Other Neighborhoods",NA, NA)))))

legend_title = expression("Gentrified: Income + ABnB")
map_GentBoth <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "gentryABnBSES", title = legend_title, palette = "seq", colorNA = NULL)  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_layout(frame = FALSE, inner.margins = 0.1, aes.palette = list(seq = "RdYlBu"))
tmap_save(map_GentBoth, "../Output/Chapter_1/Step5/Map_GentrificationAIRBNB_INCOME.png", width=1920, height=1080, asp=0)


ttt <- map_AMS_wijken_sf %>% filter(!is.na(gentryABnBSES)) %>% select(gebiedcode15, Buurtcom_1, Stadsdeel, gentryABnBSES, gentry_opDef,  gentryABnB_opDef)
ttt <- data.frame(ttt)[, -ncol(ttt)]

ttt <- map_AMS_wijken_sf %>% select(gebiedcode15, Buurtcom_1, Stadsdeel, gentryABnBSES, gentry_opDef,  gentryABnB_opDef)
ttt <- data.frame(ttt)[, -ncol(ttt)]

# # comparison indicators and gentrification 
ggplot(data_IND1_02I, aes(x = jaar, y = IND1_1, fill = gentry_opDef))+
  geom_hline(yintercept = 50, col = gray(0.8), linetype="dashed")+ 
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  facet_wrap(~gentry_opDef) + 
  scale_y_continuous(name = "Residential Mobility", limits = c(20, 80), breaks = seq(20, 80, by = 15)) + 
  xlab("Year") + 
  scale_fill_manual(name = NULL,  values =  colIntRdBu1, labels = c("Gentrified", "Other Neigh."))+ 
  theme(text = element_text(size = 15), legend.position="bottom")
ggsave("../Output/Chapter_1/Step5/GentryIncomeVSIndicator1_1.png")

ggplot(data_IND1_02I, aes(x = jaar, y = IND1_2, fill = gentry_opDef))+
  geom_hline(yintercept = 50, col = gray(0.8), linetype="dashed")+ 
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  facet_wrap(~gentry_opDef) + 
  scale_y_continuous(name = "Foreign Vibrant", limits = c(20, 80), breaks = seq(20, 80, by = 15)) + 
  xlab("Year") + 
  scale_fill_manual(name = NULL,  values =  colIntRdBu1, labels = c("Gentrified", "Other Neigh."))+ 
  theme(text = element_text(size = 15), legend.position="bottom")
ggsave("../Output/Chapter_1/Step5/GentryIncomeVSIndicator1_2.png")


ggplot(data_IND2_02I, aes(x = jaar, y = IND2_1, fill = gentry_opDef))+
  geom_hline(yintercept = 50, col = gray(0.9), linetype="dashed")+ 
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  facet_wrap(~gentry_opDef) + 
  scale_y_continuous(name = "Housing Marketization", limits = c(20, 80), breaks = seq(20, 80, by = 15)) + 
  xlab("Year") + 
  scale_fill_manual(name = NULL,  values =  colIntRdBu1, labels = c("Gentrified", "Other Neigh."))+ 
  theme(text = element_text(size = 15), legend.position="bottom")
ggsave("../Output/Chapter_1/Step5/GentryIncomeVSIndicator2_1.png")

ggplot(data_IND2_02I, aes(x = jaar, y = IND2_2, fill = gentry_opDef))+
  geom_hline(yintercept = 50, col = gray(0.9), linetype="dashed")+ 
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  facet_wrap(~gentry_opDef) + 
  scale_y_continuous(name = "Housing Acquisition Interest", limits = c(30, 80), breaks = seq(20, 80, by = 15)) + 
  xlab("Year") + 
  scale_fill_manual(name = NULL,  values =  colIntRdBu1, labels = c("Gentrified", "Other Neigh."))+ 
  theme(text = element_text(size = 15), legend.position="bottom")
ggsave("../Output/Chapter_1/Step5/GentryIncomeVSIndicator2_2.png")



## number of listings per each neighborhood identified 
## comparison with housing stock 

### Relationship between indicators correlations 
data_IND1_02IA <- data_IND1_02I[, c("gebiedcode15", "IND1_1", "IND1_2")] %>% group_by(gebiedcode15) %>% summarise_at(c("IND1_1", "IND1_2"), mean, na.rm = T)
data_IND2_02IA <- data_IND2_02I[, c("gebiedcode15", "IND2_1", "IND2_2")] %>% group_by(gebiedcode15) %>% summarise_at(c("IND2_1", "IND2_2"), mean, na.rm = T)
data_IND_02IA <- merge(data_IND1_02IA, data_IND2_02IA)
round(cor(data_IND_02IA[, -1]), 2)
data_airbnb_4$dist_centre2 <- as.numeric(data_airbnb_4$dist_centre)

data_LiPrABnB <- data_airbnb_4 %>% group_by(year, Buurtcombi) %>% summarise(nList = length(unique(id)),
                                                           averPrice = mean(price, na.rm = T),
                                                           averDistance = mean(dist_centre2, na.rm = T))

data_LiPrABnB <- data_LiPrABnB %>% filter(year < 2019)
dddList <- dcast(data_LiPrABnB, Buurtcombi ~year, value.var = "nList")
dddAvPrice <- dcast(data_LiPrABnB, Buurtcombi ~year, value.var = "averPrice")
tttPrice <- merge(ttt, dddAvPrice, by.x = "gebiedcode15", by.y = "Buurtcombi")
tttList <- merge(ttt, dddList, by.x = "gebiedcode15", by.y = "Buurtcombi")
data_LiPrABnB <- merge(data_LiPrABnB, ttt, by.y = "gebiedcode15", by.x = "Buurtcombi", all.x = T)
data_LiPrABnB <- merge(data_LiPrABnB, 
                       data_IND1_02I[, c("jaar", "gebiedcode15", "IND1_1", "IND1_2", "IND1_1_Q", "IND1_2_Q")],
                       by.x = c("year", "Buurtcombi"), 
                       by.y = c("jaar", "gebiedcode15"), 
                       all.x = T)

data_LiPrABnB <- merge(data_LiPrABnB, 
                       data_IND2_02I[, c("jaar", "gebiedcode15", "IND2_1", "IND2_2", "IND2_1_Q", "IND2_2_Q")],
                       by.x = c("year", "Buurtcombi"), 
                       by.y = c("jaar", "gebiedcode15"), 
                       all.x = T)

data_LiPrABnB <- merge(data_LiPrABnB, 
                       map_AMS_wijken_sf[, c("Buurtcombi", "category_SES", "aver_gr", "slope_SD")], all.x = T)
data_LiPrABnB$year <- as.factor(data_LiPrABnB$year)

dd <- ggplot(subset(data_LiPrABnB, !is.na(gentry_opDef)), aes(x = year, y = nList, fill = gentry_opDef))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Total Listings-Airbnb", limits = c(0, 1000), breaks = seq(0, 1000, by = 250)) + 
  scale_fill_manual(name = NULL,  values =  colIntRdBu1, labels = c("Gentrified", "Other Neigh."))+ 
  theme(text = element_text(size = 25), legend.position="bottom")
ggsave("../Output/Chapter_1/Step5/GentryIncomeVSListings.png", plot = dd)

dd <- ggplot(subset(data_LiPrABnB, !is.na(gentry_opDef)), aes(x = year, y = averPrice, fill = gentry_opDef))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Average price per night", limits = c(65, 175), breaks = seq(50, 175, by = 25)) + 
  scale_fill_manual(name = NULL,  values =  colIntRdBu1, labels = c("Gentrified", "Other Neigh."))+ 
  theme(text = element_text(size = 25), legend.position="bottom")
ggsave("../Output/Chapter_1/Step5/GentryIncomeVSPrice.png",  plot = dd)

dd  <- ggplot(subset(data_LiPrABnB, !is.na(gentryABnBSES)), aes(x = year, y = nList, fill = gentryABnBSES))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Total Listings-Airbnb", limits = c(0, 1000), breaks = seq(0, 1000, by = 250)) + 
  scale_fill_manual(name = "Gentrified Neigh.\nCombination",  values =  colIntAccent, labels = c("Income and Price Airbnb", "Only Airbnb", "Only Income"))
ggsave("../Output/Chapter_1/Step5/GentrySESABnBVSListings.png", plot = dd)

dd <- ggplot(subset(data_LiPrABnB, !is.na(gentryABnBSES)), aes(x = year, y = averPrice, fill = gentryABnBSES))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Average price per night", limits = c(65, 175), breaks = seq(50, 175, by = 25)) + 
  scale_fill_manual(name = "Gentrified Neigh.\nCombination",  values =  colIntAccent, labels = c("Income and Price Airbnb", "Only Airbnb", "Only Income"))
ggsave("../Output/Chapter_1/Step5/GentrySESABnBVSPrice.png", plot = dd)

dd <- ggplot(subset(data_LiPrABnB, !is.na(IND1_1_Q)), aes(x = year, y = nList, fill = IND1_1_Q))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Total Listings-Airbnb", limits = c(0, 1000), breaks = seq(0, 1000, by = 250)) + 
  scale_fill_manual(name = "Residential\nMobility",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 25), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HouseholdChar_1VSList.png", plot = dd, width = 10)

dd <- ggplot(subset(data_LiPrABnB, !is.na(IND1_1_Q)), aes(x = year, y = averPrice, fill = IND1_1_Q))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Average price per night", limits = c(65, 175), breaks = seq(50, 175, by = 25)) + 
  scale_fill_manual(name = "Residential\nMobility",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 25), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HouseholdChar_1VSPrice.png", plot = dd, width = 10)

dd <- ggplot(subset(data_LiPrABnB, !is.na(IND1_2_Q)), aes(x = year, y = nList, fill = IND1_2_Q))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Total Listings-Airbnb", limits = c(0, 1000), breaks = seq(0, 1000, by = 250)) + 
  scale_fill_manual(name = "Foreign\nVibrant",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 25), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HouseholdChar_2VSList.png", plot = dd, width = 10)

dd <- ggplot(subset(data_LiPrABnB, !is.na(IND2_1_Q)), aes(x = year, y = averPrice, fill = IND1_2_Q))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Average price per night", limits = c(65, 175), breaks = seq(50, 175, by = 25)) + 
  scale_fill_manual(name = "Foreign\nVibrant",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 25), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HouseholdChar_2VSPrice.png", plot = dd, width = 10)


dd <- ggplot(subset(data_LiPrABnB, !is.na(IND2_2_Q)), aes(x = year, y = nList, fill = IND2_1_Q))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Total Listings-Airbnb", limits = c(0, 1000), breaks = seq(0, 1000, by = 250)) + 
  scale_fill_manual(name = "Housing\nMarketization",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 25), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave("../Output/Chapter_1/Step5/Ind_HousingDynamics_1VSList.png", plot = dd, width = 10)

dd <- ggplot(subset(data_LiPrABnB, !is.na(IND2_1_Q)), aes(x = year, y = averPrice, fill = IND2_1_Q))+
  geom_boxplot()  + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Average price per night", limits = c(65, 175), breaks = seq(50, 175, by = 25)) + 
  scale_fill_manual(name = "Housing\nMarketization",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 25), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HousingDynamics_1VSPrice.png", plot = dd, width = 10)

dd <- ggplot(subset(data_LiPrABnB, !is.na(IND2_2_Q)), aes(x = year, y = nList, fill = IND2_2_Q))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Total Listings-Airbnb", limits = c(0, 1000), breaks = seq(0, 1000, by = 250)) + 
  scale_fill_manual(name = "Housing \nAcquisition\nInterest",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 25), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HousingDynamics_2VSList.png", plot = dd, width = 10)

dd <- ggplot(subset(data_LiPrABnB, !is.na(IND2_2_Q)), aes(x = year, y = averPrice, fill = IND2_2_Q))+
  geom_boxplot()   + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Average price per night", limits = c(65, 175), breaks = seq(50, 175, by = 25)) + 
  scale_fill_manual(name = "Housing\nAcquisition\nInterest",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 25), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HousingDynamics_2VSPrice.png", plot = dd, width = 10)

### Adding info to the indicators table 
data_IND1_02I <- merge(data_IND1_02I, 
                       map_AMS_wijken_sf[, c("Buurtcombi", "gentryABnB_opDef", "gentry_opDef", "gentryABnBSES")],
                       by.x = "gebiedcode15", 
                       by.y = "Buurtcombi", all.x = T)

data_IND2_02I <- merge(data_IND2_02I, 
                       map_AMS_wijken_sf[, c("Buurtcombi", "gentryABnB_opDef", "gentry_opDef", "gentryABnBSES")],
                       by.x = "gebiedcode15", 
                       by.y = "Buurtcombi", all.x = T)

### cross table gentrified and indicators 

www1_1 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND1_1_Q) %>% 
  summarise(medList = median(nList), 
            averList = mean(nList),
            medPrice = median(averPrice),
            averPrice = median(averPrice),
            nPrABnB = sum(gentryABnB_opDef == "Gentrified"),
            nPrIncome = sum(gentry_opDef == "Gentrified"))
dcast(subset(www1_1), 
      gentry_opDef + year ~ IND1_1_Q, value.var = "nPrABnB")

dcast(subset(www1_1), 
      gentry_opDef + year ~ IND1_1_Q, value.var = "nPrIncome")

dcast(subset(www1_1), 
      gentry_opDef + year ~ IND1_1_Q, value.var = "averList")

dcast(subset(www1_1), 
      gentry_opDef + year ~ IND1_1_Q, value.var = "averPrice")


www1_2 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND1_2_Q) %>% 
  summarise(medList = median(nList), 
            averList = median(nList),
            medPrice = median(averPrice),
            averPrice = median(averPrice),
            nPrABnB = sum(gentryABnB_opDef == "Gentrified"))
dcast(subset(www1_2), 
      gentry_opDef + year ~ IND1_2_Q, value.var = "nPrABnB")

dcast(subset(www1_2), 
      gentry_opDef + year ~ IND1_2_Q, value.var = "averList")

dcast(subset(www1_2), 
      gentry_opDef + year ~ IND1_2_Q, value.var = "averPrice")


www2_1 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND2_1_Q) %>% 
  summarise(medList = median(nList), 
            averList = median(nList),
            medPrice = median(averPrice),
            averPrice = median(averPrice),
            nPrABnB = sum(gentryABnB_opDef == "Gentrified"))
dcast(subset(www2_1), 
      gentry_opDef + year ~ IND2_1_Q, value.var = "nPrABnB")

dcast(subset(www2_1), 
      gentry_opDef + year ~ IND2_1_Q, value.var = "averList")

dcast(subset(www2_1), 
      gentry_opDef + year ~ IND2_1_Q, value.var = "averPrice")


www2_2 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND2_2_Q) %>% 
  summarise(medList = median(nList), 
            averList = median(nList),
            medPrice = median(averPrice),
            averPrice = median(averPrice),
            nPrABnB = sum(gentryABnB_opDef == "Gentrified"))
dcast(subset(www2_2), 
      gentry_opDef + year ~ IND2_2_Q, value.var = "nPrABnB")

dcast(subset(www2_2), 
      gentry_opDef + year ~ IND2_2_Q, value.var = "averList")

dcast(subset(www2_2), 
      gentry_opDef + year ~ IND2_2_Q, value.var = "averPrice")

#### Calculation for Expected revenue 

data_airbnbYEAR_4 <- data_airbnb_4 %>% 
  group_by(id) %>% 
  summarise(maxReW = ceiling(max(reviews_per_month)),
            maxMinNight = max(minimum_nights), 
            estReveMin = 12 *maxReW * maxMinNight * mean(price, na.rm = T),
            averReview = mean(number_of_reviews),
            averDist = mean(dist_centre2))

imputed_Data <- mice(subset(data_airbnbYEAR_4, select = -c(id, estReveMin)),
                     maxit = 10, m = 1,
                     method = 'pmm', seed = 500)
completeData <- complete(imputed_Data, 1)
data_airbnbYEAR_4$maxReW <- completeData$maxReW
data_airbnb_4 <- merge(data_airbnb_4, data_airbnbYEAR_4[, c("id", "maxReW")], all.x = T)

data_airbnb_4$reviews_per_monthImp <- with(data_airbnb_4, 
                                           ifelse(is.na(reviews_per_month), maxReW, reviews_per_month))

data_airbnb_4$estReveMin = with(data_airbnb_4, 12 *maxReW * minimum_nights * price)

data_airbnbIDYEAR_4 <- data_airbnb_4 %>% 
  group_by(year, id) %>% 
  summarise(maxReW = ceiling(max(reviews_per_monthImp)),
            maxMinNight = max(minimum_nights), 
            estReveMin = 12 *maxReW * maxMinNight * mean(price, na.rm = T),
            averReview = mean(number_of_reviews),
            averDist = mean(dist_centre2))
## cleaning removing less 5% and 10% of the data 
ddd <- quantile(data_airbnbIDYEAR_4$estReveMin, c(0.05, 0.95))
data_airbnbIDYEAR_4$estReveMinC <- with(data_airbnbIDYEAR_4, 
                                        ifelse(estReveMin<ddd[1] | estReveMin>ddd[2], NA, estReveMin))
data_airbnbIDYEAR_4 <- subset(data_airbnbIDYEAR_4, !is.na(estReveMinC))
data_airbnb_4Sub <- data_airbnb_4 %>% filter(estReveMin>ddd[1] & estReveMin<ddd[2])

# # merging per neighborhood
ppp <- data_airbnb_4[, c("id","Buurtcombi", "room_type")]
ppp <- unique(ppp)
dim(ppp)
ppp <- subset(ppp, !duplicated(ppp$id))
dim(ppp)
data_airbnbIDYEAR_4 <- merge(data_airbnbIDYEAR_4, 
                             ppp, all.x = T)
data_airbnbIDYEAR_4 <- subset(data_airbnbIDYEAR_4, year < 2019)
data_RevenueEst <- data_airbnbIDYEAR_4 %>% 
  group_by(year, Buurtcombi) %>% 
  summarise(estRevTotal = sum(estReveMinC),
            estRevAverage = mean(estReveMinC))
dim(data_LiPrABnB)
data_LiPrABnB <- merge(data_LiPrABnB, data_RevenueEst, by = c("year", "Buurtcombi"), all.x = T)
dim(data_LiPrABnB)

# # comparison using gentrified neighborhoods
ggplot(subset(data_LiPrABnB, !is.na(gentry_opDef)), 
       aes(x = year, y = estRevTotal/1000, fill = gentry_opDef))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Expected total revenue x 1000" , limits = c(0, 6000), breaks = seq(0, 10000, by = 1000)) + 
  scale_fill_manual(name = NULL,  values =  colIntRdBu1, labels = c("Gentrified", "Other Neigh."))+ 
  theme(text = element_text(size = 25), legend.position="bottom")
ggsave("../Output/Chapter_1/Step5/GentryIncomeVSRevenue.png")


# # comparison using revenue and category SES
dd <- ggplot(subset(data_LiPrABnB, !is.na(category_SES)), 
       aes(x = year, y = estRevTotal/1000, fill = category_SES))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Expected total revenue x 1000" , limits = c(0, 5000), breaks = seq(0, 10000, by = 1000)) + 
  scale_fill_manual(name = NULL,  values =  colIntCatSES, labels = c("Low", "Average", "High"))+ 
  theme(text = element_text(size = 25), legend.position="bottom")
ggsave("../Output/Chapter_1/Step5/Ind_GentryIncome_CategorySESVSRevenue.png", plot = dd)


dd <- ggplot(subset(data_LiPrABnB, !is.na(IND1_1_Q)), aes(x = year, y = estRevTotal/1000, fill = IND1_1_Q))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Expected total revenue x 1000" , limits = c(0, 5000), breaks = seq(0, 10000, by = 1000)) + 
  scale_fill_manual(name = "Residential\nMobility",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 17), legend.position="bottom") + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HouseholdChar_1VSRevenue.png", plot = dd)


dd <- ggplot(subset(data_LiPrABnB, !is.na(IND2_1_Q)), aes(x = year, y = estRevTotal/1000, fill = IND1_2_Q))+
  geom_boxplot() + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Expected total revenue x 1000" , limits = c(0, 5000), breaks = seq(0, 10000, by = 1000)) + 
  scale_fill_manual(name = "Foreign\nVibrant",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 17), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HouseholdChar_2VSRevenue.png", plot = dd)

dd <- ggplot(subset(data_LiPrABnB, !is.na(IND2_1_Q) & gentry_opDef == "Gentrified"), aes(x = year, y = estRevTotal/1000, fill = IND2_1_Q))+
  geom_boxplot()  + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Expected total revenue x 1000" , limits = c(0, 5000), breaks = seq(0, 10000, by = 1000)) + 
  scale_fill_manual(name = "Housing\nMarketization",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 17), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HousingDynamics_1VSRevenue.png", plot = dd)


dd <- ggplot(subset(data_LiPrABnB, !is.na(IND2_2_Q) & gentry_opDef == "Gentrified"), aes(x = year, y = estRevTotal/1000, fill = IND2_2_Q))+
  geom_boxplot()   + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + 
  scale_y_continuous(name = "Expected total revenue x 1000" , limits = c(0, 5000), breaks = seq(0, 10000, by = 1000)) + 
  scale_fill_manual(name = "Housing\nAcquisition\nInterest",  values =  colIntOrRd)+ 
  theme(text = element_text(size = 17), legend.position="bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("../Output/Chapter_1/Step5/Ind_HousingDynamics_2VSRevenue.png", plot = dd)

zzz1_1 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND1_1_Q) %>% 
  summarise(medianPropEnH = median(estRevTotal), 
            avPropEnH = mean(estRevTotal))
ttt1_1 <- dcast(subset(zzz1_1), 
                gentry_opDef + year ~ IND1_1_Q, value.var = "avPropEnH")

zzz1_2 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND1_2_Q) %>% 
  summarise(medianPropEnH = median(estRevTotal), 
            avPropEnH = mean(estRevTotal))
ttt1_2 <- dcast(subset(zzz1_2), 
                gentry_opDef + year ~ IND1_2_Q, value.var = "avPropEnH")

zzz2_1 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND2_1_Q) %>% 
  summarise(medianPropEnH = median(estRevTotal), 
            avPropEnH = mean(estRevTotal))
ttt2_1 <- dcast(subset(zzz2_1), 
                gentry_opDef + year ~ IND2_1_Q, value.var = "avPropEnH")

zzz2_2 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND2_2_Q) %>% 
  summarise(medianPropEnH = median(estRevTotal), 
            avPropEnH = mean(estRevTotal))
ttt2_2 <- dcast(subset(zzz2_2), 
                gentry_opDef + year ~ IND2_2_Q, value.var = "avPropEnH")

#### PLots bubble and highlighting neighborhoods
# Most basic bubble plot
IND1_1_Q <- c(35.08, 45.73, 56.63, 64.29)
IND1_2_Q <- c(39.48, 46.16, 51.66, 60.76)

IND2_1_Q <- c(37.36, 42.74, 50.61, 64.40)
IND2_2_Q <- c(45.12, 52.28, 55.04, 58.99)
sizeBubble <- c(90, 200, 400)

##################################################################  
# # INDICATOR 1 -INDICATOR 1 LISTINGS 
##################################################################
data_LiPrABnB %>%
  filter(year == "2015" & !is.na(nList) & !is.na(gentry_opDef)) %>%
  ggplot(aes(x=IND1_1, y=IND1_2, size=nList, fill=gentry_opDef)) +
  geom_hline(yintercept = IND1_1_Q, linetype="dashed", color = grey(0.9)) + 
  geom_vline(xintercept = IND1_2_Q, linetype="dashed", color = grey(0.9)) + 
  geom_point(alpha=0.6, shape = 21) +
  scale_size_continuous(name="Total Listings-Airbnb 2015", range = c(1,8), 
                        breaks = sizeBubble) +
  scale_fill_manual(name = "Gentrified Neigh.\n Income",
                    values = colIntRdBu1, label = c("Gentrified", "Other Neigh."), ) +
  scale_x_continuous(name = "Housing Char. (Ind 1)", breaks = seq(5, 120, by = 15), limits = c(15, 85)) + 
  scale_y_continuous(name = "Housing Char. (Ind 2)", breaks = seq(5, 120, by = 15), limits = c(0, 90)) + 
  theme_ipsum(grid_col = grey(0.5), grid = F, axis = T)
ggsave("../Output/Chapter_1/Step5/HouseholdVsListings_2015.png", width = 10)

data_LiPrABnB %>%
  filter(year == "2016" & !is.na(nList) & !is.na(gentry_opDef)) %>%
  ggplot(aes(x=IND1_1, y=IND1_2, size=nList, fill=gentry_opDef)) +
  geom_hline(yintercept = IND1_1_Q, linetype="dashed", color = grey(0.9)) + 
  geom_vline(xintercept = IND1_2_Q, linetype="dashed", color = grey(0.9)) + 
  geom_point(alpha=0.6, shape = 21) +
  scale_size_continuous(name="Total Listings-Airbnb 2016", range = c(1,8), 
                        breaks = sizeBubble) +
  scale_fill_manual(name = "Gentrified Neigh.\n Income",
                    values = colIntRdBu1, label = c("Gentrified", "Other Neigh."), ) +
  scale_x_continuous(name = "Housing Char. (Ind 1)", breaks = seq(5, 120, by = 15), limits = c(15, 85)) + 
  scale_y_continuous(name = "Housing Char. (Ind 2)", breaks = seq(5, 120, by = 15), limits = c(0, 90)) + 
  theme_ipsum(grid_col = grey(0.5), grid = F, axis = T)
ggsave("../Output/Chapter_1/Step5/HouseholdVsListings_2016.png", width = 10)

data_LiPrABnB %>%
  filter(year == "2017" & !is.na(nList) & !is.na(gentry_opDef)) %>%
  ggplot(aes(x=IND1_1, y=IND1_2, size=nList, fill=gentry_opDef)) +
  geom_hline(yintercept = IND1_1_Q, linetype="dashed", color = grey(0.9)) + 
  geom_vline(xintercept = IND1_2_Q, linetype="dashed", color = grey(0.9)) + 
  geom_point(alpha=0.6, shape = 21) +
  scale_size_continuous(name="Total Listings-Airbnb 2017", range = c(1,8), 
                        breaks = sizeBubble) +
  scale_fill_manual(name = "Gentrified Neigh.\n Income",
                    values = colIntRdBu1, label = c("Gentrified", "Other Neigh."), ) +
  scale_x_continuous(name = "Housing Char. (Ind 1)", breaks = seq(5, 120, by = 15), limits = c(15, 85)) + 
  scale_y_continuous(name = "Housing Char. (Ind 2)", breaks = seq(5, 120, by = 15), limits = c(0, 90)) + 
  theme_ipsum(grid_col = grey(0.5), grid = F, axis = T)
ggsave("../Output/Chapter_1/Step5/HouseholdVsListings_2017.png", width = 10)

data_LiPrABnB %>%
  filter(year == "2018" & !is.na(nList) & !is.na(gentry_opDef)) %>%
  ggplot(aes(x=IND1_1, y=IND1_2, size=nList, fill=gentry_opDef)) +
  geom_hline(yintercept = IND1_1_Q, linetype="dashed", color = grey(0.9)) + 
  geom_vline(xintercept = IND1_2_Q, linetype="dashed", color = grey(0.9)) + 
  geom_point(alpha=0.6, shape = 21) +
  scale_size_continuous(name="Total Listings-Airbnb 2018", range = c(1,8), 
                        breaks = sizeBubble) +
  scale_fill_manual(name = "Gentrified Neigh.\n Income",
                    values = colIntRdBu1, label = c("Gentrified", "Other Neigh."), ) +
  scale_x_continuous(name = "Housing Char. (Ind 1)", breaks = seq(5, 120, by = 15), limits = c(15, 85)) + 
  scale_y_continuous(name = "Housing Char. (Ind 2)", breaks = seq(5, 120, by = 15), limits = c(0, 90)) + 
  theme_ipsum(grid_col = grey(0.5), grid = F, axis = T)
ggsave("../Output/Chapter_1/Step5/HouseholdVsListings_2018.png", width = 10)

data_LiPrABnB$ID <- 1:nrow(data_LiPrABnB)
data_AuxPlot1 <- melt(data_LiPrABnB[, c("ID", "year", "nList", "gentryABnB_opDef", "IND1_1", "IND1_2")], 
                     id.vars = c("ID", "year", "nList", "gentryABnB_opDef"), variable.name = "HouseChar", value.name = "IndHousechar")
data_AuxPlot2 <- melt(data_LiPrABnB[, c("ID", "year", "nList", "gentryABnB_opDef", "IND2_1","IND2_2")], 
                      id.vars = c("ID", "year", "nList", "gentryABnB_opDef"), variable.name = "HousiDyn", value.name = "IndHousiDin")
table(data_AuxPlot1$nList == data_AuxPlot2$nList)
data_AuxPlot2$IndHousiDin <- ifelse(data_AuxPlot2$IndHousiDin < 0, 0, data_AuxPlot2$IndHousiDin)
data_AuxPlot <- merge(data_AuxPlot1, data_AuxPlot2[, c("ID", "HousiDyn", "IndHousiDin")], by = "ID")
data_AuxPlot$IndHousechar <- with(data_AuxPlot, ifelse(HouseChar == "IND1_2", -1 * IndHousechar, IndHousechar))
data_AuxPlot$IndHousiDin <- with(data_AuxPlot, ifelse(HousiDyn == "IND2_2", -1 * IndHousiDin, IndHousiDin))

data_AuxPlot %>%
  filter(year == "2015" & !is.na(nList) & !is.na(gentryABnB_opDef)) %>%
  ggplot(aes(x=IndHousechar, y=IndHousiDin, size=nList, fill=gentryABnB_opDef)) +
  geom_hline(yintercept = c(IND1_1_Q, -IND1_2_Q), linetype="dashed", color = grey(0.9)) + 
  geom_vline(xintercept = c(IND2_1_Q, -IND2_2_Q), linetype="dashed", color = grey(0.9)) + 
  geom_point(alpha=0.6, shape = 21) +
  scale_size_continuous(name="Total Listings-Airbnb 2018", 
                        range = c(1,8), 
                        breaks = sizeBubble) +
  scale_fill_manual(name = "Gentrified Neigh.\n Income",
                    values = colIntRdBu1, 
                    label = c("Gentrified", "Other Neigh.")) +
  scale_x_continuous(name = "Housing Char. (Ind 1)", 
                     breaks = seq(-100, 100, by = 20), 
                     limits = c(-100, 100)) + 
  scale_y_continuous(name = "Housing Char. (Ind 2)", 
                     breaks = seq(-100, 100, by = 20), 
                     limits = c(-100, 100)) + 
  theme_ipsum(grid_col = grey(0.5), grid = F, axis = T)



tttPrice %>% group_by(gentryABnBSES) %>% summarise_at(as.character(2015:2018), mean, na.rm = TRUE)
tttList %>% group_by(gentryABnBSES) %>% summarise_at(as.character(2015:2019), mean, na.rm = TRUE)

data_IND1_02I$gentryABnBSES2 <- ifelse(is.na(data_IND1_02I$gentryABnBSES), NA, data_IND2_02I$gentryABnBSES)
##sub_data_IND1_02I <- data_IND1_02I %>% filter(gentryABnBSES != "Other")
## Calculating statistics for Airbnb
# # Comparison entire home with the private housing rent stock 
## loading data Airbnb and municipality housing stock and private and owner 

head(data_airbnb_4)
data_airbnb_4$ID_YP <- with(data_airbnb_4, paste(year, id, sep = "_"))
data_airbnbEntHouse_4 <- data_airbnb_4 %>% filter(room_type == "Entire home/apt")
count_EntHouse <- data_airbnbEntHouse_4[, c("ID_YP", "year", "Buurtcombi", "Buurtcom_1")]
count_EntHouse <- unique(count_EntHouse)
count_EntHouseABnB <- count_EntHouse %>% group_by(year, Buurtcombi, Buurtcom_1) %>% summarise(totEnHom = n())

data_IND2_02I$PriHouStock <- with(data_IND2_02I, HousStock * (Prop_Priv +Prop_Owner)/100)
data_IND2_02I$PriHouStock <- round(data_IND2_02I$PriHouStock)
countTotHousAMS <- data_IND2_02I %>% group_by(jaar, gebiedcode15, Stadsdeel) %>% summarise(TotPrivate = sum(PriHouStock))

countEntHou_AMS <- merge(count_EntHouseABnB, countTotHousAMS, all.x = T, 
      by.x = c("year", "Buurtcombi"), 
      by.y = c("jaar", "gebiedcode15"))
countEntHou_AMS$propEntAMS <- with(countEntHou_AMS, totEnHom / TotPrivate)
countEntHou_AMS$Buurtcom_1 <- NULL
countEntHou_AMS$Stadsdeel <- NULL
# # Analysis with Indexes and Gentrified Neigh 

data_LiPrABnB <- merge(data_LiPrABnB, countEntHou_AMS, by = c("year", "Buurtcombi"), all.x = T)
zzz1_1 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND1_1_Q) %>% 
  summarise(medianPropEnH = median(propEntAMS)* 100, 
            avPropEnH = mean(propEntAMS) * 100)
ttt1_1 <- dcast(subset(zzz1_1), 
      gentry_opDef + year ~ IND1_1_Q, value.var = "avPropEnH")

zzz1_2 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND1_2_Q) %>% 
  summarise(medianPropEnH = median(propEntAMS)* 100, 
            avPropEnH = mean(propEntAMS)* 100)
ttt1_2 <- dcast(subset(zzz1_2), 
      gentry_opDef + year ~ IND1_2_Q, value.var = "avPropEnH")

zzz2_1 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND2_1_Q) %>% 
  summarise(medianPropEnH = median(propEntAMS)* 100, 
            avPropEnH = mean(propEntAMS)* 100)
ttt2_1 <- dcast(subset(zzz2_1), 
      gentry_opDef + year ~ IND2_1_Q, value.var = "avPropEnH")

zzz2_2 <- subset(data_LiPrABnB, !is.na(gentry_opDef )) %>% 
  group_by(year, gentry_opDef , IND2_2_Q) %>% 
  summarise(medianPropEnH = median(propEntAMS)* 100, 
            avPropEnH = mean(propEntAMS)* 100)
ttt2_2 <- dcast(subset(zzz2_2), 
      gentry_opDef + year ~ IND2_2_Q, value.var = "avPropEnH")

library(xtable)
print(xtable(ttt1_1[, -1], 
       caption = "Cross table gentrified neighborhoods - Residential Mobility. Average of proportion of Entire Houses rented in Airbnb given long-term rental", 
       label = "tab:CH06:CrossGentIND1_1", 
       digits = 2), include.rownames = FALSE)
print(xtable(ttt1_2[, -1], 
             caption = "Cross table gentrified neighborhoods - Foreign Vibrant. Average of proportion of Entire Houses rented in Airbnb given long-term rental", 
             label = "tab:CH06:CrossGentIND1_2", 
             digits = 2), include.rownames = FALSE)
print(xtable(ttt2_1[, -1], 
             caption = "Cross table gentrified neighborhoods - Housing Marketization. Average of proportion of Entire Houses rented in Airbnb given long-term rental", 
             label = "tab:CH06:CrossGentIND2_1", 
             digits = 2), include.rownames = FALSE)
print(xtable(ttt2_2[, -1], 
             caption = "Cross table gentrified neighborhoods - Housing Acquisition Interest. Average of proportion of Entire Houses rented in Airbnb given long-term rental", 
             label = "tab:CH06:CrossGentIND2_2", 
             digits = 2), include.rownames = FALSE)

# # neighborhoods identified in these categories long term to short term
catIdent <- c("Medium-High", "Medium-High", "Low", "Medium")
catIdent_1_1 <- c("Medium-High", "High", "Medium", "Medium-Low", "Low")

rrr1_1_A <- data_LiPrABnB %>% filter(IND1_1_Q == catIdent_1_1[1] & gentry_opDef == "Gentrified") %>% dplyr:::select(Buurtcombi, Buurtcom_1, Stadsdeel)
rrr1_1_A <- unique(rrr1_1_A)

rrr1_1_B <- data_LiPrABnB %>% filter(IND1_1_Q == catIdent_1_1[2] & gentry_opDef == "Gentrified") %>% dplyr:::select(Buurtcombi, Buurtcom_1, Stadsdeel)
rrr1_1_B <- unique(rrr1_1_B)


listByYear <-  list() 
rrr1_1 <- data_LiPrABnB %>% filter(IND1_1_Q == catIdent[1] & gentry_opDef == "Gentrified") %>% dplyr:::select(Buurtcombi, Buurtcom_1, Stadsdeel)
rrr1_1 <- unique(rrr1_1)
rrr1_2 <- data_LiPrABnB %>% filter(IND1_2_Q == catIdent[2] & gentry_opDef == "Gentrified") %>% dplyr:::select(Buurtcombi, Buurtcom_1, Stadsdeel)
rrr1_2 <- unique(rrr1_2)
rrr2_1 <- data_LiPrABnB %>% filter(IND2_1_Q == catIdent[3] & gentry_opDef == "Gentrified") %>% dplyr:::select(Buurtcombi, Buurtcom_1, Stadsdeel)
rrr2_1 <- unique(rrr2_1)
rrr2_2 <- data_LiPrABnB %>% filter(IND2_2_Q == catIdent[4] & gentry_opDef == "Gentrified") %>% dplyr:::select(Buurtcombi, Buurtcom_1, Stadsdeel)
rrr2_2 <- unique(rrr2_2)

map_AMS_wijken_sf2 <- map_AMS_wijken_sf
map_AMS_wijken_sf2$entHouGent_1_1 <- with(map_AMS_wijken_sf2, 
                                          ifelse(Buurtcombi %in% rrr1_1$Buurtcombi, "Propensity Short-Term", NA))
map_AMS_wijken_sf2$entHouGent_1_2 <- with(map_AMS_wijken_sf2, 
                                          ifelse(Buurtcombi %in% rrr1_2$Buurtcombi, "Propensity Short-Term", NA))
map_AMS_wijken_sf2$entHouGent_2_1 <- with(map_AMS_wijken_sf2, 
                                          ifelse(Buurtcombi %in% rrr2_1$Buurtcombi, "Propensity Short-Term", NA))
map_AMS_wijken_sf2$entHouGent_2_2 <- with(map_AMS_wijken_sf2, 
                                          ifelse(Buurtcombi %in% rrr2_2$Buurtcombi, "Propensity Short-Term", NA))

intersect(intersect(intersect(rrr1_1$Buurtcombi, rrr1_2$Buurtcombi), rrr2_1$Buurtcombi), rrr2_2$Buurtcombi)
intersect(intersect(intersect(rrr1_1$Buurtcombi, rrr1_2$Buurtcombi), rrr2_1$Buurtcombi), rrr2_2$Buurtcombi)

length(unique(c(rrr1_1$Buurtcombi, rrr1_2$Buurtcombi, rrr2_1$Buurtcombi, rrr2_2$Buurtcombi)))
rrr1_1$Ind <- "1"
rrr1_2$Ind <- "1"
rrr2_1$Ind <- "1"
rrr2_2$Ind <- "1"

rrr1_1$Type <- "1"
rrr1_2$Type <- "2"
rrr2_1$Type <- "3"
rrr2_2$Type <- "4"
qqq <- rbind(rrr1_1, 
          rrr1_2, 
          rrr2_1, 
          rrr2_2)
qqq <- merge(qqq, map_AMS_wijken_sf[, c("Buurtcombi", "category_SES")], all.x = T)
dcast(data = data.frame(qqq)[, -7], Buurtcombi + Buurtcom_1 + Stadsdeel + category_SES ~ Type, value.var = "Ind")

## areas with low and medium SES in gentrif neighborhoods  
## map for Airbnb 
## average price - list all the years 

data_TimeLiPrABnB <- data_LiPrABnB %>% 
  group_by(Buurtcombi) %>% 
  summarise(averPrice = mean(averPrice), 
            averList = mean(nList))

map_AMS_wijken_sf <- merge(map_AMS_wijken_sf, 
                           data_TimeLiPrABnB, all.x = T, by = "Buurtcombi")
map_AMS_wijken_sf$ABnBNeigh <- ifelse(is.na(map_AMS_wijken_sf$gentryABnB_opDef), 0, 1)
map_AMS_wijken_sf$ABnBNeigh <- as.factor(map_AMS_wijken_sf$ABnBNeigh)

tm_shape(map_AMS_wijken_sf) +
  tm_borders()+
  tm_fill(col = "averPrice")

tm_shape(map_AMS_wijken_sf) +
  tm_borders()+
  tm_fill(col = "averList")

map_ListSumm <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "averList", title = "Average number of listings\nAirbnb 2015-2018", textNA = "Without short-term rental")  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_layout(frame = T, inner.margins = 0.08, 
            legend.outside = F,
            legend.frame = F,
            legend.frame.lwd = 0.5, 
            legend.title.size = 0.8,
            legend.text.size = 0.7, 
            legend.width = -0.35,
            legend.title.fontfamily = "serif") + 
  tm_compass(position = c("right", "top"), type = "arrow" ) + 
  tm_scale_bar(position=c("right", "bottom"), width= 0.2, just = "left")
map_ListSumm
tmap_save(map_ListSumm, "../Output/Chapter_1/Step5/Map_Airbnb_Neigh_List.png", width=1920, height=1080, asp=0)


sss <- data_LiPrABnB %>% filter(IND1_1_Q == catIdent[1] & gentry_opDef == "Gentrified") %>% dplyr:::select(Buurtcombi, Buurtcom_1, Stadsdeel)

legend_title1 <- "Gentrified Neigh - Income"
legend_title2 <- "Short-Term given Residential Mobility"
map_rrr_1_1 <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "gentry_opDef", palette = c("#fdae61", "white"), title = legend_title1)  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_shape(subset(map_AMS_wijken_sf2, entHouGent_1_1 == "Propensity Short-Term")) +
  tm_fill(col = "entHouGent_1_1", palette = c("#fdae61"), title = legend_title2, label = "High propensity Short-Term")  +
  tm_borders(col = "#5e3c99", lwd=4)+
  tm_layout(frame = T, inner.margins = 0.06, 
          aes.palette = list(seq = "-RdYlBu"),
          legend.outside = F,
          legend.frame = F,
          legend.frame.lwd = 0.5, 
          legend.title.size = 1,
          legend.width = -0.45,
          legend.title.fontfamily = "serif") + 
  tm_borders(col = "#5e3c99", lwd=2) + 
  tm_compass(position = c("right", "top"), type = "arrow" ) + 
  tm_scale_bar(position=c("right", "bottom"), width= 0.2, just = "left")

tmap_save(map_rrr_1_1, "../Output/Chapter_1/Step5/Map_GentPropShortTerm_1_1.png", width=1800, height=1080, asp=0)


legend_title1 <- "Gentrified Neigh - Income"
legend_title2 <- "Short-Term given Foreign Vibrant"
map_rrr_1_2 <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "gentry_opDef", palette = c("#fdae61", "white"), title = legend_title1)  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_shape(subset(map_AMS_wijken_sf2, entHouGent_1_2 == "Propensity Short-Term")) +
  tm_fill(col = "entHouGent_1_2", palette = c("#fdae61"), title = legend_title2, label = "High propensity Short-Term")  +
  tm_borders(col = "#5e3c99", lwd=4) + 
  tm_layout(frame = T, inner.margins = 0.06, 
            aes.palette = list(seq = "-RdYlBu"),
            legend.outside = F,
            legend.frame = F,
            legend.frame.lwd = 0.5, 
            legend.title.size = 1,
            legend.width = -0.45,
            legend.title.fontfamily = "serif") + 
  tm_borders(col = "#5e3c99", lwd=2) + 
  tm_compass(position = c("right", "top"), type = "arrow" ) + 
  tm_scale_bar(position=c("right", "bottom"), width= 0.2, just = "left")
tmap_save(map_rrr_1_2, "../Output/Chapter_1/Step5/Map_GentPropShortTerm_1_2.png", width=1800, height=1080, asp=0)

legend_title1 <- "Gentrified Neigh - Income"
legend_title2 <- "Short-Term given Housing Marketization"
map_rrr_2_1 <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "gentry_opDef", palette = c("#fdae61", "white"), title = legend_title1)  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_shape(subset(map_AMS_wijken_sf2, entHouGent_2_1 == "Propensity Short-Term")) +
  tm_fill(col = "entHouGent_2_1", palette = c("#fdae61"), title = legend_title2, label = "High propensity Short-Term")  +
  tm_borders(col = "#5e3c99", lwd=4) + 
  tm_layout(frame = T, inner.margins = 0.06, 
            aes.palette = list(seq = "-RdYlBu"),
            legend.outside = F,
            legend.frame = F,
            legend.frame.lwd = 0.5, 
            legend.title.size = 1,
            legend.width = -0.45,
            legend.title.fontfamily = "serif") + 
  tm_borders(col = "#5e3c99", lwd=2) + 
  tm_compass(position = c("right", "top"), type = "arrow" ) + 
  tm_scale_bar(position=c("right", "bottom"), width= 0.2, just = "left")
  
tmap_save(map_rrr_2_1, "../Output/Chapter_1/Step5/Map_GentPropShortTerm_2_1.png", width=1800, height=1080, asp=0)

legend_title1 <- "Gentrified Neigh - Income"
legend_title2 <- "Short-Term given Housing Acquisition Int."
map_rrr_2_2 <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "gentry_opDef", palette = c("#fdae61", "white"), title = legend_title1)  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_shape(subset(map_AMS_wijken_sf2, entHouGent_2_2 == "Propensity Short-Term")) +
  tm_fill(col = "entHouGent_2_2", palette = c("#fdae61"), title = legend_title2, label = "High propensity Short-Term")  +
  tm_borders(col = "#5e3c99", lwd=4) + 
  tm_layout(frame = T, inner.margins = 0.06, 
            aes.palette = list(seq = "-RdYlBu"),
            legend.outside = F,
            legend.frame = F,
            legend.frame.lwd = 0.5, 
            legend.title.size = 1,
            legend.width = -0.45,
            legend.title.fontfamily = "serif") + 
  tm_borders(col = "#5e3c99", lwd=2) + 
  tm_compass(position = c("right", "top"), type = "arrow" ) + 
  tm_scale_bar(position=c("right", "bottom"), width= 0.2, just = "left")
tmap_save(map_rrr_2_2, "../Output/Chapter_1/Step5/Map_GentPropShortTerm_2_2.png", width=1800, height=1080, asp=0)


# # 4 neighborhoods identified in all the analysis 
map_AMS_wijken_sf3 <- map_AMS_wijken_sf
map_AMS_wijken_sf$GentryHighProp <- "GentryHighProp"
legend_title1 <- "Gentrified Neigh - Income"
legend_title2 <- "Growing presence of Airbnb"
map_qqq <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "gentry_opDef", palette = c("#fdae61", "white"), title = legend_title1)  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_shape(subset(map_AMS_wijken_sf, Buurtcombi %in% c("E18", "K25", "M28", "M31") )) +
  tm_fill(col = "GentryHighProp", palette = c("#fdae61"), title = legend_title2, label = "Gentrified Airbnb price + high propensity")  +
  tm_borders(col = "#5e3c99", lwd=4)+
  tm_layout(frame = T, inner.margins = 0.08, 
            aes.palette = list(seq = "-RdYlBu"),
            legend.outside = F,
            legend.frame = F,
            legend.frame.lwd = 0.5, 
            legend.title.size = 1,
            legend.width = -0.35,
            legend.title.fontfamily = "serif") + 
  tm_borders(col = "#5e3c99", lwd=2) + 
  tm_compass(position = c("right", "top"), type = "arrow" ) + 
  tm_scale_bar(position=c("right", "bottom"), width= 0.2, just = "left")

tmap_save(map_qqq, "../Output/Chapter_1/Step5/Map_GentPropShortTerm_GentryABnB.png", width=1800, height=1080, asp=0)




  
tmap_save(map_GentBoth, "../Output/Chapter_1/Step5/Map_GentrificationAIRBNB_INCOME.png", width=1920, height=1080, asp=0)

map_GentBoth <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "gentryABnBSES", title = legend_title, palette = "seq", colorNA = NULL)  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_layout(frame = FALSE, inner.margins = 0.1, aes.palette = list(seq = "RdYlBu"))
tmap_save(map_GentBoth, "../Output/Chapter_1/Step5/Map_GentrificationAIRBNB_INCOME.png", width=1920, height=1080, asp=0)




### calculating the inequality distribution of Airbnb
data_LiPrABnB <- data_LiPrABnB[order(data_LiPrABnB$year, data_LiPrABnB$estReveMinC, decreasing = T), ]
rrr <- subset(data_LiPrABnB, year == 2015)
rrr <- rrr[order(rrr$estReveMinC, decreasing = F), ]
rrr$cumRev <- cumsum(rrr$estReveMinC)
rrr$cumRevPer <- rrr$cumRev/sum(rrr$estReveMinC)
rrr$numNeigh <- 1:nrow(rrr)/nrow(rrr)
plot(rrr$numNeigh, rrr$cumRevPer)
abline(a=0, b = 1)
library(ineq)
ineq(rrr$estReveMinC,type="Gini")
plot(Lc(rrr$estReveMinC), general = F) 

rrr <- subset(data_LiPrABnB, year == 2016)
rrr <- rrr[order(rrr$estReveMinC, decreasing = F), ]
rrr$cumRev <- cumsum(rrr$estReveMinC)
rrr$cumRevPer <- rrr$cumRev/sum(rrr$estReveMinC)
rrr$numNeigh <- 1:nrow(rrr)/nrow(rrr)
plot(rrr$numNeigh, rrr$cumRevPer)
abline(a=0, b = 1)
library(ineq)
ineq(rrr$estReveMinC,type="Gini")
points(Lc(rrr$estReveMinC)$p, Lc(rrr$estReveMinC)$L, col = "red")


# # analysis using regression model- Inequality 
library(ineq)
data_airbnb_5 <- merge(data_airbnb_4, 
                       data_LiPrABnB[, c("Buurtcombi", "gentry_opDef", "IND1_1_Q","IND1_2_Q", "IND2_1", "IND2_2", "IND2_1_Q", "IND2_2_Q")], 
                       by = "Buurtcombi", all.x = T)
data_airbnb_5$estReveMinC <- with(data_airbnb_5, 
                                  ifelse(estReveMin<ddd[1] | estReveMin>ddd[2], NA, estReveMin))
data_airbnb_5 <- subset(data_airbnb_5, !is.na(estReveMinC))
data_airbnb_5Sub <- data_airbnb_5 %>% filter(estReveMin>ddd[1] & estReveMin<ddd[2])
ppp <- data_airbnb_5Sub %>% filter(!is.na(gentry_opDef) & room_type == "Entire home/apt") %>% group_by(year, IND1_1_Q) %>% summarise(gini =  ineq(estReveMinC, type="Gini"))
ttt1_1 <- dcast(ppp, 
                year ~ IND1_1_Q, value.var = "gini")

data_LiPrABnB$y <- -as.numeric(data_LiPrABnB$gentryABnB_opDef) + 2
data_LiPrABnB$estRevPerList_2 <- with(data_LiPrABnB, estRevTotal/nList)

logLinear1<- lm(estRevTotal ~ IND1_1_Q + IND1_2_Q + IND2_1_Q + IND2_2_Q + 
                  year, 
                data = data_LiPrABnB)
summary(logLinear1)

logLinear1<- lm(log(estRevTotal) ~ IND1_1_Q + IND1_2_Q + IND2_1_Q + IND2_2_Q + 
                  year, 
                data = data_LiPrABnB)
summary(logLinear1)


# loglinear models per each type of neighborhood 
data_LiPrABnB$year <- as.factor(data_LiPrABnB$year)
logLinear_Gentry_0<- lm(estRevTotal ~ IND1_1_Q + IND1_2_Q + IND2_1_Q + IND2_2_Q + year, 
                      data = subset(data_LiPrABnB, gentry_opDef == "Gentrified"))
summary(logLinear_Gentry_0)

logLinear_ONeigh_0<- lm(estRevTotal ~ IND1_1_Q + IND1_2_Q + IND2_1_Q + IND2_2_Q + year, 
                      data = subset(data_LiPrABnB, gentry_opDef == "Other Neighborhoods"))
summary(logLinear_ONeigh_0)

logLinear_Total_0<- lm(estRevTotal ~ IND1_1_Q + IND1_2_Q + IND2_1_Q + IND2_2_Q + year, 
                        data = data_LiPrABnB)
summary(logLinear_Total_0)


png("../Output/Chapter_1/Step5/GentryIncomeVSRevenue_BOXCOX.png")
boxcox(logLinear_Gentry_0)
dev.off()

png("../Output/Chapter_1/Step5/NON_GentryIncomeVSRevenue_BOXCOX.png")
boxcox(logLinear_ONeigh_0)
dev.off()

png("../Output/Chapter_1/Step5/ALLNeighVSRevenue_BOXCOX.png")
boxcox(logLinear_Total_0)
dev.off()

logLinear_Gentry<- lm(log(estRevTotal) ~ IND1_1_Q + IND1_2_Q + IND2_1_Q + IND2_2_Q + year, 
                      data = subset(data_LiPrABnB, gentry_opDef == "Gentrified"))
summary(logLinear_Gentry)
anova(logLinear_Gentry, test = "Chisq")

logLinear_ONeigh<- lm(log(estRevTotal) ~ IND1_1_Q + IND1_2_Q + IND2_1_Q + IND2_2_Q + year, 
               data = subset(data_LiPrABnB, gentry_opDef == "Other Neighborhoods"))
summary(logLinear_ONeigh)
anova(logLinear_ONeigh, test = "Chisq")

logLinear_Total <- lm(log(estRevTotal) ~ IND1_1_Q + IND1_2_Q + IND2_1_Q + IND2_2_Q + year, 
                      data = data_LiPrABnB)
summary(logLinear_Total)
anova(logLinear_Total, test = "Chisq")



plot(subset(data_LiPrABnB, gentry_opDef == "Gentrified" & year == 2018)[, c("estReveMinC", "IND2_1")])
# logLinear<- glm(estRevTotal ~ IND1_1_Q + IND1_2_Q + IND2_1_Q + IND2_2_Q + year, 
#                data = data_LiPrABnB, family = Gamma(link = "log"))
# summary(logLinear)


library(MASS)
library(survival)
library(fitdistrplus)
library(sjPlot)
descdist(subset(data_LiPrABnB, year == 2015)$estRevTotal)
descdist(subset(data_LiPrABnB, year == 2016)$estRevTotal)
descdist(subset(data_LiPrABnB, year == 2017)$estRevTotal)
descdist(subset(data_LiPrABnB, year == 2018)$estRevTotal)
fw<-fitdist(data_LiPrABnB$estRevTotal, "gamma", method = "mme")
summary(fw)
plot(fw)
ks.test(data_LiPrABnB$estRevTotal,"pgamma",shape = 1.286998e+00, rate = 7.007205e-07)



logLinear2<- lm(nList ~ IND1_1_Q + IND1_2_Q + IND2_1_Q + IND2_2_Q + year, 
                data = data_LiPrABnB)
summary(logLinear2)
boxcox(logLinear2)

anova(logLinear2, test = "Chisq")

