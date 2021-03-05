##########################################################################
  # # R version: R version 3.6.1 
  # # File Name: 01_identify_gen_neigh.R
  # # Author: Fabio Tejedor
  # # Process: This script identifies gentrified neighborhoods by income growth and Airbnb price per night
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# #  functions 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
fun_cut <- function(x, nCut=6) {
  z <- cut(x,quantile(x, seq(0, 1, len = nCut), na.rm = T), 
           include.lowest = TRUE, dig.lab = 10, ordered_result = T)
  z
}

fun_sta <- function(x){
  z <- (x- mean(x, na.rm = T))/sd(x, na.rm = T)
  z
}
# # # Loading GIS information

map_AMS_Buurten <- readOGR(dsn = "./GIS/geojson_buurten", layer = "geojson_buurten")
plot(map_AMS_Buurten)

map_AMS_wijken <- readOGR(dsn = "./GIS/geojson_wijken", layer = "geojson_wijken")
plot(map_AMS_wijken)

map_AMS_district <- readOGR(dsn = "./GIS/geojson_district", layer = "geojson_districts-polygon")
plot(map_AMS_district)

# colType <- read_excel("./OIS/2020_BBGA_0303.xlsx", sheet = "coltypes")
# data_AMS_BBGA <- read_excel("./OIS/2020_BBGA_0303.xlsx", sheet = "bbga_excel_2020-03-03", col_types = colType$Type)

colType <- read_excel("./OIS/AMS_DATA_NEIGH_0720.xlsx", sheet = "coltypes")
data_AMS_BBGA <- read_excel("./OIS/AMS_DATA_NEIGH_0720.xlsx", sheet = "bbga_excel_2020-07-09", col_types = colType$Type)

data_CPI <- read.delim("./CBS/CPI_NL_Base2015.csv", sep = ";", header = TRUE)

list_jaar = 2007:2018
list_Var <- c("IHHINK_GEM")
level <- c("Wijken", "Amsterdam")
level_region <- "Buurtcombi"
level_unknow <- "Z onbekend"
codeNeiG <- "gebiedcode15"
nameNeiG <- "gebiednaam"

data_AMS_BBGA_Nei <- data_AMS_BBGA %>% 
  filter(niveaunaam %in% level & jaar %in% list_jaar & !sdnaam %in% level_unknow) %>% 
  select(codeNeiG, nameNeiG, jaar, list_Var)

list_neigh <- unique(data_AMS_BBGA_Nei$gebiednaam)
list_neigh <- list_neigh[-1] # Not AMS

data_AMS_BBGA_Nei <- data_AMS_BBGA_Nei %>% filter(gebiednaam %in% list_neigh)
list_growYear <- list()
list_growth <- list()
list_slopes <- list()
list_LinMod <- list()
list_Loess <- list()
list_LinMod_SD <- list()
list_slopes_SD <- list()
list_Loess_SD <- list()
list_stats <- list()
for(ii in 1:length(list_neigh)){
  data_LoopNeigh <- data_AMS_BBGA_Nei %>% 
    filter(gebiednaam %in% list_neigh[ii] & !is.na(IHHINK_GEM))  
  lll <- length(unique(data_LoopNeigh$jaar))
  if(lll != length(list_jaar))cat(list_neigh[ii], "-", lll, "-",ii, "\n")
  data_LoopNeigh <- data_LoopNeigh[order(data_LoopNeigh$jaar), ]
  #data_LoopNeigh <- merge(data_LoopNeigh, data_CPI[, c("jaar", "CPI")], all.x = T)
  #data_LoopNeigh$IHHINK_GEM_D2015 <- with(data_LoopNeigh, IHHINK_GEM * CPI)
  vvv <- NULL
  #lll <- data_LoopNeigh$IHHINK_GEM_D2015
  lll <- data_LoopNeigh$IHHINK_GEM
  data_LoopNeigh$Income_sd <- fun_sta(lll) ## disposable income standardied 
  ddd <- data_LoopNeigh$jaar
  
  for(jj in 1:(nrow(data_LoopNeigh)-1)){
    # # growth year per year
    vvv[jj] <- (lll[jj+1] -lll[jj] )/lll[jj]
  }
    # # growth total, more than two observations in time
  if(all(is.na(vvv)) | length(vvv) == 1){
    cat("Return NA:", list_neigh[ii], "\n")
    list_growth[[ii]] <- NA
    list_stats[[ii]] <- NA
  }else{
    if(nrow(data_LoopNeigh)<5){
      cat("Less than 5:", list_neigh[ii], "\n")
      slo_Mneigh <- NA
      lin_Mneigh <- NA
      slo_Mneigh_SD <- NA
      lin_MneighSD <- NA
      dataLoess <- NA
      dataLoess_SD <- NA
    }else{
      lin_Mneigh <- lm(IHHINK_GEM ~ jaar, data =  data_LoopNeigh)
      lin_MneighSD <- lm(Income_sd ~ jaar, data =  data_LoopNeigh)
      loessMod <- loess(IHHINK_GEM ~ jaar, data=data_LoopNeigh) 
      loessModsd <- loess(Income_sd ~ jaar, data=data_LoopNeigh) 
      
      minLoess <- min(data_LoopNeigh$jaar, na.rm = T)
      maxLoess <- max(data_LoopNeigh$jaar, na.rm = T)
      toLoess <- seq(minLoess, maxLoess, length = 100)
      predLoess <- predict(loessMod, toLoess)
      dataLoess <- data.frame(jaarX = toLoess, incomeY = predLoess,neigh = list_neigh[ii])
      
      
      minLoesssd <- min(data_LoopNeigh$jaar, na.rm = T)
      maxLoesssd <- max(data_LoopNeigh$jaar, na.rm = T)
      toLoesssd <- seq(minLoess, maxLoess, length = 100)
      predLoesssd <- predict(loessModsd, toLoesssd)
      dataLoess_SD <- data.frame(jaarX = toLoess, incomeY = predLoesssd,neigh = list_neigh[ii])
      
      
      slo_Mneigh <- coef(lin_Mneigh)[2]      
      slo_Mneigh_SD <- coef(lin_MneighSD)[2]  
    }
    names(vvv) <- ddd[-1]
    vvv <- data.frame("growth" = vvv, neigh = list_neigh[ii])
    vvv$jaar <- rownames(vvv)
    list_growth[[ii]] <- vvv  
    list_slopes[[ii]] <- slo_Mneigh
    list_LinMod[[ii]] <- lin_Mneigh
    list_LinMod_SD[[ii]] <- lin_MneighSD
    list_slopes_SD[[ii]] <- slo_Mneigh_SD
    list_Loess[[ii]] <- dataLoess
    list_Loess_SD[[ii]] <- dataLoess_SD
    list_stats[[ii]] <- c(mean(lll, na.rm = T), sd(lll, na.rm = T))
  }
}
names(list_growth) <- list_neigh
names(list_slopes) <- list_neigh
names(list_LinMod) <- list_neigh
names(list_LinMod_SD) <- list_neigh
names(list_slopes_SD) <- list_neigh
names(list_Loess) <- list_neigh
names(list_Loess_SD) <- list_neigh
names(list_stats) <- list_neigh
dataLoess_All <- do.call("rbind", list_Loess)
dataLoess_All_SD <- do.call("rbind", list_Loess_SD)


vect_slopes <- unlist(list_slopes)
vect_slopes_SD <- unlist(list_slopes_SD)
names(vect_slopes) <- gsub("\\.jaar","",names(vect_slopes))
names(vect_slopes_SD) <- gsub("\\.jaar","",names(vect_slopes))
vect_slopes <- data.frame("slope" = vect_slopes, neigh = names(vect_slopes))
vect_slopes_SD <- data.frame("slope_SD" = vect_slopes_SD, neigh = names(vect_slopes_SD))
list_stats <- data.frame(list_stats)
data_Stats <- do.call("rbind", list_stats)
data_Stats <- data.frame(data_Stats)
colnames(data_Stats) <- c("aver_inc", "sd_inc")
data_Stats$neigh <- list_neigh
data_Stats <- merge(data_Stats, unique(data_AMS_BBGA_Nei[, c("gebiedcode15", "gebiednaam")]), all.x = T, by.x = "neigh", by.y = "gebiednaam")
# # 
# # delete IJburg Oost because there is not data 
list_neigh <- list_neigh[!list_neigh %in% "IJburg Oost"]
data_AMS_BBGA_Nei <- data_AMS_BBGA_Nei %>% filter(gebiednaam %in% list_neigh)
data_Stats <- data_Stats %>% filter(neigh %in% list_neigh)
list_Loess <- list_Loess[which(names(list_Loess) %in% list_neigh)]

dataLoess_All <- dataLoess_All %>% filter(neigh %in% list_neigh)
dataLoess_All_SD <- dataLoess_All_SD %>% filter(neigh %in% list_neigh)

# # description of the data 
data_AMS_BBGA_Nei <- merge(data_AMS_BBGA_Nei, data_CPI[, c("jaar", "CPI")], all.x = T)
data_AMS_BBGA_Nei$IHHINK_GEM_D2015 <- with(data_AMS_BBGA_Nei, IHHINK_GEM * 100/CPI)


ppp1 <- ggplot(data_AMS_BBGA_Nei, aes(x = factor(jaar), y = IHHINK_GEM)) +
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
  xlab("Year") + ylab("Nominal Income") + 
  scale_y_continuous(limits = c(0, 1.2e5), breaks = pretty(data_AMS_BBGA_Nei$IHHINK_GEM))+
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point")

ppp2 <- ggplot(data_AMS_BBGA_Nei, aes(x = factor(jaar), y = IHHINK_GEM_D2015)) +
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
  xlab("Year") + ylab("Real Income (values 2015)") +
  scale_y_continuous(limits = c(0, 1.2e5), breaks = pretty(data_AMS_BBGA_Nei$IHHINK_GEM_D2015)) +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point")
  
ppp3 <- grid.arrange(ppp1, ppp2, nrow = 1)
ggsave("../Output/Chapter_1/Step3/Income_Comparison.png", plot = ppp3, width = 10, height = 6)

# # Outlier Bedrijventerrein Sloterdijk 
data_AMS_BBGA_Nei %>% group_by(jaar) %>% summarise(mean(IHHINK_GEM, na.rm = T))
ppp <- data_AMS_BBGA_Nei %>% group_by(jaar) %>% summarise(min(IHHINK_GEM, na.rm = T),
                                                   max(IHHINK_GEM, na.rm = T))
summary(ppp)
ppp <- boxplot(IHHINK_GEM ~jaar, data_AMS_BBGA_Nei)
data_AMS_BBGA_Nei <- merge(data_AMS_BBGA_Nei, data.frame(jaar = ppp$names, maxIncBox = ppp$stats[5, ]), all.x = T, by = "jaar")
table(subset(data_AMS_BBGA_Nei, IHHINK_GEM > maxIncBox)$gebiednaam)
ggbetweenstats(data = data_AMS_BBGA_Nei, 
               x = IHHINK_GEM,
               y = jaar,
               outlier.tagging = TRUE,
               outlier.label = name)

data_AMS_BBGA_Nei %>% group_by(jaar) %>% summarise(quantile(IHHINK_GEM, na.rm = T, probs = 0.9))
tapply(data_AMS_BBGA_Nei$IHHINK_GEM,  data_AMS_BBGA_Nei$jaar, quantile, na.rm = T)
map_AMS_wijken_sf
# # I am here 
# # regression for AMS

data_AgIncomeAMS <-   data_AMS_BBGA_Nei %>% 
  group_by(jaar) %>% 
  summarise(averIncome = mean(IHHINK_GEM, na.rm = T))
data_AgIncomeAMS$averIncome_sd <- fun_sta(data_AgIncomeAMS$averIncome)

lin_MneighAMS <- lm(averIncome ~ jaar, data =  data_AgIncomeAMS)
lin_MneighAMS_SD <- lm(averIncome_sd ~ jaar, data =  data_AgIncomeAMS)

summary(lin_MneighAMS)
slo_MneighAMS <- coef(lin_MneighAMS)[2]      
slo_MneighAMS_SD <- coef(lin_MneighAMS_SD)[2]

vect_slopes <- unlist(list_slopes)
names(vect_slopes) <- list_neigh

vect_slopes_SD <- unlist(list_slopes_SD)
names(vect_slopes_SD) <- list_neigh

vect_slopes <- data.frame("slope" = vect_slopes, neigh = names(vect_slopes))
vect_slopes_SD <- data.frame("slope_SD" = vect_slopes_SD, neigh = names(vect_slopes_SD))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
data_GENT_NEIGH_1 <- data.frame("neigh" = list_neigh)
data_GENT_NEIGH_1 <- merge(data_GENT_NEIGH_1, 
                           unique(data_AMS_BBGA_Nei[, c("gebiedcode15", "gebiednaam")]), 
                           by.x = "neigh", "gebiednaam", all.x = T)

data_GENT_NEIGH_1$neigh <- ordered(data_GENT_NEIGH_1$neigh, levels = data_GENT_NEIGH_1$neigh)
data_GENT_NEIGH_1 <- merge(data_GENT_NEIGH_1, vect_slopes, all.x = T)
data_GENT_NEIGH_1 <- merge(data_GENT_NEIGH_1, vect_slopes_SD, all.x = T)

data_GENT_NEIGH_1$gentry_op1 <- with(data_GENT_NEIGH_1, ifelse(slope>= slo_MneighAMS, 1, 0))
data_GENT_NEIGH_1$gentry_op1SD <- with(data_GENT_NEIGH_1, ifelse(slope_SD>= slo_MneighAMS_SD, 1, 0))

# data_GENT_NEIGH_1$gentry_op1 <- with(data_GENT_NEIGH_1, ifelse(slope>= slope[1], 1, 0))
# data_GENT_NEIGH_1$gentry_op1SD <- with(data_GENT_NEIGH_1, ifelse(slope_SD>= slope_SD[1], 1, 0))
data_GENT_NEIGH_1$gentry_op1Com <- with(data_GENT_NEIGH_1, 
                                        ifelse(gentry_op1 == 1 & gentry_op1SD == 1, 1,
                                               ifelse(gentry_op1 == 1 & gentry_op1SD == 0, 2,
                                                      ifelse(gentry_op1 == 0 & gentry_op1SD == 1, 3,
                                                             ifelse(gentry_op1 == 0 & gentry_op1SD == 0, 4,NA)))))

map_AMS_wijken_sf <- st_as_sf(map_AMS_wijken)
map_AMS_wijken_sf <- merge(map_AMS_wijken_sf, data_GENT_NEIGH_1, by.x = "Buurtcombi", by.y = "gebiedcode15")
map_AMS_wijken_sf$gentry_op1 <- as.factor(map_AMS_wijken_sf$gentry_op1)
map_AMS_wijken_sf$gentry_op1SD <- as.factor(map_AMS_wijken_sf$gentry_op1SD)
map_AMS_wijken_sf$gentry_op1Com <- as.factor(map_AMS_wijken_sf$gentry_op1Com)
map_AMS_wijken_sf$Buurt_Line <- gsub(" |\\/|-", "\n", map_AMS_wijken_sf$Buurtcom_1)

summary(map_AMS_wijken_sf$aver_gr)

tm_shape(map_AMS_wijken_sf) + 
  tm_fill("gentry_op1Com") + 
  tm_text("Buurt_Line", size = 0.3, shadow=TRUE)

tm_shape(map_AMS_wijken_sf) + 
  tm_fill("gentry_op1SD") + 
  tm_text("Buurt_Line", size = 0.3, shadow=TRUE)

# # # Second option. Hochtenbach/ increasing rate  
data_growth <- do.call("rbind", list_growth)
data_growth <- na.omit(data_growth)
vect_aver <- data_growth %>% group_by(neigh) %>% summarise(aver_gr = mean(growth, na.rm = T),
                                                           sd_gr = sd(growth, na.rm = T), 
                                                           geo_aver_gr = geoMean(growth + 1, na.rm = T) - 1)
cut_off <- vect_aver[1,"aver_gr"] + 0.5 * vect_aver[1,"sd_gr"] 
cut_off <- as.numeric(cut_off)

cut_off_2 <- vect_aver[1,"geo_aver_gr"] + 0.5 * vect_aver[1,"sd_gr"] 
cut_off_2 <- as.numeric(cut_off_2)

data_GENT_NEIGH_2 <- vect_aver %>% mutate(gentry_op2 = ifelse(aver_gr > cut_off, 1, 0), 
                                          gentry_op2_2 = ifelse(geo_aver_gr > cut_off_2, 1, 0))
data_GENT_NEIGH_2 <- merge(data_GENT_NEIGH_2, 
                           unique(data_AMS_BBGA_Nei[, c("gebiedcode15", "gebiednaam")]), 
                           by.x = "neigh", "gebiednaam", all.x = T)


map_AMS_wijken_sf <- merge(map_AMS_wijken_sf, 
                           subset(data_GENT_NEIGH_2, select = -c(neigh)), by.x = "Buurtcombi", by.y = "gebiedcode15", all.x = T)
map_AMS_wijken_sf$gentry_op12Com <- with(map_AMS_wijken_sf, 
                                        ifelse(gentry_op2 == 1 & gentry_op1SD == 1, 1,
                                               ifelse(gentry_op2 == 1 & gentry_op1SD == 0, 2,
                                                      ifelse(gentry_op2 == 0 & gentry_op1SD == 1, 3,
                                                             ifelse(gentry_op2 == 0 & gentry_op1SD == 0, 4,NA)))))

map_AMS_wijken_sf$gentry_op12Com <- as.factor(map_AMS_wijken_sf$gentry_op12Com)
map_AMS_wijken_sf$gentry_op12Com_2 <- with(map_AMS_wijken_sf, 
                                         ifelse(gentry_op2_2 == 1 & gentry_op1SD == 1, 1,
                                                ifelse(gentry_op2_2 == 1 & gentry_op1SD == 0, 2,
                                                       ifelse(gentry_op2_2 == 0 & gentry_op1SD == 1, 3,
                                                              ifelse(gentry_op2_2 == 0 & gentry_op1SD == 0, 4,NA)))))


map_AMS_wijken_sf$gentry_op12Com_2 <- as.factor(map_AMS_wijken_sf$gentry_op12Com_2)


map1 <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.6))+
  tm_fill("gentry_op12Com")  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.6)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) 

map2 <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.6))+
  tm_fill("gentry_op12Com_2")  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.6)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) 

tmap_arrange(map1, map2)

# # Maps for option 2 
# # The last option is average (0.5 sd) + rate of growth higher than
map_AMS_wijken_sf$gentry_opDef <- ifelse(map_AMS_wijken_sf$gentry_op12Com %in% 1:3, "Gentrified", "Other Neighborhoods")
map_AMS_wijken_sf$gentry_opDef <- as.factor(map_AMS_wijken_sf$gentry_opDef)

legend_title = expression("Gentrified neighborhoods")
mpp_Ch1_Gen <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "gentry_opDef", palette = c("#fdae61", "white"), title = legend_title)  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_layout(frame = FALSE, inner.margins = 0.1)
tmap_save(mpp_Ch1_Gen, 
          "../Output/Chapter_1/Step3/Map_Gentrification_Income.png", 
          width=1920, height=1080, asp=0)

# # identification of three categories of neighborhoods 
ddd <- c(mean(data_AgIncomeAMS$averIncome, na.rm = T), 
         sd(data_AgIncomeAMS$averIncome, na.rm = T))
cut_SES <- c(-Inf,ddd[1] - ddd[2], ddd[1] + ddd[2], Inf)

data_Stats$category_SES <- cut(data_Stats$aver_inc, breaks = cut_SES, 
                               labels = c("Low-Income", "Average-Income", "High-Income"), 
                               ordered_result = TRUE)
vect_neighGent <- map_AMS_wijken_sf$Buurtcombi[map_AMS_wijken_sf$gentry_opDef == "Gentrified"]
data_Stats$category_SES <- ifelse(data_Stats$gebiedcode15 %in% vect_neighGent, data_Stats$category_SES, NA)
data_Stats$category_SES <- factor(data_Stats$category_SES, labels = c("Low-Income", "Average-Income", "High-Income"), ordered = TRUE)  
map_AMS_wijken_sf <- merge(map_AMS_wijken_sf, data_Stats[, c("gebiedcode15", "category_SES")], by.x = "Buurtcombi", by.y = "gebiedcode15", all.x = T)

map_AMS_wijken_sf <- merge(map_AMS_wijken_sf, map_AMS_district@data[, 1:2], by = "Stadsdeel_", all.x = T)

legend_title = expression(bold("Gentrified neighborhoods-Income"))

mpp_Ch1_SES <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "category_SES",  palette = "seq", 
          textNA = "Other Neighborhoods", colorNA = "white",
          title = legend_title,
          labels = c("Low", "Average", "High"))  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.45)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE, fontface="bold") + 
  tm_layout(frame = T, inner.margins = 0.08, 
            aes.palette = list(seq = "-RdYlBu"),
            legend.outside = F,
            legend.frame = F,
            legend.frame.lwd = 0.5, 
            legend.title.size = 1,
            legend.width = -0.35,
            legend.title.fontfamily = "serif") + 
  tm_compass(position = c("right", "top"), type = "arrow" ) + 
  tm_scale_bar(position=c("right", "bottom"), width= 0.2, just = "left")
#mpp_Ch1_SES
tmap_save(mpp_Ch1_SES, 
          "../Output/Chapter_1/Step3/Map_Gentrification_Income_SES.png",
          width=1920, height=1080, asp=0)


# 
# tm_shape(map_AMS_wijken_sf) +
#   tm_borders(col = "white") + 
#   tm_shape(map_AMS_wijken_sf_GEN) + 
#   tm_fill("geo_aver_gr", palette = brewer.pal(4, "Reds")) + 
#   tm_text("Buurt_Line", size = 0.3, shadow=TRUE) + 
#   tm_shape(map_AMS_wijken_sf_NOGEN) + 
#   tm_fill("geo_aver_gr", palette = brewer.pal(4, "Blues")) + 
#   tm_shape(map_AMS_district) +
#   tm_borders(alpha = 1, lwd = 2, col = gray(0.6)) + 
#   tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
#   tm_shape(map_AMS_wijken_sf) +
#   tm_borders(col = gray(0.6))+
#   tm_fill("gentry_op1", palette = "div")  
# 
# col_PinkY<-c("#fef6b5","#ffdd9a","#ffc285","#ffa679","#fa8a76","#f16d7a","#e15383")
# col_Earth <-c("#A16928","#bd925a","#d6bd8d","#edeac2","#b5c8b8","#79a7ac","#2887a1") 
# col_Temps <- c("#009392","#39b185","#9ccb86","#e9e29c","#eeb479","#e88471","#cf597e")
# 
# #https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=9
# col_Brewer9_Div <- c("#d73027" ,"#f46d43","#fdae61"  ,"#fee090"  ,"#ffffbf"  ,"#e0f3f8"  ,"#abd9e9"  ,"#74add1"  ,"#4575b4")
# col_Brewer9_Red <- c("#ffffcc","#ffeda0", "#fed976", "#feb24c", "#fd8d3c","#fc4e2a", "#e31a1c", "#bd0026", "#800026")
# col_Brewer9_Blue <- c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8","#253494", "#081d58")
# 
# #3d5941,#778868,#b5b991,#f6edbd,#edbb8a,#de8a5a,#ca562c
# tm_shape(map_AMS_wijken_sf) +
#   tm_borders(alpha = 0.3, col = "gray") + 
#   tm_fill("gentry_op1", palette = "div") + 
#   #tm_text("Buurt_Line", size = 0.3, shadow=TRUE) + 
#   tm_shape(map_AMS_district) +
#   tm_borders(lwd = 2, col = "gray") + 
#   tm_text("Stadsdeel", size = 0.5, shadow=TRUE) +
#   tm_basemap(server="OpenStreetMap",alpha=0.5)
# https://carto.com/carto-colors/
#PinkY: #fef6b5,#ffdd9a,#ffc285,#ffa679,#fa8a76,#f16d7a,#e15383
col_gent <- gray(0.8)
col_Nogent <- gray(0.2)
col_AMS <- "red"
# # plots for gentrified neigh 

plot_p <- ggplot(data_AMS_BBGA_Nei, aes(x = jaar, y = IHHINK_GEM))
for(kk in length(list_neigh):1){
  if(is.na(list_Loess[[kk]]))next
  data_LoopNeigh <- data_AMS_BBGA_Nei %>% filter(gebiednaam %in% list_neigh[kk] & !is.na(IHHINK_GEM))  
  subpp <- ggplot(data_LoopNeigh, aes(x = jaar, y = IHHINK_GEM)) + geom_point()
  subpp <- subpp + geom_line(data = list_Loess[[kk]], aes(x = jaarX, y = incomeY), color = "red")
  subpp <- subpp + ggtitle(list_neigh[kk]) + ylab(label = "Income") + xlab("Year")
  ggsave(paste("../Output/Chapter_1/Step3/LOESS/", kk, "-LOESS-",".png", sep = ""), plot = subpp)
  if(list_neigh[kk] %in% vect_neighGent){
      plot_p <- plot_p + geom_line(data = list_Loess[[kk]], aes(x = jaarX, y = incomeY), color = col_gent)
    }else{
      plot_p <- plot_p + geom_line(data = list_Loess[[kk]], aes(x = jaarX, y = incomeY), color = col_Nogent)
    }
}

loessMod <- loess(averIncome ~ jaar, data=data_AgIncomeAMS) 
minLoess <- min(data_AgIncomeAMS$jaar, na.rm = T)
maxLoess <- max(data_AgIncomeAMS$jaar, na.rm = T)
toLoess <- seq(minLoess, maxLoess, length = 100)
predLoess <- predict(loessMod, toLoess)

data_AgIncomeAMS$Income_sd <- fun_sta(data_AgIncomeAMS$averIncome)
loessMod_SD <- loess(Income_sd ~ jaar, data=data_AgIncomeAMS) 
minLoess <- min(data_AgIncomeAMS$jaar, na.rm = T)
maxLoess <- max(data_AgIncomeAMS$jaar, na.rm = T)
toLoess <- seq(minLoess, maxLoess, length = 100)
predLoess_SD <- predict(loessMod_SD, toLoess)
dataLoessAMS <- data.frame(jaarX = toLoess, 
                           incomeY = predLoess,
                           incomeY_SD = predLoess_SD, 
                           neigh = "Amsterdam")

dataLoess_All <- do.call("rbind", list_Loess)
# dataLoess_All <- rbind(dataLoess_All, dataLoessAMS)
dataLoess_All <- merge(dataLoess_All, map_AMS_wijken_sf[, c("neigh", "gentry_opDef", "Stadsdeel")], by = "neigh", all.x = T)
dataLoess_All_SD <- merge(dataLoess_All_SD, map_AMS_wijken_sf[, c("neigh", "gentry_opDef", "Stadsdeel")], by = "neigh", all.x = T)

list_neigh_loess <- levels(dataLoess_All_SD$neigh)



library("RColorBrewer")
display.brewer.pal(n = 8, name = 'RdYlBu')
brewer.pal(n = 8, name = "RdYlBu")

list_distr <- unique(map_AMS_wijken_sf$Stadsdeel)
list_distrLoess <-  list_distr[list_distr %in% dataLoess_All_SD$Stadsdeel]
for(ii in 1:length(list_distrLoess)){
  subLoess <- subset(dataLoess_All_SD, Stadsdeel == list_distrLoess[ii])
  if(all(subLoess$gentry_opDef  == "Other Neighborhoods"))next
  subLoess$neigh <- factor(as.character(subLoess$neigh))
  pp1 <- ggplot(subLoess, aes(x = jaarX, y = incomeY, group = neigh)) + 
    geom_line(color = "#4575B4") + 
    #scale_color_manual(values = color) + 
    gghighlight(gentry_opDef == "Gentrified" , label_key = neigh,
                label_params = list(size = 1.5, color = "#4575B4"),
                unhighlighted_colour = gray(0.8),
                use_direct_label = TRUE) + 
    facet_wrap(~ Stadsdeel) +
    geom_line(data = dataLoessAMS, aes(x = jaarX, y = incomeY_SD), 
              color = "#D73027", 
              linetype = "dotted",
              size = 1) + 
    xlab("Year") + ylab("Income Standardized") +
    scale_x_continuous(breaks = pretty(subLoess$jaarX))
  ggsave(paste("../Output/Chapter_1/Step3/LOESS/",list_distrLoess[ii] , "Gentrified-LOESS-",".png", sep = ""), plot = pp1)
  rm(pp1)
}

map_AMS_wijken_sf_COPY <- map_AMS_wijken_sf
##########################################################################################################
# # # Airbnb 
###########################################################################################################

list_FilesABnB <- dir("./InsideAirbnb")
list_FilesABnB <- list_FilesABnB[grep("201[0-9]", list_FilesABnB)]
list_FilesABnB_2 <- str_split(list_FilesABnB, "_")
year_ABnB <- sapply(list_FilesABnB_2, function(x)x[1])
year_MonthABnB <- sapply(list_FilesABnB_2, function(x)paste(x[1:2], collapse = "_"))

list_jaarABnB = 2015:2019

dataList_airbnb <- NULL
for(ii in 1:length(list_FilesABnB)){
  filePath <- file.path("./InsideAirbnb", list_FilesABnB[ii], "listings.csv")
  dataIter_ABnB <- read.delim(filePath, sep = ",", header = TRUE)
  dataIter_ABnB$year <- year_ABnB[ii]
  dataIter_ABnB$year_month <- year_MonthABnB[ii]
  ooo0 <- length(dataIter_ABnB$id)
  ooo1 <- length(unique(dataIter_ABnB$id))
  if(ooo0>ooo1)stop("para")
  dataList_airbnb <- rbind(dataList_airbnb, dataIter_ABnB)
}
dataList_airbnb <- dataList_airbnb %>% filter(year %in% list_jaarABnB) ## analysis from 2015-2019

dataList_airbnb$ID_DB <- 1:nrow(dataList_airbnb) ## ID for all the data
coord_clean <- dataList_airbnb %>% group_by(id) %>% summarise(latitude2 = mean(latitude, na.rm = T),
                                                              longitude2 = mean(longitude, na.rm = T))
coord_clean <- data.frame(coord_clean)
dim(dataList_airbnb)
dataList_airbnb <- merge(dataList_airbnb, coord_clean, all.x = T)
dim(dataList_airbnb)

dataList_airbnb <- SpatialPointsDataFrame(dataList_airbnb[, c("longitude2", "latitude2")], 
                                          dataList_airbnb,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

dataList_airbnb <- point.in.poly(dataList_airbnb, map_AMS_wijken)
# dataList_airbnb <- SpatialPointsDataFrame(dataList_airbnb[, c("longitude2", "latitude2")], 
#                                           dataList_airbnb,
#                                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

data_airbnb <- dataList_airbnb@data
data_airbnb$year <- as.numeric(data_airbnb$year)
data_airbnb$year_month <- factor(data_airbnb$year_month, levels = year_MonthABnB)
data_airbnb$ID_NY <- with(data_airbnb, paste(Buurtcom_1, year_month, sep = "."))
data_airbnb <- merge(data_airbnb, data_CPI[, c("jaar", "CPI")], all.x = T, by.x = "year", by.y = "jaar")
data_airbnb$price_D2015 <- with(data_airbnb, price * 100/CPI)


aaa1 <- ggplot(subset(data_airbnb, year %in% 2015:2018), aes(year_month, price)) + 
  geom_boxplot() + 
  scale_y_continuous(limits=c(0,800), breaks = seq(0, 800, by = 50), name = "Nominal - Price per night -Airbnb") + 
  theme(axis.text.x = element_text(angle = 90))+ 
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point")

aaa2 <- ggplot(data_airbnb, aes(year_month, price_D2015)) + 
  geom_boxplot() + 
  scale_y_continuous(limits=c(0,800), breaks = seq(0, 800, by = 50), name = "Real - Price per night -Airbnb") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point")
aaa3 <- grid.arrange(aaa1, aaa2, ncol = 1, nrow = 2)
ggsave(filename = "../Output/Chapter_1/Step3/Boxlot_RawDataAirbnbV02.png", plot = aaa3,  width = 12, height = 10)


res <-  boxplot(price ~ year_month, data = data_airbnb, plot = F)
lim_boxplot  <- res$stats
colnames(lim_boxplot) <- res$names
ddd <- apply(lim_boxplot, 2, function(x)any(is.na(x)))
lim_boxplot <- lim_boxplot[, !ddd]
lim_boxplot <- lim_boxplot[c(1,5), ]
lim_boxplot <- t(lim_boxplot)
lim_boxplot <- data.frame(lim_boxplot)
colnames(lim_boxplot) <- c("lim_inf", "lim_sup")
lim_boxplot$ID_NY <- rownames(lim_boxplot)
data_airbnb_2 <- merge(data_airbnb, lim_boxplot, 
                       by.x= "year_month", by.y= "ID_NY", all.x = T)
dim(data_airbnb_2)

### 6.35% outliers 
data_airbnb_3 <- data_airbnb_2 %>% filter(price>= lim_inf & price<= lim_sup)
cat ("# outliers removed in total:", 1-nrow(data_airbnb_3)/nrow(data_airbnb_2))

data_airbnb_Out <- data_airbnb_2 %>% filter(price> lim_sup)
cat ("# outliers removed in total:", 1-nrow(data_airbnb_3)/nrow(data_airbnb_2))
ddd <- data_airbnb_Out %>% group_by(Buurtcom_1, year) %>% summarise(meOutPrice = median(price, na.rm = T), 
                                                                    n = n())


# # Housing Stock 
data_HouStock <- data_AMS_BBGA %>% 
  filter(niveaunaam %in% level & jaar %in% list_jaar & !sdnaam %in% level_unknow) %>% 
  select(codeNeiG, nameNeiG, jaar, "WVOORRBAG")


# # filter number of properties / result shoulg be at least 1% of the housing stock in Airbnb 
data_NeighABnB <- data_airbnb %>% 
  group_by(Buurtcombi, Buurtcom_1, year, year_month) %>% 
  summarise( n_propABnB = n(),
             avPr_ABnB = mean(price, na.rm = T))

# # identification neighborhoods with a minimum of properties 

ttt <- data_NeighABnB %>% group_by(Buurtcombi, Buurtcom_1) %>% summarise(aver_NProp = mean(n_propABnB))
ttt2 <- data_HouStock %>% group_by(gebiedcode15, gebiednaam) %>% summarise(aver_HS = mean(WVOORRBAG, na.rm = T))
ttt <- merge(ttt, ttt2[, c("gebiedcode15", "aver_HS")], by.x = "Buurtcombi", by.y ="gebiedcode15", all.x = T)
ttt$propAbnb <- with(ttt, aver_NProp/aver_HS*100)
  
minPreds1 <- ceiling(quantile(na.omit(ttt$propAbnb), c(0.1)))
minPreds2 <- ceiling(quantile(na.omit(ttt$aver_NProp), c(0.1)))

ouAnNeighABnB <- ttt %>% filter(propAbnb < minPreds1 | aver_NProp < minPreds2) %>% select(Buurtcombi, Buurtcom_1)
ouAnNeighABnB <- as.character(c(ouAnNeighABnB)$Buurtcom_1)
data_airbnb_4 <- data_airbnb_3 %>% filter(!Buurtcom_1 %in% ouAnNeighABnB)

# # eliminate observations without neighborhood
data_airbnb_4 <- subset(data_airbnb_4, !is.na(Buurtcom_1))
ttt <- map_AMS_wijken_sf[, c("Buurtcombi", "Stadsdeel")]
ttt <- data.frame(ttt)[, -3]
dim(data_airbnb_4)
data_airbnb_4 <- merge(data_airbnb_4, ttt, all.x = T)
dim(data_airbnb_4)

# # # Analysis gentrification by price After clean the data
data_AgPrice_4 <- data_airbnb_4 %>% 
  group_by( Buurtcom_1, year_month) %>% 
  summarise(averPrice = mean(price, na.rm = T), 
            medianPrice = median(price, na.rm = T), 
            sdPrice =  sd(price, na.rm = T),
            CV = sdPrice/averPrice*100,
            nProp = n())
data_AgPrice_4$year <- substr(data_AgPrice_4$year_month, 1, 4)

data_AMSincome <- data_AMS_BBGA_Nei %>% filter(gebiednaam %in% list_neigh & !is.na(IHHINK_GEM))  

# # Test if disposable income and price are correlated 

data_AgPrice_5 <- data_airbnb_4 %>% 
  group_by(Buurtcombi, year) %>% 
  summarise(medianPrice = median(price, na.rm = T)) # aggregated by year
data_AgPrice_5 <- merge(data_AgPrice_5, data_AMSincome, 
                        by.y = c("gebiedcode15", "jaar"),
                        by.x = c("Buurtcombi", "year"), 
                        all.y = TRUE)
cor(subset(data_AgPrice_5, year == "2015")[, c("medianPrice", "IHHINK_GEM")],use = "pairwise.complete.obs")
cor(subset(data_AgPrice_5, year == "2016")[, c("medianPrice", "IHHINK_GEM")],use = "pairwise.complete.obs")
cor(subset(data_AgPrice_5, year == "2017")[, c("medianPrice", "IHHINK_GEM")],use = "pairwise.complete.obs")
cor(subset(data_AgPrice_5, year == "2018")[, c("medianPrice", "IHHINK_GEM")],use = "pairwise.complete.obs")


ttt <- na.omit(subset(data_AgPrice_5, year == "2015")[, c("medianPrice", "IHHINK_GEM")])
cor.test(ttt$medianPrice, ttt$IHHINK_GEM, use = "pairwise.complete.obs")
ttt <- na.omit(subset(data_AgPrice_5, year == "2016")[, c("medianPrice", "IHHINK_GEM")])
cor.test(ttt$medianPrice, ttt$IHHINK_GEM, use = "pairwise.complete.obs")
ttt <- na.omit(subset(data_AgPrice_5, year == "2017")[, c("medianPrice", "IHHINK_GEM")])
cor.test(ttt$medianPrice, ttt$IHHINK_GEM, use = "pairwise.complete.obs")
ttt <- na.omit(subset(data_AgPrice_5, year == "2018")[, c("medianPrice", "IHHINK_GEM")])
cor.test(ttt$medianPrice, ttt$IHHINK_GEM, use = "pairwise.complete.obs")


summary(lm(medianPrice ~ IHHINK_GEM, ttt))
anova(lm(medianPrice ~ IHHINK_GEM, ttt))

ttt <- na.omit(data_AgPrice_5)
cor.test(ttt$medianPrice, ttt$IHHINK_GEM, use = "pairwise.complete.obs")
ttt$year <- as.factor(ttt$year)
mod_IncPri <- lm(medianPrice ~ IHHINK_GEM + year, ttt)
summary(mod_IncPri)
anova(mod_IncPri)

# # Aggregated price per year 

data_AgPrice_6 <- data_airbnb_4 %>% 
  group_by(Buurtcombi, year) %>% 
  summarise(medianPrice = median(price, na.rm = T),
            medianPrice2015 = median(price_D2015, na.rm = T)) # aggregated by year

ppp1 <- ggplot(data_AgPrice_6, aes(x = factor(year), y = medianPrice)) +
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
  xlab("Year") + ylab("Price per night") + 
  scale_y_continuous(limits = c(30, 200), breaks = pretty(data_AgPrice_6$medianPrice)) +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point")

ppp2 <- ggplot(data_AgPrice_6, aes(x = factor(year), y = medianPrice2015)) +
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
  xlab("Year") + ylab("Price per night (without inflation effect)") +
  scale_y_continuous(limits = c(30, 200), breaks = pretty(data_AgPrice_6$medianPrice2015)) +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point")

ppp3 <- grid.arrange(ppp1, ppp2, nrow = 1)
ggsave("../Output/Chapter_1/Step3/Airbnb_NomRealPricePerYear.png", plot = ppp3, width = 10, height = 6)


# # Description of the prices for each neighborhood 
ppp <- subset(data_airbnb_4)
ppp$year <- factor(ppp$year)
library(ggridges)
ppp1 <- ggplot(ppp, aes(x = price, y = year)) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 1.5, size = 0.2, bandwidth = 8)   +
  scale_y_discrete(name = "Year", expand = c(0.01, 0)) +
  scale_x_continuous(name = "Nominal - Price per night -Airbnb", breaks = seq(0, 500, by = 50)) +
  scale_fill_gradientn(
    colours = c("#FFC300", "#C70039", "#581845"),
    name = "Price"
  ) + 
  theme_ridges() #+ theme(legend.position = "none")

ppp2 <- ggplot(ppp, aes(x = price_D2015, y = year)) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 1.5, size = 0.2, bandwidth = 8)   +
  scale_y_discrete(name = "Year", expand = c(0.01, 0)) +
  scale_x_continuous(name = "Real - Price per night -Airbnb", breaks = seq(0, 500, by = 50)) +
  scale_fill_gradientn(
    colours = c("#FFC300", "#C70039", "#581845"),
    name = "Price"
  ) + 
  theme_ridges() #+ theme(legend.position = "none")
ggsave(filename = "../Output/Chapter_1/Step3/Airbnb_NominalPricePerYear.png", plot = ppp1)
ggsave(filename = "../Output/Chapter_1/Step3/Airbnb_RealPricePerYear.png", plot = ppp2)


# # Identification neighborhoods gentrified 

list_growYearABnB <- list()
list_neighABnB <- unique(data_AgPrice_4$Buurtcom_1)
list_growthABnB <- list()
list_LinModABnB <- list()
list_LinModABnB_SD <- list()
list_slopesABnB <- list()
list_slopesABnB_SD<- list()
list_statsABnB <- list()
data_AgPrice_4$year <-substr(data_AgPrice_4$year_month, 1, 4)
data_AgPrice_4$year <- as.numeric(data_AgPrice_4$year)
data_AgPrice_4$year_month_2  <- as.numeric(data_AgPrice_4$year_month)
lin_Mneigh <- NULL
slo_Mneigh <- NULL
lin_Mneigh_SD <- NULL
slo_Mneigh_SD <- NULL


for(ii in 1:length(list_neighABnB)){
  data_LoopNeigh <- data_AgPrice_4 %>% filter(Buurtcom_1 %in% list_neighABnB[ii] & !is.na(averPrice))  
  data_LoopNeigh <- data_LoopNeigh[order(data_LoopNeigh$year_month_2), ]
  vvv <- NULL
  lll <- data_LoopNeigh$averPrice
  data_LoopNeigh$Price_sd <- fun_sta(lll) ## price  standardied 
  ddd <- data_LoopNeigh$year_month
  
  for(jj in 1:(nrow(data_LoopNeigh)-1)){
    # # growth year per year
    vvv[jj] <- (lll[jj+1] -lll[jj] )/lll[jj]
  }
  # # growth total
  if(all(is.na(vvv)) | length(vvv) == 1){
    cat("Return NA:", list_neigh[ii], "\n")
    list_growthABnB[[ii]] <- NA
    list_statsABnB[[ii]] <- NA
  }else{
    if(nrow(data_LoopNeigh)<5){
      cat("Less than 5:", list_neigh[ii], "\n")
      slo_Mneigh <- NA
      lin_Mneigh <- NA
      slo_Mneigh_SD <- NA
      lin_Mneigh_SD <- NA
      dataLoess <- NA
    }else{
      lin_Mneigh <- lm(averPrice ~ year_month_2, data =  data_LoopNeigh)
      slo_Mneigh <- coef(lin_Mneigh)[2]   
      lin_Mneigh_SD <- lm(Price_sd ~ year_month_2, data =  data_LoopNeigh)
      slo_Mneigh_SD <- coef(lin_Mneigh_SD)[2]   
      
      # 
      # lin_Mneigh <- lm(IHHINK_GEM ~ jaar, data =  data_LoopNeigh)
      # lin_MneighSD <- lm(Income_sd ~ jaar, data =  data_LoopNeigh)
      # loessMod <- loess(IHHINK_GEM ~ jaar, data=data_LoopNeigh) 
      # minLoess <- min(data_LoopNeigh$jaar, na.rm = T)
      # maxLoess <- max(data_LoopNeigh$jaar, na.rm = T)
      # toLoess <- seq(minLoess, maxLoess, length = 100)
      # predLoess <- predict(loessMod, toLoess)
      # dataLoess <- data.frame(jaarX = toLoess, incomeY = predLoess,neigh = list_neigh[ii])
      # slo_Mneigh <- coef(lin_Mneigh)[2]      
      # slo_Mneigh_SD <- coef(lin_MneighSD)[2]  
    }
    names(vvv) <- ddd[-1]
    vvv <- data.frame("growth" = vvv, neigh = list_neighABnB[ii])
    vvv$jaar <- rownames(vvv)
    
    list_growthABnB[[ii]] <- vvv
    list_LinModABnB[[ii]] <- lin_Mneigh
    list_LinModABnB_SD[[ii]] <- lin_Mneigh_SD
    
    list_slopesABnB[[ii]] <- slo_Mneigh
    list_slopesABnB_SD[[ii]] <- slo_Mneigh_SD
    list_statsABnB[[ii]] <- c(mean(lll, na.rm = T), sd(lll, na.rm = T))
  }
}
names(list_LinModABnB) <- list_neighABnB
names(list_LinModABnB_SD) <- list_neighABnB
names(list_slopesABnB) <- list_neighABnB
names(list_slopesABnB_SD) <- list_neighABnB
names(list_growthABnB) <- list_neighABnB
names(list_statsABnB) <- list_neighABnB

data_AgPriceAMS_4 <-   data_airbnb_4 %>% 
  group_by(year_month) %>% 
  summarise(averPrice = mean(price, na.rm = T))
data_AgPriceAMS_4$year_month_2  <- as.numeric(data_AgPriceAMS_4$year_month)
data_AgPriceAMS_4$averPrice_SD <- fun_sta(data_AgPriceAMS_4$averPrice)

# # regression for AMS

lin_MneighAMS <- lm(averPrice ~ year_month_2, data =  data_AgPriceAMS_4)
lin_MneighAMS_SD <- lm(averPrice_SD ~ year_month_2, data =  data_AgPriceAMS_4)

summary(lin_MneighAMS)
slo_MneighAMS <- coef(lin_MneighAMS)[2]      
slo_MneighAMS_SD <- coef(lin_MneighAMS_SD)[2]

vect_slopesABnB <- unlist(list_slopesABnB)
names(vect_slopesABnB) <- list_neighABnB

vect_slopesABnB_SD <- unlist(list_slopesABnB_SD)
names(vect_slopesABnB_SD) <- list_neighABnB

vect_slopesABnB <- data.frame("slopeABnB" = vect_slopesABnB, neigh = names(vect_slopesABnB))
vect_slopesABnB_SD <- data.frame("slopeABnB_SD" = vect_slopesABnB_SD, neigh = names(vect_slopesABnB_SD))

data_GENT_NEIGH_1 <- merge(data_GENT_NEIGH_1, vect_slopesABnB, all.x = T)
data_GENT_NEIGH_1 <- merge(data_GENT_NEIGH_1, vect_slopesABnB_SD, all.x = T)

data_GENT_NEIGH_1$gentryABnB <- with(data_GENT_NEIGH_1, ifelse(slopeABnB>= slo_MneighAMS, 1, 0))
data_GENT_NEIGH_1$gentryABnB_SD <- with(data_GENT_NEIGH_1, ifelse(slopeABnB_SD>= slo_MneighAMS_SD, 1, 0))
data_GENT_NEIGH_1$gentryABnB <- as.factor(data_GENT_NEIGH_1$gentryABnB)
data_GENT_NEIGH_1$gentryABnB_SD <- as.factor(data_GENT_NEIGH_1$gentryABnB_SD)
map_AMS_wijken_sf2 <- merge(map_AMS_wijken_sf, data_GENT_NEIGH_1[, c("neigh", "gentryABnB", "gentryABnB_SD")], by.x = "Buurtcom_1", by.y = "neigh", all.x= T)

tm_shape(map_AMS_wijken_sf2) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "gentryABnB_SD", title = legend_title)  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_layout(frame = FALSE, inner.margins = 0.1)


# # Hochtenbach for AMS
lll <- data_AgPriceAMS_4$averPrice
vvv <- NULL
for(jj in 1:(nrow(data_AgPriceAMS_4)-1)){
  # # growth year per year
  vvv[jj] <- (lll[jj+1] -lll[jj] )/lll[jj]
}

data_growthABnB <- do.call("rbind", list_growthABnB)
data_growthABnB <- na.omit(data_growthABnB)
vect_averABnB <- data_growthABnB %>% group_by(neigh) %>% summarise(averABnB_gr = mean(growth, na.rm = T),
                                                                   sdABnB_gr = sd(growth, na.rm = T), 
                                                                   geoABnB_aver_gr = geoMean(growth + 1, na.rm = T) - 1)
cut_offABnB <- mean(vvv, na.rm = T) + 0.5 * sd(vvv, na.rm = T)
cut_offABnB <- as.numeric(cut_offABnB)

cut_offABnB_2 <- geoMean(vvv + 1, na.rm = T) + 0.5 * sd(vvv, na.rm = T)
cut_offABnB_2 <- as.numeric(cut_offABnB_2)

vect_averABnB <- vect_averABnB %>% mutate(gentryABnB_op2 = ifelse(averABnB_gr > cut_offABnB, 1, 0), 
                                          gentryABnB_op2_2 = ifelse((geoABnB_aver_gr + 1) > cut_offABnB_2, 1, 0))

data_GENT_NEIGH_2 <- merge(data_GENT_NEIGH_2, vect_averABnB, all.x = T)
map_AMS_wijken_sf3 <- merge(map_AMS_wijken_sf2, data_GENT_NEIGH_2[, c(1,7:12)], by.x = "Buurtcom_1", by.y = "neigh", all.x = T)
map_AMS_wijken_sf3$gentryABnB_op2 <- as.factor(map_AMS_wijken_sf3$gentryABnB_op2)
map_AMS_wijken_sf3$gentryABnB_op12Com <- with(map_AMS_wijken_sf3, 
                                         ifelse(gentryABnB_op2 == 1 & gentryABnB_SD == 1, 1,
                                                ifelse(gentryABnB_op2 == 1 & gentryABnB_SD == 0, 2,
                                                       ifelse(gentryABnB_op2 == 0 & gentryABnB_SD == 1, 3,
                                                              ifelse(gentryABnB_op2 == 0 & gentryABnB_SD == 0, 4,NA)))))
map_AMS_wijken_sf3$gentryABnB_op12Com <- as.factor(map_AMS_wijken_sf3$gentryABnB_op12Com)
map_AMS_wijken_sf <- map_AMS_wijken_sf3
map_AMS_wijken_sf$gentryABnB_op12Com <- as.factor(map_AMS_wijken_sf$gentryABnB_op12Com)
map_AMS_wijken_sf$gentryABnB_opDef <- ifelse(map_AMS_wijken_sf$gentryABnB_op12Com %in% 1:3, "Gentrified", 
                                             ifelse(map_AMS_wijken_sf$gentryABnB_op12Com == 4, "Other Neighborhoods", NA))
map_AMS_wijken_sf$gentryABnB_opDef <- as.factor(map_AMS_wijken_sf$gentryABnB_opDef)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

legend_title = expression("Gentrified neighborhoods-Price")
pp <- subset(map_AMS_wijken_sf, gentryABnB_opDef == "Gentrified" & gentry_opDef == "Gentrified")
pp$gentry_opDef <- as.factor(as.character(pp$gentry_opDef))
mpp_Ch1ABnB_Gen <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "gentryABnB_opDef",  palette = c("#fdae61", "white"), title = legend_title, textNA = "Without short-term rental")  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_shape(pp) +
  tm_fill(col = "gentry_opDef", palette = c("#fdae61"), title = "Gentrified by income")  +
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

mpp_Ch1ABnB_Gen
tmap_save(mpp_Ch1ABnB_Gen, "../Output/Chapter_1/Step3/Map_GentrificationAIRBNB_PRICE.png", width=1920, height=1080, asp=0)


mpp_Ch1_SES <- tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6)+
  tm_fill(col = "category_SES",  palette = "seq", 
          textNA = "Other Neighborhoods", colorNA = "white",
          title = legend_title,
          labels = c("Low", "Average", "High"))  +
  tm_shape(map_AMS_district) +
  tm_borders(alpha = 1, lwd = 2, col = gray(0.45)) + 
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE, fontface="bold") + 
  tm_layout(frame = T, inner.margins = 0.08, 
            aes.palette = list(seq = "-RdYlBu"),
            legend.outside = F,
            legend.frame = F,
            legend.frame.lwd = 0.5, 
            legend.title.size = 1,
            legend.width = -0.35,
            legend.title.fontfamily = "serif") + 
  tm_compass(position = c("right", "top"), type = "arrow" ) + 
  tm_scale_bar(position=c("right", "bottom"), width= 0.2, just = "left")





tm_shape(subset(map_AMS_wijken_sf2, entHouGent_2_1 == "Propensity Short-Term")) +
  tm_fill(col = "entHouGent_2_1", palette = c("#fdae61"), title = legend_title2)  +
  tm_borders(col = "#5e3c99", lwd=4)


# # Distance from the centre (neighborhoods)
point_DAMSQR <- data.frame("latitude" = 52.3731, "longitude" = 4.8926, "place" = "DAM SQUARE")
point_DAMSQR <- SpatialPointsDataFrame(coords = point_DAMSQR[, c("longitude", "latitude")], 
                                       data = point_DAMSQR,
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
point_DAMSQR_sf <- st_as_sf(point_DAMSQR)
map_Centr_Dis = gCentroid(map_AMS_district, byid=TRUE, id = map_AMS_district@data$Stadsdeel)
map_Centr_Dis_sf <- st_as_sf(map_Centr_Dis)
map_Centr_Wijken = gCentroid(map_AMS_wijken, byid=TRUE, id = map_AMS_wijken@data$Buurtcom_1)
map_Centr_Wijken_sf <- st_as_sf(map_Centr_Wijken)


tm_shape(map_AMS_district) +
  tm_polygons() +
  tm_shape(point_DAMSQR_sf) +
  tm_symbols(col = "black", border.col = "white")

tm_shape(map_AMS_wijken) +
  tm_polygons() +
  tm_shape(map_Centr_Wijken) +
  tm_symbols(col = "black", border.col = "white")

dist_centre <- st_distance(point_DAMSQR_sf, map_Centr_Wijken_sf)
dist_centre <- data.frame(dist_centre = t(dist_centre))
dist_centre$neigh <- rownames(map_Centr_Wijken@coords)
dist_centre <- merge(dist_centre, map_AMS_wijken_sf[, c("Buurtcombi", "Buurtcom_1", "Stadsdeel")], by.x= "neigh", by.y = "Buurtcom_1")
dist_centre <- dist_centre[order(dist_centre$dist_centre), ]
map_AMS_wijken_sf <- merge(map_AMS_wijken_sf, dist_centre[, c("Buurtcombi", "dist_centre")], by = "Buurtcombi", all.x = T)
# # distance properties from the city centre 
dataList_airbnb_sf <- st_as_sf(dataList_airbnb)
dataList_airbnb_4sf <- subset(dataList_airbnb_sf, ID_DB %in%data_airbnb_4$ID_DB)
dist_centre_ABnB <- st_distance(dataList_airbnb_4sf,point_DAMSQR_sf)
dist_centre_ABnB <- data.frame(dist_centre = dist_centre_ABnB, ID_DB = dataList_airbnb_4sf$ID_DB)
data_airbnb_4 <- merge(data_airbnb_4, dist_centre_ABnB, all.x = T)
ttt <- dist_centre[, c("Buurtcombi", "dist_centre")]
ttt <- data.frame(ttt)
colnames(ttt)[2] <- "dist_centre_Wijken" 
data_airbnb_4 <- merge(data_airbnb_4, ttt, all.x = T)

data_airbnb_4$Buurtcom_1_OR <- factor(as.character(data_airbnb_4$Buurtcom_1), 
                                      levels = dist_centre$neigh, 
                                      ordered = T)

# # saving objects
#### sve for next step 
save(dataList_airbnb, file = "../Output/Chapter_1/Step3/dataList_airbnb.Rdata")
save(data_airbnb_4, file = "../Output/Chapter_1/Step3/data_airbnb_4.Rdata")
save(dataList_airbnb_sf, file = "../Output/Chapter_1/Step3/dataList_airbnb_sf.Rdata")
save(dataList_airbnb_4sf, file = "../Output/Chapter_1/Step3/dataList_airbnb_4sf.Rdata")
save(map_AMS_wijken_sf, file = "../Output/Chapter_1/Step3/map_AMS_wijken_sf.Rdata")
save(map_Centr_Wijken_sf, file = "../Output/Chapter_1/Step3/map_Centr_Wijken_sf.Rdata")



# # Characterization of neighborhoods according with their characteristics per neighborhood

# # data filtered only with non-outlier values
dataList_airbnb_4sf$id2 <- with(dataList_airbnb_4sf, paste(id, year, sep = "_"))
qqq <- dataList_airbnb_4sf %>% filter(!duplicated(id2))
tabABnB_roomYe <- with(qqq, table(room_type, year))
tabABnB_roomYe <- addmargins(tabABnB_roomYe, margin = 1)
xtable(tabABnB_roomYe)  
dataList_airbnb15_sf <- dataList_airbnb_4sf %>% filter(year == 2015) %>% dplyr:::select(id, room_type, price)
dataList_airbnb15_sf <- dataList_airbnb15_sf %>% group_by(id, room_type) %>% summarise(averPrice = mean(price, na.rm = T))
dataList_airbnb15_sf <- dataList_airbnb15_sf[!duplicated(dataList_airbnb15_sf$id), ]
fff <- table(dataList_airbnb15_sf$room_type)
fff <- names(fff[fff != 0])
dataList_airbnb15_sf$room_type <- as.character(dataList_airbnb15_sf$room_type)
dataList_airbnb15_sf$room_type <- factor(dataList_airbnb15_sf$room_type, levels = fff)
print(nrow(dataList_airbnb15_sf))

dataList_airbnb16_sf <- dataList_airbnb_sf %>% filter(year == 2016) %>% dplyr:::select(id, room_type, price)
dataList_airbnb16_sf <- dataList_airbnb16_sf %>% group_by(id, room_type) %>% summarise(averPrice = mean(price, na.rm = T))
dataList_airbnb16_sf <- dataList_airbnb16_sf[!duplicated(dataList_airbnb16_sf$id), ]
fff <- table(dataList_airbnb16_sf$room_type)
fff <- names(fff[fff != 0])
dataList_airbnb16_sf$room_type <- as.character(dataList_airbnb16_sf$room_type)
dataList_airbnb16_sf$room_type <- factor(dataList_airbnb16_sf$room_type, levels = fff)
print(nrow(dataList_airbnb16_sf))

dataList_airbnb17_sf <- dataList_airbnb_sf %>% filter(year == 2017) %>% dplyr:::select(id, room_type, price)
dataList_airbnb17_sf <- dataList_airbnb17_sf %>% group_by(id, room_type) %>% summarise(averPrice = mean(price, na.rm = T))
dataList_airbnb17_sf <- dataList_airbnb17_sf[!duplicated(dataList_airbnb17_sf$id), ]
fff <- table(dataList_airbnb17_sf$room_type)
fff <- names(fff[fff != 0])
dataList_airbnb17_sf$room_type <- as.character(dataList_airbnb17_sf$room_type)
dataList_airbnb17_sf$room_type <- factor(dataList_airbnb17_sf$room_type, levels = fff)
print(nrow(dataList_airbnb17_sf))

dataList_airbnb18_sf <- dataList_airbnb_sf %>% filter(year == 2018) %>% dplyr:::select(id, room_type, price)
dataList_airbnb18_sf <- dataList_airbnb18_sf %>% group_by(id, room_type) %>% summarise(averPrice = mean(price, na.rm = T))
dataList_airbnb18_sf <- dataList_airbnb18_sf[!duplicated(dataList_airbnb18_sf$id), ]
fff <- table(dataList_airbnb18_sf$room_type)
fff <- names(fff[fff != 0])
dataList_airbnb18_sf$room_type <- as.character(dataList_airbnb18_sf$room_type)
dataList_airbnb18_sf$room_type <- factor(dataList_airbnb18_sf$room_type, levels = fff)
print(nrow(dataList_airbnb18_sf))

dataList_airbnb19_sf <- dataList_airbnb_sf %>% filter(year == 2019) %>% dplyr:::select(id, room_type, price)
dataList_airbnb19_sf <- dataList_airbnb19_sf %>% group_by(id, room_type) %>% summarise(averPrice = mean(price, na.rm = T))
dataList_airbnb19_sf <- dataList_airbnb19_sf[!duplicated(dataList_airbnb19_sf$id), ]
fff <- table(dataList_airbnb19_sf$room_type)
fff <- names(fff[fff != 0])
dataList_airbnb19_sf$room_type <- as.character(dataList_airbnb19_sf$room_type)
dataList_airbnb19_sf$room_type <- factor(dataList_airbnb19_sf$room_type, levels = fff)
print(nrow(dataList_airbnb19_sf))

dataList_airbnb20_sf <- dataList_airbnb_sf %>% filter(year == 2020) %>% dplyr:::select(id, room_type, price)
dataList_airbnb20_sf <- dataList_airbnb20_sf %>% group_by(id, room_type) %>% summarise(averPrice = mean(price, na.rm = T))
dataList_airbnb20_sf <- dataList_airbnb20_sf[!duplicated(dataList_airbnb20_sf$id), ]
fff <- table(dataList_airbnb20_sf$room_type)
fff <- names(fff[fff != 0])
dataList_airbnb20_sf$room_type <- as.character(dataList_airbnb20_sf$room_type)
dataList_airbnb20_sf$room_type <- factor(dataList_airbnb20_sf$room_type, levels = fff)
print(nrow(dataList_airbnb20_sf))

mpp_ttt15 <- 
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = "white") +
  tm_shape(dataList_airbnb15_sf[1:1000, ]) +
  tm_symbols(col = "room_type", palette = "Accent", size = "averPrice", scale = 0.8, clustering = T) +
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6) + 
  tm_shape(map_AMS_district) +
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_borders(lwd = 1)  + 
  tm_layout(frame = FALSE, inner.margins = 0.1)
mpp_ttt15

mpp_ttt15 <- 
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = "white") +
  tm_shape(dataList_airbnb15_sf) +
  tm_dots(col = "room_type", palette = "Set2", size = 0.02, title = "Room Type 2015") +
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6) + 
  tm_shape(map_AMS_district) +
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_borders(lwd = 1)  + 
  tm_layout(frame = FALSE, inner.margins = 0.1)

mpp_ttt16 <- 
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = "white") +
  tm_shape(dataList_airbnb16_sf) +
  tm_dots(col = "room_type", palette = "Set2", size = 0.02, title = "Room Type 2016") +
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6) + 
  tm_shape(map_AMS_district) +
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_borders(lwd = 1)  + 
  tm_layout(frame = FALSE, inner.margins = 0.1)

mpp_ttt17 <- 
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = "white") +
  tm_shape(dataList_airbnb17_sf) +
  tm_dots(col = "room_type", palette = "Set2", size = 0.02, title = "Room Type 2017") +
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6) + 
  tm_shape(map_AMS_district) +
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_borders(lwd = 1)  + 
  tm_layout(frame = FALSE, inner.margins = 0.1)

mpp_ttt18 <- 
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = "white") +
  tm_shape(dataList_airbnb18_sf) +
  tm_dots(col = "room_type", palette = "Set2", size = 0.02, title = "Room Type 2018") +
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6) + 
  tm_shape(map_AMS_district) +
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_borders(lwd = 1)  + 
  tm_layout(frame = FALSE, inner.margins = 0.1)

mpp_ttt19 <- 
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = "white") +
  tm_shape(dataList_airbnb19_sf) +
  tm_dots(col = "room_type", palette = "Set2", size = 0.02, title = "Room Type 2019") +
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6) + 
  tm_shape(map_AMS_district) +
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_borders(lwd = 1)  + 
  tm_layout(frame = FALSE, inner.margins = 0.1)

mpp_ttt20 <- 
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = "white") +
  tm_shape(dataList_airbnb20_sf[1:1000, ]) +
  tm_dots(col = "room_type", palette = "Set2", size = 0.02, title = "Room Type 2020") +
  tm_shape(map_AMS_wijken_sf) +
  tm_borders(col = gray(0.5), alpha = 0.6) + 
  tm_shape(map_AMS_district) +
  tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
  tm_borders(lwd = 1)  + 
  tm_layout(frame = FALSE, inner.margins = 0.1)

#mpp_ttt1520 <- tmap_arrange(mpp_ttt15, mpp_ttt16, mpp_ttt17, mpp_ttt18, mpp_ttt19, mpp_ttt20, ncol = 3)
tmap_save(mpp_ttt15, "../Output/Chapter_1/Step3/maps_ListABnB_RT_15.png", width=1920, height=1080, asp=0) ## room type
tmap_save(mpp_ttt16, "../Output/Chapter_1/Step3/maps_ListABnB_RT_16.png", width=1920, height=1080, asp=0) ## room type
tmap_save(mpp_ttt17, "../Output/Chapter_1/Step3/maps_ListABnB_RT_17.png", width=1920, height=1080, asp=0) ## room type
tmap_save(mpp_ttt18, "../Output/Chapter_1/Step3/maps_ListABnB_RT_18.png", width=1920, height=1080, asp=0) ## room type
tmap_save(mpp_ttt19, "../Output/Chapter_1/Step3/maps_ListABnB_RT_19.png", width=1920, height=1080, asp=0) ## room type
tmap_save(mpp_ttt20, "../Output/Chapter_1/Step3/maps_ListABnB_RT_20.png", width=1920, height=1080, asp=0) ## room type




