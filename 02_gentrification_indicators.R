
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
# # # Loading GIS information
load("../Output/Chapter_1/Step3/dataList_airbnb.Rdata")
load("../Output/Chapter_1/Step3/dataList_airbnb_sf.Rdata")
load("../Output/Chapter_1/Step3/map_AMS_wijken_sf.Rdata")
load("../Output/Chapter_1/Step3/data_airbnb_4.Rdata")
map_AMS_district <- readOGR(dsn = "./GIS/geojson_district", layer = "geojson_districts-polygon")

list_jaar = 2007:2018
level <- c("Wijken")
level_region <- "Buurtcombi"
level_unknow <- "Z onbekend"
codeNeiG <- "gebiedcode15"
nameNeiG <- "gebiednaam"

# # # using the data cleaned 
data_airbnb_4$id_year <- with(data_airbnb_4, paste(id, year, sep = "_"))
ppp <- data_airbnb_4 %>% group_by(year) %>% summarise(rrr = length(unique(id_year)))
sum(ppp$rrr)
data_airbnb_4 %>% group_by(Buurtcom_1, year) %>% summarise(nList = n())

dataList_airbnb_sf

# Indicator 1: Residential characteristics 

# # Residential characteristics 
list_IND1 <- subset(data_IND, Type == "Residential Characteristics" & Selected == "Included")
list_IND1 <- list_IND1 %>% filter(!is.na(list_IND1$Variable))
list_IND1$Variable <- toupper(list_IND1$Variable)
cols_IND1 <- colType %>% filter(Variable %in% list_IND1$Variable) %>% dplyr:::select(Col, Variable)
dim(cols_IND1)
cols_IND1 <- merge(cols_IND1, list_IND1, all.x = T)
dim(cols_IND1)
cols_IND1 <- cols_IND1 %>% select(-c(Definition, Definitie))
varsIND1 <- unlist(cols_IND1$Col, use.names = FALSE)
vvv1 <- c(vars_fix, varsIND1)

# lll <- data_AMS_BBGA %>% 
#   filter(niveaunaam %in% level & jaar %in% list_jaar & !sdnaam %in% level_unknow) 
# 
# dim(lll[is.na(lll$BEVALLEENHH_P), c("gebiednaam", "jaar")])
# dim(data_IND1[is.na(data_IND1$EDU_High), ])

data_IND1 <- data_AMS_BBGA %>% 
  filter(niveaunaam %in% level & jaar %in% list_jaar & !sdnaam %in% level_unknow) %>% 
  dplyr:::select(codeNeiG, nameNeiG, jaar, vvv1)
summary(data_IND1)

ppp <- data_AMS_BBGA %>% 
  filter(niveaunaam %in% level & jaar %in% list_jaar & !sdnaam %in% level_unknow) %>% 
  dplyr:::select(codeNeiG, nameNeiG, jaar, c("BEV18_26", "BEV27_65","BEV66PLUS", "BEVVEST", "BEVVERT", "BEVVESTINT", "BEVVERTINT", "BEVGELIJKINT", "BEVWOONDUUR"))
summary(ppp)
cor(ppp[, c("BEV18_26", "BEV27_65","BEV66PLUS")], use = "pairwise.complete.obs")
cor(ppp[, c("BEV18_26", "BEV27_65","BEV66PLUS", "BEVVEST", "BEVVERT", "BEVVESTINT", "BEVVERTINT", "BEVGELIJKINT", "BEVWOONDUUR")], use = "pairwise.complete.obs")

map_info <- map_AMS_wijken_sf[, c("Stadsdeel", "Buurtcombi", "Buurtcom_1", "Stadsdeel_", "Opp_m2")]
map_info <- data.frame(map_info)[, -6] ## eliinate geometry 
data_IND1 <- merge(data_IND1, map_info, by.x = codeNeiG, by.y = "Buurtcombi", all.x= T)
list_neigh <- unique(data_IND1$Buurtcom_1)

data_IND1_Stad <- data_AMS_BBGA %>% 
  filter(niveaunaam %in% "Stadsdelen" & jaar %in% list_jaar & !sdnaam %in% level_unknow) %>% 
  dplyr:::select(codeNeiG, nameNeiG, jaar, vvv1)
summary(data_IND1_Stad)


# # neighborhoods without data 
data_IND1 <- data_IND1 %>% filter(!gebiednaam %in% "IJburg Oost")
data_IND1$ID <- 1:nrow(data_IND1)
# # Variable imputation using stadsdelen 
for(ii in 9:length(vvv1)){
  ddd1 <- is.na(data_IND1[, vvv1[ii]])
  ddd <- sum(ddd1)/length(data_IND1[, vvv1[ii]])
  if(ddd>0){
    nnn <- data_IND1_Stad[, c("SD", "jaar", vvv1[ii])]
    colnames(nnn) <- c("Stadsdeel_","jaar", "varImp")
    data_IND1 <- merge(data_IND1, nnn, by = c("Stadsdeel_", "jaar"), all.x = T)
    qqq <- sum(is.na(data_IND1[, vvv1[ii]]) & !is.na(data_IND1$varImp))
    cat(vvv1[ii], " - ", sum(ddd1), ".Replaced: ",qqq ,"\n")
    data_IND1[, vvv1[ii]] <- ifelse(is.na(data_IND1[, vvv1[ii]]), data_IND1$varImp, data_IND1[, vvv1[ii]])
    data_IND1$varImp <- NULL
   }
}

outAn <- NULL
kk <- 1
for(ii in 9:length(vvv1)){
  www <- prop.table(table(is.na(data_IND1[, vvv1[ii]])))
  if(length(www) == 1) next
  if(www[2] > 0.3){
    outAn[kk] <-  vvv1[ii]
    kk = kk + 1
  }
}

data_IND1 <- data_IND1[, !colnames(data_IND1) %in% outAn]
summary(data_IND1)

data_IND1$popKm2 <- with(data_IND1, BEVTOTAAL/(Opp_m2*1e-6))# population per Km2
varsIND1 <- c(varsIND1, "popKm2")

renameCol <- cols_IND1$Relabel[match(colnames(data_IND1), cols_IND1$Col)]
renameCol[is.na(renameCol)] <- colnames(data_IND1)[is.na(renameCol)]
colnames(data_IND1) <- renameCol
data_IND1$"PopDut65Plus" <- data_IND1$"PopDut65_79" + data_IND1$"PopDut80Plus"
data_IND1$"PopWE65Plus" <- data_IND1$"PopWE65_79" + data_IND1$"PopWE80Plus"
data_IND1$"PopNWe65Plus" <- data_IND1$"PopNWe65_79" + data_IND1$"PopNWe80Plus"

data_IND1$"PopDut65_79"  <- NULL
data_IND1$"PopDut80+" <- NULL
data_IND1$"PopWE65_79" <- NULL
data_IND1$"PopWE80+" <- NULL
data_IND1$"PopNWe65_79" <- NULL
data_IND1$"PopNWe80+" <- NULL
data_IND1$"popReg" <- NULL
list_IND1_02 <- subset(data_IND, Type == "Residential Characteristics" & Selected == "Included")
varIND1_02 <- list_IND1_02[list_IND1_02$Relabel %in%colnames(data_IND1) ,]
varIND1_02 <- varIND1_02[order(varIND1_02$ID), ]
varIND1_02 <- varIND1_02$Relabel
ttt <- colnames(data_IND1)[!colnames(data_IND1) %in% varIND1_02]
ttt <- c(ttt, varIND1_02)
data_IND1_02 <- data_IND1[, ttt]
data_IND1_02 <- data_IND1_02[order(data_IND1_02$gebiedcode15, data_IND1_02$jaar), ]

# colnames(data_IND1_02) <- gsub("\\+", "oM", colnames(data_IND1_02))
# varIND1_02 <- gsub("\\+", "oM", varIND1_02)
# ttt <- gsub("\\+", "oM", ttt)
# # dataset imputation 
varsImp <- c("jaar", varIND1_02)
md.pattern(data_IND1_02[, varsImp])
imputed_Data <- mice(data_IND1_02[, varsImp],
                     maxit = 50, m = 1,
                     method = 'pmm', seed = 500)
summary(imputed_Data)
#densityplot(imputed_Data)
# stripplot(imputed_Data, pch = 20, cex = 1.2)

completeData <- complete(imputed_Data,1)
summary(completeData)
summary(data_IND1_02)
data_IND1_02I <- completeData# imputed  data
data_IND1_02I <- cbind(data_IND1_02I, data_IND1_02[, !colnames(data_IND1_02) %in% colnames(data_IND1_02I)])
data_IND1_02I <- data_IND1_02I[, ttt]

# 
# countNA <- list()
# data_INDYEAR <- NULL
# for(ii in 1:length(varIND1_02)){
#   ddd <- data_IND1[!is.na(data_IND1[, varIND1_02[ii]]), ] %>%  select(Buurtcom_1, jaar, varIND1_02[ii])
#   eee <- ddd %>% group_by(Buurtcom_1) %>% summarise(nY = n())
#   fff <- data.frame(Variable = varIND1_02[ii], Year = sort(unique(ddd$jaar)))
#   data_INDYEAR <- rbind(data_INDYEAR, fff)
# }
# datrea <- data_INDYEAR %>% group_by(Variable) %>% summarise(1- n()/length(list_jaar))
# ppp <- data_IND1[is.na(data_IND1$EDU_High), ]
# ppp %>% filter(gebiedcode15 == "E36")
# nrow(ppp)/nrow(data_IND1)
# #IJburg Oost discarded

### aggregated data 
data_IND1_02IA <- data_IND1_02I %>% group_by(Stadsdeel, Buurtcom_1) %>% summarise_at(varIND1_02, mean, na.rm = T)
neigDeleted <- unlist(data_IND1_02IA[is.na(data_IND1_02IA$popKm2), "Buurtcom_1"], use.names = F)
neigDeleted <- as.character(neigDeleted)
cat("Neigh deleted:", neigDeleted, "\n")

# # calculating maps for each set of variables
# # # Age and Racial origin (4 Ages and 3 Racial categories)
data_IND1_02IA$Pop18_22 <- with(data_IND1_02IA, PopWE18_22+PopNWe18_22+PopDut18_22)
data_IND1_02IA$Pop23_39<- with(data_IND1_02IA, PopWE23_39+PopNWe23_39+PopDut23_39)
data_IND1_02IA$Pop40_64 <- with(data_IND1_02IA, PopWE40_64+PopNWe40_64+PopDut40_64)
data_IND1_02IA$Pop65Plus <- with(data_IND1_02IA, PopWE65Plus+PopNWe65Plus+PopDut65Plus)


pop1 <- c("PopWE18_22","PopNWe18_22", "PopDut18_22", "Pop18_22")
pop2 <- c("PopWE23_39","PopNWe23_39","PopDut23_39" , "Pop23_39")
pop3 <- c("PopWE40_64","PopNWe40_64", "PopDut40_64", "Pop40_64")
pop4 <- c("PopWE65Plus","PopNWe65Plus", "PopDut65Plus", "Pop65Plus")

ppp1 <- t(apply(data.frame(data_IND1_02IA[, pop1]), 1, function(x)x[1:3]*1/x[4]) * 100)
ppp1 <- data.frame(ppp1)
colnames(ppp1) <- paste(pop1[1:3], "_P", sep = "")
ppp2 <- t(apply(data.frame(data_IND1_02IA[, pop2]), 1, function(x)x[1:3]*1/x[4]) * 100)
ppp2 <- data.frame(ppp2)
colnames(ppp2) <- paste(pop2[1:3], "_P", sep = "")
ppp3 <- t(apply(data.frame(data_IND1_02IA[, pop3]), 1, function(x)x[1:3]*1/x[4]) * 100)
ppp3 <- data.frame(ppp3)
colnames(ppp3) <- paste(pop3[1:3], "_P", sep = "")

ppp4 <- t(apply(data.frame(data_IND1_02IA[, pop4]), 1, function(x)x[1:3]*1/x[4]) * 100)
ppp4 <- data.frame(ppp4)
colnames(ppp4) <- paste(pop4[1:3], "_P", sep = "")

data_IND1_02IA <- cbind(data.frame(data_IND1_02IA), ppp1, ppp2, ppp3, ppp4)
pop5 <- c(sort(pop1[-4]), sort(pop2[-4]),sort(pop3[-4]),sort(pop4[-4]))
pop5 <- paste(pop5, "_P", sep = "")

# # # for the maps 
map_AMS_wijken_sf <- merge(map_AMS_wijken_sf, data_IND1_02IA, by = "Buurtcom_1", all.x = T)
vect_map_AgeRac <- paste("map", pop5, sep = "_")
for(ii in 1:length(pop5)){
  mmm <- tm_shape(map_AMS_wijken_sf) +
    tm_borders(col = gray(0.6))+
    tm_fill(pop5[ii])  +
    tm_shape(map_AMS_district) +
    tm_borders(alpha = 1, lwd = 2, col = gray(0.6)) + 
    tm_text("Stadsdeel", size = 0.5, shadow=TRUE)   
  assign(x = vect_map_AgeRac[ii], value = mmm)  
  rm(mmm)
}

tpArr_AgeRac <- tmap_arrange(map_PopDut18_22_P, map_PopNWe18_22_P, map_PopWE18_22_P, 
             map_PopDut23_39_P, map_PopNWe23_39_P, map_PopWE23_39_P, 
             map_PopDut40_64_P, map_PopNWe40_64_P, map_PopWE40_64_P, 
             map_PopDut65Plus_P, map_PopNWe65Plus_P, map_PopWE65Plus_P, ncol = 3) 
tmap_save(tpArr_AgeRac, "../Output/Chapter_1/Step4/Map_PerAge_RacialDistr.png", width=5000, height=3000)

# # for the concentration of given Racial per each age group
pop6 <- c(sort(pop1[-4]), sort(pop2[-4]),sort(pop3[-4]),sort(pop4[-4]))
ppp6 <- apply(data_IND1_02IA[, pop6], 2, function(x)x/sum(x) * 100)
ppp6 <- data.frame(ppp6)
pop6_c <-paste(pop6, "_PNe", sep = "")
colnames(ppp6) <- pop6_c  # per neighborhood
data_IND1_02IA <- cbind(data_IND1_02IA, ppp6)
# # # for the maps per neighborhood
map_AMS_wijken_sf <- merge(map_AMS_wijken_sf, data_IND1_02IA[, c("Buurtcom_1", pop6_c)], 
                           by = "Buurtcom_1", all.x = T)
vect_map_AgeRac_PNe <- paste("map", pop6_c, sep = "_")
for(ii in 1:length(pop6_c)){
  mmm <- tm_shape(map_AMS_wijken_sf) +
    tm_borders(col = gray(0.6))+
    tm_fill(pop6_c[ii])  +
    tm_shape(map_AMS_district) +
    tm_borders(alpha = 1, lwd = 2, col = gray(0.6)) + 
    tm_text("Stadsdeel", size = 0.5, shadow=TRUE)   
  assign(x = vect_map_AgeRac_PNe[ii], value = mmm)  
  rm(mmm)
}

tpArr_AgeRac <- tmap_arrange(map_PopDut18_22_PNe, map_PopNWe18_22_PNe, map_PopWE18_22_PNe, 
                             map_PopDut23_39_PNe, map_PopNWe23_39_PNe, map_PopWE23_39_PNe, 
                             map_PopDut40_64_PNe, map_PopNWe40_64_PNe, map_PopWE40_64_PNe, 
                             map_PopDut65Plus_PNe, map_PopNWe65Plus_PNe, map_PopWE65Plus_PNe, ncol = 3) 
tmap_save(tpArr_AgeRac, "../Output/Chapter_1/Step4/Map_PerNeighAge&RacialDistr.png", width=5000, height=3000)

# # variables that measure concentration in neighborhoods 
# # set variables for households 
data_IND1_02IA_Copy <- data_IND1_02IA
varHouseH <- c("HSingle_PaFa", "HSingle_Pe", "HMar_WOCh", "HMar_WCh", 
               "HUnMar_WOCh", "HUnMar_Ch", "HOther")
varPopArr <- c("Pop_Arr", "Pop_Dep", "Pop_Arr_AMS", "Pop_Dep_AMS", 
               "Pop_Mig_Ar", "Dur_Res")
varYear   <- c("Pop18_22", "Pop23_39", "Pop40_64", "Pop65Plus")
varRac <- c("Dutch", "NonWest", "West")

pop1 <- c("PopWE18_22","PopNWe18_22", "PopDut18_22", "Pop18_22")
pop2 <- c("PopWE23_39","PopNWe23_39","PopDut23_39" , "Pop23_39")
pop3 <- c("PopWE40_64","PopNWe40_64", "PopDut40_64", "Pop40_64")
pop4 <- c("PopWE65Plus","PopNWe65Plus", "PopDut65Plus", "Pop65Plus")

data_IND1_02IA$Dutch <- with(data_IND1_02IA, PopDut18_22 + PopDut23_39 + PopDut40_64 + PopDut65Plus)
data_IND1_02IA$NonWest <- with(data_IND1_02IA, PopNWe18_22 + PopNWe23_39 + PopNWe40_64 + PopNWe65Plus)
data_IND1_02IA$West <- with(data_IND1_02IA, PopWE18_22 + PopWE23_39 + PopWE40_64 + PopWE65Plus)

data_IND1_02IA[, varRac] <- t(apply(data_IND1_02IA[, varRac], 1, function(x)x/sum(x) * 100))
data_IND1_02IA[, varYear] <- t(apply(data_IND1_02IA[, varYear], 1, function(x)x/sum(x) * 100))


# data_IND1_02IA[, varHouseH] <- apply(data_IND1_02IA[, varHouseH], 1, function(x)x/sum(x))
data_IND1_02IA[, varHouseH] <- t(apply(data_IND1_02IA[, varHouseH], 1, function(x)x/sum(x)))
varIND1_03 <- c("popKm2", varHouseH, varYear, varRac, varPopArr, "New_Urb", "EDU_Low", "EDU_High")

# Neigh deleted: IJburg Oost

png(filename = "../Output/Chapter_1/Step4/MatrixCor_IND1_TOTALS.png", units="px", width=1300, height=800)
xxx <- cor.plot(data_IND1_02IA[, varIND1_03], numbers=T, upper=F, diag=F,
                main="IND1: Residential Characteristics\ncorrelation", cex=.7, las = 2)
dev.off()

# # correlation among variables 

varsPCA <- c(varIND1_03, "Stadsdeel")
id_qualiSup <- which(varsPCA %in% "Stadsdeel")
pcaIND1 <- PCA(data_IND1_02IA[, varsPCA], quali.sup = id_qualiSup, ncp = 5)
summary(pcaIND1)
dimdesc(pcaIND1)

write.infile(pcaIND1, file = "../Output/Chapter_1/Step4/PCA_IND1.csv")
# # comp 1-2 (60%)
png(filename = "../Output/Chapter_1/Step4/PCA_IND1_Indiv_12.png", units="px", 
    width=1600, height=1200, res=300)
plot(pcaIND1,choix="ind", habillage = id_qualiSup, size = 5,cex= 0.7, 
     label = "quali", title = "IND1: Residential Characteristics", axes = c(1, 2))
dev.off()

png(filename = "../Output/Chapter_1/Step4/PCA_IND1_Var12.png", units="px", 
    width=1600, height=1600, res=300)
plot(pcaIND1,choix="var", 
     size = 5,cex= 0.7, 
     title = "IND1: Residential Characteristics", axes = c(1, 2))
dev.off()

# # Quintiles to classify the index
pcaDataIND1 <- pcaIND1$ind$coord[, 1:2]
IND1_1_Q <- quantile(pcaDataIND1[, 1], probs = c(0.20, 0.40, 0.60, 0.80))
IND1_1_Q <- round(IND1_1_Q, 2)
IND1_2_Q <- quantile(pcaDataIND1[, 2], probs = c(0.20, 0.40, 0.60, 0.80))
IND1_2_Q <- round(IND1_2_Q, 2)
# # how it looks in time 
data_IND1_02I$Pop18_22 <- with(data_IND1_02I, PopWE18_22+PopNWe18_22+PopDut18_22)
data_IND1_02I$Pop23_39<- with(data_IND1_02I, PopWE23_39+PopNWe23_39+PopDut23_39)
data_IND1_02I$Pop40_64 <- with(data_IND1_02I, PopWE40_64+PopNWe40_64+PopDut40_64)
data_IND1_02I$Pop65Plus <- with(data_IND1_02I, PopWE65Plus+PopNWe65Plus+PopDut65Plus)


ppp1 <- t(apply(data.frame(data_IND1_02I[, pop1]), 1, function(x)x[1:3]*1/x[4]) * 100)
ppp1 <- data.frame(ppp1)
colnames(ppp1) <- paste(pop1[1:3], "_P", sep = "")
ppp2 <- t(apply(data.frame(data_IND1_02I[, pop2]), 1, function(x)x[1:3]*1/x[4]) * 100)
ppp2 <- data.frame(ppp2)
colnames(ppp2) <- paste(pop2[1:3], "_P", sep = "")
ppp3 <- t(apply(data.frame(data_IND1_02I[, pop3]), 1, function(x)x[1:3]*1/x[4]) * 100)
ppp3 <- data.frame(ppp3)
colnames(ppp3) <- paste(pop3[1:3], "_P", sep = "")

ppp4 <- t(apply(data.frame(data_IND1_02I[, pop4]), 1, function(x)x[1:3]*1/x[4]) * 100)
ppp4 <- data.frame(ppp4)
colnames(ppp4) <- paste(pop4[1:3], "_P", sep = "")

data_IND1_02I <- cbind(data.frame(data_IND1_02I), ppp1, ppp2, ppp3, ppp4)
ppp6 <- apply(data_IND1_02I[, pop6], 2, function(x)x/sum(x) * 100)
ppp6 <- data.frame(ppp6)
pop6_c <-paste(pop6, "_PNe", sep = "")
colnames(ppp6) <- pop6_c  # per neighborhood
data_IND1_02I <- cbind(data_IND1_02I, ppp6)

data_IND1_02I_Copy <- data_IND1_02I

data_IND1_02I$Dutch <- with(data_IND1_02I, PopDut18_22 + PopDut23_39 + PopDut40_64 + PopDut65Plus)
data_IND1_02I$NonWest <- with(data_IND1_02I, PopNWe18_22 + PopNWe23_39 + PopNWe40_64 + PopNWe65Plus)
data_IND1_02I$West <- with(data_IND1_02I, PopWE18_22 + PopWE23_39 + PopWE40_64 + PopWE65Plus)

data_IND1_02I[, varRac] <- t(apply(data_IND1_02I[, varRac], 1, function(x)x/sum(x) * 100))
data_IND1_02I[, varYear] <- t(apply(data_IND1_02I[, varYear], 1, function(x)x/sum(x) * 100))
data_IND1_02I[, varHouseH] <- t(apply(data_IND1_02I[, varHouseH], 1, function(x)x/sum(x)))
varIND1_03 <- c("popKm2", varHouseH, varYear, varRac, varPopArr, "New_Urb", "EDU_Low", "EDU_High")
predIND1 <- predict(pcaIND1, data_IND1_02I)$coord
predIND1 <- predIND1[, 1:2]
pcaVarAll <- pcaIND1$var$coord
pcaVarAll <- pcaVarAll[, 1:2]
colnames(predIND1) <- c("IND1_1", "IND1_2")
data_IND1_02I <- cbind(data_IND1_02I, predIND1)
data_IND1_02I$IND1_1_Q <- cut(data_IND1_02I$IND1_1, breaks = c(-Inf,IND1_1_Q, Inf))
data_IND1_02I$IND1_2_Q <- cut(data_IND1_02I$IND1_2, breaks = c(-Inf,IND1_2_Q, Inf))
pcaStatsY <- data_IND1_02I %>% group_by(jaar) %>% summarise(pcaMean1 = mean(IND1_1),
                                                            pcaMean2= mean(IND1_2),
                                                            pcaSd1 = sd(IND1_1),
                                                            pcaSd2 = sd(IND1_2)) 
pcaStatsY_L <- melt(pcaStatsY[, 1:3], id.vars = "jaar")
plot1 <-  ggplot(pcaStatsY_L, aes(x = jaar, y = value, color = variable)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(aes(x = jaar, y = value), method = "lm",
              formula = y ~ poly(x, 5), se = FALSE) +
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10))+ 
  guides(color=guide_legend(title="Indicator")) + 
  scale_x_continuous(name = "Time", breaks = list_jaar) + 
  scale_y_continuous(name = "Average Indicators") +
  geom_hline(yintercept = 0, linetype="dotted") +
  ggtitle("Residential Characteristics in time")
ggsave("../Output/Chapter_1/Step4/ResidCharac_TimeAver.png", plot = plot1, width = 15)

vect_map_IND1 <- paste("map", list_jaar, "IND1_1",sep = "_")
for(ii in 1:length(list_jaar)){
  map_AMS_wijken_sf_Jaar <- map_AMS_wijken_sf
  data_IND1_02I_jaar <- data_IND1_02I %>% filter(jaar == list_jaar[ii]) %>% select("Buurtcom_1", "IND1_1_Q")
  map_AMS_wijken_sf_Jaar <- merge(map_AMS_wijken_sf_Jaar, data_IND1_02I_jaar, by = "Buurtcom_1", all.x = T)
  
  mmm <- tm_shape(map_AMS_wijken_sf_Jaar) +
    tm_borders(col = gray(0.5), alpha = 0.6)+
    tm_fill(col = "IND1_1_Q",  palette = "seq", textNA = "Other Neighborhoods", colorNA = "white")  +
    tm_shape(map_AMS_district) +
    tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
    tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
    tm_layout( title = list_jaar[ii], frame = FALSE, 
               inner.margins=c(0,0,.1,0), title.size=.8)
  assign(x = vect_map_IND1[ii], value = mmm)  
  rm(mmm)
}
tpArr_YearIND1 <- tmap_arrange(map_2007_IND1_1, map_2008_IND1_1, map_2009_IND1_1, map_2010_IND1_1, 
                             map_2011_IND1_1, map_2012_IND1_1, map_2013_IND1_1, map_2014_IND1_1, 
                             map_2015_IND1_1, map_2016_IND1_1, map_2017_IND1_1, map_2018_IND1_1, ncol = 3) 
tmap_save(tpArr_YearIND1, "../Output/Chapter_1/Step4/Map_PerNeighYearIND1.png", width=5000, height=3000)

vect_map_IND2 <- paste("map", list_jaar, "IND1_2",sep = "_")
for(ii in 1:length(list_jaar)){
  map_AMS_wijken_sf_Jaar <- map_AMS_wijken_sf
  data_IND1_02I_jaar <- data_IND1_02I %>% filter(jaar == list_jaar[ii]) %>% select("Buurtcom_1", "IND1_2_Q")
  map_AMS_wijken_sf_Jaar <- merge(map_AMS_wijken_sf_Jaar, data_IND1_02I_jaar, by = "Buurtcom_1", all.x = T)
  
  mmm <- tm_shape(map_AMS_wijken_sf_Jaar) +
    tm_borders(col = gray(0.5), alpha = 0.6)+
    tm_fill(col = "IND1_2_Q",  palette = "seq", textNA = "Other Neighborhoods", colorNA = "white")  +
    tm_shape(map_AMS_district) +
    tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
    tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
    tm_layout( title = list_jaar[ii], frame = FALSE, 
               inner.margins=c(0,0,.1,0), title.size=.8)
  assign(x = vect_map_IND2[ii], value = mmm)  
  rm(mmm)
}
tpArr_YearIND2 <- tmap_arrange(map_2007_IND1_2, map_2008_IND1_2, map_2009_IND1_2, map_2010_IND1_2, 
                               map_2011_IND1_2, map_2012_IND1_2, map_2013_IND1_2, map_2014_IND1_2, 
                               map_2015_IND1_2, map_2016_IND1_2, map_2017_IND1_2, map_2018_IND1_2, ncol = 3) 
tmap_save(tpArr_YearIND2, "../Output/Chapter_1/Step4/Map_PerNeighYearIND2.png", width=5000, height=3000)


# # # comp 1-3
# png(filename = "../Output/Chapter_1/Step4/PCA_IND1_Indiv13.png", units="px", 
#     width=1600, height=1200, res=300)
# plot(pcaIND1,choix="ind", habillage = id_qualiSup, size = 5,cex= 0.7, 
#      label = "quali", title = "IND1: Residential Characteristics", axes = c(1, 3))
# dev.off()
# 
# png(filename = "../Output/Chapter_1/Step4/PCA_IND1_Var13.png", units="px", 
#     width=1600, height=1600, res=300)
# plot(pcaIND1,choix="var", 
#      size = 5,cex= 0.7, 
#      title = "IND1: Residential Characteristics", axes = c(1, 3))
# dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # housing PCA 

