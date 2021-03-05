##########################################################################
# # R version:
# # File Name: 
# # Author:
# # Process:
# # Inputs:
# # Outputs:
# # File history:
##########################################################################

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

fun_growth <-  function(x){
  lll <- x
  vvv <- NULL
  for(jj in 1:(length(x)-1)){
    # # growth year per year
    vvv[jj] <- (lll[jj+1] -lll[jj] )/lll[jj]
  }
  return(mean(vvv)*100)
}
# # # Loading GIS information
load("../Output/Chapter_1/Step3/dataList_airbnb.Rdata")
load("../Output/Chapter_1/Step3/dataList_airbnb_sf.Rdata")
load("../Output/Chapter_1/Step3/map_AMS_wijken_sf.Rdata")
load("../Output/Chapter_1/Step3/data_airbnb.Rdata")
map_AMS_district <- readOGR(dsn = "./GIS/geojson_district", layer = "geojson_districts-polygon")
plot(map_AMS_district)

colType <- read_excel("./OIS/AMS_DATA_NEIGH_0720.xlsx", sheet = "coltypes")
data_AMS_BBGA <- read_excel("./OIS/AMS_DATA_NEIGH_0720.xlsx", sheet = "bbga_excel_2020-07-09", col_types = colType$Type)
map_AMS_wijken <- readOGR(dsn = "./GIS/geojson_wijken", layer = "geojson_wijken")
data_IND <- read_excel("./OIS/VARIABLE_INDICATORS.xlsx", sheet = "Indicators Total V03")
vars_fix <- c("niveau", "niveaunaam", "SD", "sdnaam",	"gebiedcode15",
              "gebiedcodenaam", "gebiednaam","jaar")


list_jaar = 2011:2018#seq(2007, 2019, by = 2)
level <- c("Wijken")
level_region <- "Buurtcombi"
level_unknow <- "Z onbekend"
codeNeiG <- "gebiedcode15"
nameNeiG <- "gebiednaam"
colIntOrRd <- c('#fef0d9','#fdcc8a','#fc8d59','#e34a33','#b30000') #  quintiles

# Indicator 2: Housing Characteristics

list_IND2 <- subset(data_IND, Type == "Housing" & Selected == "Included")
list_IND2_1 <- list_IND2 %>% filter(Source == "OIS")
list_IND2_1$Variable <- toupper(list_IND2_1$Variable)

# # Housing characteristics (OIS raw data)

cols_IND2_1 <- colType %>% filter(Variable %in% list_IND2_1$Variable) %>% dplyr:::select(Col, Variable)
dim(cols_IND2_1)
cols_IND2_1 <- merge(cols_IND2_1, list_IND2_1, all.x = T)
dim(cols_IND2_1)
head(cols_IND2_1)
cols_IND2_1 <- cols_IND2_1 %>% select(-c(Definition, Definitie))
varsIND2_1 <- unlist(cols_IND2_1$Col, use.names = FALSE)
vvv1_1 <- c(vars_fix, varsIND2_1)

data_IND2_1 <- data_AMS_BBGA %>% 
  filter(niveaunaam %in% level & jaar %in% list_jaar & !sdnaam %in% level_unknow) %>% 
  dplyr:::select(codeNeiG, nameNeiG, jaar, vvv1_1)
summary(data_IND2_1)
dim(data_IND2_1)


dataNA <- data.frame(gebiedcode15 = unique(data_IND2_1$gebiedcode15))
TTT <- NULL
for(ii in 1:nrow(dataNA)){
  YYY <- NULL
  for(jj in 1:length(list_jaar) ){
    vvv <- data_IND2_1 %>% filter(gebiedcode15 %in% dataNA$gebiedcode15[ii] & jaar == list_jaar[jj])
    vvv <- data.frame(vvv)[, varsIND2_1[-1]]
    www <- sum(is.na(vvv))/length(vvv) * 100
    www <- round(www, 2)
    YYY[jj] <- www
  }
  TTT <- rbind(TTT, YYY)
}

colnames(TTT) <- list_jaar
cbind(TTT, dataNA)
# 
# # neigh que no tienen medicion de renta para descartarlos ()
# TTT <- NULL
# for(ii in 1:nrow(dataNA)){
#   YYY <- NULL
#   for(jj in 1:length(list_jaar) ){
#     vvv <- data_IND2_1 %>% filter(gebiedcode15 %in% dataNA$gebiedcode15[ii] & jaar == list_jaar[jj])
#     vvv <- data.frame(vvv)[, "WHUUR_GEM"]
#     www <- as.numeric(is.na(vvv))
#     YYY[jj] <- www
#   }
#   TTT <- rbind(TTT, YYY)
# }
# 
# colnames(TTT) <- list_jaar
# TTT <- cbind(TTT, dataNA)
# TTT$Tot <- apply(TTT[, as.character(list_jaar)], 1, sum)
# TTT$Tot <- 9 - TTT$Tot
# TTT <- TTT[order(TTT$Tot, decreasing = T), ]
# map_AMS_wijken_sf2 <- merge(map_AMS_wijken_sf, TTT[, c("gebiedcode15", "Tot")], by.y = "gebiedcode15", all.x = T, by.x = "Buurtcombi")
# map_AMS_wijken_sf2$Tot <- as.factor(map_AMS_wijken_sf2$Tot)
# map_AMS_wijken_sf2$Tot2 <- ifelse(map_AMS_wijken_sf2$gentry_opDef == "Gentrified", map_AMS_wijken_sf2$Tot, NA) 
# tm_shape(map_AMS_wijken_sf2) +
#   tm_borders(col = gray(0.6))+
#   tm_fill("Tot")  +
#   tm_shape(map_AMS_district) +
#   tm_borders(alpha = 1, lwd = 2, col = gray(0.6)) + 
#   tm_text("Stadsdeel", size = 0.5, shadow=TRUE)
# 
# vvv <- data_IND2_1 %>% filter(jaar == list_jaar[1])
# vvv <- data.frame(vvv)[, c("gebiedcode15", "WHUUR_GEM")]
# map_AMS_wijken_sf2 <- merge(map_AMS_wijken_sf, 
#                             vvv, 
#                             by.y = "gebiedcode15", all.x = T, by.x = "Buurtcombi")
# tm_shape(map_AMS_wijken_sf2) +
#   tm_borders(col = gray(0.6))+
#   tm_fill("WHUUR_GEM")  +
#   tm_shape(map_AMS_district) +
#   tm_borders(alpha = 1, lwd = 2, col = gray(0.6)) + 
#   tm_text("Stadsdeel", size = 0.5, shadow=TRUE)
# map_AMS_wijken_sf2$WHUUR_GEM <- NULL
# # # List of neighborhoods gentrified 
# gentryNeigh <- subset(map_AMS_wijken_sf, gentry_opDef== "Gentrified")
# gentryNeigh <- gentryNeigh$Buurtcom_1
# 
# # # list of neighborhoods with listings 
# calculate in the Step 3 and, call here. 
# 
# data_airbnb$ID_idY <- with(data_airbnb, paste(id, year, sep = "_"))
# data_TLiNegh <- data_airbnb %>% filter(!duplicated(ID_idY))## Total listings per neigh across all the periods ()
# data_TLiNeghAG <- data_TLiNegh %>% group_by(Buurtcom_1, year) %>% summarise(n = n()) 
# data_TLiNeghAG <- na.omit(data_TLiNeghAG)
# data_TLiNeghAG <-  data_TLiNeghAG %>% filter(n > 50)
# 
# map_AMS_wijken_sf2
# tm_shape(map_AMS_wijken_sf2) +
#   tm_borders(col = gray(0.6))+
#   tm_fill("Tot")  +
#   tm_shape(map_AMS_district) +
#   tm_borders(alpha = 1, lwd = 2, col = gray(0.6)) + 
#   tm_text("Stadsdeel", size = 0.5, shadow=TRUE)

# # reading data from he CBS 
listCBS <- dir("CBS/Key figures districts and neighborhoods 2004-2019/")
listCBS <- listCBS[listCBS %in% list_jaar]
listCBS <- sort(listCBS)
data_CBS_1 <- NULL 
for(ii in 1:2){
  ppp <- file.path("CBS/Key figures districts and neighborhoods 2004-2019", listCBS[ii])
  qqq <- dir(ppp)
  rrr <- qqq[grep("xls", qqq)]
  list_IND2_2 <- list_IND2 %>% filter(Source == "CBS")
  dat_CBSY <-  read_excel(file.path(ppp, rrr))
  dat_CBSY <- dat_CBSY %>% 
    filter(GM_NAAM == "Amsterdam" & RECS == "Buurt") %>%
    dplyr:::select(BU_CODE, GWB_NAAM11_60pos, RECS, STED, list_IND2_2$Variable)
  dat_CBSY  <- data.frame(dat_CBSY)
  dat_CBSY$Year <- listCBS[ii]
  data_CBS_1 <- rbind(data_CBS_1, dat_CBSY)
}

data_CBS_2 <- NULL
listVar13_19 <- c("a_woning", "g_woz", "p_koopw", "p_bjj2k", "p_bjo2k")
for(ii in 3:length(listCBS)){
  ppp <- file.path("CBS/Key figures districts and neighborhoods 2004-2019", listCBS[ii])
  qqq <- dir(ppp)
  rrr <- qqq[grep("xls", qqq)]
  list_IND2_2 <- list_IND2 %>% filter(Source == "CBS")
  dat_CBSY <-  read_excel(file.path(ppp, rrr))
  if(as.numeric(listCBS[ii]) < 2016){
    dat_CBSY <- dat_CBSY %>% 
      filter(gm_naam == "Amsterdam" & recs == "Buurt") %>%
      dplyr:::select(gwb_code, regio, recs, listVar13_19)
  }else{
    dat_CBSY <- dat_CBSY %>% 
      filter(gm_naam == "Amsterdam" & recs == "Wijk") %>%
      dplyr:::select(gwb_code, regio, recs, listVar13_19)
  }
  
  dat_CBSY  <- data.frame(dat_CBSY)
  dat_CBSY$Year <- listCBS[ii]
  data_CBS_2 <- rbind(data_CBS_2, dat_CBSY)
}
# colnames(data_CBS_2)[c(2)] <- c("Buurtcom_1")

# # fixing the two table to make them compatible 

table(unique(data_CBS_2$regio) %in% unique(data_CBS_1$GWB_NAAM11_60pos))

data_CBS_1$WijkCode <- data_CBS_1$BU_CODE
data_CBS_2$WijkCode <- str_sub(data_CBS_2$gwb_code, -2, -1)

data_CBS_1 <- data_CBS_1 %>% filter(GWB_NAAM11_60pos != "Duivelseiland" & WijkCode != "50") 
data_CBS_2 <- data_CBS_2 %>% filter(regio != "Duivelseiland" & WijkCode != "50") 

data_CBS_1 <- data_CBS_1 %>% filter(GWB_NAAM11_60pos != "IJburg Oost" & WijkCode != "74") 
data_CBS_2 <- data_CBS_2 %>% filter(regio != "IJburg Oost" & WijkCode != "74") 

data_CBS_1 <- data_CBS_1 %>% filter(GWB_NAAM11_60pos != "Spieringhorn" & WijkCode != "75") 
data_CBS_2 <- data_CBS_2 %>% filter(regio != "Spieringhorn" & WijkCode != "75") 
data_CBS_1$STED <- NULL
data_CBS_1$RECS <- NULL
data_CBS_1$BU_CODE <-  NULL
data_CBS_1$GWB_NAAM11_60pos <- NULL

data_CBS_2$gwb_code <- NULL
data_CBS_2$regio <- NULL
data_CBS_2$recs <-  NULL

colnames(data_CBS_2) <- c("WONINGEN", "WOZ", "P_KOOPWON", 
                          "P_WONT2000", "P_WONV2000", "Year", "WijkCode")
data_CBS <- rbind(data_CBS_1, data_CBS_2)
data_IND2_1$WijkCode <- substr(data_IND2_1$gebiedcode15, 2, 3)

dim(data_IND2_1)
data_IND2 <- merge(data_IND2_1, data_CBS, by.x = c("WijkCode", "jaar"), by.y = c("WijkCode", "Year"),  all.x = T)
dim(data_IND2)
# datCode <- data.frame(code = unique(data_CBS_2$WijkCode))
# data_IND2_1$code <- substr(data_IND2_1$gebiedcode15, 2, 3)
# datCode <- merge(datCode, unique(data_IND2_1[, c("code", "Buurtcom_1")]), all.x = T)
# datCode <- merge(datCode, unique(data_CBS_1[, c("WijkCode", "GWB_NAAM11_60pos")]), by.x = "code", by.y="WijkCode", all.x = T)
# datCode <- merge(datCode, unique(data_CBS_2[, c("WijkCode", "regio")]), by.x = "code", by.y="WijkCode", all.x = T)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
map_info <- map_AMS_wijken_sf[, c("Stadsdeel", "Buurtcombi", "Buurtcom_1", "Stadsdeel_", "Opp_m2")]
map_info <- data.frame(map_info)[, -6] ## eliinate geometry 
data_IND2 <- merge(data_IND2, map_info, by.x = codeNeiG, by.y = "Buurtcombi", all.x= T)
list_neigh <- unique(data_IND2$Buurtcom_1)

data_IND2[, list_IND2$Variable] <- apply(data_IND2[, list_IND2$Variable], 2, as.numeric) 
data_IND2_Stad <- data_IND2 %>% group_by(SD, jaar) %>% summarise_at(list_IND2$Variable, mean, na.rm = TRUE)

# # neighborhoods without data 
# data_IND2 <- data_IND2 %>% filter(!gebiednaam %in% "IJburg Oost")
data_IND2$ID <- 1:nrow(data_IND2)
vvv1 <- list_IND2$Variable
# # Variable imputation using stadsdelen 
for(ii in 9:length(vvv1)){
  ddd1 <- is.na(data_IND2[, vvv1[ii]])
  ddd <- sum(ddd1)/length(data_IND2[, vvv1[ii]])
  if(ddd>0){
    nnn <- data_IND2_Stad[, c("SD", "jaar", vvv1[ii])]
    colnames(nnn) <- c("SD","jaar", "varImp")
    data_IND2 <- merge(data_IND2, nnn, by = c("SD", "jaar"), all.x = T)
    qqq <- sum(is.na(data_IND2[, vvv1[ii]]) & !is.na(data_IND2$varImp))
    cat(vvv1[ii], " - ", sum(ddd1), ".Replaced: ",qqq ,"\n")
    data_IND2[, vvv1[ii]] <- ifelse(is.na(data_IND2[, vvv1[ii]]), data_IND2$varImp, data_IND2[, vvv1[ii]])
    data_IND2$varImp <- NULL
   }
}

outAn <- NULL
kk <- 1
for(ii in 9:length(vvv1)){
  www <- prop.table(table(is.na(data_IND2[, vvv1[ii]])))
  if(length(www) == 1) next
  if(www[2] > 0.3){
    outAn[kk] <-  vvv1[ii]
    kk = kk + 1
  }
}

data_IND2 <- data_IND2[, !colnames(data_IND2) %in% outAn]
summary(data_IND2)

varsIND2 <- colnames(data_IND2)
cols_IND2 <- list_IND2
renameCol <- cols_IND2$Relabel[match(varsIND2, cols_IND2$Variable)]
renameCol[is.na(renameCol)] <- colnames(data_IND2)[is.na(renameCol)]
colnames(data_IND2) <- renameCol

varIND2_02 <- list_IND2[list_IND2$Relabel %in%colnames(data_IND2) ,]
varIND2_02 <- varIND2_02[order(varIND2_02$ID), ]
varIND2_02 <- varIND2_02$Relabel
ttt <- colnames(data_IND2)[!colnames(data_IND2) %in% varIND2_02]
ttt <- c(ttt, varIND2_02)
data_IND2_02 <- data_IND2[, ttt]
data_IND2_02 <- data_IND2_02[order(data_IND2_02$gebiedcode15, data_IND2_02$jaar), ]

# colnames(data_IND1_02) <- gsub("\\+", "oM", colnames(data_IND1_02))
# varIND1_02 <- gsub("\\+", "oM", varIND1_02)
# ttt <- gsub("\\+", "oM", ttt)
# # dataset imputation 
varsImp <- c("jaar", varIND2_02)
md.pattern(data_IND2_02[, varsImp])
imputed_Data <- mice(data_IND2_02[, varsImp],
                     maxit = 50, m = 1,
                     method = 'pmm', seed = 500)
summary(imputed_Data)
png(filename = "../Output/Chapter_1/Step4/ImputedData_HouseDynamics.png", units="px", width=1300, height=800)
densityplot(imputed_Data, bw = 30)
dev.off()
# stripplot(imputed_Data, pch = 20, cex = 1.2)

completeData <- complete(imputed_Data,1)
summary(completeData)
summary(data_IND2_02)
data_IND2_02I <- completeData# imputed  data
data_IND2_02I <- cbind(data_IND2_02I, data_IND2_02[, !colnames(data_IND2_02) %in% colnames(data_IND2_02I)])
data_IND2_02I <- data_IND2_02I[, ttt]
data_IND2_02I <- subset(data_IND2_02I, gebiedcode15 != "M50")
### aggregated data 
data_IND2_02IA <- data_IND2_02I %>% group_by(Stadsdeel, Buurtcom_1) %>% summarise_at(varIND2_02, mean, na.rm = T)
# Neigh deleted: IJburg Oost
library(corrplot)
library(xtable)
M <- cor(data_IND2_02IA[, varIND2_02])


png(filename = "../Output/Chapter_1/Step4//MatrixCor_IND2_TOTALS.png", units="px", width=1300, height=800)
# xxx <- cor.plot(data_IND2_02IA[, varIND2_02], numbers=T, upper=F, diag=F,
                # main="IND2: Housing Characteristics\ncorrelation", cex=.7, las = 2)
corrplot(M, method = "circle", type = "upper", tl.col = "black", diag = F)
dev.off()

xtable(M, caption = "Correlation matrix Housing Dynamics")


# # correlation among variables 
# # summary main variables 
forSumm <- merge(data_IND2_02I, map_AMS_wijken_sf[, c("Buurtcombi", "gentry_opDef", "gentryABnB_opDef")],
                 by.x = "gebiedcode15", by.y = "Buurtcombi", all.x = T)
mmm <- forSumm[, c(varIND2_02, "jaar", "gentry_opDef")] %>% 
  group_by(jaar, gentry_opDef) %>% 
  summarise_at(varIND2_02, mean)

mmm2 <- forSumm[, c(varIND2_02, "jaar", "gentry_opDef")] %>% 
  group_by(jaar) %>% 
  summarise_at(varIND2_02, mean)
mmm3 <- cbind(
  t(mmm2 %>% summarise_at(varIND2_02, mean)),
  t(mmm2 %>% summarise_at(varIND2_02, sd)),
  t((mmm %>% group_by(gentry_opDef) %>% summarise_at(varIND2_02, mean))[, -1]),
  t((mmm %>% group_by(gentry_opDef) %>% summarise_at(varIND2_02, fun_growth))[, -1])
)
colnames(mmm3) <- c("mean", "sd", "mean_g", "mean_ng", "growth_g", "growth_ng")
mmm3 <- data.frame(mmm3)
mmm3$mean_g <- mmm3$mean_g - mmm3$mean  
mmm3$mean_ng <- mmm3$mean_ng -mmm3$mean 

# # correlation among variables 

varsPCA <- c(varIND2_02, "Stadsdeel")
id_qualiSup <- which(varsPCA %in% "Stadsdeel")
pcaIND2 <- PCA(data_IND2_02IA[, varsPCA], quali.sup = id_qualiSup)
summary(pcaIND2)
dimdesc(pcaIND2)

pca_Weigh <- data.frame(pcaIND2$svd$V[, 1:2])
rownames(pca_Weigh) <- varIND2_02
colnames(pca_Weigh) <- c("Dim1", "Dim2")
xtable(pca_Weigh, caption = "Weights PCA - Housing Dynamics", label = "tab:CH06:Weights2_ACP")
summary(pcaIND2)
anly_PCAIND2 <- dimdesc(pcaIND2, proba = 0.1) # analysis PCA
xtable(anly_PCAIND2$Dim.1$quanti, caption = "Correlation test between First dimension and variables - Household Dynamics", label = "tab:CH06:CorrFactor2_1ACP")
xtable(anly_PCAIND2$Dim.2$quanti, caption = "Correlation test between Second dimension and variables - Household Dynamics", label = "tab:CH06:CorrFactor2_2ACP")
xtable(pcaIND2$var$cos2[, 1:2], caption = "Cosine square for columns representation - Household Dynamics", label = "tab:CH06:Cosine_2ACP")
write.infile(pcaIND2, file = "../Output/Chapter_1/Step4/PCA_IND2.csv")


# # comp 1-2 (60%)
png(filename = "../Output/Chapter_1/Step4/PCA_IND2_Indiv_12.png", units="px", 
    width=1600, height=1200, res=300)
plot(pcaIND2,choix="ind", habillage = id_qualiSup, size = 5,cex= 0.7, 
     label = "quali", title = "IND2: Housing Dynamics", axes = c(1, 2))
dev.off()

png(filename = "../Output/Chapter_1/Step4/PCA_IND2_Var12.png", units="px", 
    width=1600, height=1600, res=300)
plot(pcaIND2,choix="var", 
     size = 5,cex= 0.7, 
     title = "IND2: Housing Dynamics", axes = c(1, 2))
dev.off()

# # comp 1-3 (60%)
png(filename = "../Output/Chapter_1/Step4/PCA_IND2_Indiv_13.png", units="px", 
    width=1600, height=1200, res=300)
plot(pcaIND2,choix="ind", habillage = id_qualiSup, size = 5,cex= 0.7, 
     label = "quali", title = "IND2: Housing Dynamics", axes = c(1, 3))
dev.off()

png(filename = "../Output/Chapter_1/Step4/PCA_IND2_Var13.png", units="px", 
    width=1600, height=1600, res=300)
plot(pcaIND2,choix="var", 
     size = 5,cex= 0.7, 
     title = "IND2: Housing Dynamics", axes = c(1, 3))
dev.off()

# # Quintiles to classify the index
pcaDataIND2 <- pcaIND2$ind$coord[, 1:2]

varEig <- pcaIND2$eig[1:2, 1]
pcaDataIND2[, 1] <-  (pcaDataIND2[, 1] / sqrt(varEig[1]))* 15 + 50
pcaDataIND2[, 2] <-  (pcaDataIND2[, 2] / sqrt(varEig[2]))* 15 + 50

IND2_1_Q <- quantile(pcaDataIND2[, 1], probs = c(0.20, 0.40, 0.60, 0.80))
IND2_1_Q <- round(IND2_1_Q, 2)
IND2_2_Q <- quantile(pcaDataIND2[, 2], probs = c(0.20, 0.40, 0.60, 0.80))
IND2_2_Q <- round(IND2_2_Q, 2)

colnames(pcaDataIND2) <- c("IND2_1", "IND2_2")
data_IND2_02IA <- cbind(data.frame(data_IND2_02IA), pcaDataIND2)

data_IND2_02IA$IND2_1_Q <- cut(data_IND2_02IA$IND2_1, breaks = c(-Inf,IND2_1_Q, Inf), labels = c("Low", "Medium-Low", "Medium", "Medium-High", "High"))
data_IND2_02IA$IND2_2_Q <- cut(data_IND2_02IA$IND2_2, breaks = c(-Inf,IND2_2_Q, Inf), labels = c("Low", "Medium-Low", "Medium", "Medium-High", "High"))

# # Characterization of each quintiles 
ttIND1 <- data_IND2_02IA[, c(varIND2_02, "IND2_1", "IND2_2", "IND2_1_Q", "IND2_2_Q")] %>% 
  group_by(IND2_1_Q) %>%
  summarise_at(varIND2_02, mean)
ttIND1 <- data.frame(ttIND1)
t(ttIND1)

ttIND2 <- data_IND2_02IA[, c(varIND2_02, "IND2_1", "IND2_2", "IND2_1_Q", "IND2_2_Q")] %>% 
  group_by(IND2_2_Q) %>%
  summarise_at(varIND2_02, mean)
t(ttIND2)


# # Characterization of each quintiles 
# # comparison with income 

data_AMS_INCOME <- data_AMS_BBGA %>% 
  filter(niveaunaam %in% level & jaar %in% list_jaar & !sdnaam %in% level_unknow) %>% 
  select(codeNeiG, nameNeiG, jaar, "IHHINK_GEM")

data_AMS_INCOME <- data_AMS_INCOME %>% filter(gebiedcode15 %in% data_IND2_02I$gebiedcode15)
data_AMS_INCOMEA <- data_AMS_INCOME %>% group_by(gebiedcode15) %>% summarise(avIncome= median(IHHINK_GEM, na.rm = T))
data_IND2_02I <- merge(data_IND2_02I, data_AMS_INCOMEA, all.x = T)


ttIND1 <- data_IND2_02I[, c(varIND2_02, "avIncome", "IND2_1", "IND2_2", "IND2_1_Q", "IND2_2_Q")] %>% 
  group_by(IND2_1_Q) %>%
  summarise_at(c(varIND2_02, "avIncome"), mean, na.rm = T)
ttIND1 <- data.frame(ttIND1)
t(ttIND1)

ttIND2 <- data_IND2_02I[, c(varIND2_02, "avIncome", "IND2_1", "IND2_2", "IND2_1_Q", "IND2_2_Q")] %>% 
  group_by(IND2_2_Q) %>%
  summarise_at(c(varIND2_02, "avIncome"), mean, na.rm = T)
t(ttIND2)

data_IND2_02IA %>% group_by(Stadsdeel) %>% summarise(mean(IND2_1), mean(IND2_2))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # Projection on time
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
predIND2 <- predict(pcaIND2, data_IND2_02I)$coord
predIND2 <- predIND2[, 1:2]
# predIND2[, 2] <- -1 * predIND2[, 2]
predIND2[, 1] <-  (predIND2[, 1] / sqrt(varEig[1]))* 15 + 50
predIND2[, 2] <-  (predIND2[, 2] / sqrt(varEig[2]))* 15 + 50

pcaVarAll <- pcaIND2$var$coord
pcaVarAll <- pcaVarAll[, 1:2]
colnames(predIND2) <- c("IND2_1", "IND2_2")
data_IND2_02I <- cbind(data_IND2_02I, predIND2)
data_IND2_02I$IND2_1_Q <- cut(data_IND2_02I$IND2_1, breaks = c(-Inf,IND2_1_Q, Inf), labels = c("Low", "Medium-Low", "Medium", "Medium-High", "High"))
data_IND2_02I$IND2_2_Q <- cut(data_IND2_02I$IND2_2, breaks = c(-Inf,IND2_2_Q, Inf), labels = c("Low", "Medium-Low", "Medium", "Medium-High", "High"))
pcaStatsY <- data_IND2_02I %>% group_by(jaar) %>% summarise(pcaMean1 = mean(IND2_1),
                                                            pcaMean2= mean(IND2_2),
                                                            pcaMean1M = median(IND2_1),
                                                            pcaMean2M= median(IND2_2),
                                                            pcaSd1 = sd(IND2_1),
                                                            pcaSd2 = sd(IND2_2)) 

library(reshape2)
pcaStatsY_L <- melt(pcaStatsY[, 1:3], id.vars = "jaar")
pcaStatsY_L$jaar <- as.numeric(pcaStatsY_L$jaar)
plot1 <-  ggplot(subset(pcaStatsY_L, jaar != 2019), aes(x = jaar, y = value, color = variable)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(aes(x = jaar, y = value), method = "lm",
              formula = y ~ poly(x, 4), se = FALSE) +
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10))+ 
  guides(color=guide_legend(title="Indicator")) + 
  scale_x_continuous(name = "Time", breaks = list_jaar) + 
  scale_y_continuous(name = "Average Indicators") +
  geom_hline(yintercept = 50, linetype="dotted") +
  ggtitle("Housing Dynamics in time")
ggsave("../Output/Chapter_1/Step4/Housing_TimeAver.png", plot = plot1, width = 15)

############################################################
### INDICATOR 1 
############################################################
# # boxplot 
data_IND2_02I$jaar <- as.factor(data_IND2_02I$jaar)
plot1 <-  ggplot(data_IND2_02I, aes(x = jaar, y =  IND2_1)) + 
  geom_boxplot() + theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + scale_x_discrete(name = "Time", breaks = list_jaar) + 
  scale_y_continuous(name = "Indicator-Housing Dynamics 1", 
                     breaks = seq(0, 100, by =10), limits = c(10, 100)) +
  geom_hline(yintercept = 50, linetype="dotted") +
  ggtitle("Housing Dynamics (Indicator 1)")

ggsave("../Output/Chapter_1/Step4/HousingDynamics_IND1_TimeAver_Boxplot.png", plot = plot1, width = 15)


vect_map_IND2 <- paste("map", list_jaar, "IND2_1",sep = "_")
for(ii in 1:length(list_jaar)){
  map_AMS_wijken_sf_Jaar <- map_AMS_wijken_sf
  data_IND2_02I_jaar <- data_IND2_02I %>% filter(jaar == list_jaar[ii]) %>% select("Buurtcom_1", "IND2_1_Q")
  map_AMS_wijken_sf_Jaar <- merge(map_AMS_wijken_sf_Jaar, data_IND2_02I_jaar, by = "Buurtcom_1", all.x = T)
  
  mmm <- tm_shape(map_AMS_wijken_sf_Jaar) +
    tm_borders(col = gray(0.5), alpha = 0.6)+
    tm_fill(col = "IND2_1_Q",  palette = "seq", textNA = "Other Neighborhoods", colorNA = "white")  +
    tm_shape(map_AMS_district) +
    tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
    tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
    tm_layout( title = list_jaar[ii], frame = FALSE, 
               inner.margins=c(0,0,.1,0), title.size=.8)
  assign(x = vect_map_IND2[ii], value = mmm)  
  rm(mmm)
}
tpArr_YearIND2 <- tmap_arrange(map_2011_IND2_1, map_2012_IND2_1, map_2013_IND2_1, map_2014_IND2_1, 
                               map_2015_IND2_1, map_2016_IND2_1, map_2017_IND2_1, map_2018_IND2_1,ncol = 3) 
tmap_save(tpArr_YearIND2, "../Output/Chapter_1/Step4/Map_PerNeighYear_Housing_IND1.png", width=5000, height=3000)

# # Tile plot
# # resume characteristics neighborhoods and Quintiles 
map_AMS_wijken_sf <- map_AMS_wijken_sf[order(map_AMS_wijken_sf$Buurtcombi), ] 
data_IND2_02I$Buurtcom_1 <- factor(data_IND2_02I$Buurtcom_1, levels = as.character(map_AMS_wijken_sf$Buurtcom_1))
plot2 <- ggplot(data = data_IND2_02I, mapping = aes(y = Buurtcom_1,
                                                    x = jaar,
                                                    fill = IND2_1_Q)) +
  geom_tile() + 
  scale_fill_manual(values= colIntOrRd) + 
  labs(fill = "Indicator 1") + xlab("Time") + ylab("Neighborhood") 
ggsave("../Output/Chapter_1/Step4/HeatMap_HouseDynam_Indic1Time.png", 
       plot = plot2, height = 15)

############################################################
### INDICATOR 2
############################################################

# # boxplot 
data_IND2_02I$jaar <- as.factor(data_IND2_02I$jaar)
plot2 <-  ggplot(data_IND2_02I, aes(x = jaar, y =  IND2_2)) + 
  geom_boxplot() + theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) + scale_x_discrete(name = "Time", breaks = list_jaar) + 
  scale_y_continuous(name = "Indicator-Housing Dynamics 2", 
                     breaks = seq(0, 100, by =10), limits = c(10, 100)) +
  geom_hline(yintercept = 50, linetype="dotted") +
  ggtitle("Housing Dynamics (Indicator 2)")

ggsave("../Output/Chapter_1/Step4/HousingDynamics_IND2_TimeAver_Boxplot.png", plot = plot2, width = 15)


vect_map_IND2 <- paste("map", list_jaar, "IND2_2",sep = "_")
for(ii in 1:length(list_jaar)){
  map_AMS_wijken_sf_Jaar <- map_AMS_wijken_sf
  data_IND2_02I_jaar <- data_IND2_02I %>% filter(jaar == list_jaar[ii]) %>% select("Buurtcom_1", "IND2_2_Q")
  map_AMS_wijken_sf_Jaar <- merge(map_AMS_wijken_sf_Jaar, data_IND2_02I_jaar, by = "Buurtcom_1", all.x = T)
  
  mmm <- tm_shape(map_AMS_wijken_sf_Jaar) +
    tm_borders(col = gray(0.5), alpha = 0.6)+
    tm_fill(col = "IND2_2_Q",  palette = "seq", textNA = "Other Neighborhoods", colorNA = "white")  +
    tm_shape(map_AMS_district) +
    tm_borders(alpha = 1, lwd = 2, col = gray(0.5)) + 
    tm_text("Stadsdeel", size = 0.5, shadow=TRUE) + 
    tm_layout( title = list_jaar[ii], frame = FALSE, 
               inner.margins=c(0,0,.1,0), title.size=.8)
  assign(x = vect_map_IND2[ii], value = mmm)  
  rm(mmm)
}
tpArr_YearIND2 <- tmap_arrange(map_2011_IND2_2, map_2012_IND2_2, map_2013_IND2_2, map_2014_IND2_2, 
                               map_2015_IND2_2, map_2016_IND2_2, map_2017_IND2_2, map_2018_IND2_2, 
                               ncol = 3) 
tmap_save(tpArr_YearIND2, "../Output/Chapter_1/Step4/Map_PerNeighYear_Housing_IND2.png", width=5000, height=3000)

# # Tile plot
# # resume characteristics neighborhoods and Quintiles 
# map_AMS_wijken_sf <- map_AMS_wijken_sf[order(map_AMS_wijken_sf$Buurtcombi), ] 
# data_IND2_02I$Buurtcom_1 <- factor(data_IND2_02I$Buurtcom_1, levels = as.character(map_AMS_wijken_sf$Buurtcom_1))
plot2 <- ggplot(data = data_IND2_02I, mapping = aes(y = Buurtcom_1,
                                                    x = jaar,
                                                    fill = IND2_2_Q)) +
  geom_tile() + 
  scale_fill_manual(values= colIntOrRd) + 
  labs(fill = "Indicator 2") + xlab("Time") + ylab("Neighborhood") 
ggsave("../Output/Chapter_1/Step4/HeatMap_HouseDynam_Indic2Time.png", 
       plot = plot2, height = 15)

# # save data 
save(data_IND2_02I, file = "../Output/Chapter_1/Step4/data_IND2_02I.Rdata")
