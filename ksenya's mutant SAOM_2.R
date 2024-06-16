library(RSiena)
library(xtable)
library(igraph)
library(Matrix)
library(reshape2)
library(ggplot2)
library(scales)
library(readxl)
library(network)
library(btergm)
library(ggforestplot)
setwd('C:/Users/suppo/Downloads')
# information on state system (obtained from Correlates of War Project Site)
Ssystem_base <-read.table("states2016.csv",
                          header = TRUE,
                          sep = ";",
                          stringsAsFactors = FALSE) 


#CINC Data (obtained through Peace Scientist Package)
lnCINC <- read.table("CINC_COW_data1.csv",
                     header = TRUE,
                     sep = ";",
                     stringsAsFactors = FALSE,
                     check.names=FALSE)


### RGDP in constant 1996 dollars (Penn World Tables + Gledisch, 2001 + World Development Indicators)
lnRGDP <- read.csv("log_RGDP.csv",
                   header = TRUE,
                   sep = ";",
                   encoding = "ASCI",
                   stringsAsFactors = FALSE)


MainDyadicData<- read.csv("main_dyadic.csv")
qog_std_ts_jan24 <- read.csv("qog_std_ts_jan24.csv")

############################################################################
############################################################################
######   SET ALL THE NECESSARY PARAMETERS ##################################
############################################################################
############################################################################

id <- unique(c(MainDyadicData$ccode1, MainDyadicData$ccode2))

### CHOOSE MICROSTATES TO EXCLUDE IF NECESSARY
id <- id[! id %in% c(31, 53,	55,	56,	57,	58,	60,	80,	221, 223, 232, 331,	338,	347,	395,	403, 511,	591, 626,	781,	835, 946,	947, 970,	983, 986,	987, 990)]
print(id)
# here are the excluded microstates in order: Bahamas	Barbados	Grenada	St. Lucia	St. Vincent and the Grenadines	Antigua & Barbuda	St. Kitts and Nevis	Belize	Monaco	Liechtenstein	Andorra	San Marino	Malta	Kosovo	Iceland	Sao Tome and Principe	Zanzibar	Seychelles	South Sudan	Maldives	Brunei	Kiribati	Tuvalu	Nauru	Marshall Islands	Palau	Federated States of Micronesia	Samoa
# 
ex_id <- c(31, 53,	55,	56,	57,	58,	60,	80,	221, 223, 232, 331,	338,	347,	395,	403, 511,	591, 626,	781,	835, 946,	947, 970,	983, 986,	987, 990)

############################################################################
#fixed parameters
Ssystem_base <- Ssystem_base[! Ssystem_base$ccode %in% ex_id,]
SSystem <- Ssystem_base


###############################################################
###############################################################
### ЗАДАЕМ НАСТРОЙКИ ДЛЯ МОДЕЛИ
###############################################################
i=1992    #### стартовое значение
CUT_OFF <- 3 ### пороговое значение бинаризации сети
LAG <- 0  #### лаг эффекта предиктора
codes <- unique(MainDyadicData$ccode1[MainDyadicData$year == i])
yr<-(seq(2002,2010))  ### период построения модели динамиики сети
ly_r <- length(yr)  ### длина периода построения модели динамиики сети
ly_rc <- ly_r-1  ### технические параметры для генерации матриц
y_pr<-1987    ### технические параметры для генерации матриц
window<-5   ### период построения модели динамиики сети +1
Nodes<- 8  ### number of CPU cores used

SienaCompositionChange_default <- "YES" #### set "YES" to enable default Siena Composition Change setting

###############################################################
###############################################################
### RSiena State System Composition Change
###############################################################


SSystem<- Ssystem_base
codes <- unique(SSystem$ccode)
codes1 <- codes
for  (p in codes1) {
  if (SSystem$endyear[SSystem$ccode == p] < yr[1]) {
    SSystem <- subset(SSystem, (SSystem$ccode == p) == FALSE)
    codes <- subset(codes, (codes == p) == FALSE)
  }
} 
rm(codes1)
for (p in codes) {
  if (SSystem$endyear[SSystem$ccode == p] < yr[length(yr)]) {  
    SSystem$eperiod[SSystem$ccode == p] <- yr[length(yr)]-SSystem$endyear[SSystem$ccode == p]
  } 
  else {
    SSystem$eperiod[SSystem$ccode == p] <- length(yr)
  }
}
remove(p)

for (p in codes) {
  if (SSystem$styear[SSystem$ccode == p] < yr[1]) {  
    SSystem$speriod[SSystem$ccode == p] <- 1
  } 
  else {
    SSystem$speriod[SSystem$ccode == p] <- SSystem$styear[SSystem$ccode == p] - yr[1] + 1
  }
}
remove(p)

SSystem <- subset(SSystem, (SSystem$speriod<6) == TRUE)
codes <- unique(SSystem$ccode)

comp <- rep(list(c(1,length(yr))), length(unique(length(codes))))
l_id <- seq(1, length(codes))
for (p in l_id) {
  comp[[p]] <- c(SSystem$speriod[p],SSystem$eperiod[p])
}
rm(l_id)

if (SienaCompositionChange_default == "YES")  {
  schanges <- sienaCompositionChange(comp)
  rm(SSystem)
}


###############################################################
###############################################################

### Ksenya's Data
library(countrycode)
#наш базовый ооновский датасет, проводим с ним все необходимые преобразования
data <- read.csv('C:/Users/suppo/Downloads/full_data_2020.csv')
data<-full_data_2020
#Cow кодим
data$ccode <- countrycode(data$Contributor_ISO.3, "iso3c", 'cown')
data$ccode[data$Contributor_ISO.3 =="SRB"]<- 345

data$ccode_mis <- countrycode(data$Mission_Country_ISO.3, "iso3c", 'cown')
data$ccode_mis[data$Mission_Country_ISO.3 =="SRB"]<- 345 
data$ccode_mis[data$Mission_Country_ISO.3 =="ESH"]<- 732


country_names<-codes
years <- yr

data <- data %>% mutate(year = as.numeric(format(as.Date(Date), "%Y")))

nms <- vector("character", length(years))

for (i in years) {
  wdata <- data[data$year == i, ]
  mission_list <- unique(wdata$Mission)
  mat <- matrix(0, nrow = length(country_names), ncol = length(country_names),
                dimnames = list(country_names, country_names))
  
  for (y in mission_list) {
    miss_part <- wdata$ccode[wdata$Mission == y]
    miss_part<- miss_part[miss_part %in% codes]
    mat[as.character(unique(miss_part)), as.character(unique(miss_part))] <- 1
  }
  
  assign(paste("peace_ops_sum", i, sep = "_"), mat)
  nms[i - min(years) + 1] <- paste("peace_ops_sum", i, sep = "_")
}

nms1 <- lapply(nms, get)
peace_SAOM_new <- array(do.call("c", nms1), dim = c(length(country_names), length(country_names), length(years)))


##### МАТРИЦА POWER MATCH-MISMATCH
##### CINC
nms <- vector()
nms3 <- vector()

ct <- 1

for (i in yr) {
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  NMC_SLICE<-NMC_DATASET[NMC_DATASET$year==i,][c("ccode","cinc")]
  for (country1 in unique(codes)){
    print(country1)
    print(i)
    for (country2 in unique(codes)){
      if(country1!=country2){
        power_match_cinc<- abs(NMC_SLICE[NMC_SLICE$ccode==country1,2]-NMC_SLICE[NMC_SLICE$ccode==country2,2])
        if (length(power_match_cinc)>0) {
          mat[as.character(country1),as.character(country2)]<-power_match_cinc
        }}}}
  assign(paste("CINC Power Mismatch", i, sep="_"), mat)
  nms[ct] <- paste("CINC Power Mismatch", i, sep="_")
  ct <- ct+1
}; rm(ct, i)
nms1 <- lapply(nms, get)
CINC_Mismatch_SAOM <- array(do.call("c", nms1), dim = c(length(codes), length(codes), ly_rc)) ### ly_rc - array length for independent variable for SAOM
CINC_Mismatch <- lapply(nms, get)
rm(list=nms); rm(nms)


##### MMP
nms <- vector()
nms3 <- vector()

ct <- 1
MMP$year1<-format(MMP$year, format="%Y")

for (i in yr) {
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  MMP_SLICE<-as.data.frame(na.omit(MMP[MMP$year1==i,][c("ccode","MMP")]))
  for (country1 in unique(codes)){
    print(country1)
    print(i)
    for (country2 in unique(codes)){
      if(country1!=country2){
        power_match_mmp<- abs(MMP_SLICE[MMP_SLICE$ccode==country1,2]-MMP_SLICE[MMP_SLICE$ccode==country2,2])
        if (length(power_match_mmp)>0) {
          mat[as.character(country1),as.character(country2)]<-power_match_mmp
        }}}}
  assign(paste("MMP Power Mismatch", i, sep="_"), mat)
  nms[ct] <- paste("MMP Power Mismatch", i, sep="_")
  ct <- ct+1
}; rm(ct, i)
nms1 <- lapply(nms, get)
MMP_Mismatch_SAOM <- array(do.call("c", nms1), dim = c(length(codes), length(codes), ly_rc)) ### ly_rc - array length for independent variable for SAOM
MMP_Mismatch <- lapply(nms, get)
rm(list=nms); rm(nms)



##### MIDS
nms <- vector()
nms3 <- vector()

ct <- 1
for (i in yr) {
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  edges1 <- MainDyadicData[c("ccode1","ccode2","year","hostlev","hostlev2")]
  yr1<-seq(i-5,i)
  for (country1 in unique(codes)){
    print(country1)
    print(i)
    for (x1 in yr1) {
      edges <- edges1[edges1$year == x1-LAG,][c("ccode1","ccode2","hostlev","hostlev2")]
    threats_list<-na.omit(edges$ccode2[edges$hostlev>1&edges$ccode1==country1 & edges$ccode2 %in% codes ])
    if (length(threats_list)>0) {
    for (country2 in unique(threats_list)){
      mat[as.character(country1),as.character(country2)]<-mat[as.character(country1),as.character(country2)]+edges$hostlev[edges$ccode1==country1 & edges$ccode2==country2]
    }}
}}
write.csv(mat, paste( i, "_", "Militarized interstate disputes°.csv", sep=""))
assign(paste("Hostility", i, sep="_"), mat)
nms[ct] <- paste("Hostility", i, sep="_")
}
nms1 <- lapply(nms, get)
Hostility_SAOM <- array(do.call("c", nms1), dim = c(length(codes), length(codes), ly_rc)) ### ly_rc - array length for independent variable for SAOM
Hostility <- lapply(nms, get)
rm(list=nms); rm(nms)


##### COMMON THREATS
nms <- vector()
nms1 <- vector()
nms2 <- vector()
nms3 <- vector()

ct <- 1
for (i in yr) {
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  mat1 <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  edges <- MainDyadicData[MainDyadicData$year <i+1 & MainDyadicData$year >i-5,][c("ccode1","ccode2","year","hostlev","hostlev2")]
  for (country1 in unique(codes)){
    print(country1)
    print(i)
    threats_list<-na.omit(edges$ccode2[edges$hostlev2>1&edges$ccode1==country1])
    if (length(threats_list)>0) {
    for (country2 in unique(codes)){
      if (country1!=country2) {
      HostLev_total<-0
      threat_num<-0
      for (country3 in unique(threats_list)) {
        threat1 <-sum(na.omit(edges[edges$ccode1==country1&edges$ccode2==country3,"hostlev2"]))
        if (length(threat1)>0) {
          if (is.na(threat1)==FALSE) {
            if (threat1>1) {
              threat2 <-sum(na.omit(edges[edges$ccode1==country2&edges$ccode2==country3,"hostlev2"]))
              if (length(threat2)>0 ) {
                if (is.na(threat2)==FALSE) {
                  if (threat2>1) {
                    threat_num<-threat_num+1
                    HostLev_total<-HostLev_total+threat1+threat2
                    print(HostLev_total)
                  }}}}}}
      }
      mat[as.character(country1),as.character(country2)]<- mat[as.character(country1),as.character(country2)]+HostLev_total
      mat1[as.character(country1),as.character(country2)]<-mat1[as.character(country1),as.character(country2)]+threat_num
    }
    }}
  }
  write.csv(mat, paste( i, "_", "Common Hostility°.csv", sep=""))
  write.csv(mat1, paste( i, "_", "Number of Common Threats°.csv", sep=""))
  assign(paste("Common Hostility Level", i, sep="_"), mat)
  nms[ct] <- paste("Common Hostility Level", i, sep="_")
  
  assign(paste("Common threats number", i, sep="_"), mat1)
  nms1[ct] <- paste("Common threats number", i, sep="_")
}
nms2 <- lapply(nms, get)
nms3 <- lapply(nms1, get)
CommonThreatsHostSM<- array(do.call("c", nms2), dim = c(length(codes), length(codes), ly_rc)) ### ly_rc - array length for independent variable for SAOM
CommonThreatsNumSM<- array(do.call("c", nms3), dim = c(length(codes), length(codes), ly_rc)) ### ly_rc - array length for independent variable for SAOM

CommonThreatsHost <- lapply(nms, get)
CommonThreatsNum <- lapply(nms1, get)
rm(list=nms); rm(nms); rm(list=nms1); rm(nms1); rm(list=nms2); rm(nms2); rm(list=nms3); rm(nms3)



##### Dyadic Institutional Lock
nms <- vector()
nms3 <- vector()
ct <- 1

for (i in yr) {
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  mat2 <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  constr_SLICE<-as.data.frame(na.omit(qog_std_ts_jan24[qog_std_ts_jan24$year==i,][c("ccodecow","h_polcon3")]))
  for (country1 in unique(codes)){
    print(country1)
    print(i)
    for (country2 in unique(codes)){
      if(country1!=country2){
        constraints_sum<-constr_SLICE[constr_SLICE$ccodecow==country1,2]+constr_SLICE[constr_SLICE$ccodecow==country2,2]
        if (length(constraints_sum)>0) {
          mat[as.character(country1),as.character(country2)]<-constraints_sum
        }}}}
  assign(paste("Constraints Sum", i, sep="_"), mat)
  nms[ct] <- paste("Constraints Sum", i, sep="_")
}
nms1 <- lapply(nms, get)
Constraints_Sum_SAOM <- array(do.call("c", nms1), dim = c(length(codes), length(codes), ly_rc)) ### ly_rc - array length for independent variable for SAOM
Constraints_Sum <- lapply(nms, get)
rm(list=nms); rm(nms)


##### МАТРИЦА АЛЬЯНСЫ
nms <- vector()
nms3 <- vector()

ct <- 1

for (i in yr) {
  
  edges <- MainDyadicData[MainDyadicData$year == i-LAG,][c("ccode1","ccode2","atop_defense")]
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  for (country1 in unique(codes)){
    print("alliance")
    print(country1)
    print(i)
    for (country2 in unique(codes)){
      if(country1!=country2){
        if ((length(edges[edges$ccode1==country1&edges$ccode2==country2,"atop_defense"])>0)==TRUE) {
          if(is.na(edges[edges$ccode1==country1&edges$ccode2==country2,"atop_defense"])==FALSE){
            if(edges[edges$ccode1==country1&edges$ccode2==country2,"atop_defense"]>0){
              mat[as.character(country1),as.character(country2)]<-edges[edges$ccode1==country1&edges$ccode2==country2,"atop_defense"]
            }}}}}}
  print(isSymmetric(mat))
  mat1<-network(mat)
  
  assign(paste("Alliance_S", i, sep="_"), mat)
  polity_networks_5 <- mat
  nms3[ct] <- paste("Alliance_S", i, sep="_")
  
  assign(paste("Alliance", i, sep="_"), mat1)
  nms[ct] <- paste("Alliance", i, sep="_")
  ct <- ct+1
  rm(edges, mat)
}; rm(ct, i)

nms1 <- lapply(nms, get)
nms4 <- lapply(nms3, get)

nms1 <- lapply(nms, get)
Alliance_SAOM <- array(do.call("c", nms4), dim = c(length(codes), length(codes), ly_rc)) ### ly_rc - array length for independent variable for SAOM
Alliance_net <- lapply(nms, get)
rm(list=nms); rm(nms)


###############################################################
##### МАТРИЦА JME
nms <- vector()
nms5 <- vector()

ct <- 1


for (i in yr) {
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  edges <- jmeDataPublic[jmeDataPublic$startYear == i-LAG,]
  JME_u<-unique(edges$xID)
  for (ex in JME_u) {
    miss_part<-c()
    for (n in seq(1,nrow(edges))) {
      if (edges$xID[n] == ex) {
        miss_part<-c(miss_part,edges$countryCode[n])
      }}
    for (country1 in unique(miss_part)){
      for (country2 in unique(miss_part)){
        if (country1 != country2) {
        if ((country1 %in% codes)&(country2 %in% codes)) {
          mat[as.character(country1),as.character(country2)]<- 1
        }}}}
  }
  print(isSymmetric(mat))
  assign(paste("JME_ops_sum", i, sep="_"), mat)
  nms[ct] <- paste("JME_ops_sum", i, sep="_")
  ct<-ct+1
}
nms5 <- lapply(nms, get)
JMEs_SAOM <- array(do.call("c", nms5), dim = c(length(codes), length(codes), ly_rc))
JMEs_SAOM_dep<- array(do.call("c", nms5), dim = c(length(codes), length(codes), ly_r))
rm(list=nms); rm(nms); rm(list=nms1); rm(nms1); rm(list=nms2); rm(nms2); rm(list=nms3); rm(nms3)


###############################################################
##### МАТРИЦА deployments

nms <- vector()
nms3 <- vector()

ct <- 1

for (i in yr) {
  edges <- IMDT_12_01_24[IMDT_12_01_24$year == i-LAG,]
  edges<-as.data.frame(edges)
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  for (country1 in unique(edges$ccode1)){
    print("deployments")
    print(country1)
    print(i)
    for (country2 in unique(edges$ccode2)){
      if ((country1 %in% codes)&(country2 %in% codes)) {
        if(country1!=country2){
          if ((length(edges[edges$ccode1==country1&edges$ccode2==country2,"Troops"])>0)==TRUE) {
            if(is.na(edges[edges$ccode1==country1&edges$ccode2==country2,"Troops"])==FALSE){
              if(edges[edges$ccode1==country1&edges$ccode2==country2,"Troops"]>0){
                mat[as.character(country1),as.character(country2)]<-1
              }}}}}}}
  print(isSymmetric(mat))
  mat1<-network(mat)
  
  assign(paste("Deployments_S", i, sep="_"), mat)
  polity_networks_5 <- mat
  nms3[ct] <- paste("Deployments_S", i, sep="_")
  
  assign(paste("Deployments", i, sep="_"), mat1)
  nms[ct] <- paste("Deployments", i, sep="_")
  ct <- ct+1
  rm(edges, mat)
}; rm(ct, i)

nms1 <- lapply(nms, get)
nms4 <- lapply(nms3, get)

nms1 <- lapply(nms, get)
Deployments_SAOM <- array(do.call("c", nms4), dim = c(length(codes), length(codes), ly_rc))
Deployments_SAOM_dep<- array(do.call("c", nms4), dim = c(length(codes), length(codes), ly_r))
Deployments_net <- lapply(nms, get)
rm(list=nms); rm(nms); rm(list=nms1); rm(nms1); rm(list=nms2); rm(nms2); rm(list=nms3); rm(nms3); rm(list=nms4); rm(nms4)



##################################################################################################
### SIPRI ARMS TRANSFERS
nms <- vector()
ct <- 1
for (i in yr) {
  edges <- MainDyadicData[MainDyadicData$year == i,][c("ccode1","ccode2","all_edge", "all_tiv_total_sum")]
  edges <- edges[edges$ccode1 %in% codes,]
  edges <- edges[edges$ccode2 %in% codes,]
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  for (country1 in unique(edges$ccode1)){
    for (country2 in unique(edges$ccode2)){
      if ((country1!=country2)==TRUE){
        if (is.na(edges[edges$ccode1==country1&edges$ccode2==country2,"all_edge"])==FALSE){
          if(length(edges[edges$ccode1==country1&edges$ccode2==country2,"all_edge"])>0){
            if(edges[edges$ccode1==country1&edges$ccode2==country2,"all_tiv_total_sum"]>CUT_OFF){
              mat[as.character(country1),as.character(country2)]<-edges[edges$ccode1==country1&edges$ccode2==country2,"all_edge"]
            }}}}}}
  print(isSymmetric(mat))
  assign(paste("all_edge", i, sep="_"), mat)
  nms[ct] <- paste("all_edge", i, sep="_")
  ct <- ct+1
  rm(edges, mat)
}; rm(ct, i)

edges <- MainDyadicData[MainDyadicData$year == y_pr,][c("ccode1","ccode2","all_edge", "all_tiv_total_sum")]
edges <- edges[edges$ccode1 %in% codes,]
edges <- edges[edges$ccode2 %in% codes,]
mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
for (country1 in unique(edges$ccode1)){
  for (country2 in unique(edges$ccode2)){
    if ((country1!=country2)==TRUE){
      if (is.na(edges[edges$ccode1==country1&edges$ccode2==country2,"all_edge"])==FALSE){
        if(length(edges[edges$ccode1==country1&edges$ccode2==country2,"all_edge"])>0){
          if(edges[edges$ccode1==country1&edges$ccode2==country2,"all_tiv_total_sum"]>CUT_OFF){
            mat[as.character(country1),as.character(country2)]<-edges[edges$ccode1==country1&edges$ccode2==country2,"all_edge"]
          }}}}}}
SIPRI_transfers_networks_OoS <- mat
rm(mat)
nms1 <- lapply(nms, get)
SIPRI_transfers_networks <- array(do.call("c", nms1), dim = c(length(codes), length(codes), ly_r))
SIPRI_transfers_networks_D <- SIPRI_transfers_networks
SIPRI_transfers_networks <- array(do.call("c", nms1), dim = c(length(codes), length(codes), ly_rc))

SIPRI_transfers_networks_OoS <- array(c(SIPRI_transfers_networks[,,5], SIPRI_transfers_networks_OoS), dim = c(length(codes), length(codes), 2) )
rm(list=nms); rm(nms, nms1)



###############################################################
##### МАТРИЦЫ ТОРГОВЛИ

nms <- vector()
nms3 <- vector()
ct <- 1
lx<-yr[1]-1946-LAG
cx<-yr[1]-1947-LAG

for (i in yr) {
  
  edges <- MainDyadicData[MainDyadicData$year == i,][c("ccode1","ccode2","log_flow2")]
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  for (country1 in unique(codes)){
    print("trade")
    print(i)
    print(country1)
    for (country2 in unique(codes)){
      if(country1!=country2){
        if ((length(edges[edges$ccode1==country1&edges$ccode2==country2,"log_flow2"])>0)==TRUE) {
          if(is.na(edges[edges$ccode1==country1&edges$ccode2==country2,"log_flow2"])==FALSE){
            if(edges[edges$ccode1==country1&edges$ccode2==country2,"log_flow2"]>=0){
              if (edges[edges$ccode1==country1&edges$ccode2==country2,"log_flow2"]>CUT_OFF) {
                mat[as.character(country1),as.character(country2)]<-1
              }}}}}}}
  print(isSymmetric(mat))
  assign(paste("tradeS", i, sep="_"), mat)
  
  nms3[ct] <- paste("tradeS", i, sep="_")
  
  mat<-network(mat)
  
  lx<-i-1946-LAG
  cx<-i-1947-LAG
  
  CINC_paste <- lnCINC[[cx]]
  mat%v%"CINC" <- CINC_paste
  
  RGDP_paste <- lnRGDP[[lx]][lnRGDP$ccode %in% codes]
  mat%v%"RGDP" <- RGDP_paste
  assign(paste("trade", i, sep="_"), mat)
  nms[ct] <- paste("trade", i, sep="_")
  lx<- lx+1
  cx<- cx+1
  ct <- ct+1
}
nms1 <- lapply(nms, get)
nms4 <- lapply(nms3, get)
trade_net_SAOM  <- array(do.call("c", nms4), dim = c(length(codes), length(codes), ly_rc))
trade_net <- nms1
rm(list=nms); rm(nms); rm(list=nms1); rm(nms1); rm(list=nms2); rm(nms2); rm(list=nms3); rm(nms3); rm(list=nms4); rm(nms4)



###############################################################
##### МАТРИЦА POLITY

print("polity4 data")
nms <- vector()
nms3 <- vector()
ct <- 1

for (i in yr) {
  
  edges <- MainDyadicData[MainDyadicData$year == i-LAG,][c("ccode1","ccode2","polity_dif")]
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  for (country1 in unique(codes)){
    print("polity")
    print(country1)
    print(i)
    for (country2 in unique(codes)){
      if(country1!=country2){
        if ((length(edges[edges$ccode1==country1&edges$ccode2==country2,"polity_dif"]) >0) == TRUE) {
          if(is.na(edges[edges$ccode1==country1&edges$ccode2==country2,"polity_dif"])==FALSE){
            mat[as.character(country1),as.character(country2)]<-edges[edges$ccode1==country1&edges$ccode2==country2,"polity_dif"]
          }}}}}
  print(isSymmetric(mat))
  mat1<-network(mat)
  
  assign(paste("polity_S", i, sep="_"), mat)
  polity_networks_5 <- mat
  nms3[ct] <- paste("polity_S", i, sep="_")
  
  assign(paste("polity_dif", i, sep="_"), mat1)
  nms[ct] <- paste("polity_dif", i, sep="_")
  ct <- ct+1
  rm(edges, mat)
}; rm(ct, i)
pol_net <- lapply(nms, get)
nms1 <- lapply(nms, get)
nms4 <- lapply(nms3, get)

pol_net_SAOM <- array(do.call("c", nms4), dim = c(length(codes), length(codes), ly_rc))

rm(list=nms); rm(nms, nms1)



###############################################################
##### МАТРИЦА UN
nms <- vector()
nms3 <- vector()
ct <- 1

for (i in yr) {
  edges <- MainDyadicData[MainDyadicData$year == i-LAG,][c("ccode1","ccode2","idealpointdistance")]
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  for (country1 in unique(codes)){
    print("UN")
    print(country1)
    print(i)
    for (country2 in unique(codes)){
      if(country1!=country2){
        if ((length(edges[edges$ccode1==country1&edges$ccode2==country2,"idealpointdistance"])>0)==TRUE) {
          if(is.na(edges[edges$ccode1==country1&edges$ccode2==country2,"idealpointdistance"])==FALSE){
            if(edges[edges$ccode1==country1&edges$ccode2==country2,"idealpointdistance"]>0){
              mat[as.character(country1),as.character(country2)]<-edges[edges$ccode1==country1&edges$ccode2==country2,"idealpointdistance"]
            }}}}}}
  print(isSymmetric(mat))
  mat1<-network(mat)
  
  assign(paste("UN_S", i, sep="_"), mat)
  polity_networks_5 <- mat
  nms3[ct] <- paste("UN_S", i, sep="_")
  
  assign(paste("UN", i, sep="_"), mat1)
  nms[ct] <- paste("UN", i, sep="_")
  ct <- ct+1
  rm(edges, mat)
}; rm(ct, i)
UN_net <- lapply(nms, get)

nms1 <- lapply(nms, get)
nms4 <- lapply(nms3, get)

UN_S_SAOM <- array(do.call("c", nms4), dim = c(length(codes), length(codes), ly_rc))

rm(list=nms); rm(nms); rm(list=nms1); rm(nms1); rm(list=nms2); rm(nms2); rm(list=nms3); rm(nms3); rm(list=nms4); rm(nms4)

###############################################################
##### МАТРИЦА РАССТОЯНИЕ

print("geographic distance")
nms <- vector()
ct <- 1
for (i in yr) {
  
  edges <- MainDyadicData[MainDyadicData$year == i-LAG,][c("ccode1","ccode2","LN_mindist")]
  mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
  for (country1 in unique(codes)){
    print("distance")
    print(country1)
    for (country2 in unique(codes)){
      if(country1!=country2){
        if ((length(edges[edges$ccode1==country1&edges$ccode2==country2,"LN_mindist"])>0)==TRUE) {
          if(is.na(edges[edges$ccode1==country1&edges$ccode2==country2,"LN_mindist"])==FALSE){
            mat[as.character(country1),as.character(country2)]<-edges[edges$ccode1==country1&edges$ccode2==country2,"LN_mindist"]
          }}}}}
  mat1<-mat
  mat <- network(mat)
  mat <- handleMissings(mat, na = NA, method = "zero", logical = FALSE)
  assign(paste("geo_distance", i, sep="_"), mat)
  nms[ct] <- paste("geo_distance", i, sep="_")
  ct <- ct+1
  rm(edges, mat)    
}; rm(ct, i)
nms1 <- lapply(nms, get)
Distance_net <- nms1

### create matrix for geographic distance
edges <- MainDyadicData[c("ccode1","ccode2","LN_mindist")]
edges <- edges[edges$ccode1 %in% codes,]
edges <- edges[edges$ccode2 %in% codes,]
edges <- subset(edges, is.na(edges$LN_mindist) == FALSE )
mat <- matrix(0, nrow=length(codes), ncol=length(codes), dimnames=list(codes, codes))
for (country1 in unique(edges$ccode1)){
  for (country2 in unique(edges$ccode2)){
    if ((country1!=country2)==TRUE){
      if(length(edges[edges$ccode1==country1&edges$ccode2==country2,"LN_mindist"])>0){
        mat[as.character(country1),as.character(country2)]<-mean(edges[edges$ccode1==country1&edges$ccode2==country2,"LN_mindist"])
      }}}}
print(isSymmetric(mat))
Mindist_networks1 <- coDyadCovar(mat)
rm(edges, mat)


###############################################################
##### MONADIC VARIABLES


mon_template<-data.frame(unique(codes))
colnames(mon_template)<-"ccode"

lnRGDP1 <- lnRGDP[ lnRGDP$ccode %in% codes,]
RGDP <- vector()
for (x1 in yr) {
  RGDP_vector<-vector()
  i <- x1 - 1949+1-LAG
  for (country1 in codes){
    RGDP_vector<- c(RGDP_vector,lnRGDP1[lnRGDP1$ccode==country1,i])
  }
  RGDP <- cbind(RGDP,RGDP_vector)
}
colnames(RGDP)<-seq(1,25)



INST_CONSTR  <- vector()
for (i in yr) {
  constr_SLICE<-as.data.frame(na.omit(qog_std_ts_jan24[qog_std_ts_jan24$year==i,][c("ccodecow","h_polcon3")]))
  constr_SLICE <- constr_SLICE[constr_SLICE$ccodecow %in% codes,]
  IC_vector<-vector()
  for (country1 in codes){
    if (length(constr_SLICE$h_polcon3[constr_SLICE$ccodecow==country1])>0)
    {
      IC_vector<-c(IC_vector,constr_SLICE$h_polcon3[constr_SLICE$ccodecow==country1])}
    else {
      IC_vector<-c(IC_vector,0)
    }
  }
  INST_CONSTR<-cbind(INST_CONSTR,IC_vector)
}

GDP_PC <- vector()
for (i in yr) {
  constr_SLICE<-as.data.frame(na.omit(qog_std_ts_jan24[qog_std_ts_jan24$year==i,][c("ccodecow","wdi_gdpcapcon2015")]))
  constr_SLICE <- constr_SLICE[constr_SLICE$ccodecow %in% codes,]
  IC_vector<-vector()
  for (country1 in codes){
    if (length(constr_SLICE$wdi_gdpcapcon2015[constr_SLICE$ccodecow==country1])>0)
    {
      IC_vector<-c(IC_vector,log(constr_SLICE$wdi_gdpcapcon2015[constr_SLICE$ccodecow==country1]))}
    else {
      IC_vector<-c(IC_vector,0)
    }
  }
  GDP_PC<-cbind(GDP_PC,IC_vector)
}

p_polity2 <- vector()
for (i in yr) {
  constr_SLICE<-as.data.frame(na.omit(qog_std_ts_jan24[qog_std_ts_jan24$year==i,][c("ccodecow","p_polity2")]))
  constr_SLICE <- constr_SLICE[constr_SLICE$ccodecow %in% codes,]
  IC_vector<-vector()
  for (country1 in codes){
    if (length(constr_SLICE$p_polity2[constr_SLICE$ccodecow==country1])>0)
    {
      IC_vector<-c(IC_vector,constr_SLICE$p_polity2[constr_SLICE$ccodecow==country1])}
    else {
      IC_vector<-c(IC_vector,0)
    }
  }
  p_polity2<-cbind(p_polity2,IC_vector)
}



UCDP_internal_conflict <- vector()
for (i in yr) {
  constr_SLICE<-as.data.frame(na.omit(qog_std_ts_jan24[qog_std_ts_jan24$year==i,][c("ccodecow","ucdp_type3")]))
  constr_SLICE <- constr_SLICE[constr_SLICE$ccodecow %in% codes,]
  IC_vector<-vector()
  for (country1 in codes){
    if (length(constr_SLICE$ucdp_type3[constr_SLICE$ccodecow==country1])>0)
    {
      IC_vector<-c(IC_vector,constr_SLICE$ucdp_type3[constr_SLICE$ccodecow==country1])}
    else {
      IC_vector<-c(IC_vector,0)
    }
  }
  UCDP_internal_conflict<-cbind(UCDP_internal_conflict,IC_vector)
}

wdi_expmil <- vector()
for (i in yr) {
  constr_SLICE<-as.data.frame(na.omit(qog_std_ts_jan24[qog_std_ts_jan24$year==i,][c("ccodecow","wdi_expmil")]))
  constr_SLICE <- constr_SLICE[constr_SLICE$ccodecow %in% codes,]
  IC_vector<-vector()
  for (country1 in codes){
    if (length(constr_SLICE$wdi_expmil[constr_SLICE$ccodecow==country1])>0)
    {
      IC_vector<-c(IC_vector,constr_SLICE$wdi_expmil[constr_SLICE$ccodecow==country1])}
    else {
      IC_vector<-c(IC_vector,0)
    }
  }
  wdi_expmil<-cbind(wdi_expmil,IC_vector)
}



lnCINC1 <- lnCINC[ lnCINC$ccode %in% codes,]
CINC <- vector()
for (x1 in yr) {
  CINC_vector<-vector()
  i <- x1 - 1949+2-LAG
  for (country1 in codes){
    CINC_vector<- c(CINC_vector,lnCINC1[lnCINC1$ccode==country1,i])
  }
  CINC <- cbind(CINC,CINC_vector)
}
colnames(CINC)<-seq(1,25)


###############################################################

dependent_peace <- sienaDependent(peace_SAOM )

##### specify Rsiena dyadic covariates 

polity_net2 <- varDyadCovar(pol_net_SAOM)
Alliance_net2  <- varDyadCovar(Alliance_SAOM)
Deployments_net2  <- varDyadCovar(Deployments_SAOM)
ComThreatsHost_net2  <- varDyadCovar(CommonThreatsHostSM)
ComThreatsNum_net2 <- varDyadCovar(CommonThreatsNumSM)
bitrade_net2 <- varDyadCovar(trade_net_SAOM)
host_net2 <- varDyadCovar(Hostility_SAOM)
MMP_Mismatch_net2 <- varDyadCovar(MMP_Mismatch_SAOM)
CINC_Mismatch_net2 <- varDyadCovar(CINC_Mismatch_SAOM)
UN_GA_net2 <- varDyadCovar(UN_S_SAOM)
ArmsTrans_net2 <- varDyadCovar(SIPRI_transfers_networks)
JMEs_net2 <- varDyadCovar(JMEs_SAOM)
Constraints_net2 <- varDyadCovar(Constraints_Sum_SAOM)


##### specify Rsiena monadic covariates 
CINC_var <-varCovar(CINC)
IC_var <-varCovar(INST_CONSTR)
RGDP_var <-varCovar(RGDP)

GDP_PC_var <-varCovar(GDP_PC)
UCDP_Conflict_var <-varCovar(UCDP_internal_conflict)
wdi_expmil_var <-varCovar(wdi_expmil)
p_polity2_var <-varCovar(p_polity2)


################################
###  Базовая модель  ###
# 1.  Sender effect, GW Indegree, gwdspFB 
# 2.  Receiver effect, GW Outdegree, gwdspFF
# 3.  Mutuality, Reciprocity
# 4.  Friend-of-a-friend-is-a-friend dynamics (GSWEP, Transitivity and etc.). Effects: gwespFF, gwespBB
# 5.  Path Dependency (X)           LP_PD, NLP_PD
# 6.  Defense alliances (X)         ATOP_networks1
# 7.  Regime similarity (X)         polity_networks1
# 8.  CINC (altX, egoX, simX)       CINC_var
# 9.  GDP (altX, egoX, simX)        GDP_var   
# 10.  Geographic distance (X)        Mindist_networks1
# 11.  Intrastate conflicts (altX)

data_model1 <- sienaDataCreate(dependent_peace , 
                               polity_net2, Alliance_net2, bitrade_net2, 
                               ArmsTrans_net2,
                               Constraints_net2,
                               CINC_Mismatch_net2,MMP_Mismatch_net2, ComThreatsHost_net2,ComThreatsNum_net2,
                               Mindist_networks1,RGDP_var,GDP_PC_var, wdi_expmil_var, UCDP_Conflict_var, p_polity2_var,
                               schanges) 



myeff <- getEffects( data_model1 )
myeff <- includeEffects( myeff, Rate, type = "rate", include = FALSE) 
myeff <- includeEffects( myeff, outRate, type = "rate", include = FALSE) 

myeff <- includeEffects( myeff, RateX, type = "rate", interaction1 = "CINC_var", include = TRUE) 
myeff <- includeEffects( myeff, RateX, type = "rate", interaction1 = "RGDP_var") 
myeff <- includeEffects( myeff, RateX, type = "rate", interaction1 = "IC_var", include = FALSE) 
 
myeff <- includeEffects( myeff, recip, include = FALSE)
myeff <- includeEffects( myeff, density, include = TRUE)
myeff <- includeEffects( myeff, degPlus, include = FALSE)
myeff <- includeEffects( myeff, gwesp, include = FALSE, parm = "100")


#dyadic effects
myeff <- includeEffects( myeff, X, interaction1 = "Mindist_networks1" )
myeff <- includeEffects( myeff, X, interaction1 = "polity_net2" )
myeff <- includeEffects( myeff, X, interaction1 = "Alliance_net2" )
myeff <- includeEffects( myeff, X, interaction1 = "bitrade_net2" )
myeff <- includeEffects( myeff, X, interaction1 = "ArmsTrans_net2" )
myeff <- includeEffects( myeff, X, interaction1 = "CINC_Mismatch_net2" )
myeff <- includeEffects( myeff, X, interaction1 = "Constraints_net2",  include = FALSE )
myeff <- includeEffects( myeff, X, interaction1 = "ComThreatsHost_net2" )

myeff <- includeEffects( myeff, altX, egoX, interaction1 = "CINC_var") 
myeff <- includeEffects( myeff, altX, egoX, interaction1 = "RGDP_var")
myeff <- includeEffects( myeff, altX, egoX, interaction1 = "IC_var", type = "creation", include = TRUE)
myeff <- includeEffects( myeff, altX, egoX, interaction1 = "IC_var", type = "endow", include = TRUE)
myeff <- includeEffects( myeff, altX, egoX, interaction1 = "IC_var", type = "eval", include = TRUE)
effectsDocumentation(myeff)
myeff <- includeEffects( myeff, egoPlusAltX, interaction1 = "CINC_var")
myeff <- includeEffects( myeff, egoPlusAltX, interaction1 = "IC_var")
myeff <- includeEffects( myeff, egoPlusAltX, interaction1 = "UCDP_Conflict_var")
myeff <- includeEffects( myeff, egoPlusAltX, interaction1 = "GDP_PC_var")
myeff <- includeEffects( myeff, egoPlusAltX, interaction1 = "p_polity2_var")

Model1 <- sienaModelCreate(useStdInits = TRUE, projname = "pioneer_Ksusha")
Model1.results <- siena07(Model1, data = data_model1, effects = myeff, batch=F,verbose=F,useCluster=T, nbrNodes=Nodes, returnDeps = TRUE)
siena.table(Model1.results, type="html", tstat=TRUE, d=2, file=paste("pioneer_Ksusha", ".html", sep=""))
df <- data.frame(Betas=Model1.results$theta, SEs=Model1.results$se, Conv.=Model1.results$tconv)

dfSAOM<-df
dfSAOM$index <- nrow(dfSAOM):1
dfSAOM$CI_low <- dfSAOM$Betas - (qnorm(0.975)*dfSAOM$SEs)
dfSAOM$CI_high <- dfSAOM$Betas + (qnorm(0.975)*dfSAOM$SEs)
z_stat <- abs(dfSAOM[,"Betas"] / dfSAOM[,"SEs"])
dfSAOM$p.value <- exp(-0.717*z_stat- 0.416*z_stat*z_stat)
dfSAOM$p.value <- round(dfSAOM$p.value, digits=3)

dfSAOM$model <- paste("SAOM ", yr[1], "-", 2001, sep="")
dfSAOM$Names <- Model1.results$effects$effectName

print(ggforestplot::forestplot(
  df = dfSAOM,
  name = Names,
  estimate = Betas,
  se = SEs,
  pvalue = p.value,
  psignif = 0.05,
  title = paste("SAOM model", sep=" "),
  colour = model
))


summary(Model1.results)

