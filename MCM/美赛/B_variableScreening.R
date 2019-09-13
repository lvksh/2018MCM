# B

# first try
##### 
library(readxl)
library(dplyr)
library(Ball)
library(plyr)
ProblemCData <- read_excel("ProblemCData.xlsx")
Y <- filter(ProblemCData,
              # consumption
              MSN == 'PATXB' |  # petro
              MSN == 'CLTXB' |  # coal
              MSN == 'ESTXB' |  # elec
              MSN == 'NGTXB' |  # NG
              MSN == 'RETCB' |  # renew 
              # production
              MSN == 'CLPRB' |  # caal
              MSN == 'REPRB' |  # renew
              MSN == 'NGMPB' |  # Natural gas marketed production.
              # expenditures
              MSN == 'CLTCV' |  # caal
              MSN == 'NGTCV' |  # Natural gas total expenditures (including supplemental gaseous fuels).
              MSN == 'NUETV' |  # Nuclear fuel total expenditures.
              MSN == 'PATCV' |  # All petroleum products total expenditures.
              MSN == 'PETXV' )  # Primary energy total end-use expenditures.)  
X <- setdiff(ProblemCData,Y) 
Y <- filter(Y,Year >= 1980)
X <- filter(X,Year >= 1980)

YY <- sapply(1:n_distinct(Y$MSN),function(e){
    temp <- filter(Y,MSN == unique(Y$MSN)[e])
    temp <- temp[,'Data']
    temp
})

XX <- sapply(1:n_distinct(X$MSN),function(e){
  temp <- filter(X,MSN == unique(X$MSN)[e])
  temp <- temp[,'Data']
  temp
})

YY <- data.frame(YY)
XX <- data.frame(XX)
colnames(YY) <- unique(Y$MSN)
colnames(XX) <- unique(X$MSN)


YY_AZ <- YY[1:30,]
YY_CA <- YY[31:60,]
YY_NM <- YY[61:90,]
YY_TX <- YY[91:120,]
XX_AZ <- XX[1:30,]
XX_CA <- XX[31:60,]
XX_NM <- XX[61:90,]
XX_TX <- XX[91:120,]

screening_AZ <- bcorsis(x=XX_AZ,y=YY_AZ,d=20)
screening_CA <- bcorsis(x=XX_CA,y=YY_CA,d=20)
screening_NM <- bcorsis(x=XX_NM,y=YY_NM,d=20)
screening_TX <- bcorsis(x=XX_TX,y=YY_TX,d=20)

MSNCODE <- colnames(XX)
AZ_index <- unlist(screening_AZ)
CA_index <- unlist(screening_CA)
NM_index <- unlist(screening_NM)
TX_index <- unlist(screening_TX)
AZ_var <- MSNCODE[AZ_index]
CA_var <- MSNCODE[CA_index]
NM_var <- MSNCODE[NM_index]
TX_var <- MSNCODE[TX_index]
#####

# second try
#####
library(readxl)
library(dplyr)
library(Ball)
library(plyr)
ProblemCData_CRTCB <- read_excel("ProblemCData_CRTCB.xlsx")
Y <- filter(ProblemCData_CRTCB,
              MSN == 'TPOPP' |  # population
              MSN == 'TETCB' |  # total energy consumption
              MSN == 'CRTCB')   # renewable + nuclear total consumption
X <- setdiff(ProblemCData_CRTCB,Y) 
Y <- filter(Y,Year >= 1980)
X <- filter(X,Year >= 1980)
YY <- sapply(1:n_distinct(Y$MSN),function(e){
  temp <- filter(Y,MSN == unique(Y$MSN)[e])
  temp <- temp[,'Data']
  temp
})

XX <- sapply(1:n_distinct(X$MSN),function(e){
  temp <- filter(X,MSN == unique(X$MSN)[e])
  temp <- temp[,'Data']
  temp
})

YY <- data.frame(YY)
XX <- data.frame(XX)
colnames(YY) <- unique(Y$MSN)
colnames(XX) <- unique(X$MSN)


YY_AZ <- YY[1:30,]
YY_CA <- YY[31:60,]
YY_NM <- YY[61:90,]
YY_TX <- YY[91:120,]
XX_AZ <- XX[1:30,]
XX_CA <- XX[31:60,]
XX_NM <- XX[61:90,]
XX_TX <- XX[91:120,]

screening_AZ <- bcorsis(x=XX_AZ,y=YY_AZ,d=20)
screening_CA <- bcorsis(x=XX_CA,y=YY_CA)
screening_NM <- bcorsis(x=XX_NM,y=YY_NM)
screening_TX <- bcorsis(x=XX_TX,y=YY_TX)

MSNCODE <- colnames(XX)
AZ_index <- unlist(screening_AZ)
CA_index <- unlist(screening_CA)
NM_index <- unlist(screening_NM)
TX_index <- unlist(screening_TX)
AZ_var <- MSNCODE[AZ_index]
CA_var <- MSNCODE[CA_index]
NM_var <- MSNCODE[NM_index]
TX_var <- MSNCODE[TX_index]
#####

# for CA mod2
#####
library(readxl)
library(dplyr)
library(Ball)
library(plyr)
ProblemCData_CRTCB <- read_excel("ProblemCData_CRTCB.xlsx")
Y <- filter(ProblemCData_CRTCB,MSN == 'CRTCB')   # renewable + nuclear total consumption
X <- setdiff(ProblemCData_CRTCB,Y) 
Y <- filter(Y,Year >= 1980 & StateCode == 'CA')
X <- filter(X,Year >= 1980 & StateCode == 'CA')

YY <- sapply(1:n_distinct(Y$MSN),function(e){
  temp <- filter(Y,MSN == unique(Y$MSN)[e])
  temp <- temp[,'Data']
  temp
})

XX <- sapply(1:n_distinct(X$MSN),function(e){
  temp <- filter(X,MSN == unique(X$MSN)[e])
  temp <- temp[,'Data']
  temp
})

YY <- data.frame(YY)
XX <- data.frame(XX)
colnames(YY) <- unique(Y$MSN)
colnames(XX) <- unique(X$MSN)

screening_mod2_CA <- bcorsis(x=XX,y=YY,d=20)
MSNCODE <- colnames(XX)
CA_index <- unlist(screening_mod2_CA) # 219 570 186
CA_var <- MSNCODE[CA_index]


#KSCCB WWIXB GOCCB 
X_KSCCB <- XX[,219]
X_WWIXB <- XX[,570]
X_GOCCB <- XX[,186]

YYY <- as.vector(YY)
mod_2_CA_newnew <- lm(YYY~X_KSCCB + X_WWIXB + X_GOCCB)

mod_2_CA_newnewnew <- lm(YYY~X_GOCCB) # 0.7

Year = 1980:2009
mod_2_GOCCB <- lm(X_GOCCB~Year) # 0.8




#####
