# D predict the energy profile
# load data_2 & analysi
#####
ProblemCData_CRTCB <- read_excel("ProblemCData_CRTCB.xlsx")

Y <- filter(ProblemCData_CRTCB,
            MSN == 'TPOPP' |  # population
              MSN == 'TETCB' |  # total energy consumption
              MSN == 'CRTCB')   # renewable + nuclear total consumption


X <- filter(ProblemCData_CRTCB,
            # consumption for energies and sectors
            # NG
            MSN == 'NNACB' | # NG tran
              MSN == 'NNCCB' | # NG comercial 
              MSN == 'NNICB' | # NG industry
              MSN == 'NNRCB' |  # NG residential
              # petro
              MSN == 'PAACB' | 
              MSN == 'PACCB' |  
              MSN == 'PAICB' | 
              MSN == 'PARCB' | 
              # expen
              # NG
              MSN == 'NGACV' | 
              MSN == 'NGCCV' |  
              MSN == 'NGICV' | 
              MSN == 'NGRCV' |
              # petro
              MSN == 'PAACV' | 
              MSN == 'PACCV' |  
              MSN == 'PAICV' | 
              MSN == 'PARCV' )   

Y <- filter(Y,Year >= 1970)
X <- filter(X,Year >= 1970)

Y_distinct <- function(statecode) {
  YY <- sapply(1:n_distinct(Y$MSN),function(e){
    temp <- filter(eval(parse(text = paste0("Y_",statecode))),MSN == unique(Y$MSN)[e])
    temp <- temp[,'Data']
    temp
  })
  YY
}

X_distinct <- function(statecode) {
  XX <- sapply(1:n_distinct(X$MSN),function(e){
    temp <- filter(eval(parse(text = paste0("X_",statecode))),MSN == unique(X$MSN)[e])
    temp <- temp[,'Data']
    temp
  })
  XX
}

for (statecode in c("AZ","CA","NM","TX")) {
  eval(parse(text = paste0('X_',statecode,' <- filter(X,StateCode == ',paste0('"',statecode,'"'),')'))) 
  eval(parse(text = paste0('Y_',statecode,' <- filter(Y,StateCode == ',paste0('"',statecode,'"'),')')))
}

for (statecode in c("AZ","CA","NM","TX")) {
  eval(parse(text = paste0('X_',statecode,'_name <- X_distinct(',paste0('"',statecode,'"'),')'))) 
  eval(parse(text = paste0('Y_',statecode,'_name <- Y_distinct(',paste0('"',statecode,'"'),')')))
}

for (statecode in c("AZ","CA","NM","TX")) {
  eval(parse(text = paste0('X_',statecode,'_name <- data.frame(X_',statecode,'_name)')))
  eval(parse(text = paste0('Y_',statecode,'_name <- data.frame(Y_',statecode,'_name)')))
}

for (statecode in c("AZ","CA","NM","TX")) {
  eval(parse(text = paste0('colnames(X_',statecode,'_name) <- unique(X$MSN)')))
  eval(parse(text = paste0('colnames(Y_',statecode,'_name) <- unique(Y$MSN)')))
}

# for (statecode in c("AZ","CA","NM","TX")) {
#   eval(parse(text = paste0('write.table(X_',statecode,'_name',",'","X_",statecode,'_modelbuilding.csv',"',",'sep="',",","\")")))
#   eval(parse(text = paste0('write.table(Y_',statecode,'_name',",'","Y_",statecode,'_modelbuilding.csv',"',",'sep="',",","\")")))
# }

#####

# model building 


## population
#####
Year <- 1970:2009

## AZ

Y <- Y_AZ_name[,2]
mod_population_AZ <- lm(Y~Year) # R=0.98

## CA

Y <- Y_CA_name[,2]
mod_population_CA <- lm(Y~Year) # R = 0.98

## TX

Y <- Y_TX_name[,2] 
mod_population_TX <- lm(Y~Year) # R = 0.99

## NM

Y <- Y_NM_name[,2]
mod_population_NM <- lm(Y~Year) # R = 0.99
#####



## AZ

Y <- Y_AZ_name[,-2]
X <- X_AZ_name
Y1 <- Y[,1]
Y2 <- Y[,2]
library(MASS)
mod_1_AZ_new <- lm(Y1~X[,9]+X[,16]) # 0.99
mod_2_AZ_new <- lm(Y2~X[,9]+X[,10]) # 可以加入交叉项 提高R方

mod_X9_AZ <- lm(X[,9]~Year) # R=0.94
mod_X16_AZ <- lm(X[,16]~Year+I(Year^2))  # R = 0.65 考虑指数关系 查询后续数据确定
mod_X10_AZ <- lm(X[,10]~Year+I(Year^2))  # R = 0.74 考虑指数关系 查询后续数据确定

## CA

Y <- Y_CA_name[,-2]
X <- X_CA_name
Y1 <- Y[,1]
Y2 <- Y[,2]


mod_1_CA_new <- lm(Y1~X[,9]) # R=0.98
# mod_2_CA_new <- lm(Y2~GOCCB) # 0.7

mod_X9_CA <- lm(X[,9]~Year)  # R = 0.946
JKTCB <- unlist(CA_JKTCB)
YYYY <- 1960:2009
mod_JKTCB <- lm(JKTCB~YYYY)
mod_X3_CA <- lm(X[,3]~Year)
mod_X4_CA <- lm(X[,4]~Year)
# mod_GOCCB_CA <-  lm(GOCCB~Year) # 0.8

## TX

Y <- Y_TX_name[,-2]
X <- X_TX_name
Y1 <- Y[,1]
Y2 <- Y[,2]

mod_1_TX_new <- lm(Y1~X[,4]+X[,9]+X[,13]) # 0.97
mod_2_TX_new <- lm(Y2~X[,4]) # 0.88

mod_X4_TX <- lm(X[,4]~Year) # R = 0.92
mod_X9_TX <- lm(X[,9]~Year)  # R = 0.96
mod_X13_TX <- lm(X[,13]~Year)  # R = 0.8861 

## NM

Y <- Y_NM_name[,-2]
X <- X_NM_name

Y1 <- Y[,1]
Y2 <- Y[,2]

mod_1_NM_new <- lm(Y1~X[,9]) # 0.97
mod_2_NM_new <- lm(Y2~X[,9]+X[,10]) # 0.85

mod_X9_NM <- lm(X[,9]~Year)  # R = 0.87
mod_X10_NM <- lm(X[,10]~Year+I(Year^2)) # R = 0.82

# predict
newyear <- data.frame(c(2025,2050))
colnames(newyear) <- 'Year'
predict.lm(mod_population_AZ,newyear)
predict.lm(mod_population_CA,newyear)
predict.lm(mod_population_NM,newyear)
predict.lm(mod_population_TX,newyear)

predict.lm(mod_X9_AZ,newyear)
predict.lm(mod_X16_AZ,newyear)
predict.lm(mod_X10_AZ,newyear)

predict.lm(mod_X9_CA,newyear)

predict.lm(mod_X9_NM,newyear)
predict.lm(mod_X10_NM,newyear)

predict.lm(mod_X4_TX,newyear)
predict.lm(mod_X9_TX,newyear)
predict.lm(mod_X13_TX,newyear)

AZ_newX <- data.frame(matrix(c(650952.2,224.0283),ncol = 2))
colnames(AZ_newX) <- c('X[, 9]','X[, 16]')
predict.lm(mod_1_AZ_new,AZ_newX)
##           AZ
#            # 2025       # 2050
# P          # 0.452402  # 0.4723146
# Q          # 75.54587   # 79.75629
# Y1         # 1997790    # 2807026
# Y2         # 622282.4   # 900486.5
# population # 8237.146   # 11290.476
# PAACB(X9)  # 650952.2   # 876085.5
# PARCV(X16) # 224.0283   # 497.3537
# PAICB(X13) # 72445   # 80723


##           CA
#            # 2025       # 2050
# P          # 0.1887399  # 0.2097572
# Q          # 33.66994   # 34.60925
# Y1         # 9643963    # 11427588
# Y2         # 1531202    # 1981405
# population # 45476.83   # 57250.74
# PAACB(X9)  # 3930179    # 4868929
# JKTCB      # 844500     # 1139000
# NGICV(X3)  # 7390.7     # 10943.2

##           NM
#            # 2025       # 2050
# P          # 0.06419801 # 0.130102
# Q          # 19.57257   # 35.67529
# Y1         # 792942.3   # 954717.7
# Y2         # 47834.44   # 109911
# population # 2443.953   # 3080.872
# PAACB(X9)  # 233505.7   # 288289.1
# PAACV(X10) # 8187.221   # 17995.202
# PARCV(X16) # 489.5409   # 870.4646

##           TX
#            # 2025       # 2050
# P          # 0.06082602  # 0.07269759
# Q          # 28.73616   # 32.58344
# Y1         # 14754519   # 18204769
# Y2         # 845999.8   # 1233752
# population # 29440.25   # 37864.39
# NGRCV(X4)  # 2985.782   # 4231.769
# PAACB(X9)  # 3460351    # 4458210
# PAICB(X13) # 4000992    # 5163818
# NGICV(X3)  # 12216   # 16588
