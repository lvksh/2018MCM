# 灵敏度分析

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

# AZ
# Y1 : PRESS : 27527580451  SSE : 24208194049
# Y2 : PRESS : 225347199092 SSE:  195387643205
Y <- Y_AZ_name[,-2]
X <- X_AZ_name

Y1 <- Y[,1]
Y2 <- Y[,2]

X9 <- X[,9]
X16 <- X[,16]
X13 <- X[,13]

p <- numeric(40)
for (i in 1:40){
  mod <- lm(Y1[-i]~X9[-i]+X16[-i])
  co <- mod$coefficients
  Yh <- co[1]*1+co[2]*X9[i]+co[3]*X16[i]
  p[i] <- Y1[i]-Yh
}
sum(p^2)

p <- numeric(40)
for (i in 1:40){
  mod <- lm(Y2[-i]~X9[-i]+X13[-i])
  co <- mod$coefficients
  Yh <- co[1]*1+co[2]*X9[i]+co[3]*X13[i]
  p[i] <- Y2[i]-Yh
}
sum(p^2)

# CA
# Y1 : PRESS : 9.78881890542e+11  SSE : 8.94406597838e+11
# Y2 : PRESS : 5.15478e+11 SSE:  9.1466691981e+10
Y <- Y_CA_name[,-2]
X <- X_CA_name

Y1 <- Y[,1]
Y2 <- Y[,2]

X9 <- X[,9]
# JKTCB
X3 <- X[,3]

p <- numeric(40)
for (i in 1:40){
  mod <- lm(Y1[-i]~X9[-i])
  co <- mod$coefficients
  Yh <- co[1]*1+co[2]*X9[i]
  p[i] <- Y1[i]-Yh
}
sum(p^2)

p <- numeric(40)
for (i in 1:40){
  mod <- lm(Y2[-i]~JKTCB[-i]+X3[-i])
  co <- mod$coefficients
  Yh <- co[1]*1+co[2]*JKTCB[i]+co[3]*X3[i]
  p[i] <- Y2[i]-Yh
}
sum(p^2)

# NM
# Y1 : PRESS : 61755441738  SSE : 56942150649
# Y2 : PRESS : 480666650    SSE:  388610135
Y <- Y_NM_name[,-2]
X <- X_NM_name

Y1 <- Y[,1]
Y2 <- Y[,2]

X9 <- X[,9]
X10 <- X[,10]
X16 <- X[,16]

p <- numeric(40)
for (i in 1:40){
  mod <- lm(Y1[-i]~X9[-i])
  co <- mod$coefficients
  Yh <- co[1]*1+co[2]*X9[i]
  p[i] <- Y1[i]-Yh
}
sum(p^2)

p <- numeric(40)
for (i in 1:40){
  mod <- lm(Y2[-i]~X10[-i]+X16[-i])
  co <- mod$coefficients
  Yh <- co[1]*1+co[2]*X10[i]+co[3]*X16[i]
  p[i] <- Y2[i]-Yh
}
sum(p^2)

# TX
# Y1 : PRESS : 2.250407e+14  SSE : 2.906097e+12
# Y2 : PRESS : 724282174263    SSE:  616417874230
Y <- Y_TX_name[,-2]
X <- X_TX_name

Y1 <- Y[,1]
Y2 <- Y[,2]

X9 <- X[,9]
X4 <- X[,4]
X3 <- X[,3]
X13 <- X[,13]

p <- numeric(40)
for (i in 1:40){
  mod <- lm(Y1[-i]~X4[-i]+X9[-i]+X13[-i])
  co <- mod$coefficients
  Yh <- co[1]*1+co[2]*X4[i]+co[3]*X9[-i]+co[4]*X13[i]
  p[i] <- Y1[i]-Yh
}
sum(p^2)

p <- numeric(40)
for (i in 1:40){
  mod <- lm(Y2[-i]~X4[-i]+X3[-i])
  co <- mod$coefficients
  Yh <- co[1]*1+co[2]*X4[i]+co[3]*X3[i]
  p[i] <- Y2[i]-Yh
}
sum(p^2)