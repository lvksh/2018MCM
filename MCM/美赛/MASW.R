## AZ

Y <- Y_AZ_name[,-2]
X <- X_AZ_name

Y1 <- Y[,1]
Y2 <- Y[,2]

mod1 <- lm(Y1~X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16])
mod2 <- lm(Y2~X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16])

mod_1_AZ <- lm(Y1~X[,9]+X[,13]+X[,15]+X[,16])
mod_2_AZ <- lm(Y2~X[,9]+X[,10])

Year = 1970:2009
mod_X9_AZ <- lm(X[,9]~Year) # R=0.94
mod_X13_AZ <- lm(X[,13]~Year) # R = 0.3 可能需要丢弃1980前数据 可查询后续数据确定是否为Sin关系
mod_X15_AZ <- lm(X[,15]~Year+I(Year^2)) # R = 0.29 图中大概为二次关系
plot(Year,X[,15])
mod_X16_AZ <- lm(X[,16]~Year)  # R = 0.65 考虑指数关系 查询后续数据确定
mod_X10_AZ <- lm(X[,10]~Year)  # R = 0.74 考虑指数关系 查询后续数据确定
plot(Year,X[,10])

## CA

Y <- Y_CA_name[,-2]
X <- X_CA_name

Y1 <- Y[,1]
Y2 <- Y[,2]
mod_1_CA <- lm(Y1~X[,3]+X[,4]+X[,6]+X[,7]+X[,9]+X[,11]+X[,12]+X[,13]+X[,14])
mod_2_CA <- lm(Y2~X[,13]) 

mod_X3_CA <- lm(X[,3]~Year+I(Year^2)) # R=0.86 考虑指数关系
mod_X4_CA <- lm(X[,4]~Year) # R = 0.917
mod_X6_CA <- lm(X[,6]~Year)  # R = 0.04 考虑sin或扔掉
plot(Year,X[,6])
mod_X7_CA <- lm(X[,7]~Year+I(Year^2)) # R = 0.54 类似二次关系 查询后续数据确定
plot(Year,X[,7])
mod_X9_CA <- lm(X[,9]~Year)  # R = 0.946
mod_X11_CA <- lm(X[,11]~Year) # R = 0.64 考虑后续二次关系 需要查询后续数据 
mod_X12_CA <- lm(X[,12]~Year) # R = 0.08 考虑sin关系 需要查询后续数据
plot(Year,X[,12])
mod_X13_CA <- lm(X[,13]~Year+I(Year^2)) # R = 0.3 考虑sin或二次 需要查询后续数据 
plot(Year,X[,13])
mod_X14_CA <- lm(X[,14]~Year+I(Year^2)+I(Year^3)) # R = 0.8 

## TX

Y <- Y_TX_name[,-2]
X <- X_TX_name

Y1 <- Y[,1]
Y2 <- Y[,2]

mod1 <- lm(Y1~X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16])
mod2 <- lm(Y2~X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16])

mod_1_TX <- lm(Y1~X[,1]+X[,4]+X[,7]+X[,9]+X[,13])
mod_2_TX <- lm(Y2~X[,3]+X[,4]+X[,5]+X[,7]+X[,11]+X[,12])

mod_X1_TX <- lm(X[,1]~Year) # R=0.56 考虑丢弃零数据 查询后续数据确定是指数关系还是sqrt(X)关系
plot(Year,X[,1])
mod_X4_TX <- lm(X[,4]~Year+I(Year^2)+I(Year^3)) # R = 0.92
mod_X7_TX <- lm(X[,7]~Year) # R = 0.18 图中大概为sin关系 查询后续数据确定
mod_X9_TX <- lm(X[,9]~Year)  # R = 0.96
mod_X13_TX <- lm(X[,13]~Year)  # R = 0.8861 波动上升 预测效果可能不太好
mod_X3_TX <- lm(X[,3]~Year) # R = 0.75 考虑指数关系 
mod_X5_TX <- lm(X[,5]~Year) # 毫无关系 考虑丢掉
plot(Year,X[,5])
mod_X11_TX <- lm(X[,11]~Year) # R = 0.3 关系复杂 可以查询后续数据 或扔掉 
plot(Year,X[,11])
mod_X12_TX <- lm(X[,12]~Year) # R = 0.04 考虑丢掉80年前数据 和查询后续数据
plot(Year,X[,12])

## NM

Y <- Y_NM_name[,-2]
X <- X_NM_name

Y1 <- Y[,1]
Y2 <- Y[,2]

mod1 <- lm(Y1~X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16])
mod2 <- lm(Y2~X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16])

mod_1_NM <- lm(Y1~X[,3]+X[,4]+X[,5]+X[,7]+X[,9]+X[,13])
mod_2_NM <- lm(Y2~X[,3]+X[,5]+X[,9]+X[,10])

mod_X3_NM <- lm(X[,3]~Year) # R=0.11  删去90年前数据 查询后续数据
plot(Year,X[,1])
mod_X4_NM <- lm(X[,4]~Year) # R = 0.918
mod_X5_NM <- lm(X[,5]~Year+I(Year^2)) # R = 0.3168  查询后续数据确定是sin还是二次
plot(Year,X[,5])
mod_X7_NM <- lm(X[,7]~Year+I(Year^2))  # R = 0.6829 查询后续数据确定是sin还是二次
plot(Year,X[,7])
mod_X9_NM <- lm(X[,9]~Year)  # R = 0.87
mod_X13_NM <- lm(X[,13]~Year) # R = 0.05 考虑sin或扔掉
plot(Year,X[,13])
mod_X10_NM <- lm(X[,10]~Year+I(Year^2)) # R = 0.82 考虑指数关系