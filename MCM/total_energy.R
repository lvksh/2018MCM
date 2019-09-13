# energy total 
library(readxl)
library(dplyr)
library(ggplot2)
require(Cairo)






# Figure1:different kind of energy
#####
diff_energy <- filter(ProblemCData,
                        MSN == 'PATXB'|
                        MSN == 'CLTXB'|
                        MSN == 'NGTXB'|
                        MSN == 'RETCB'|
                        MSN == 'TETXB')
diff_energy_AZ <- filter(diff_energy,StateCode == 'AZ')
diff_energy_CA <- filter(diff_energy,StateCode == 'CA')
diff_energy_TX <- filter(diff_energy,StateCode == 'TX')
diff_energy_NM <- filter(diff_energy,StateCode == 'NM')

cal_ratio <- function(data) {
  ratio <- vector()
  for (i in 1960:2009) {
    year <- filter(data,Year == i)
    r <- sapply(1:4,function(e){year[e,'Data'] / year[5,'Data']})
    r <- c(r,(year[5,'Data']-sum(year[-5,'Data']))/year[5,'Data'])
    ratio <- cbind(ratio,t(r))
  }
  unlist(ratio)
}

ratio_AZ <- data.frame(cal_ratio(diff_energy_AZ))
ratio_CA <- data.frame(cal_ratio(diff_energy_CA))
ratio_TX <- data.frame(cal_ratio(diff_energy_TX))
ratio_NM <- data.frame(cal_ratio(diff_energy_NM))
ratio_AZ <- cbind(ratio_AZ,rep(1960:2009,each=5),c('coal_ratio','NGas_ratio','petro_ratio','renew_ratio','other'))
ratio_CA <- cbind(ratio_CA,rep(1960:2009,each=5),c('coal_ratio','NGas_ratio','petro_ratio','renew_ratio','other'))
ratio_TX <- cbind(ratio_TX,rep(1960:2009,each=5),c('coal_ratio','NGas_ratio','petro_ratio','renew_ratio','other'))
ratio_NM <- cbind(ratio_NM,rep(1960:2009,each=5),c('coal_ratio','NGas_ratio','petro_ratio','renew_ratio','other'))

colnames(ratio_AZ) <- c('Data','Year','Kind') 
colnames(ratio_CA) <- c('Data','Year','Kind') 
colnames(ratio_TX) <- c('Data','Year','Kind') 
colnames(ratio_NM) <- c('Data','Year','Kind') 

AZ_ratio_plot <- ggplot(data = data.frame(ratio_AZ))+
  geom_point(aes(x=Year,y=Data,colour=Kind))+
  geom_line(aes(x=Year,y=Data,colour=Kind))+
  labs(x = "Year", y = "Ratio")+
  ggtitle("AZ different energy ratio")+
  theme(plot.title = element_text(hjust = 0.5))
CA_ratio_plot <- ggplot(data = data.frame(ratio_CA))+
  geom_point(aes(x=Year,y=Data,colour=Kind))+
  geom_line(aes(x=Year,y=Data,colour=Kind))+
  labs(x = "Year", y = "Ratio")+
  ggtitle("CA different energy ratio")+
  theme(plot.title = element_text(hjust = 0.5))
TX_ratio_plot <- ggplot(data = data.frame(ratio_TX))+
  geom_point(aes(x=Year,y=Data,colour=Kind))+
  geom_line(aes(x=Year,y=Data,colour=Kind))+
  labs(x = "Year", y = "Ratio")+
  ggtitle("TX different energy ratio")+
  theme(plot.title = element_text(hjust = 0.5))
NM_ratio_plot <- ggplot(data = data.frame(ratio_NM))+
  geom_point(aes(x=Year,y=Data,colour=Kind))+
  geom_line(aes(x=Year,y=Data,colour=Kind))+
  labs(x = "Year", y = "Ratio")+
  ggtitle("NM different energy ratio")+
  theme(plot.title = element_text(hjust = 0.5))
print(AZ_ratio_plot)
print(CA_ratio_plot)
print(TX_ratio_plot)
print(NM_ratio_plot)

#####

# Figure2:different sector
#####

total_energy <- read_excel("ProblemCData.xlsx")
foursectors <- filter(total_energy,MSN == 'TEICB' | MSN == 'TERCB' | MSN == 'TECCB' | MSN == 'TEACB')
foursectors_AZ <- filter(foursectors,StateCode == 'AZ')
foursectors_CA <- filter(foursectors,StateCode == 'CA')
foursectors_TX <- filter(foursectors,StateCode == 'TX')
foursectors_NM <- filter(foursectors,StateCode == 'NM')

cal_ratio <- function(data) {
  ratio <- vector()
  for (i in 1960:2009) {
    year <- filter(data,Year == i)
    r <- sapply(1:4,function(e){year[e,'Data'] / sum(year[,'Data'])})
    ratio <- cbind(ratio,t(r))
  }
  unlist(ratio)
}

ratio_AZ <- data.frame(cal_ratio(foursectors_AZ))
ratio_CA <- data.frame(cal_ratio(foursectors_CA))
ratio_TX <- data.frame(cal_ratio(foursectors_TX))
ratio_NM <- data.frame(cal_ratio(foursectors_NM))
ratio_AZ <- cbind(ratio_AZ,rep(1960:2009,each=4),c('TEACB','TECCB','TEICB','TERCB'))
ratio_CA <- cbind(ratio_CA,rep(1960:2009,each=4),c('TEACB','TECCB','TEICB','TERCB'))
ratio_TX <- cbind(ratio_TX,rep(1960:2009,each=4),c('TEACB','TECCB','TEICB','TERCB'))
ratio_NM <- cbind(ratio_NM,rep(1960:2009,each=4),c('TEACB','TECCB','TEICB','TERCB'))
colnames(ratio_AZ) <- c('Data','Year','Kind') 
colnames(ratio_CA) <- c('Data','Year','Kind') 
colnames(ratio_TX) <- c('Data','Year','Kind') 
colnames(ratio_NM) <- c('Data','Year','Kind') 
ratio_AZ <- matrix(ratio_AZ[,1],nrow=4)
ratio_CA <- matrix(ratio_CA[,1],nrow=4)
ratio_TX <- matrix(ratio_TX[,1],nrow=4)
ratio_NM <- matrix(ratio_NM[,1],nrow=4)
write.table(ratio_AZ,'ratio_AZ_sector_rose2.csv',sep=",")
write.table(ratio_CA,'ratio_CA_sector_rose2.csv',sep=",")
write.table(ratio_TX,'ratio_TX_sector_rose2.csv',sep=",")
write.table(ratio_NM,'ratio_NM_sector_rose2.csv',sep=",")

foursector_AZ <- ggplot(data=foursectors_AZ)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  geom_line(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("AZ")+
  theme(plot.title = element_text(hjust = 0.5))
foursector_CA <- ggplot(data=foursectors_CA)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  geom_line(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("CA")+
  theme(plot.title = element_text(hjust = 0.5))
foursector_TX <- ggplot(data=foursectors_TX)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  geom_line(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("TX")+
  theme(plot.title = element_text(hjust = 0.5))
foursector_NM <- ggplot(data=foursectors_NM)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  geom_line(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("NM")+
  theme(plot.title = element_text(hjust = 0.5))

print(foursector_AZ)
print(foursector_CA)
print(foursector_TX)
print(foursector_NM)

#####

# Figure3:expediction
#####
mulplot <- function(data,state) {
    ggplot(data)+
    ggtitle(state)+
    labs(y='Expenditures')+
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA))
}
energy_total_data <- read_excel("能源分类/Energy_Total_Data.xlsx")
total_exp <- filter(energy_total_data,MSN == 'TETXV')
total_exp_AZ <- filter(total_exp,StateCode == 'AZ')
total_exp_CA <- filter(total_exp,StateCode == 'CA')
total_exp_TX <- filter(total_exp,StateCode == 'TX')
total_exp_NM <- filter(total_exp,StateCode == 'NM')
AZ <- mulplot(total_exp_AZ,'Arizona')+geom_point(aes(x=Year,y=Data),color= '#b888c3')+geom_line(aes(x=Year,y=Data),color= '#b888c3')
CA <- mulplot(total_exp_CA,'California')+geom_point(aes(x=Year,y=Data),color= '#ff5c4f')+geom_line(aes(x=Year,y=Data),color= '#ff5c4f')
TX <- mulplot(total_exp_TX,'Texas')+geom_point(aes(x=Year,y=Data),color= '#ff9742')+geom_line(aes(x=Year,y=Data),color= '#ff9742')
NM <- mulplot(total_exp_NM,'New Mexico')+geom_point(aes(x=Year,y=Data),color= '#39c25e')+geom_line(aes(x=Year,y=Data),color= '#39c25e')
ggsave(plot=AZ, filename='Figure3_AZ.eps')
ggsave(plot=CA, filename='Figure3_CA.eps')
ggsave(plot=TX, filename='Figure3_TX.eps')
ggsave(plot=NM, filename='Figure3_NM.eps')
print(AZ)
print(CA)
print(TX)
print(NM)

#####

# Figure4:different states total consumption and production
#####
energy_total_data <- read_excel("能源分类/Energy_Total_Data.xlsx")
total_com_pro <- filter(energy_total_data,MSN == 'TEPRB' | MSN == 'TETCB')
total_com_pro_AZ <- filter(total_com_pro,StateCode == 'AZ')
total_com_pro_CA <- filter(total_com_pro,StateCode == 'CA')
total_com_pro_TX <- filter(total_com_pro,StateCode == 'TX')
total_com_pro_NM <- filter(total_com_pro,StateCode == 'NM')
AZ <- ggplot(data=total_com_pro_AZ)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Energy/(Billion Btu)")+
  guides(color = guide_legend(title = NULL))+
  scale_colour_discrete(labels = c('Total Production','Total Consumption'))+
  theme(legend.position = 'top',
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))+
  ggtitle("Arizona")+
  theme(plot.title = element_text(hjust = 0.5))
CA <- ggplot(data = total_com_pro_CA)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Energy/(Billion Btu)")+
  guides(color = guide_legend(title = NULL))+
  scale_colour_discrete(labels = c('Total Production','Total Consumption'))+
  theme(legend.position = 'top',
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))+
  ggtitle("California")+
  theme(plot.title = element_text(hjust = 0.5))
TX <- ggplot(data = total_com_pro_TX)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Energy/(Billion Btu)")+
  guides(color = guide_legend(title = NULL))+
  scale_colour_discrete(labels = c('Total Production','Total Consumption'))+
  theme(legend.position = 'top',
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))+
  ggtitle("Texas")+
  theme(plot.title = element_text(hjust = 0.5))
NM <- ggplot(data = total_com_pro_NM)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Energy/(Billion Btu)")+
  guides(color = guide_legend(title = NULL))+
  scale_colour_discrete(labels = c('Total Production','Total Consumption'))+
  theme(legend.position = 'top',
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))+
  ggtitle("New mexico")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(plot=AZ, filename='Figure4_AZ.eps')
ggsave(plot=CA, filename='Figure4_CA.eps')
ggsave(plot=TX, filename='Figure4_TX.eps')
ggsave(plot=NM, filename='Figure4_NM.eps')
print(AZ)
print(CA)
print(TX)
print(NM)

#####

# Figure5:Rose
#####
ratio_AZ_rose <- matrix(ratio_AZ[,1],nrow = 5)
colnames(ratio_AZ_rose) <- 1960:2009
ratio_AZ_rose <- ratio_AZ_rose[-5,]
rownames(ratio_AZ_rose) <- c('coal_ratio','NGas_ratio','petro_ratio','renew_ratio')
write.table(ratio_AZ_rose,"ratio_AZ_rose.csv",sep=",")


ratio_CA_rose <- matrix(ratio_CA[,1],nrow = 5)
colnames(ratio_CA_rose) <- 1960:2009
ratio_CA_rose <- ratio_CA_rose[-5,]
rownames(ratio_CA_rose) <- c('coal_ratio','NGas_ratio','petro_ratio','renew_ratio')
write.table(ratio_CA_rose,'ratio_CA_rose.csv',sep=",")

ratio_TX_rose <- matrix(ratio_TX[,1],nrow = 5)
colnames(ratio_TX_rose) <- 1960:2009
ratio_TX_rose <- ratio_TX_rose[-5,]
rownames(ratio_TX_rose) <- c('coal_ratio','NGas_ratio','petro_ratio','renew_ratio')
write.table(ratio_TX_rose,'ratio_TX_rose.csv',sep=",")

ratio_NM_rose <- matrix(ratio_NM[,1],nrow = 5)
colnames(ratio_NM_rose) <- 1960:2009
ratio_NM_rose <- ratio_NM_rose[-5,]
rownames(ratio_NM_rose) <- c('coal_ratio','NGas_ratio','petro_ratio','renew_ratio')
write.table(ratio_NM_rose,'ratio_NM_rose.csv',sep=",")
#####

# renew bar
#####
data_con <- filter(ProblemCData,
                 MSN == 'RETCB' | # renew consumption
                 MSN == 'TETCB' ) # total consumption
data_pro <- filter(ProblemCData,                 
                 MSN == 'TEPRB' | # total production
                 MSN == 'REPRB' ) # renew production
data_con_CA <- filter(data_con,StateCode=='CA')
data_con_AZ <- filter(data_con,StateCode=='AZ')
data_con_TX <- filter(data_con,StateCode=='TX')
data_con_NM <- filter(data_con,StateCode=='NM')
data_pro_CA <- filter(data_pro,StateCode=='CA')
data_pro_AZ <- filter(data_pro,StateCode=='AZ')
data_pro_TX <- filter(data_pro,StateCode=='TX')
data_pro_NM <- filter(data_pro,StateCode=='NM')

consumption_bar_CA <-ggplot(data_con_NM,aes(x=Year,y=Data,fill=MSN))+geom_bar(stat = "identity",position="fill")
consumption_bar_CA
#####

# electry consumption 
#####
ele_con_data <- filter(ProblemCData,MSN == 'ESTCB')
ele_con_plot <- ggplot(data=ele_con_data)+
  geom_point(aes(x=Year,y=Data,colour=StateCode))+
  geom_line(aes(x=Year,y=Data,colour=StateCode))+
  labs(x = "Year", y = "Electrical Consumption/Billion Btu")+
  ggtitle("Electrical Consumption of four states")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(plot=ele_con_plot, filename='Figure2.eps')


ele_loss_data <- filter(ProblemCData,MSN == 'LOTCB')
ele_loss_plot <- ggplot(data=ele_loss_data)+
  geom_point(aes(x=Year,y=Data,colour=StateCode))+
  geom_line(aes(x=Year,y=Data,colour=StateCode))+
  labs(x = "Year", y = "Electrical Losses/Billion Btu")+
  ggtitle("Electrical Losses of four states")+
  theme(plot.title = element_text(hjust = 0.5))
#####

# some plots about X in different states against time
#####
ProblemCData_CRTCB <- read_excel("ProblemCData_CRTCB.xlsx")
Y <- filter(ProblemCData_CRTCB,MSN == 'TPOPP' |  MSN == 'TETCB' |  MSN == 'CRTCB')   
X <- filter(ProblemCData_CRTCB,
              MSN == 'PAACB' | 
              MSN == 'PAICB' | 
              MSN == 'NGRCV' |
              MSN == 'PAACV' | 
              MSN == 'PARCV' |
              MSN == 'GOCCB') 
Y <- filter(Y,Year >= 1970)
X <- filter(X,Year >= 1970 & Data != 0)

Year <- 1970:2009

# CA
X <- ProblemCData_CRTCB
X_CA_NGICV <- filter(X,MSN == 'NGICV')
X_CA_NGICV <- filter(X_CA_NGICV,StateCode == 'CA')

plot_CA_NGICV <- ggplot(data = X_AZ_PAACB)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('California NGICV')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")
ggsave(plot=plot_CA_NGICV, filename='plot_CA_NGICV.eps')

X_CA_GOCCB <- filter(X,MSN == 'GOCCB')
X_CA_GOCCB <- filter(X_CA_GOCCB,StateCode == 'CA')

plot_CA_GOCCB <- ggplot(data = X_CA_GOCCB)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('California GOCCB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

X_CA_PAACB <- filter(X,MSN == 'PAACB')
X_CA_PAACB <- filter(X_CA_PAACB,StateCode == 'CA')

plot_CA_PAACB <- ggplot(data = X_CA_PAACB)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('California PAACB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

CA_JKTCB <- CA_JKTCB[,1]
year = 1960:2009

plot_CA_JKTCB <- ggplot(data = CA_JKTCB)+
  geom_point(aes(x=year,y=JKTCB))+
  geom_smooth(aes(x=year,y=JKTCB),method='lm',se=FALSE)+
  ggtitle('California JKTCB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

ggsave(plot=plot_CA_JKTCB, filename='plot_CA_JKTCB.eps')
ggsave(plot=plot_CA_GOCCB, filename='plot_CA_GOCCB.eps')
ggsave(plot=plot_CA_PAACB, filename='plot_CA_PAACB.eps')
# AZ

X <- ProblemCData_CRTCB
X_AZ_PAACB <- filter(X,MSN == 'PAACB')
X_AZ_PAACB <- filter(X_AZ_PAACB,StateCode == 'AZ')

plot_AZ_PAACB <- ggplot(data = X_AZ_PAACB)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('Arizona PAACB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

X_AZ_PAACV <- filter(X,MSN == 'PAACV')
X_AZ_PAACV <- filter(X_AZ_PAACV,StateCode == 'AZ')

plot_AZ_PAACV <- ggplot(data = X_AZ_PAACV)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('Arizona PAACV')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

X_AZ_PAICB <- filter(X,MSN == 'PAICB')
X_AZ_PAICB <- filter(X_AZ_PAICB,StateCode == 'AZ')

plot_AZ_PAICB <- ggplot(data = X_AZ_PAACB)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('Arizona PAICB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")
ggsave(plot=plot_AZ_PAACV, filename='plot_AZ_PAACV.eps')
ggsave(plot=plot_AZ_PAICB, filename='plot_AZ_PAICB.eps')
ggsave(plot=plot_AZ_PAACB, filename='plot_AZ_PAACB.eps')

-----
X_AZ_PAACB <- filter(X,MSN == 'PAACB')
X_AZ_PAACB <- filter(X_AZ_PAACB,StateCode == 'AZ')

plot_AZ_PAACB <- ggplot(data = X_AZ_PAACB)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('Arizona GOCCB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

X_AZ_PARCV <- filter(X,MSN == 'PARCV')
X_AZ_PARCV <- filter(X_AZ_PARCV,StateCode == 'AZ')

plot_AZ_PARCV <- ggplot(data = X_AZ_PARCV)+ # 有点问题！！ 要改二次
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',formula = y ~ x + I(x^2),se=FALSE)+
  ggtitle('Arizona PAACB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

X_AZ_PAACV  <- filter(X,MSN == 'PAACV')
X_AZ_PAACV  <- filter(X_AZ_PAACV ,StateCode == 'AZ')

plot_AZ_PAACV  <- ggplot(data = X_AZ_PAACV)+ 
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',formula = y ~ x + I(x^2),se=FALSE)+
  ggtitle('Arizona PAACB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

ggsave(plot=plot_AZ_PAACB, filename='plot_AZ_PAACB.eps')
ggsave(plot=plot_AZ_PARCV, filename='plot_AZ_PARCV.eps')
ggsave(plot=plot_AZ_PAACV, filename='plot_AZ_PAACV.eps')
# NM

X_NM_PAACB <- filter(X,MSN == 'PAACB')
X_NM_PAACB <- filter(X_NM_PAACB,StateCode == 'NM')

plot_NM_PAACB <- ggplot(data = X_NM_PAACB)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('New Mexico PAACB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

X_NM_PAACV <- filter(X,MSN == 'PAACV')
X_NM_PAACV <- filter(X_NM_PAACV,StateCode == 'NM')

plot_NM_PAACV <- ggplot(data = X_NM_PAACV)+  
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',formula = y ~ x + I(x^2),se=FALSE)+
  ggtitle('New Mexico PAACV')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

ggsave(plot=plot_NM_PAACB, filename='plot_NM_PAACB.eps')
ggsave(plot=plot_NM_PAACV, filename='plot_NM_PAACV.eps')
# TX

X_TX_NGRCV <- filter(X,MSN == 'NGRCV')
X_TX_NGRCV <- filter(X_TX_NGRCV,StateCode == 'TX')

plot_TX_NGRCV <- ggplot(data = X_TX_NGRCV)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('Texas NGRCV')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

X_TX_PAACB <- filter(X,MSN == 'PAACB')
X_TX_PAACB <- filter(X_TX_PAACB,StateCode == 'TX')

plot_TX_PAACB <- ggplot(data = X_TX_PAACB)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('Texas PAACB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

X_TX_PAICB <- filter(X,MSN == 'PAICB')
X_TX_PAICB <- filter(X_TX_PAICB,StateCode == 'TX')

plot_TX_PAICB <- ggplot(data = X_TX_PAICB)+
  geom_point(aes(x=Year,y=Data))+
  geom_smooth(aes(x=Year,y=Data),method='lm',se=FALSE)+
  ggtitle('Texas PAICB')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Energy/Billion Btu")

ggsave(plot=plot_TX_NGRCV, filename='plot_TX_NGRCV.eps')
ggsave(plot=plot_TX_PAACB, filename='plot_TX_PAACB.eps')
ggsave(plot=plot_TX_PAICB, filename='plot_TX_PAICB.eps')
#####

# CRTCB against year
#####
ProblemCData_CRTCB <- read_excel("ProblemCData_CRTCB.xlsx")
CRTCB <- filter(ProblemCData_CRTCB,MSN == 'CRTCB')
CRTCB_CA <- filter(CRTCB,StateCode == 'CA')
CRTCB_AZ <- filter(CRTCB,StateCode == 'AZ')
CRTCB_NM <- filter(CRTCB,StateCode == 'NM')
CRTCB_TX <- filter(CRTCB,StateCode == 'TX')

CRTCB_plot <- ggplot(CRTCB)+
  geom_point(aes(x=Year,y=Data,colour = StateCode))+
  geom_line(aes(x=Year,y=Data,colour = StateCode))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("CRTCB of 4 states")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(plot=CRTCB_plot, filename='CRTCB_plot.eps')

CRTCB_CA_plot <- ggplot(CRTCB_CA)+
  geom_point(aes(x=Year,y=Data),colour = '#b888c3')+
  geom_smooth(aes(x=Year,y=Data),method = 'glm',formula = y~x+I(x^2))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("Canifornia")+
  theme(plot.title = element_text(hjust = 0.5))

CRTCB_AZ_plot <- ggplot(CRTCB_AZ)+
  geom_point(aes(x=Year,y=Data),colour = '#ff5c4f')+
  geom_smooth(aes(x=Year,y=Data),method = 'glm',formula = y~x+I(x^2))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("Arizona")+
  theme(plot.title = element_text(hjust = 0.5))

CRTCB_NM_plot <- ggplot(CRTCB_NM)+
  geom_point(aes(x=Year,y=Data),colour = '#ff9742')+
  geom_smooth(aes(x=Year,y=Data),method = 'loess')+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("New Mexico")+
  theme(plot.title = element_text(hjust = 0.5))

CRTCB_TX_plot <- ggplot(CRTCB_TX)+
  geom_point(aes(x=Year,y=Data),colour = '#39c25e')+
  geom_smooth(aes(x=Year,y=Data),method = 'loess')+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("Texas")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(plot=CRTCB_CA_plot, filename='CRTCB_CA_plot.eps')
ggsave(plot=CRTCB_AZ_plot, filename='CRTCB_AZ_plot.eps')
ggsave(plot=CRTCB_NM_plot, filename='CRTCB_NM_plot.eps')
ggsave(plot=CRTCB_TX_plot, filename='CRTCB_TX_plot.eps')
#####

