library(readxl)
library(dplyr)
library(ggplot2)
require(grid)
petroleum <- read_excel("能源分类/Petroleum_Data.xlsx")
petro_total_enduse <- filter(petroleum,MSN == 'PATXB') 
petro_total_enduse_AZ <- filter(petro_total_enduse,StateCode == 'AZ')
petro_total_enduse_CA <- filter(petro_total_enduse,StateCode == 'CA')
petro_total_enduse_TX <- filter(petro_total_enduse,StateCode == 'TX')
petro_total_enduse_NM <- filter(petro_total_enduse,StateCode == 'NM')

# petroleum total end-use for four states
#####   
AZ <- ggplot(data = petro_total_enduse_AZ)+
  geom_point(aes(x=Year,y=Data))+
  labs(x = "Year", y = "PATXB/(Billion Btu)")+
  ggtitle("AZ")+
  theme(plot.title = element_text(hjust = 0.5))
CA <- ggplot(data = petro_total_enduse_CA)+
  geom_point(aes(x=Year,y=Data))+
  labs(x = "Year", y = "PATXB/(Billion Btu)")+
  ggtitle("CA")+
  theme(plot.title = element_text(hjust = 0.5))
TX <- ggplot(data = petro_total_enduse_TX)+
  geom_point(aes(x=Year,y=Data))+
  labs(x = "Year", y = "PATXB/(Billion Btu)")+
  ggtitle("TX")+
  theme(plot.title = element_text(hjust = 0.5))
NM <- ggplot(data = petro_total_enduse_NM)+
  geom_point(aes(x=Year,y=Data))+
  labs(x = "Year", y = "PATXB/(Billion Btu)")+
  ggtitle("NM")+
  theme(plot.title = element_text(hjust = 0.5))
#####

# petroleum different sector
#####
petro_foursectors <- filter(petroleum,MSN == 'PAICB' | MSN == 'PARCB' | MSN == 'PACCB' | MSN == 'PAACB')
petro_foursectors_AZ <- filter(petro_foursectors,StateCode == 'AZ')
petro_foursectors_CA <- filter(petro_foursectors,StateCode == 'CA')
petro_foursectors_TX <- filter(petro_foursectors,StateCode == 'TX')
petro_foursectors_NM <- filter(petro_foursectors,StateCode == 'NM')


foursector_AZ <- ggplot(data=petro_foursectors_AZ)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("AZ")+
  theme(plot.title = element_text(hjust = 0.5))
foursector_CA <- ggplot(data=petro_foursectors_CA)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("CA")+
  theme(plot.title = element_text(hjust = 0.5))
foursector_TX <- ggplot(data=petro_foursectors_TX)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("TX")+
  theme(plot.title = element_text(hjust = 0.5))
foursector_NM <- ggplot(data=petro_foursectors_NM)+
  geom_point(aes(x=Year,y=Data,colour=MSN))+
  labs(x = "Year", y = "Consumption/Billion Btu")+
  ggtitle("NM")+
  theme(plot.title = element_text(hjust = 0.5))

  
petro_industral <- filter(petroleum,MSN == 'PAICB')
petro_residential <- filter(petroleum,MSN == 'PARCB')
petro_commercial <- filter(petroleum,MSN == 'PACCB')
petro_transportation <- filter(petroleum,MSN == 'PAACB')
#####



# ggplot(data = petro_total_enduse)+geom_point(aes(x=Year,y=Data,colour=StateCode))
# grid.newpage()  
# pushViewport(viewport(layout = grid.layout(2,2))) 
# vplayout <- function(x,y){
#   viewport(layout.pos.row = x, layout.pos.col = y)
# }
# print(AZ, vp = vplayout(1,1))   
# print(CA, vp = vplayout(1,2))   
# print(TX, vp = vplayout(2,1))  
# print(NM, vp = vplayout(2,2))
#dev.off()