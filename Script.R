rm(list=ls())
library(MASS)
library(sp)
library(spdep)
library(splm)
library(plyr)
library(rgdal)
library(foreign)
library(spData)
library(rgeos)
library(plyr)
library(rgdal)
library(rJava)
library(ggplot2)
library(raster)
library(extrafont)
library(ggplot2)
library(ggthemes)
library(stringr)
library(exactextractr)
library(terra)
library(here)
library(dplyr)
library(rstudioapi)

# Getting the path of the current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

here::i_am(path="Script.R") #I set the relative path to the project directory

# 1
A_harv=raster(here("data/SPAM_2005_v3.2/","SPAM2005V3r2_global_H_TA_WHEA_A.tif")) # harvested area in hectares
Y=raster(here("data/SPAM_2005_v3.2/","SPAM2005V3r2_global_Y_TA_WHEA_A.tif")) #Yield in kg/ha

A_harv_r=rast(A_harv)
Y_r=rast(Y)

P_r=(A_harv_r*(Y_r/1000))/10^6 #production in million tons

writeRaster(P_r, here("results","P_wheat_Mt.tif"))



#2
adm0=readOGR(here("data/GAUL", "g2015_2005_0.shp"))
adm0=st_as_sf(adm0)


p_countries=exact_extract(P_r,adm0,fun="sum")


data_p=as.data.frame(cbind(adm0$ADM0_NAME,as.vector(p_countries)))
names(data_p)=c("Country","P_Mt")
write.csv2(data_p, file=here("results","P_wheat_Countries.csv"))

#3
N_y=0.02*Y_r
writeRaster(N_y, here("results","N_yield_kg_ha.tif"))


png(file=here("results","Plot_N_yield_kg_ha.png"), units="in", width=12, height=8, res=300)
plot(N_y, main=expression( 'N'[Yield] ~' ' ~bgroup("[",frac(kg,ha), "]" )))
dev.off()



#4

NUE=read.csv2(here("data/NUE_Zhang_et_al_2015", "Country_NUE_assumption.csv"))
Ny_countries=exact_extract(N_y,adm0,fun="mean")

data_Ny=as.data.frame(cbind(adm0$ADM0_NAME,as.vector(Ny_countries)))
names(data_Ny)=c("Country","Ny_kg_ha")

data_N=as.data.frame(merge(merge(NUE, data_p,by.x="Country", by.y="Country"),data_Ny, by.x="Country", by.y="Country"))

data_N$NUE=as.numeric(data_N$NUE)
data_N$Ny_kg_ha=as.numeric(data_N$Ny_kg_ha)
data_N$P_Mt =as.numeric(data_N$P_Mt )

data_N=data_N%>%mutate(Ninput_kg_ha=Ny_kg_ha/NUE)%>%mutate(Nsur_kg_ha=Ninput_kg_ha-Ny_kg_ha)


data_N_ord=data_N[order(data_N$P_Mt, decreasing = T),]

data_N_10=data_N_ord[c(1:10),]

write.csv2(data_N_10, file=here("results","N_10Countries.csv"), row.names = F)

data_plot=data_N_10[,c(1,4,5,6)]
names(data_plot)=c("Country","output","input","losses")

data_plot=data_plot%>%tidyr::gather("N", "kg_ha", 2:4)

p<-ggplot(data_plot, aes(x=Country, y=kg_ha, group=N)) +
  geom_line(aes(color=N),size=1)+
  geom_point(aes(color=N),size=1.5) + scale_color_brewer(palette="Paired")+
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))


png(file=here("results","Plot_N_10Countries.png"), units="in", width=8, height=8, res=300)
p
dev.off()




