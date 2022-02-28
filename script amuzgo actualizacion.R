# Actualizacion de datos del pueblo AMUZGO 
# Autor : Daniela Molina
# Github : @danielalmolina 

# Configuracion
rm(list=ls())

# Paqueterias ----
library(pacman)
p_load(tidyverse,dplyr,readxl,ggplot2)

# Directorio ----
setwd("D:/Users/daniela.molina/Documents/COBERTURA PUEBLOS INDIGENAS 2022/AMUZGO")

# Database ----
loc_amuzgo <- read_excel("AMUZGO LOCALIDADES.xlsx")
im_loc <- read_excel("IML_2020.xlsx")
im_loc2 <- read_excel("IML_2020_2.xlsx")
censo_20 <- read.csv("censo2020.csv")

# Preparacion del censo ----
bd1<- censo_20 %>%
  mutate(POBTOT = as.numeric(POBTOT,na.rm=TRUE),
         TOTHOG = as.numeric(TOTHOG,na.rm=TRUE),
         POBHOG = as.numeric(POBHOG,na.rm=TRUE),
         TVIVPARHAB = as.numeric(TVIVPARHAB,na.rm=TRUE),
         VPH_PC = as.numeric(VPH_PC,na.rm=TRUE),
         VPH_INTER = as.numeric(VPH_INTER,na.rm=TRUE),
         VPH_TELEF = as.numeric(VPH_TELEF,na.rm=TRUE),
         VPH_CEL = as.numeric(VPH_CEL,na.rm=TRUE)
  ) %>%
  group_by(ENT,MUN,LOC)%>%
  summarise(POBTOT = sum(POBTOT,na.rm=TRUE),
            TOTHOG = sum(TOTHOG,na.rm=TRUE),
            POBHOG = sum(POBHOG,na.rm=TRUE),
            TVIVPARHAB = sum(TVIVPARHAB,na.rm=TRUE),
            VPH_PC = sum(VPH_PC,na.rm=TRUE),
            VPH_INTER = sum(VPH_INTER,na.rm=TRUE),
            VPH_TELEF = sum(VPH_TELEF,na.rm=TRUE),
            VPH_CEL = sum(VPH_CEL,na.rm=TRUE)
  )%>%
  ungroup()%>%
  mutate(HOG_CP= (VPH_PC/TVIVPARHAB)*100,
         HOG_INTER= (VPH_INTER/TVIVPARHAB)*100,
         HOG_TELEF= (VPH_TELEF/TVIVPARHAB)*100,
         HOG_CEL= (VPH_CEL/TVIVPARHAB)*100
  )

# Match loc_amuzgo y censo_2020
match1 <- merge(x = bd1,y =loc_amuzgo)

# obtener el promedio de viviendas con celulares


#preparar im_loc
im_loc <- im_loc%>%
  select(ENT,MUN,LOC,GM_2020)%>%
  mutate(ENT =as.numeric(ENT),
         MUN =as.numeric(MUN),
         LOC =as.numeric(LOC))

#Match de im_loc y loc_amuzgo
match2 <- merge(x = im_loc,y =loc_amuzgo)

#preparar im_loc
im_loc2 <- im_loc2%>%
  select(ENT,MUN,LOC,GM_2020)%>%
  mutate(ENT =as.numeric(ENT),
         MUN =as.numeric(MUN),
         LOC =as.numeric(LOC))

#Match de im_loc y loc_amuzgo
match3 <- merge(x = im_loc2,y =loc_amuzgo)

write.csv(match2, "match2.csv")
write.csv(match3, "match3.csv")
