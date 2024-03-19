library(raster)
library(sf)
library(stars)
library(dplyr)

rm(list = ls())
#SBS---------------
sbs<-read_sf('input_data/sbs_area.fgb')
zone<-read_sf('input_data/sbs_area.fgb')
plot(zone)

file_list<-list.files(path='input_data/gfw_trawler',full.names = T)
i=1

df<-list()
for(i in 1:length(file_list)){
  print(file_list[i])
  r<-raster(file_list[i])
  r<-crop(r,sbs)
  zone.r<-rasterize(zone,r)
  # plot(r)
  h<-data.frame(zonal(r,zone.r,fun='sum'))
  h$file<-file_list[i]
  df[[i]] <- h

  rm(r,h)  
}
df_gfw_sbs<-data.frame(do.call(rbind, df))
df_gfw_sbs<-df_gfw_sbs%>%transmute(sbs_hours=sum,
                           file=file)

rm(df)
save(df_gfw_sbs, file='output_data/gfw_sbs.Rda')
#Albardao-----------------
zone<-read_sf('input_data/albardao.fgb')
plot(zone)

df<-list()
for(i in 1:length(file_list)){
  print(file_list[i])
  r<-raster(file_list[i])
  r<-crop(r,sbs)
  zone.r<-rasterize(zone,r)
  # plot(r)
  h<-data.frame(zonal(r,zone.r,fun='sum'))
  h$file<-file_list[i]
  df[[i]] <- h
  
  rm(r,h)  
}
df_gfw_alb<-data.frame(do.call(rbind, df))
df_gfw_alb<-df_gfw_alb%>%transmute(uc_hours=sum,
                           file=file)

rm(df)

#Eólicos------------
zone<-read_sf('input_data/eolicos.fgb')
zone<-st_union(zone)%>%st_cast('POLYGON')
plot(zone)

library(fasterize)

plot(zone.r)
df<-list()
for(i in 1:length(file_list)){
  print(file_list[i])
  r<-raster(file_list[i])
  r<-crop(r,sbs)
  zone.r<-fasterize(zone,r)
  # plot(r)
  h<-data.frame(zonal(r,zone.r,fun='sum'))
  h$file<-file_list[i]
  df[[i]] <- h
  
  rm(r,h)  
}
df_gfw_eol<-data.frame(do.call(rbind, df))
df_gfw_eol<-df_gfw_eol%>%transmute(eol_hours=sum,
                           file=file)


#12 mn SC------------
zone<-read_sf('input_data/mar_territorial_sc.shp')
plot(zone)


df<-list()
for(i in 1:length(file_list)){
  print(file_list[i])
  r<-raster(file_list[i])
  r<-crop(r,zone)
  zone.r<-fasterize(zone,r)
  # plot(r)
  h<-data.frame(zonal(r,zone.r,fun='sum'))
  h$file<-file_list[i]
  df[[i]] <- h
  
  rm(r,h)  
}
df_gfw_sc<-data.frame(do.call(rbind, df))
df_gfw_sc<-df_gfw_sc%>%transmute(sc_hours=sum,
                           file=file)

save(df_gfw_sc, file='output_data/gfw_sc.Rda')

#Merge all---------

df<-merge(df_gfw_sbs,merge(df_gfw_alb,df_gfw_eol,by='file'),by='file')
df<-df%>%mutate(year=as.numeric(substring(df$file,44,47)),
                season=as.numeric(substring(df$file,41,42)),
                uc_percentage=uc_hours/sbs_hours,
                eol_percentage=eol_hours/sbs_hours)
df$season<-factor(df$season, levels=c(1,2,3,4),
                  labels=c('Summer','Autumn','Winter','Spring'))
write.csv(df,'output_data/trawler_seasonal_sbs.csv')
save(df_gfw_sc,df_gfw_eol,df_gfw_alb,df_gfw_sbs,file='output_data/gfw_all.Rda')

#Exploratory graphs----------
library(ggplot2)
library(tidyr)
df$season<-factor(df$season, levels=c(1,2,3,4),
                  labels=c('Summer','Autumn','Winter','Spring'))
df$year<-factor(df$year)
df.m<-df%>%select(-alb_percentage,-eol_percentage)%>%pivot_longer(cols = c('sbs_hours','alb_hours','eol_hours'))

