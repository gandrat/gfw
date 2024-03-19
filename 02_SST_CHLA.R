## Environmental Variables


library(raster)
library(sf)
library(stars)
library(dplyr)

rm(list = ls())
#SBS---------------
sbs<-read_sf('input_data/sbs_area.fgb')
zone<-read_sf('input_data/sbs_area.fgb')
plot(zone)


##SST---------------
file_list<-list.files(path='input_data/sst',full.names = T)
i=1


df<-list()
for(i in 1:length(file_list)){
  print(file_list[i])
  r<-raster(file_list[i])
  r<-crop(r,sbs)
  r<-r/100
  zone.r<-rasterize(zone,r)
  # plot(r)
  h<-data.frame(file_list[i])
  h$sst_mean<-data.frame(zonal(r,zone.r,fun='mean'))
  h$sst_max<-data.frame(zonal(r,zone.r,fun='max'))
  h$sst_min<-data.frame(zonal(r,zone.r,fun='min'))
  h$sst_sd<-data.frame(zonal(r,zone.r,fun='sd'))
  df[[i]] <- h
  
  rm(r,h)  
}
df2<-data.frame(do.call(rbind, df))
df2<-df2%>%mutate(year=as.integer(substring(file_list.i.,24,27)),
                           season=substring(file_list.i.,29,30))
df_sst_sbs<-df2%>%transmute(year=year,
                           season=season,
                           sst_min=sst_min$min,
                           sst_max=sst_max$max,
                           sst_mean=sst_mean$mean,
                           sst_std=sst_sd$sd)

rm(df)

##ChlA-----------

file_list<-list.files(path='input_data/chl',full.names = T)
i=1


df<-list()
for(i in 1:length(file_list)){
  print(i)
  print(file_list[i])
  r<-raster(file_list[i])
  r<-crop(r,sbs)
  zone.r<-rasterize(zone,r)
  #plot(r)
  h<-data.frame(file_list[i])
  h$chl_mean<-data.frame(zonal(r,zone.r,fun='mean'))
  h$chl_max<-data.frame(zonal(r,zone.r,fun='max'))
  h$chl_min<-data.frame(zonal(r,zone.r,fun='min'))
  h$chl_sd<-data.frame(zonal(r,zone.r,fun='sd'))
  df[[i]] <- h
  
  rm(r,h)  
}
df2<-data.frame(do.call(rbind, df))
df2<-df2%>%mutate(year=as.integer(substring(file_list.i.,30,33)),
                        season=substring(file_list.i.,35,36))
df_chl_sbs<-df2%>%transmute(year=year,
                           season=season,
                           chl_min=chl_min$min,
                           chl_max=chl_max$max,
                           chl_mean=chl_mean$mean,
                           chl_std=chl_sd$sd)



#Mar territorial SC---------------

zone<-read_sf('input_data/mar_territorial_sc.shp')
plot(zone)


##SST---------------
file_list<-list.files(path='input_data/sst',full.names = T)
i=1


df<-list()
for(i in 1:length(file_list)){
  print(file_list[i])
  r<-raster(file_list[i])
  r<-crop(r,sbs)
  r<-r/100
  zone.r<-rasterize(zone,r)
  plot(r)
  h<-data.frame(file_list[i])
  h$sst_mean<-data.frame(zonal(r,zone.r,fun='mean'))
  h$sst_max<-data.frame(zonal(r,zone.r,fun='max'))
  h$sst_min<-data.frame(zonal(r,zone.r,fun='min'))
  h$sst_sd<-data.frame(zonal(r,zone.r,fun='sd'))
  df[[i]] <- h
  
  rm(r,h)  
}
df2<-data.frame(do.call(rbind, df))
df2<-df2%>%mutate(year=as.integer(substring(file_list.i.,24,27)),
                        season=substring(file_list.i.,29,30))
df_sst_sc<-df2%>%transmute(year=year,
                           season=season,
                           sst_min=sst_min$min,
                           sst_max=sst_max$max,
                           sst_mean=sst_mean$mean,
                           sst_std=sst_sd$sd)

rm(df)

##ChlA-----------

file_list<-list.files(path='input_data/chl',full.names = T)
i=1


df<-list()
for(i in 1:length(file_list)){
  print(i)
  print(file_list[i])
  r<-raster(file_list[i])
  r<-crop(r,sbs)
  zone.r<-rasterize(zone,r)
  # plot(r)
  h<-data.frame(file_list[i])
  h$chl_mean<-data.frame(zonal(r,zone.r,fun='mean'))
  h$chl_max<-data.frame(zonal(r,zone.r,fun='max'))
  h$chl_min<-data.frame(zonal(r,zone.r,fun='min'))
  h$chl_sd<-data.frame(zonal(r,zone.r,fun='sd'))
  df[[i]] <- h
  
  rm(r,h)  
}
df2<-data.frame(do.call(rbind, df))
df2<-df2%>%mutate(year=as.integer(substring(file_list.i.,30,33)),
                  season=substring(file_list.i.,35,36))
df_chl_sc<-df2%>%transmute(year=year,
                           season=season,
                           chl_min=chl_min$min,
                           chl_max=chl_max$max,
                           chl_mean=chl_mean$mean,
                           chl_std=chl_sd$sd)


# Salvando dados----------
df_sbs<-merge(df_sst_sbs,df_chl_sbs,by=c('year','season'))
df_sc<-merge(df_sst_sc,df_chl_sc,by=c('year','season'))

save(df_sc,df_sbs,file='output_data/sst_chl.Rda')

#Mesclando todas as variáveis em uma planilha-----------
library(dplyr)
rm(list = ls())

load('output_data/sst_chl.Rda')
load('output_data/gfw_all.Rda')



df_gfw_sc<-df_gfw_sc%>%transmute(year=as.numeric(substring(file,44,47)),
                                 season=paste0('s',substring(file,42,42)),
                                 trawler_hours=sc_hours)

df_gfw_sbs<-df_gfw_sbs%>%transmute(year=as.numeric(substring(file,44,47)),
                                 season=paste0('s',substring(file,42,42)),
                                 trawler_hours=sbs_hours)

df_sbs_all<-df_gfw_sbs%>%left_join(df_sbs, by=c('year','season'))
df_sc_all<-df_gfw_sc%>%left_join(df_sc, by=c('year','season'))

write.csv(df_sbs_all,file='output_data/gfw_sst_chl_sbs.csv')                
write.csv(df_sc_all,file='output_data/gfw_sst_chl_sc.csv')                
