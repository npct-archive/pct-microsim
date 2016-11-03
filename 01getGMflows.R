
rm(list=ls())

path='./0-sources/'
ficheros = list.files(path, pattern ='.Rds')
msoas = read.csv(file.path(path,'lkp_LSOA_MSOA.csv'),header = T,as.is = T)
msoas.gm = readRDS(file.choose())
msoas.gm =msoas.gm@data[, c(1,2)]

#builds flows file where MSOA orig + destination are in G.M.
for (i in 1:length(ficheros)) {
    
    flow <- readRDS(paste0(path,ficheros[i]))
    flow = dplyr::inner_join(flow, msoas[, c(1,3,4)], by=c('Area.of.usual.residence' = 'LSOA11CD'))
    
    flow = dplyr::rename(.data = flow, lsoa1 = Area.of.usual.residence ,
                                                 lsoa1.name = Area.Name, 
                                                 msoa1 =  MSOA11CD  , 
                                                 msoa1.name = MSOA11NM)
    
    
    flow = inner_join(flow, msoas[, c(1,3,4)], by=c('Area.of.Workplace' = 'LSOA11CD'))
    flow = dplyr::rename(.data = flow, lsoa2 = Area.of.Workplace,
                                                 lsoa2.name = Area.of.Workplace.name, 
                                                 msoa2 =  MSOA11CD  , 
                                                 msoa2.name = MSOA11NM)
    
    
    flow = flow[, c(257:260, 1:255)]
    
    #keep flows only from  G.M. area 
    flow = subset(x = flow,subset = msoa1 %in% msoas.gm$geo_code)
    
    if (i==1)   { flow.gm = flow
    }   else {flow.gm=rbind(flow.gm, flow)  }
    
    cat(ficheros[i], ' done')
    
    rm(flow)        
}

flow.gm <-aggregate(flow.gm[, c(9:259)],by=list(flow.gm$msoa1,flow.gm$msoa2), FUN=sum,na.rm=T)
flow.gm = dplyr::rename(.data =flow.gm1, msoa1 = Group.1, msoa2=Group.2 )
saveRDS(flow.gm, file.path(path,'flow.gm.Rds'))    #flows in G.M., MSOA-level
