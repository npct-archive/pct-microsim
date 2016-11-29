#########################
##  gets the flows from GM
##  & aggregates them to create 1 file [127,627 x 253 var]
##  reads ethnicity file
##  merges w. flows file

rm(list=ls())
library(dplyr)

path='./0-sources/'
ficheros = list.files(path, pattern ='.Rds')

#lsoa<>msoa lookup file
msoas = read.csv(file.path(path,'lkp_LSOA_MSOA.csv'),header = T,as.is = T)
msoas.gm = read.csv(file.path(path,'msoas.gm.csv'),header = T, as.is = T)


flow.gm = data.frame(stringsAsFactors = F)


# 1) add the MSOA matching  every LSOA (origin, then destination)
# 2) builds 1 large flow file where MSOA orig + destination are in G.M.
for (i in 1:length(ficheros)) {
    
    flow <- readRDS(paste0(path,ficheros[i]))
    flow = inner_join(flow, msoas[, c(1,3,4)], by=c('Area.of.usual.residence' = 'LSOA11CD'))
    
    flow = rename(.data = flow, lsoa1 = Area.of.usual.residence ,
                                                 lsoa1.name = Area.Name, 
                                                 msoa1 =  MSOA11CD  , 
                                                 msoa1.name = MSOA11NM)
    
    
    flow = inner_join(flow, msoas[, c(1,3,4)], by=c('Area.of.Workplace' = 'LSOA11CD'))
    flow = rename(.data = flow, lsoa2 = Area.of.Workplace,
                                                 lsoa2.name = Area.of.Workplace.name, 
                                                 msoa2 =  MSOA11CD  , 
                                                 msoa2.name = MSOA11NM)
    
    
    flow = flow[, c(257:260, 1:255)]     #reorder cols to have msoas first
    
    #keep flows only from  G.M. area (only origin OR origin+destination)
    flow = subset(x = flow,subset = msoa1 %in% msoas.gm$msoa1)
    
    ifelse( i==1, yes = (flow.gm = flow), no = (flow.gm=rbind(flow.gm, flow))   )    
    
    cat(ficheros[i], ' done | ')
    
    rm(flow)                }
                           

#aggregate by msoa orig-dest => get desired GM flow file
flow.gm <-aggregate(flow.gm[, c(9:259)],by=list(flow.gm$msoa1,flow.gm$msoa2), FUN=sum,na.rm=T)
flow.gm = rename(.data =flow.gm, msoa1 = Group.1, msoa2=Group.2 )


flow.eth = read.csv('./0-sources/wu08cew_msoa_v1.csv', header = F, as.is = T) 

names(flow.eth) = c('msoa1', 'msoa2', 'All', 'WhiteEnglish', 'WhiteIrish',
                    'WhiteGypsyorIrishTraveller', 'WhiteOther', 'Mixed.W&Bl.Caribbean', 
                    'Mixed.W&Bl.African', 'Mixed.WhiteandAsian', 'Mixed.Other',
                    'AsianBritishIndian', 'AsianBritishPakistani', 'AsianBritishBangladeshi',
                    'AsianBritishChinese', 'AsianBritishOther', 'BlackBritishAfrican',
                    'BlackBritishCaribbean', 'BlackBritishOtherBlack', 'OtherethnicgroupArab',
                    'Otherethnicgroup')



#add ethnicity columns to flows file, now ready for saving

flow.gm = inner_join(flow.gm, flow.eth, by=c("msoa1"="msoa1", "msoa2"="msoa2"))
saveRDS(flow.gm, file.path('./2-output/','flow.gm.Rds'))    #flows in G.M., MSOA-level
