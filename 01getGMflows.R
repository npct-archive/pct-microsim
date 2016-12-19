#########################
##  gets the flows from GM
##  & aggregates them to create 1 file [127,627 x 253 var]
##  reads ethnicity/car ownsership files
##  merges w. flows file

rm(list=ls())
library(dplyr)

path='./0-sources/'
ficheros = list.files(path, pattern ='.Rds')

# read:  1) lsoa<>msoa lookup file + 2) list of msoas in G.M.
msoas = read.csv(file.path(path,'lkp_LSOA_MSOA.csv'),header = T,as.is = T)
msoas.gm = read.csv(file.path(path,'msoas.gm.csv'),header = T, as.is = T)

flow.gm = data.frame(stringsAsFactors = F)


# 1) read the 8 .Rds files
# 2) add the MSOA matching  every LSOA (origin, then destination)
# 3) builds 1 large flow file where MSOA orig + destination are in G.M.
for (i in 1:length(ficheros)) {
    
    flow <- readRDS(paste0(path,ficheros[i]))
    flow = inner_join(flow, msoas[, c('LSOA11CD','MSOA11CD','MSOA11NM')], 
                      by=c('Area.of.usual.residence' = 'LSOA11CD'))
    
    flow = rename(.data = flow, lsoa1 = Area.of.usual.residence ,
                                                 lsoa1.name = Area.Name, 
                                                 msoa1 =  MSOA11CD  , 
                                                 msoa1.name = MSOA11NM)
    
    
    flow = inner_join(flow, msoas[, c('LSOA11CD','MSOA11CD','MSOA11NM')], 
                      by=c('Area.of.Workplace' = 'LSOA11CD'))
    
    flow = rename(.data = flow, lsoa2 = Area.of.Workplace,
                                                 lsoa2.name = Area.of.Workplace.name, 
                                                 msoa2 =  MSOA11CD  , 
                                                 msoa2.name = MSOA11NM)
    
    
    flow = flow[, c(257:260, 1:256)]     #reorder cols to have msoas first
    
    #keep flows only from G.M. area (origin must be in G.M.)
    flow = subset(x = flow, subset = msoa1 %in% msoas.gm$msoa1)
    
    ifelse( i==1, yes = (flow.gm = flow), no = (flow.gm=rbind(flow.gm, flow))   )    
    
    cat(ficheros[i], ' done | ')
    
    rm(flow)                }


#delete totals columns
flow.gm = dplyr::rename(flow.gm, Total = AllMethods_AllSexes_Age16Plus)


dropcols= grep(pattern = 'All', names(flow.gm),value = T)
flow.gm = flow.gm[, !(names(flow.gm) %in% dropcols)]

#delete 16plus columns
dropcols= grep(pattern = '16Plus', names(flow.gm),value = T)
flow.gm = flow.gm[, !(names(flow.gm) %in% dropcols)]


#aggregate by msoa orig-dest => get desired GM flow file
flow.gm <-aggregate(flow.gm[, c(9:ncol(flow.gm))],by=list(flow.gm$msoa1,flow.gm$msoa2), FUN=sum,na.rm=T)
flow.gm = rename(.data =flow.gm, msoa1 = Group.1, msoa2=Group.2 )


##read ETHNICITY
flow.eth = read.csv('./0-sources/wu08cew_msoa_v1.csv', header = F, as.is = T) 

names(flow.eth) = c('msoa1', 'msoa2', 'All', 'WhiteEnglish', 'WhiteIrish',
                    'WhiteGypsyorIrishTraveller', 'WhiteOther', 'Mixed.W&Bl.Caribbean', 
                    'Mixed.W&Bl.African', 'Mixed.WhiteandAsian', 'Mixed.Other',
                    'AsianBritishIndian', 'AsianBritishPakistani', 'AsianBritishBangladeshi',
                    'AsianBritishChinese', 'AsianBritishOther', 'BlackBritishAfrican',
                    'BlackBritishCaribbean', 'BlackBritishOtherBlack', 'OtherethnicgroupArab',
                    'Otherethnicgroup')


##read CAR OWNERSHIP
flow.car = read.csv('./0-sources/wu09auk_msoa_v1.csv', header = F, as.is = T) 
cols=c('msoa1','msoa2','All','car0' ,'car1','car2','car3')
names(flow.car)=cols


#create ethnicity/car ownership flows files
flow.gm1 = inner_join(flow.gm, flow.eth, by=c("msoa1"="msoa1", "msoa2"="msoa2"))
flow.gm2 = inner_join(flow.gm, flow.car, by=c("msoa1"="msoa1", "msoa2"="msoa2"))


saveRDS(flow.gm1, file.path('./2-output/','flow.gm1.Rds'))    #flows in G.M., MSOA-level
saveRDS(flow.gm2, file.path('./2-output/','flow.gm2.Rds'))    #flows in G.M., MSOA-level

rm(list=ls())
