################
#   Reads superCensus file in 1M rows bits,
#   by using the fields types to cut the megafile in 8 pieces


library(sqldf)
rm(list=ls())     #reset

#path to superCensus dataset
ruta <-'V:/Studies/MOVED/HealthImpact/Data/Census_OD_DfT2.0/CensusODflow_by_LSOA'

fichero <-paste0(ruta,"WM12EWCT0489_lsoa.csv")

#first 100 lines
flow.lsoa <- read.csv(fichero, stringsAsFactors=FALSE, header=T, nrows=100)

#field types
flow.lsoa.colclass <- sapply(flow.lsoa,class)

#apply type to read whole file
flow.lsoa <- read.csv(fichero,sep = ",",nrows = 3000000, stringsAsFactors=FALSE, 
                      header=T,colClasses=flow.lsoa.colclass,comment.char ="",quote = "" )

#select just Manchester
flow.lsoa <- read.table(file = fichero ,sep = ",",quote = "",
                        colClasses = flow.lsoa.colclass,comment.char ="",
                        stringsAsFactors = F,header=T,colClasses=flow.lsoa.colclass,pipe('grep "Manchester" "WM12EWCT0489_lsoa.csv" ') )
 

#saving w. a loop
for (i in (1:8)) {
  
  flow.lsoa <- read.csv(fichero,header=T, sep = ",",nrows = 1000000, skip=(i-1)*1000000, stringsAsFactors=FALSE, 
  colClasses=flow.lsoa.colclass,comment.char ="",quote = "" )
  
  if (i==1) {cols <- colnames(flow.lsoa)}
  colnames(flow.lsoa) <-cols
  
#write.csv(flow.lsoa,file=paste("C:/temp/test/","flow",i,'.csv',sep=''),row.names=F) 
  saveRDS(flow.lsoa,file =paste0("C:/temp/Census.File.split/","flow",i,'.Rds') )
  message(i, ' done')
  
  rm(flow.lsoa)        
}

setwd("C:/temp/Census.File.split/")
#read each file and select ONLY rows where flow originates in MANCHESTER
for (i in (1:8)) {

#flow <- read.csv(file=paste('flow',i,'.csv',sep=''),header=T,stringsAsFactors=FALSE, colClasses=flow.lsoa.colclass)
flow <- readRDS (file=paste0('flow',i,'.Rds'))   
target <- grep(pattern='Manchester',cbind(flow$Area.Name,flow$Area.of.Workplace.name))
x=length(target)
message(x, ' lines')
##flowManch <-flow[target,]
#write.csv(flowManch, file=paste('flow',i,'_Manch','.csv',sep=''),row.names = F)
#saveRDS(paste0('flow',i,'_Manch','.Rds'))
rm(flowManch, flow)
                }