
######################
######### BUILDS 4-VARIABLE dataset (age-sex-mode-ethnicity)
######### using OPTIMAL PROBABILITY allocation model

rm(list=ls())
library(dplyr)
library(stringr)

#get flows file (ethnicity IS included)
flow.gm = readRDS('./2-output/flow.gm.Rds')
names(flow.gm)

#rename cols., add W/NonW, etc
flow.gm = dplyr::rename(flow.gm, Total = AllMethods_AllSexes_Age16Plus,
                        Total.ethn = All      )

flow.gm = arrange(flow.gm, Total, msoa1,msoa2)
right.order = read.csv('./0-sources/right_order.csv', header=T, as.is = T)

flow.gm = flow.gm[, c(1:3,255,4:254,256:273)]#flow.gm = flow.gm[, right.order]

flow.gm$White = flow.gm[,"WhiteEnglish"]  +  flow.gm[,"WhiteIrish"]
                +flow.gm[,"WhiteGypsyorIrishTraveller"]+  flow.gm[, "WhiteOther" ]

flow.gm$Non.Wh <- flow.gm$Total.ethn  - flow.gm$White

                 
             
#delete any other ethnicity columns
dropcols= c("WhiteEnglish", "WhiteIrish",   "WhiteGypsyorIrishTraveller"  ,
            "WhiteOther" ,  "Mixed.W&Bl.Caribbean",  "Mixed.W&Bl.African", 
            "Mixed.WhiteandAsian", "Mixed.Other" , "AsianBritishIndian", 
            "AsianBritishPakistani", "AsianBritishBangladeshi" ,     "AsianBritishChinese",  
            "AsianBritishOther",   "BlackBritishAfrican", "BlackBritishCaribbean",       
            "BlackBritishOtherBlack",       "OtherethnicgroupArab",  "Otherethnicgroup"  )

flow.gm = flow.gm[, !(names(flow.gm) %in% dropcols)]

#delete totals columns
dropcols= grep(pattern = 'All', names(flow.gm),value = T)

flow.gm = flow.gm[, !(names(flow.gm) %in% dropcols)]

#delete 16plus columns
dropcols= grep(pattern = '16Plus', names(flow.gm),value = T)
flow.gm = flow.gm[, !(names(flow.gm) %in% dropcols)]

#check: 
sum(colSums(flow.gm[5:136]))    # all mode colums     
sum(colSums(flow.gm[137:138]))  # both must be similar, ~1,030,716


### arrange and prepare for prob. vector
flow.gm = arrange(flow.gm, Total,Total.ethn, msoa1, msoa2)
agesexmode =  names(flow.gm)[5:(ncol(flow.gm)-2)]     # 131 'proper' categories
ethnicity = c('White','Non.Wh')

#create the SP types
types = merge(x = agesexmode, y = ethnicity)
types = paste0(types$x,'_', types$y)

#create vector of probabilities for all SP types
vprob = data.frame(matrix(data = 0, nrow = 1,ncol = length(types))  )
names(vprob) = types

# select 'univocal' flows
sel = which( (flow.gm$White==0  | flow.gm$Non.Wh==0) & flow.gm$Total==flow.gm$Total.ethn)
sel1 = which( (flow.gm$White!=0  & flow.gm$Non.Wh!=0) & flow.gm$Total==flow.gm$Total.ethn)
# sel+sel1 cover all flow.gm rows
    
allmodes= names(flow.gm)[5:136]
allethnics = names(flow.gm)[137:138]


#df to hold the SP rebuilt population
flow.sp = as.data.frame(matrix(0,nrow = nrow(flow.gm), ncol = length(types)))
names(flow.sp) = types


################## REBUILDING POPULATION
######### STAGE 1: GET PROB. VECTOR per TYPE  +  POPULATE THEM (PERFECT REBUILD)

for (i in sel)  {
            singleflow = flow.gm[i, c(1:ncol(flow.gm))]                
            notnull = which(singleflow !=0)
            colsnotnull = names(flow.gm)[notnull]
            colsnotnull = colsnotnull[! colsnotnull %in% c('msoa1', 'msoa2', 'Total','Total.ethn')]
            
            ethnics= colsnotnull[colsnotnull %in% c('White', 'Non.Wh')]
            modes  = colsnotnull[ !(colsnotnull %in% ethnics) ]    
            
            typesnn = merge(x=modes, y=ethnics)
            typesnn = paste0(typesnn$x,'_', typesnn$y )
            
            vprob[typesnn] = vprob[typesnn]+ flow.gm[i, modes]
            flow.sp[i,typesnn] = flow.sp[i , typesnn] + flow.gm[i, modes]
                       
                }

cat('STAGE 1: completed....')    #all flows w. 1-person rebuilt
saveRDS(flow.sp, file.choose())

#################
# some types w still have prob=0 => will never come out.
# Replace prob=0 by general probability in dataset
# using colSums (for these types) in total -unfiltered-  flow.gm


#save vprob -apply to rest of flows (non-trivial flows)

######### STAGE 2: REBUILD FLOWS WITH MULTIPLE INDIVIDUALS (PROBABILISTIC APPROACH)

for (i in sel1) {
    
    singleflow = flow.gm[i,]          #flow categories
    nopeople   = flow.gm[i, 'Total']  #individuals to allocate
    
    colsnotnull = selColumns(singleflow)    #selects relevant cols. in singleflow
    singleflow = singleflow[, colsnotnull]   #select potential candidates
    
    ethnics= colsnotnull[colsnotnull %in% c('White', 'Non.Wh')]
    modes  = colsnotnull[ !(colsnotnull %in% ethnics) ]    
    
    #build candidates type
    candidates = merge(x=modes, y=ethnics)
    candidates = paste0(typesnn$x,'_', typesnn$y )

    subflow= sort(singleflow)   #sort before allocating
        
    for (j in 1:length(subflow))  {
        
        subtype = names(subflow)[j]      #type targetted
        n = subflow[[ subtype ]]  #no people in category
        
        #find candidates
        subcandidates = grep(pattern = subtype, candidates, value = T)
        
        
        if (length(subcandidates)==1) {
            flow.sp[i, target] = flow.sp[i, target] + nopeople.categ
            updateType (target)
            
                
        } else {
            vprob.subflow = vprob[subcandidates]   #reduce prob. vector to possible types
            
            
            updateType()
            }
        
        
    }
    
}

cat('Flows stage 1 completed....')




