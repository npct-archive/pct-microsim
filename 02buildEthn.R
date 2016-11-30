
######################
######### BUILDS 4-VARIABLE dataset (age-sex-mode-ethnicity)
######### using OPTIMAL PROBABILITY 

rm(list=ls())
library(dplyr)

#get flows file (ethnicity IS included)
flow.gm = readRDS('./2-output/flow.gm.Rds')
names(flow.gm)

#rename cols., add W/NonW, etc
flow.gm = dplyr::rename(flow.gm, Total = AllMethods_AllSexes_Age16Plus,
                        Total.ethn = All      )

flow.gm = flow.gm[, c(1:3,254,4:253,255:272)]

flow.gm$White = flow.gm[,"WhiteEnglish"]  +  flow.gm[,"WhiteIrish"]
                +flow.gm[,"WhiteGypsyorIrishTraveller"]+  flow.gm[, "WhiteOther" ]

flow.gm$Non.White <- flow.gm$Total.ethn  - flow.gm$White

                 
             
#delete repeated columns
dropcols= c("WhiteEnglish", "WhiteIrish",   "WhiteGypsyorIrishTraveller"  ,
            "WhiteOther" ,  "Mixed.W&Bl.Caribbean",  "Mixed.W&Bl.African", 
            "Mixed.WhiteandAsian", "Mixed.Other" , "AsianBritishIndian", 
            "AsianBritishPakistani", "AsianBritishBangladeshi" ,     "AsianBritishChinese",  
            "AsianBritishOther",   "BlackBritishAfrican", "BlackBritishCaribbean",       
            "BlackBritishOtherBlack",       "OtherethnicgroupArab",  "Otherethnicgroup"  )

flow.gm = flow.gm[, !(names(flow.gm) %in% dropcols)]

dropcols= grep(pattern = 'All', names(flow.gm),value = T)

flow.gm = flow.gm[, !(names(flow.gm) %in% dropcols)]

dropcols= grep(pattern = '16Plus', names(flow.gm),value = T)
flow.gm = flow.gm[, !(names(flow.gm) %in% dropcols)]

#check: 
sum(colSums(flow.gm[5:135]))    #all mode colums     
sum(colSums(flow.gm[136:137]))  # both must be similar, ~1,030,716


### arrange and prepare for prob. vector
flow.gm = arrange(flow.gm, Total,Total.ethn, msoa1, msoa2)
agesexmode.types =  names(flow.gm)[5:135]     # 131 'proper' categories
ethnicity = c('White','Non.White')

#create the SP types
types= merge(x = agesexmode.types, y = ethnicity)
types= paste0(types$x,'_', types$y)

#create vector of probabilities for all SP types
vprob = data.frame(matrix(data = 0, nrow = 1,ncol = length(types))  )
names(vprob) = types


sel = which(flow.gm$Total==1  & flow.gm$Total.ethn==1 & flow.gm$Total==flow.gm$Total.ethn)

#calculate correlations based on 1-individual flows
# CAUTION: we need to remove rows where modes DON'T MATCH totals. E.g row 45931
for (i in (45932:length(sel)))  {
            singleflow = flow.gm[i, c(1:ncol(flow.gm))]                
            notnull = which(singleflow !=0)
            colsnotnull = names(flow.gm)[notnull]
            colsnotnull = colsnotnull[! colsnotnull %in% c('msoa1', 'msoa2', 'Total','Total.ethn')]
            
            sp.type = paste0(colsnotnull[1],'_', colsnotnull[2])
            vprob[[sp.type]] = vprob[[sp.type]]+ 1
                       
                }

#################
# some types w prob=0, will never come out. Replace prob=0 by general probability in dataset
# using colSums (for these types) in total -unfiltered-  flow.gm


#save vprob -apply to rest of flows w. >1 individuals



#df to hold the SP rebuilt population
flow.sp = as.data.frame(matrix(0,nrow = nrow(flow.gm), ncol = length(types)))
names(flow.sp) = types

################## REBUILDING POPULATION

######### STAGE 1: REBUILD FLOWS WITH 1 INDIVIDUAL (PERFECT REBUILD)
sel = which(flow.gm$Total==1  & flow.gm$Total.ethn==1 & flow.gm$Total==flow.gm$Total.ethn)

for (i in 1:length(sel)) {
    
    singleflow = flow.gm[i,]
    notnull = which(singleflow !=0)
    notnull = notnull[notnull>4]
    colsnotnull = names(flow.gm)[notnull]
    
    sp.type= paste0(colsnotnull[1],'_', colsnotnull[2])
    flow.sp[i, sp.type ]   = 1
}

cat('Flows stage 1 completed....')    #all flows w. 1-person rebuilt

######### STAGE 2: REBUILD FLOWS WITH MULTIPLE INDIVIDUALS (PROBABILISTIC APPROACH)

for (i in length(sel)+1:nrow(flow.gm)) {
    
    singleflow = flow.gm[i,]          #flow categories
    
    nopeople   = flow.gm[i, 'Total']  #individuals to allocate
    
    notnull = which(singleflow !=0)
    notnull = notnull[notnull>4]
    notnull.mode = notnull[notnull<135]
    notnull.ethn = notnull[notnull>135]
    
    
    cols.mode = names(flow.gm)[notnull.mode]
    cols.ethn = names(flow.gm)[notnull.ethn]
    types = merge(x = cols.mode, y = cols.ethn)    
    
    types= paste0(types$x,'_', types$y) #types w. possible positives
    
    if (length(cols.mode)<=nopeople) {
            flow.sp[i, types] = 1
            nopeople   = nopeople - sum(flow.sp[i, types])   #remaining allocation
                                }
    
    if (nopeople >0) {     #continues allocation
        vprob.target = vprob[, (names(vprob) %in% types) ]
        alloc= rmultinom(n=1,size=nopeople,prob=vprob.target) 
                     }  # allocate randomly
    
    
}

cat('Flows stage 1 completed....')




