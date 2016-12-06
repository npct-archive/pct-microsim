
######################
######### BUILDS 4-VARIABLE dataset (age-sex-mode-ethnicity)
######### using OPTIMAL PROBABILITY allocation model

rm(list=ls())
library(dplyr)
library(stringr)
source('functions.R')

#get flows file (ethnicity IS included)
flow.gm = readRDS('./2-output/flow.gm.Rds')
names(flow.gm)

#rename cols., add W/NonW, etc
flow.gm = dplyr::rename(flow.gm, Total = AllMethods_AllSexes_Age16Plus,
                        Total.ethn = All      )

flow.gm = arrange(flow.gm, Total, msoa1, msoa2)   #sort by increasing Total
#right.order = read.csv('./0-sources/right_order.csv', header=T, as.is = T)

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
agesexmode =  names(flow.gm)[5:(ncol(flow.gm)-2)]     # 131 age-sex-mode categories
ethnicity = c('White','Non.Wh')

#create the SP types
types = merge(x = agesexmode, y = ethnicity)
types = paste0(types$x,'_', types$y)

#create vector of probabilities for all SP types
vprob = data.frame(matrix(data = 0, nrow = 1,ncol = length(types))  )
names(vprob) = sort(types)

# select 'univocal' flows
sum(flow.gm$Total==flow.gm$Total.ethn)==nrow(flow.gm)  #must be TRUE, otherwise next
#commands could give wrong results

#separates flows into: univocal / probabilistic
sel = which( (flow.gm$White==0  | flow.gm$Non.Wh==0)   )   #| (sel =sum(flow.gm[,c(5:136) !=0])==1 )
sel1 = which( (flow.gm$White!=0  & flow.gm$Non.Wh!=0))
# sel+sel1 MUST cover all flow.gm rows=127,627
    
# allmodes= names(flow.gm)[5:136]
# allethnics = names(flow.gm)[137:138]

#DF to hold the SP rebuilt population
flow.sp = as.data.frame(matrix(0,nrow = nrow(flow.gm), ncol = length(types)))
names(flow.sp) = sort(types)


################## REBUILDING POPULATION
######### STAGE 1: GET PROB. VECTOR per TYPE  +  POPULATE THEM (PERFECT REBUILD)

for (i in sel)  {

            singleflow = flow.gm[i,]          #flow categories
            
            colsnotnull = selColumns(singleflow)    #selects candidate cols. in singleflow
            singleflow = singleflow[, colsnotnull]   #add nos. to candidates cols. 
            
            #separate modes cols. from ethics cols.
            ethnics= colsnotnull[colsnotnull %in% c('White', 'Non.Wh')]
            modes  = colsnotnull[ !(colsnotnull %in% ethnics) ]    
            
            #build candidates using modes/ethnics
            candidates = merge(x=modes, y=ethnics)
            candidates = paste0(candidates$x,'_', candidates$y )
            
            for (j in 1:length(modes))  {
                candidate = grep(pattern = modes[j],x = candidates, value = T)    
                flow.sp[i,candidate ] = flow.sp[i , candidate] + flow.gm[i, modes[j]]   }
    
            if (i %% 10000==0)  { cat(i,' rows processed \n')}         
                }

cat('STAGE 1: completed....')    #all univocal flows rebuilt
saveRDS(flow.sp, file.choose())

#################
# some types w still have prob=0 => will never come out !!
# Replace by 1s to make them possible in dataset
vprob[vprob==0] = 1

######### STAGE 2: REBUILD FLOWS WITH MULTIPLE INDIVIDUALS (PROBABILISTIC APPROACH)

for (i in sel1)    {
    
    singleflow = flow.gm[i,]          #flow categories
    nopeople   = singleflow$Total     # no. indiv to allocate
    
    colsnotnull = selColumns(singleflow)    #selects candidate cols. in singleflow
    singleflow = singleflow[, colsnotnull]   #add nos. to candidates cols. 
    
    #separate modes cols. from ethics cols.
    ethnics= colsnotnull[colsnotnull %in% c('White', 'Non.Wh')]
    modes  = colsnotnull[ !(colsnotnull %in% ethnics) ]    
    
    #build candidates using modes/ethnics
    candidates = merge(x=modes, y=ethnics)
    candidates = paste0(candidates$x,'_', candidates$y )

    subflow = sort(singleflow)   #sort from min to max. Use subflow in loop
        
  while (sum(subflow)>0)    {  #loop through types while there are people to allocate
        
        colsnotnull=selColumns(subflow)
        subflow = subflow[colsnotnull]
        subflow = sort(subflow)
        
        ethnics= colsnotnull[colsnotnull %in% c('White', 'Non.Wh')]
        modes  = colsnotnull[ !(colsnotnull %in% ethnics) ]    
        
        #build candidates using modes/ethnics
        candidates = merge(x=modes, y=ethnics)
        candidates = paste0(candidates$x,'_', candidates$y )
        
        #
        subtype = names(subflow)[1]      #type targetted
        n = 1         #no people in category
        
        #find sub-candidates
        subcandidates = grep(pattern = subtype, candidates, value = T)
        
        
        if (length(subcandidates)==1) {
            flow.sp = updateResults(subcandidates, n)  #SP matrix allocation
            subflow = updateSubflow(subcandidates, n)  # subflow figures update
            
                
        } else {      #multiple options, use multinomial
            
            allocated = allocateMulti(subcandidates)        #allocates & updates subflow    
            flow.sp = updateResults(allocated, 1)
            subflow = updateSubflow(allocated, 1)
            }

    }   #end while
   
    if (i %% 1000==0)  { 
        cat('Row no ', i,' processed \n')  }     
     
}

cat('STAGE 2: completed....')    #all probabilistic flows rebuilt

##### CHECKS on flow.sp
sum(rowSums(flow.sp)==flow.gm$Total)  # must be 127,627

row.correct= which(rowSums(flow.sp)==flow.gm$Total)  # both in sel/sel1
row.defect = which(rowSums(flow.sp) < flow.gm$Total) # all in sel
row.excess = which(rowSums(flow.sp) > flow.gm$Total) #all in sel

saveRDS(flow.sp, './2-output/flow.sp.Rds')


