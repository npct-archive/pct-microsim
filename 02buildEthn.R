
######################
######### BUILDS 4-VARIABLE dataset (age-sex-mode-ethnicity)
######### using OPTIMAL PROBABILITY allocation model

rm(list=ls())
library(dplyr)
library(stringr)
source('functions.R')

#read FLOWS G.M. + ETHNICITY flows=flow.gm1
flow.gm1 = readRDS('./2-output/flow.gm1.Rds')
names(flow.gm1)

#rename cols., add W/NonW, etc
flow.gm1 = dplyr::rename(flow.gm1, Total.ethn = All )

flow.gm1 = arrange(flow.gm1, Total, msoa1, msoa2)   #sort by increasing Total

#reorder columns
flow.gm1 = flow.gm1[, c(1:3,136,4:135,137:154)]

flow.gm1$White = flow.gm1[,"WhiteEnglish"]+ flow.gm1[,"WhiteIrish"]+flow.gm1[,"WhiteGypsyorIrishTraveller"]+  flow.gm1[,"WhiteOther" ]
flow.gm1$Non.Wh <- flow.gm1$Total.ethn  - flow.gm1$White

                 
     
#delete any other ethnicity columns
dropcols= c("WhiteEnglish", "WhiteIrish",   "WhiteGypsyorIrishTraveller"  ,
            "WhiteOther" ,  "Mixed.W&Bl.Caribbean",  "Mixed.W&Bl.African", 
            "Mixed.WhiteandAsian", "Mixed.Other" , "AsianBritishIndian", 
            "AsianBritishPakistani", "AsianBritishBangladeshi" ,     "AsianBritishChinese",  
            "AsianBritishOther",   "BlackBritishAfrican", "BlackBritishCaribbean",       
            "BlackBritishOtherBlack",       "OtherethnicgroupArab",  "Otherethnicgroup"  )

flow.gm1 = flow.gm1[, !(names(flow.gm1) %in% dropcols)]

#check: 
sum(colSums(flow.gm1[5:136]))    # all mode colums     
sum(colSums(flow.gm1[137:138]))  # both must be similar, ~1,030,716
(sum(flow.gm1$Total==flow.gm1$Total.ethn)==nrow(flow.gm1))  #must be TRUE: all flows consistent

### arrange and prepare for prob. vector
flow.gm1 = arrange(flow.gm1, Total,Total.ethn, msoa1, msoa2)
agesexmode =  names(flow.gm1)[5:(ncol(flow.gm1)-2)]     # 132 age-sex-mode categories
ethnicity = c('White','Non.Wh')

#create the SP types
types = merge(x = agesexmode, y = ethnicity)
types = paste0(types$x,'_', types$y)

#create vector of probabilities for all SP types (264)
vprob = data.frame(matrix(data = 0, nrow = 1,ncol = length(types))  )
names(vprob) = sort(types)

# select 'univocal' flows
#separates flows into: univocal / probabilistic
sel = which((flow.gm1$White==0  | flow.gm1$Non.Wh==0)==1)      #| (sel2 =sum(flow.gm1[,c(5:136) !=0])==1 )
sel1 = which( (flow.gm1$White!=0  & flow.gm1$Non.Wh!=0)==1) 
# sel+sel1 tesselate flow.gm1, COVERING all flow.gm1 rows  (127,627)
    
#DF to hold the SP rebuilt population
flow.sp1 = as.data.frame(matrix(0,nrow = nrow(flow.gm1), ncol = length(types)))
names(flow.sp1) = sort(types)


################## REBUILDING POPULATION
# run STAGE 1 only if NOT built before....)
######### STAGE 1: GET PROB. VECTOR per TYPE  +  POPULATE THEM (PERFECT REBUILD)

for (i in sel)  {

            singleflow = flow.gm1[i,]          #flow categories
            
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
                flow.sp1[i,candidate ] = flow.sp1[i , candidate] + flow.gm1[i, modes[j]]   }
    
            if (i %% 10000==0)  { cat(i,' rows processed \n')}         
                }

cat('STAGE 1: completed....')    #all univocal flows rebuilt
saveRDS(flow.sp1, './2-output/flow.sp1.Rds')

#################  vprob calculation
vprob=colSums(flow.sp1)

#check: some types w still have prob=0 & will never come out
sum(vprob==0)


# Replace by 1s/realistic probability to make them technically possible in dataset
vprob[vprob==0] = 0.01
saveRDS(vprob, './2-output/vprob.sp1.Rds')

######### STAGE 2: REBUILD FLOWS WITH MULTIPLE INDIVIDUALS (PROBABILISTIC APPROACH)

for (i in sel1)    {
    
    singleflow = flow.gm1[i,]          #flow categories
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
        
  while (sum(subflow[,modes])>0)    {  #loop through types while there are people to allocate
        
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
            flow.sp1 = updateResults(subcandidates, n)  # allocation
            subflow = updateSubflow(subcandidates, n)  # subflow figures update
            
                
        } else {      #multiple options, use multinomial
            
            allocated = allocateMulti(subcandidates)        #allocates & updates subflow    
            flow.sp1 = updateResults(allocated, 1)
            subflow = updateSubflow(allocated, 1)
            }

    }   #end while
   
    if (i %% 1000==0)  { 
        cat('Row no ', i,' processed \n')  }     
     
}

cat('STAGE 2: completed....')    #all probabilistic flows rebuilt

##### CHECKS on flow.sp1
sum(rowSums(flow.sp1)==flow.gm1$Total)  # must be 127,627

row.correct= which(rowSums(flow.sp1)==flow.gm1$Total)  # should match nrows
row.defect = which(rowSums(flow.sp1) < flow.gm1$Total) # should be 0
row.excess = which(rowSums(flow.sp1) > flow.gm1$Total) # should be 0

cor(colSums(flow.sp1), vprob)   #correlation  prob - SP (>0.97)


#add totals & flows
flow.sp1 =cbind(Total=rowSums(flow.sp1), msoa1=as.character(flow.gm1$msoa1), msoa2=as.character(flow.gm1$msoa2), flow.sp1 )

saveRDS(flow.sp1, './2-output/flow.sp1.Rds')


