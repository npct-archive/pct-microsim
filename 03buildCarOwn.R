
######################
######### BUILDS 4-VARIABLE dataset (age-sex-mode-CAR OWNERSHIP)
######### using OPTIMAL PROBABILITY allocation model

rm(list=ls())
library(dplyr)
library(stringr)
source('functions.R')


#read  flow.gm popul w. CAR OWNERSHIP (for G.M.)
flow.gm2 = readRDS('./2-output/flow.gm2.Rds')
names(flow.gm2)

#rename cols. + sort
flow.gm2 = dplyr::rename(flow.gm2, TotalCar=All)

#group car2+car3 into 1 single col.
flow.gm2$car2plus = flow.gm2$car2 + flow.gm2$car3

#delete cols. + reorder
dropcols= c('car2', 'car3')
flow.gm2 = flow.gm2[, !(names(flow.gm2) %in% dropcols)]
flow.gm2 = flow.gm2[, c(1:3,136, 4:135, 137:139) ]

#flujos problemáticos
flow.gm2= flow.gm2[flow.gm2$Total ==flow.gm2$TotalCar, ]

flow.gm2 = arrange(flow.gm2, Total, msoa1, msoa2)   #sort by increasing Total

#build categories
agesexmode =  names(flow.gm2)[5:(ncol(flow.gm2)-3)]     # 132 age-sex-mode categories
carown = c('car0','car1', 'car2plus')

types = merge(x = agesexmode, y = carown)      # 528 TYPES?
types = paste0(types$x,'_', types$y)

#create vector of probabilities for all SP types
vprob = data.frame(matrix(data = 0, nrow = 1, ncol = length(types))  )
names(vprob) = sort(types)

# #clean file ?
# flow.car  = flow.car[flow.car$All!=0, ]
# sum(flow.car$All[flow.car$All==1])      #1-person flows
# sum(flow.car$All)                       #total no. people  


#separates flows into univocal/probabilistic
non.null=  apply(X = flow.gm2[,c(137:139)], MARGIN = 1, function(x) length(x[x==0]))
sel= which(non.null==2)     # 2 out of 3 columns null => univocal
sel1 = subset(x=c(1:nrow(flow.gm2)), subset = !((1:nrow(flow.gm2)) %in% sel) )
# sel+sel1 tesselate flow.gm2, COVERING all flow.gm1 rows  (127,071)

sum(flow.gm2$Total[sel])     # total univocal population (~99K)

#DF to hold the SP rebuilt population
flow.sp2 = as.data.frame(matrix(0,nrow = nrow(flow.gm2), ncol = length(types)))
names(flow.sp2) = sort(types)


################## REBUILDING POPULATION
# run STAGE 1 only if NOT built before....)
######### STAGE 1: GET PROB. VECTOR per TYPE  +  POPULATE THEM (PERFECT REBUILD)

for (i in sel)  {
    
    singleflow = flow.gm2[i, c(5:ncol(flow.gm2))]          #flow categories
    
    colsnotnull = selColumns(singleflow)    #selects candidate cols. in singleflow
    singleflow = singleflow[, colsnotnull]   #add nos. to candidates cols. 
    
    #separate modes / carown cols.
    carown= colsnotnull[colsnotnull %in% c('car0', 'car1','car2plus')]
    modes  = colsnotnull[ !(colsnotnull %in% carown) ]    
    
    #build candidates using modes/carown
    candidates = merge(x=modes, y=carown)
    candidates = paste0(candidates$x,'_', candidates$y )
    
    for (j in 1:length(modes))  {
        candidate = grep(pattern = modes[j],x = candidates, value = T)    
        flow.sp2[i,candidate ] = flow.sp2[i , candidate] + flow.gm2[i, modes[j]]   }
    
    if (i %% 10000==0)  { cat(i,' rows processed \n')}         
}

cat('STAGE 1: completed....')    #all univocal flows rebuilt
saveRDS(flow.sp2, './2-output/flow.sp2.Rds')

vprob=colSums(flow.sp2)

# some types w still have prob=0 & will never come out 
# Replace by 1s to make them technically possible in dataset
vprob[vprob==0] = 0.1


######### STAGE 2: REBUILD FLOWS WITH MULTIPLE INDIVIDUALS (PROBABILISTIC APPROACH)

for (i in sel1)    {
    
    singleflow = flow.gm2[i, c(5:139)]               #flow categories
    #nopeople   = singleflow$Total           # no. indiv to allocate
    
    colsnotnull = selColumns(singleflow)     #selects candidate cols. in singleflow
    singleflow = singleflow[, colsnotnull]   #add nos. to candidates cols. 
    
    #separate modes cols. from car cols.
    carown= colsnotnull[colsnotnull %in% c('car0', 'car1','car2plus')]
    modes  = colsnotnull[ !(colsnotnull %in% carown) ]    
    
    #build candidates using modes/ethnics
    candidates = merge(x=modes, y=carown)
    candidates = paste0(candidates$x,'_', candidates$y )
    
    subflow = sort(singleflow)   #sort from min to max. Use subflow in loop
    
    while (sum(subflow[,modes])>0)    {  #loop through types while there are people to allocate
        
        colsnotnull=selColumns(subflow)
        subflow = subflow[colsnotnull]
        subflow = sort(subflow)
        
        carown= colsnotnull[colsnotnull %in% c('car0', 'car1','car2plus')]
        modes  = colsnotnull[ !(colsnotnull %in% carown) ]    
        
        #build candidates using modes/car
        candidates = merge(x=modes, y=carown)
        candidates = paste0(candidates$x,'_', candidates$y )
        
        #
        subtype = names(subflow)[1]      #type targetted
        n = 1         #no people in category
        
        #find sub-candidates
        subcandidates = grep(pattern = subtype, candidates, value = T)
        
        
        if (length(subcandidates)==1) {
            flow.sp2 = updateResults2(subcandidates, n)  # allocation
            subflow = updateSubflow(subcandidates, n)  # subflow figures update
            
            
        } else {      #multiple options, use multinomial
            
            allocated = allocateMulti(subcandidates)        #allocates & updates subflow    
            flow.sp2 = updateResults2(allocated, 1)
            subflow = updateSubflow(allocated, 1)
        }
        
    }   #end while
    
    if (i %% 1000==0)  { 
        cat('Row no ', i,' processed \n')  }     
    
}

cat('STAGE 2: completed....')    #all probabilistic flows rebuilt

##### CHECKS on flow.sp2
sum(rowSums(flow.sp2)==flow.gm2$Total)  # must be all

row.correct= which(rowSums(flow.sp2)==flow.gm2$Total)  # should match nrows
row.defect = which(rowSums(flow.sp2) < flow.gm2$Total) # should be null
row.excess = which(rowSums(flow.sp2) > flow.gm2$Total) # should be null

cor(colSums(flow.sp2), vprob)   #correlation  prob - SP (>0.97)


#add totals & flows
flow.sp2 =cbind(Total=rowSums(flow.sp2), msoa1=as.character(flow.gm2$msoa1), msoa2=as.character(flow.gm2$msoa2), flow.sp2 )

saveRDS(flow.sp2, './2-output/flow.sp2.Rds')


