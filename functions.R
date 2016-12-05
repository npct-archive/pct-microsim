
selColumns =    function(singleflow){
    
    notnull = which(singleflow !=0)
    colsnotnull = names(flow.gm)[notnull]
    colsnotnull = colsnotnull[! colsnotnull %in% c('msoa1', 'msoa2', 'Total','Total.ethn')]
    
    return(colsnotnull)
                }


updateType   <- function(typetoUpdate)   {
    
                parts= str_split_fixed(typetoUpdate,pattern = '_', n = 4)
                
                ethnia = parts[4]                             
                singleflow[ethnia] = singleflow[ethnia]-1  #check it is 1
                
                
                ethnia = paste0('_', ethnia)
                mode= gsub(pattern = ethnia,replacement = '', x = typetoUpdate)
                
                singleflow[mode] = singleflow[mode]-1  #check it is 1                        
                
    
}


allocateMulti     <- function (typetoAllocate)    {
    
                minialloc <- rmultinom(n=1,size=n,prob=vprob.subflow)   # allocate randomly
                
                
                
                allocated = names(minialloc)[minialloc!=0]     
                flow.sp[i, typetoAllocate] = flow.sp[i, typetoAllocate] + 1
    
                        }