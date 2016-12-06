

selColumns =    function(flow){   #extracts not null cols. for flow + subset to relevant columns
    
    notnull = which(flow !=0)
    colsnotnull = names(flow)[notnull]
    colsnotnull = colsnotnull[! colsnotnull %in% c('msoa1', 'msoa2', 'Total','Total.ethn')]
    
    return(colsnotnull)
                }


updateSubflow   <- function(type, n)   {   #given a candidate: splits into mode/ethnicity 
                                               #  & updates its subtypes figures + Total
                                                
    
                parts= str_split_fixed(type,pattern = '_', n = 4)
                
                ethnia = parts[4]                             
                subflow[ethnia] = subflow[ethnia]-1  #check it is 1
                
                
                ethnia = paste0('_', ethnia)
                mode= gsub(pattern = ethnia,replacement = '', x = type)
                
                subflow[mode] = subflow[mode]-1  #check it is 1
                return(subflow)
}


allocateMulti     <- function (type)    {
                
                vprob.subflow = vprob[type]   #prob. vector => filter for type/s
                suballoc <- rmultinom(n=1,size=1,prob=vprob.subflow)   # allocate randomly
                
                allocated = row.names(suballoc)[suballoc!=0]     
                
                return(allocated)
                
}


updateResults   <-  function (type, no)    {
    
                    flow.sp[i, type] = flow.sp[i, type] + no
                    return(flow.sp)
}                    