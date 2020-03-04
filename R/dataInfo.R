#' Get detailed information of a data.frame
#'
#' @param dataFrame A data.frame object.
#' @return A data.frame with summary information of the input `dataFrame`
#' @export

dataInfo <- function(dataFrame,roundDigits = 5){
    options(scipen = 999)
    if(!is.data.frame(dataFrame)){
        stop(paste0("The object '",deparse(substitute(dataFrame)),"' should be a data frame."))
    }
    if(!require(tidyverse)){
        stop("Install 'tidyverse' package to run this function.")
    }
    info <- data.frame(
        varType = sapply(dataFrame,function(x)class(x)[1]),
        records = nrow(dataFrame),
        uniqueValues = sapply(dataFrame,function(x){length(unique(x))}),
        missingValues = sapply(dataFrame,function(x){sum(is.na(x))})
    )
    info <- info %>%
        rownames_to_column(var = "columnName")  %>%
        mutate(cardinality = round(1.0*uniqueValues/records),roundDigits)
    numVars <- nrow(filter(info,varType %in% c("numeric","integer")))
    if(numVars>0){
        numbers <- filter(info,varType %in% c("numeric","integer"))
        numbers <- dataFrame %>%
            select(numbers$columnName)
        numbers <- data.frame(min = sapply(numbers,min,na.rm=TRUE),
                              mean = round(sapply(numbers,mean,na.rm=TRUE),roundDigits),
                              median = round(sapply(numbers,median,na.rm=TRUE),roundDigits),
                              max = sapply(numbers,max,na.rm=TRUE),
                              stDev = round(sapply(numbers,sd,na.rm=TRUE),roundDigits),
                              total = sapply(numbers,sum,na.rm=TRUE))
        numbers <- rownames_to_column(numbers,var = "columnName")
        info <- info %>%
            left_join(numbers,by="columnName")
    }
    options(scipen=0, digits=7)
    return(info)
}
