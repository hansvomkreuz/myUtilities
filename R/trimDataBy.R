#' Trim data by variable
#'
#' @param dataFrame A data.frame object.
#' @param dataFrame A data.frame object.
#' @return A data.frame with summary information of the input `dataFrame`
#' @export
#'
#'

trimData <- function(dataFrame,byVar,trimPercent = 0.10,trimUpper = T,trimLower = T){
    dataCopy <- dplyr::arrange(dataFrame,byVar)
    if(between(trimPercent,0,0.5)){
        print("ok")
        return(dataCopy)
    } else {
        messageString <- "INFO: Trim unsuccessful. Trim percentage should be between 0% and 50%"
        tictoc::tic(messageString)
        tictoc::toc()
        return(dataFrame)
    }

}
