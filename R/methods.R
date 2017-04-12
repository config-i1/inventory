#### Prints of smooth ####
#' @export
print.inventory <- function(x, ...){
    cat(paste0("Achieved Service Level is:", round(x$CSL,3),"\n"));
    cat(paste0("Total Costs:", round(x$TC,3),"\n"));
    # cat(paste0("Bullwhip effect:", x$varOrders/x$varOriginal,"\n"));
}
