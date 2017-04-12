#' Order-Up-To level inventory simulation function
#'
#' This is the first inventory simulation function.
#'
#' Function uses data and rolling origin forecasts in order to do simulations
#'
#' @param data Demand observations, both insample and holdout (simulation) windows.
#' @param fcs Lead time forecasts (aggregated), equal size as demand data. These are
#' forecasts produced from each observation i for the period of i+(1:L) and written down to
#' the element i of the vector.
#' @param fitted Fitted valus from forecasting model. Needed to initialise the model.
#' If NULL, then the first forecast value is used.
#' @param holdout The size of the window where the inventory simulation will be performed.
#' @param ss Safety stock; single value, vector, or an option between "constant" or "dynamic".
#' If single value, then ss is considered fixed equal to that value. If vector, then ss is
#' considered to be updated dynamically; in this case, the length of ss should be equal to
#' the holdout. If "constant", then ss is calculated empirically once (based on the insample
#' data) and kept fixed. If "dynamic", then ss is calculated empirically and updated for
#' each period. It is recommended that both "constant" and "dynamic" are used only if at
#' least 20 observations are available in the insample (length of data minus holdout).
#' @param L Lead time.
#' @param CSL Targeted Customer Service Level.
#'
#' @return The list, containing the following, is returned:
#' \itemize{
#' \item varOrders - Orders variance
#' \item varInventory - Inventory variance
#' \item TC - Total (inventory and backlog) cost
#' \item CSL - Realised CSL
#' }
#'
#' @examples
#' #No example here yet.
#'
#' @export outp
outp <- function(data, fcs, fitted=NULL, holdout, ss=c("constant","dynamic"), L=1, CSL=0.95){

    ss_input = ss

    # If user provides 95 instead of 0.95, fix it.
    if(CSL>1){
        CSL <- CSL/100;
    }

    # the length of the whole data
    N = length(data);
    # Number of observations in-sample
    NInSample = N-holdout;
    # Number of observations in-sample before the initialisation of the model
    NBeforeInit = NInSample-L;

    if(is.null(fitted)){
        fitted = rep(fcs[1],NInSample)
    }
    else{
        if(length(fitted)==1){
            fitted = rep(fitted[1],NInSample)
        }
    }

    # Vector of aggregated data
    aggdata = data

    ###!!! ss, inv, wip and ord are now lagged. The first value there corresponds to NBeforeInit+1 in the data!!!###
    # Vector of safety stocks
    ss = array(NA, holdout+L)
    # Vector of orders
    ord = array(NA, holdout+L)
    # Work in progress
    wip = array(NA, holdout+L)
    # Vector of inventory
    inv = array(NA, holdout+L)

    if(L>1){
        for (i in 1:(N-L+1)){
            aggdata[i] = sum(data[i:(i+L-1)])
        }
    }

    # If a character is provided in ss, use it
    if(!is.numeric(ss_input)){
        # This means that only the first letter can be provided instead of the whole word
        ss_input = substr(ss_input[1],1,1)
        if (ss_input=="c"){
            # Here we use only the data from the in-sample
            ss[L+(1:holdout)] = rep(quantile(aggdata[1:NInSample] - L*fitted[1:NInSample], CSL), holdout)
        } else if (ss_input=="d"){
            # Fill in the values for the holdout
            for (i in 1:(holdout-L)){
                ss[L+i] = quantile((aggdata[NInSample+(1:i)] - fcs[1:i]), CSL)
            }
        }
        ## Fill in the same ss for the initialisation part
        ss[1:L] = ss[L+1]
    }
    else{
        if(length(ss_input)==1){
            ss = rep(ss_input, holdout+L)
        }
    }

    ##### Initialise the thing #####
    for (l in 1:L){
        wip[l] = (L-1)*mean(data[1:(NBeforeInit+l)])
        inv[l] = mean(fitted[1:(NBeforeInit+l)]) - mean(data[1:(NBeforeInit+l)])
        # inv[l] = ss[1] + mean(fitted[1:(NBeforeInit+l)]/L) - mean(data[1:(NBeforeInit+l)])
        # This initialisation may be slightly incorrect - it uses only fitted values
        ord[l] = fitted[NBeforeInit+l] + ss[l] - inv[l] - wip[l]
    }

    cost = 0
    demand_met = 0

    ##### Do the calculations #####
    for (t in L+(1:holdout)){
        # Inventory balance equation
        # max is needed for cases, when inv or ord become negative
        inv[t] = inv[t-1] + ord[t-L] - data[NBeforeInit+t]

        # Work-in-process balance equation
        wip[t] = wip[t-1] + ord[t-1] - ord[t-L]
        # wip[t] = wip[t-1] - ord[t-L] + sum(ord[t-(1:(L-1))])

        # Ordering policy
        # if (t <= (N-1)){
        ord[t] = fcs[t-L] + ss[t-L] - inv[t] - wip[t]
        # }

        # Total inventory and backlog cost
        if (inv[t]>0){
            cost = cost + inv[t]
        } else {
            cost = cost + abs(inv[t])*(1/(1-CSL) - 1)
        }

        # For calculating Realised Customer Service Level
        # if ((inv[t-1]+ord[t-L])>data[t]){
        #     demand_met = demand_met + data[t]
        # } else {
        #     demand_met = demand_met + inv[t-1]+ord[t-L]
        # }
        demand_met = demand_met + min(data[NBeforeInit+t],inv[t-1]+ord[t-L])
    }
    rcsl = demand_met / sum(data[(NInSample+1):N])


    return(list(varOrders=var(inv), varInventory=var(ord[1:(holdout+L-1)]), TC=cost, CSL=rcsl))
}
