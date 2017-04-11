#' Order-Up-To level inventory simulation function
#'
#' This is the first inventory simulation function.
#'
#' Function uses data and rolling origin forecasts in order to do simulations
#'
#' @param data Demand observations, both insample and holdout (simulation) windows.
#' @param fcs Lead time forecasts (aggregated), equal size as demand data.
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
outp <- function(data, fcs, holdout, ss=c("constant","dynamic"), L=1, CSL=95){

    ss_input = ss

    # the length of the whole data
    N = length(data);
    NInSample = N-holdout;
    NInitialisation = NInSample-L;

    # Vector of aggregated data
    aggdata = array(NA, N)
    # Vector of safety stocks
    ss = array(NA, N)
    # Vector of orders
    ord = array(NA, N)
    # ?
    wip = array(NA, N)
    # Vector of inventory
    inv = array(NA, N)

    for (i in 1:(N-L+1)){
        aggdata[i] = sum(data[i:(i+L-1)])
    }

    # If a character is provided in ss, use it
    if (!is.numeric(ss_input)){
        if (ss_input=="constant"){
            ss[(NInitialisation+1):(N-L+1)] = rep(quantile((aggdata[1:(NInitialisation+1)] - fcs[1:(NInitialisation+1)]), CSL/100), holdout+1)
        } else if (ss_input=="dynamic"){
            for (i in 1:(holdout+1)){
                ss[NInitialisation+i] = quantile((aggdata[1:(NInitialisation+i)] - fcs[1:(NInitialisation+i)]), CSL/100)
            }
        }
    }
    # else {
    #   if (length(ss_input)==1){
    #     ss = rep(ss_input, holdout)
    #   } else {
    #     ss
    #   }
    # }

    ##### Initialise the thing #####
    for (l in 1:L){
        wip[NInitialisation+l] = (L-1)*mean(data[1:(NInitialisation+l)])
        inv[NInitialisation+l] = ss[NInitialisation+1] + mean(fcs[1:(NInitialisation+l)]/L) - mean(data[1:(NInitialisation+l)])
        ord[NInitialisation+l] = fcs[NInitialisation+l+1] + ss[NInitialisation+1] - inv[NInitialisation+l] - wip[NInitialisation+l]
    }

    cost = 0
    demand_met = 0

    ##### Do the calculations #####
    for (t in (N-holdout+1):N){
        # Inventory balance equation
        inv[t] = ord[t-L] + inv[t-1] - data[t]

        # Work-in-process balance equation
        wip[t] = wip[t-1] - ord[t-L] + ord[t-1]

        # Ordering policy
        if (t <= (N-1)){
            ord[t] = fcs[t+1] + ss[t-L] - inv[t] - wip[t]
        }

        # Total inventory and backlog cost
        if (inv[t]>0){
            cost = cost + inv[t]
        } else {
            cost = cost + abs(inv[t])*(100/(100-CSL) - 1)
        }

        # For calculating Realised Customer Service Level
        if ((inv[t-1]+ord[t-L])>data[t]){
            demand_met = demand_met + data[t]
        } else {
            demand_met = demand_met + (inv[t-1]+ord[t-L])
        }
    }
    rcsl = demand_met / sum(data[(N-holdout+1):N])

    return(list(varOrders=var(inv[(N-holdout+1):(N)]), varInventory=var(ord[(N-holdout+1):(N-1)]), TC=cost, CSL=rcsl))
}
