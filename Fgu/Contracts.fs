namespace Fgu
  module Contracts =


    open Fgu.Events
    type Percent = float

    type QuotaShareContract = {
        quota_share : Percent; // shared risk given to the reinsurances
        quota_part : Percent; // the part of the quota_share for us
        commission : float; 
    }

    let applyEventsQS(contract: QuotaShareContract)  (events: RiskEvent list) : ContractEvent list = 
        let factor = contract.quota_share * contract.quota_part
        in
            events 
            |> List.map(fun e -> {id = e.id; loss = e.loss * factor})

    type StopLossContract = {
        estimated_premium_income : float; // EPI: what the insurance get from their customers
        priority_p : Percent; // this is the share that the insurance pays (% of EPI)
        stop_loss_p : Percent; // this is the share that reinsurance pays (% of EPI)
        priority : float;
        stop_loss : float;
        plafond : float;
    }

    // constructor for a Stop-Loss-Contract
    let makeSL epi prio sl = 
        { 
            estimated_premium_income = epi;
            priority_p = prio;
            stop_loss_p = sl;
            priority = epi * prio;
            stop_loss = epi * sl;
            plafond = epi * (prio + sl);
        }

    // An SL contracts summarizies about a year (or financial period, i.e. all members of of 
    // the event list). Payments occur only within the band of prio and sl. We map here the 
    // events to the their share of payments to be made. This means that the first events 
    // (in sum lower than prio) have nothing to pay, whereas as later events have to pay. 
    // This may be a partial payment of the loss, if we jump from lower than prio to above prio. 
    // Therefore we the min-function to derive the payment.
    let applyEventsSL (contract: StopLossContract) (events: RiskEvent list) : ContractEvent list = 
        let cut sum loss = 
            if sum + loss < contract.plafond then min loss (sum + loss - contract.priority)
            else min loss (contract.plafond - sum)
        let payment sum loss = 
            match (sum, loss) with
            | (sum, loss) when sum + loss < contract.priority  -> 0.0
            | (sum, loss) when sum > contract.plafond    -> 0.0
            | (sum, loss) -> cut sum loss
        let calcCumul(cumul:float) (e : RiskEvent) = ((e, payment cumul e.loss ), cumul + e.loss)
        in 
            events
            |> List.mapFold calcCumul 0.0
            |> fst // ignore the cumul summary here (technically required from mapFold)
            |> List.map (fun (e, cumul) -> {id = e.id; loss = min e.loss cumul})


    type Contract = 
        | QuotaShare of QuotaShareContract
        | StopLoss of StopLossContract



    