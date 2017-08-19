﻿namespace Fgu
  module Contracts =


    open Fgu.Events
    type Percent = float

    type QuotaShareContract = {
        quota_share : Percent; // shared risk given to the reinsurances
        quota_part : Percent; // the part of the quota_share for us
        commission : float; 
    }

    let applyEventsQS(events: RiskEvent list, contract: QuotaShareContract) : ContractEvent list = 
        let factor = contract.quota_share * contract.quota_part
        in
            events 
            |> List.map(fun e -> {id = e.id; loss = e.loss * factor})

    type StopLossContract = {
        estimated_premium_income : float; // EPI: what the insurance get from their customers
        priority : Percent; // this is the share that the insurance pays (% of EPI)
        stop_loss : Percent; // this is the share that reinsurance pays (% of EPI)
    }

    // An SL contracts summarizies about a year (or financial period, i.e. all members of of 
    // the event list). Payments occur only within the band of prio and sl. We map here the 
    // events to the their share of payments to be made. This means that the first events 
    // (in sum lower than prio) have nothing to pay, whereas as later events have to pay. 
    // This may be a partial payment of the loss, if we jump from lower than prio to above prio. 
    // Therefore we the min-function to derive the payment.
    let applyEventsSL(events: RiskEvent list, contract: StopLossContract) : ContractEvent list = 
        let prio = contract.estimated_premium_income * contract.priority
        let sl = contract.estimated_premium_income * contract.stop_loss
        let payment = function
            | loss when loss < prio      -> 0
            | loss when loss > prio + sl -> 0
            | loss                       -> loss - prio
        let apply cumul ev = 
            (cumul + ev.loss, payment cumul + ev.loss)
        in 
            events
            |> List.mapFold (fun cumul e -> (e, cumul + e.loss)) 0
            |> List.map (fun (e, cumul) -> {id = e.id, loss = min(e.loss, cumul)})


    type Contract = 
        | QuotaShare of QuotaShareContract
        | StopLoss of StopLossContract



    