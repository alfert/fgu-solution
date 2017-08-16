namespace Fgu
  module Contracts =


    open Fgu.Events
    type Percent = float

    type QuotaShareContract = {
        quota_share : Percent; // shared risk given to the reinsurances
        quota_part : Percent; // the part of the quota_share for us
        commission : float; 
    }

    // apply_events :: RiskEvents list -> QuotaShareContract -> float
    let applyEventsQS(events: RiskEvent list, contract: QuotaShareContract) : ContractEvent list = 
        let factor = contract.quota_share * contract.quota_part
        in
            events 
            |> List.map(fun e -> {id = e.id; loss = e.loss * factor})

    type Contract = 
        | QuotaShare of QuotaShareContract



    