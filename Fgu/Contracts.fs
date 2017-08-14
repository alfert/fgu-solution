module Fgu.Contracts

open Fgu.Events
type Percent = float

type QuotaShareContract = {
    quota_share : Percent; // shared risk given to the reinsurances
    quota_part : Percent; // the part of the quota_share for us
    commission : float; 
}

// apply_events :: List Events -> QuotaShareContract -> float
let apply_events_qs(events: Event list, contract: QuotaShareContract) : float = 
    let factor = contract.quota_share * contract.quota_part
    in
        events 
        |> List.sumBy(fun e -> e.loss * factor)

type Contract = 
    | QuotaShare of QuotaShareContract



    