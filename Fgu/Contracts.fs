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
            |> List.map(fun e -> makeContractEvent e (e.loss * factor))

    type ExcessOfLossContract = {
        estimated_premium_income : float; // EPI: what the insurance get from their customers
        priority_p : Percent; // this is the share that the insurance pays (% of EPI)
        liability_p : Percent; // this is the share that reinsurance pays (% of EPI)
        priority : float;
        liability : float;
        plafond : float;
    }

    // constructor for a Stop-Loss-Contract
    let makeXL epi prio sl = 
        { 
            estimated_premium_income = epi;
            priority_p = prio;
            liability_p = sl;
            priority = epi * prio;
            liability = epi * sl;
            plafond = epi * (prio + sl);
        }

    let xs (liability: float) (priority: float) = 
        let plafond = liability + priority
        in 
        { 
            estimated_premium_income = plafond;
            priority_p = priority / plafond;
            liability_p = liability / plafond;
            priority = priority;
            liability = liability;
            plafond = plafond;
        }

    // An SL contracts summarizies about a year (or financial period, i.e. all members of of 
    // the event list). Payments occur only within the band of prio and sl. We map here the 
    // events to the their share of payments to be made. This means that the first events 
    // (in sum lower than prio) have nothing to pay, whereas as later events have to pay. 
    // This may be a partial payment of the loss, if we jump from lower than prio to above prio. 
    // Therefore we the min-function to derive the payment.

    let layerPayment (contract: ExcessOfLossContract) (cumul: float) (loss: float) = 
        let fullLoss = cumul + loss
        if   fullLoss <= contract.priority then 0.0
        elif cumul > contract.plafond then 0.0
        // loss > prio
        elif cumul <= contract.priority then fullLoss - contract.priority
        else (min fullLoss contract.plafond) - cumul
    
    let calcCumul (contract: ExcessOfLossContract) (cumul: float) (e : RiskEvent) = 
        ((e, layerPayment contract cumul e.loss), cumul + e.loss)

    let applyEventsXL (contract: ExcessOfLossContract) (events: RiskEvent list) : ContractEvent list = 
        events
        |> List.mapFold (calcCumul contract) 0.0
        |> fst // ignore the cumul summary here (technically required from mapFold)
        |> List.map (fun (e, payment) -> makeContractEvent e payment)


    // An XL per Risk Contract is a list of layers, which are ExcessOfLoss Contracts
    type XLperRiskContract = {
        risks : NatCatRisk list;
        layers : ExcessOfLossContract list;
    }

    let makeXLperRisk (rs: NatCatRisk list) (xlLayers: ExcessOfLossContract list) = 
        {
            risks = rs;
            layers = xlLayers;
        }

    let contains x ys = 
        match x with 
        | Some(e) -> ys |> List.contains e
        | None    -> false

    let applyEventsXLperRisk (xl : XLperRiskContract) (es : RiskEvent list) = 
        let (yes, no) = es |> List.partition (fun e -> xl.risks |> contains e.risk)
        
        (xl.layers |> List.collect (fun layer -> applyEventsXL layer yes))
        // |> List.append (no |> List.map (fun e -> makeContractEvent e 0.0))

    type Contract = 
        | QuotaShare of QuotaShareContract
        | ExcessOfLoss of ExcessOfLossContract
        | XLperRisk of XLperRiskContract


    let applyEvents contract events = 
        match contract with
        | QuotaShare qs -> applyEventsQS qs events    
        | ExcessOfLoss sl   -> applyEventsXL sl events
        | XLperRisk xl  -> applyEventsXLperRisk xl events