module TestContracts
    open NUnit.Framework
    open FsUnit
    open FsCheck
    open FsCheck.NUnit

    open Fgu.Contracts
    open Fgu.Events

    // Comparison that prints the left and righ parameters
    let (.=.) left right = left = right |@ sprintf "%A = %A" left right
   
    [<Property(QuietOnSuccess = true)>]
    let ``unsigned ints are always positive`` (eventCount : uint16) = 
        int(eventCount) >= 0
        
    [<Property(QuietOnSuccess = true)>]
    let ``Proportial sums are correct`` (eventCount : uint16) = 
        // generate events and calc the summary here, compared to the sum of contract events
        let es = generateEvents (int eventCount) [DE] [Storm] 2017
        let q = 0.5
        let c = { quota_share = q; quota_part = q; commission = 1.0}
        ((es |> List.sumBy (fun (ev : RiskEvent) -> ev.loss)) * q * q)  =
            (es |> (applyEventsQS c) |> List.sumBy (fun (ev : ContractEvent) -> ev.loss))

    [<Property(Verbose = true)>]
    let ``StopLoss sums are correct`` (eventCount : uint16) = 
        // generate events and calc the summary here, compared to the sum of contract events
        let es = generateEvents (int eventCount) [DE] [Storm] 2017
        let q = 0.5
        let c = { estimated_premium_income = 20.0 * 1000.0 * 1000.0; stop_loss = q; priority= q}
        // min(Haftung, max(0, X- Priorität))
        let sl = c.estimated_premium_income * (c.stop_loss + c.priority)
        let prio = c.estimated_premium_income * c.priority
        let cut s = max 0.0 ((min s sl) - prio)
        let allLosses = es |> List.sumBy (fun (ev : RiskEvent) -> ev.loss)
        (allLosses)  .=.
            (es |> (applyEventsSL c) |> List.sumBy (fun (ev : ContractEvent) -> ev.loss))
        |> Prop.classify(allLosses >= sl) "loss > plafond"
        |> Prop.classify(allLosses < sl && allLosses >= prio) "loss > prio"
        |> Prop.classify(allLosses < prio) "loss < prio"
        |> Prop.collect(List.length(es))
    
    [<Test>]
    let ``1 small events means no payment``() = 
        let es = generateEvents (1) [DE] [Storm] 2017
        let q = 0.5
        let c = { estimated_premium_income = 20.0 * 1000.0 * 1000.0; stop_loss = q; priority= q}
        let losses = es |> applyEventsSL c
        List.length(losses) |> should equal 1
        let [l] = losses
        let [e] = es 
        let sl = c.estimated_premium_income * (c.stop_loss + c.priority)
        let prio = c.estimated_premium_income * c.priority
        let payment = max 0.0 ((min e.loss sl) - prio)
        l.loss |> should equal payment
