module TestContracts
    open NUnit.Framework
    open FsUnit
    open FsCheck
    open FsCheck.NUnit

    open Fgu.Contracts
    open Fgu.Events

    // Comparison that prints the left and righ parameters
    let (.=.) (left: float) (right: float) =
         (abs left - right) < 1.0e-5 |@ sprintf "%A = %A" left right
   
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
        let c = makeSL (20.0 * 1000.0 * 1000.0) q q
        // min(Haftung, max(0, X- Priorität))
        let cut s = min c.stop_loss (max 0.0 (s - c.priority))
        let allLosses = es |> List.sumBy (fun (ev : RiskEvent) -> ev.loss) |> cut
        (allLosses)  .=.
            (es |> (applyEventsSL c) |> List.sumBy (fun (ev : ContractEvent) -> ev.loss))
        |> Prop.classify(allLosses >= c.stop_loss) "loss > plafond"
        |> Prop.classify(allLosses < c.stop_loss && allLosses >= c.priority) "loss > prio"
        |> Prop.classify(allLosses < c.priority) "loss < prio"
        |> Prop.collect(List.length(es))
    
    [<Test>]
    let ``1 event means easier argumentation``() = 
        let es = generateEvents 1 [DE] [Storm] 2017
        let q = 0.5
        let c = makeSL (20.0 * 1000.0 * 1000.0) q q 
        let losses = es |> applyEventsSL c
        List.length(losses) |> should equal 1
        let l = losses.Head
        let e = es.Head 
        let expectedPayment = min c.stop_loss (max 0.0 (e.loss - c.priority))
        l.loss |> should equal expectedPayment
