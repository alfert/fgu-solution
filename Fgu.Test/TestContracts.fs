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

    [<Property()>]
    let ``ìnvariant sums of XL contracts: ignore irrelevant risks``(eventCount : uint16) = 
        let es = generateEvents (int eventCount) [DE] [Storm] 2017
        let c = makeXLperRisk [Earthquake] [(xs 3000000.0  5000000.0); (xs 8000000.0  10000000.0) ]
        es |> applyEventsXLperRisk c 
        |> List.sumBy (fun e -> e.loss) 
        = 0.0

    [<Property(Verbose=true)>]
    let ``ìnvariant sums of a single layered XL contract: ``(ls : uint32 list) = 
        let es = generateEvents2  [DE] [Storm] 2017 (ls |> List.map float)
        let minLoss = 3000000.0 
        let maxLoss = 10000000.0
        let c = makeXLperRisk [Storm] [(xs minLoss maxLoss) ]
        let losses = es |> applyEventsXLperRisk c |> List.sumBy (fun e -> e.loss) 
        // min(Haftung, max(0, X- Priorität))
        let cut s = min (maxLoss - minLoss) (max 0.0 (s - minLoss))
        in 
            (es 
            |> List.sumBy(fun e -> e.loss))
            |> cut
            .=. losses

    [<Property(Verbose=true)>]
    let ``ìnvariant sums of a doubled layered XL contract: ``(ls : uint16 list) = 
        let es = generateEvents2  [DE] [Storm] 2017 (ls |> List.map float)
        let minLoss =  3000.0 
        let maxLoss = 10000.0
        let c = makeXLperRisk [Storm] [(xs minLoss 5000.0); (xs 8000.0  maxLoss) ]
        let losses = es |> applyEventsXLperRisk c |> List.sumBy (fun e -> e.loss) 
        // min(Haftung, max(0, X- Priorität))
        let cut s = min (maxLoss - minLoss) (max 0.0 (s - minLoss))
        in 
            (es 
            // |> List.filter(fun e -> e.loss >= minLoss && e.loss <= maxLoss)
            |> List.sumBy(fun e -> e.loss))
            |> cut
            .=. losses
