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

    [<Property(QuietOnSuccess = true)>]
    let ``StopLoss sums are correct`` (eventCount : uint16) = 
        // generate events and calc the summary here, compared to the sum of contract events
        let es = generateEvents (int eventCount) [DE] [Storm] 2017
        let q = 0.5
        let c = makeXL (20.0 * 1000.0 * 1000.0) q q
        // min(Haftung, max(0, X- Priorität))
        let cut s = min c.liability (max 0.0 (s - c.priority))
        let allLosses = es |> List.sumBy (fun (ev : RiskEvent) -> ev.loss) |> cut
        (allLosses)  .=.
            (es |> (applyEventsXL c) |> List.sumBy (fun (ev : ContractEvent) -> ev.loss))
        |> Prop.classify(allLosses >= c.liability) "loss > plafond"
        |> Prop.classify(allLosses < c.liability && allLosses >= c.priority) "loss > prio"
        |> Prop.classify(allLosses < c.priority) "loss < prio"
        |> Prop.collect(List.length(es))

    [<Test>]
    let ``1 event means easier argumentation``() = 
        let es = generateEvents 1 [DE] [Storm] 2017
        let q = 0.5
        let c = makeXL (20.0 * 1000.0 * 1000.0) q q 
        let losses = es |> applyEventsXL c
        List.length(losses) |> should equal 1
        let l = losses.Head
        let e = es.Head 
        let expectedPayment = min c.liability (max 0.0 (e.loss - c.priority))
        l.loss |> should equal expectedPayment

    [<Property()>]
    let ``ìnvariant sums of XL contracts: ignore irrelevant risks``(eventCount : uint16) = 
        let es = generateEvents (int eventCount) [DE] [Storm] 2017
        let c = makeXLperRisk [Earthquake] [(xs 3000000.0  5000000.0); (xs 8000000.0  10000000.0) ]
        es |> applyEventsXLperRisk c 
        |> List.sumBy (fun e -> e.loss) 
        = 0.0

    [<Property(QuietOnSuccess=true)>]
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

    [<Property(QuietOnSuccess=false)>]
    let ``ìnvariant sums of a doubled layered XL contract: ``(ls : uint32 list) = 
        let times10 x = 10.0 * x
        let lFloats = (ls |> List.map (float >> times10))
        let es = generateEvents2  [DE] [Storm] 2017 lFloats
        let minLoss =  3000.0 
        let maxLoss = 10000.0
        let liability1 = 5000.0
        let liability2 = maxLoss - liability1 - minLoss
        let c = makeXLperRisk [Storm] [(xs liability1 minLoss); (xs liability2 (minLoss + liability1)  ) ]
        let losses = es |> applyEventsXLperRisk c |> List.sumBy (fun e -> e.loss) 
        // min(Haftung, max(0, X- Priorität))
        let cut s = min (maxLoss - minLoss) (max 0.0 (s - minLoss))
        in 
            (es 
            |> List.sumBy(fun e -> e.loss))
            |> cut
            .=. losses
            |> (Prop.classify (List.sum(lFloats) > minLoss) "lFloats above minLoss")
            |> (Prop.classify (List.sum(lFloats) > maxLoss) "lFloats above maxLoss")
            |> (Prop.classify (List.sum(lFloats) <= maxLoss) "lFloats below maxLoss")

    [<Property(QuietOnSuccess=true)>]
    let ``contains for proper subsets`` (x : NatCatRisk) (ys: NatCatRisk list) = 
        (List.contains x ys) = (contains (Some x) ys)

    [<Property()>] 
    let ``calcCumul calcs correctly with cumul = 0`` (l : uint16) =
        let loss = float l
        let prio = 3000.0
        let liability = 5000.0
        let contract =  xs liability prio
        let e = (generateEvents2  [DE] [Storm] 2017 [loss]) |> List.head
        let (re, payment), cumul = calcCumul contract 0.0 e 
        let expected = (min (max (loss - prio) 0.0) liability) 
        sprintf "payment = %A, loss = %A" payment loss @| (
            "loss >=0" @| (payment >= 0.0) .&. 
            "loss < lia" @| (payment < liability) .&.
            (sprintf "loss = expected (%A)" expected) @| (payment = expected)
        )

    
    [<Property(QuietOnSuccess = true)>]
    let ``layerPayment works with a zero cumul`` (l : uint16) = 
        let loss = float l
        let prio = 3000.0
        let liability = 5000.0
        let contract =  xs liability prio
        let expected = (min (max (loss - prio) 0.0) liability) 
        let payment = layerPayment contract 0.0 loss
        sprintf "payment = %A, loss = %A" payment loss @| (
            "loss >=0" @| (payment >= 0.0) .&. 
            "loss < lia" @| (payment < liability) .&.
            (sprintf "payment = expected (%A)" expected) @| (payment = expected)
        )

    [<Property(QuietOnSuccess = true)>]
    let ``layerPayment works with a non-zero cumul below prio`` (l : uint16) = 
        let loss = float l
        let prio = 3000.0
        let liability = 5000.0
        let contract =  xs liability prio
        let cumul = 100.0
        let expected = (min (max (cumul+loss - prio) 0.0) liability) 
        let payment = layerPayment contract cumul loss
        sprintf "payment = %A, loss = %A" payment loss @| (
            "loss >=0" @| (payment >= 0.0) .&. 
            "loss < lia" @| (payment < liability) .&.
            (sprintf "payment = expected (%A)" expected) @| (payment = expected)
        )
    [<Property(Verbose = false)>]
    let ``layerPayment works with a non-zero cumul above prio`` (l : uint16) = 
        let loss = float l
        let prio = 300.0
        let liability = 50.0
        let contract =  xs liability prio
        let cumul = 345.0
        let expected = (min (max (cumul+loss - prio) 0.0) liability) + prio - cumul
        let payment = layerPayment contract cumul loss
        sprintf "payment = %A, loss = %A" payment loss @| (
            "payment >=0" @| (payment >= 0.0) .&. 
            "payment <= lia" @| (payment <= liability) .&.
            (sprintf "payment = expected (%A)" expected) @| (payment = expected)
        )
        |> Prop.classify (loss + cumul > prio + liability) "above plafond"
         |> Prop.classify (loss + cumul < prio + liability) "below plafond"
        // |> Prop.collect l