module TestContracts
    open NUnit.Framework
    open FsUnit
    open FsCheck
    open FsCheck.NUnit

    open Fgu.Contracts
    open Fgu.Events

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