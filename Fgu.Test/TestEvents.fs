module TestEvents
    open NUnit.Framework
    open FsUnit
    open FsCheck
    open FsCheck.NUnit

    open Fgu.Events

    [<Test>]
    let ``create an event`` () = 
        1 |> should equal 1
        let es = generate_events 1 2017
        in
            (List.length es) |> should equal 1
        

    [<Property(QuietOnSuccess = true)>]
    let AddPlusOneProperty (eventCount) = 
        eventCount >= 0 ==> 
            let es = generate_events eventCount 2017
            List.length(es) <= eventCount