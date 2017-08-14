module TestEvents
    open NUnit.Framework
    open FsUnit

    open Fgu.Events

    [<Test>]
    let ``create an event`` () = 
        1 |> should equal 1
        let es = generate_events 1 2017
        in
            (List.length es) |> should equal 1
        

