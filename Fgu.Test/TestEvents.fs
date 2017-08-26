module TestEvents
    open NUnit.Framework
    open FsUnit
    open FsCheck
    open FsCheck.NUnit

    open Fgu.Events

    [<Test>]
    let ``create an event`` () = 
        1 |> should equal 1
        let es = generateEvents 1 [DE] [Storm] 2017
        in
            (List.length es) |> should equal 1
     
    [<Test>]
    let ``create no event`` () = 
        let es = generateEvents 0 [DE] [Storm] 2017
        in
            (List.length es) |> should equal 0
            

    [<Property(QuietOnSuccess = true)>]
    let AddPlusOneProperty (eventCount) = 
        let c = [DE]
        let r = [Storm]
        eventCount >= 0 ==> 
            let es = generateEvents eventCount c r 2017
            List.length(es) <= eventCount

    [<Property(QuietOnSuccess = true)>]
    let ``select an arbitrary element`` (xs : int list) = 
        match selectOneOf xs with
        | None   -> List.isEmpty xs 
        | Some x -> List.contains x xs  

    [<Property(QuietOnSuccess = true)>]
    let ``empty countries and risks generate None events`` (eventCount) = 
        (generateEvents eventCount [] [] 2017)
        |> List.forall(fun ev -> ev.country = None && ev.risk = None)