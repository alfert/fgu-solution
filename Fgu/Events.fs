namespace Fgu 
  module Events =

    open System

    // 2 letter country codes 
    type Country = DE | AT | FR | GB | ES | US | PL | CH | IT | NL | BE

    // Nat Cat Risk types
    type NatCatRisk = Storm | Earthquake | Fire | Flood | Hail

    type RiskEvent = { 
        id : int;
        day_of_year : int;
        year : int;
        loss : float;
        country : Country option;
        risk : NatCatRisk option;
    }

    type ContractEvent = {
        id : int;
        loss : float;
    }

    let makeContractEvent (risk: RiskEvent) (loss: float) = {id = risk.id; loss = loss}
    
    let rnd = System.Random()  
    let selectOneOf elements = 
        match elements with
        | [] -> None
        | _ -> 
            let n = List.length elements
            let i = rnd.Next(n)
            in Some(List.item i elements)

    let generateEvents (n : int) (countries : Country list) (risks: NatCatRisk list) (the_year: int) : RiskEvent list= 
        let newDayAndId k = (rnd.Next(365) + 1, k)
        let c = selectOneOf countries
        let r = selectOneOf risks
        let singleEvent day anId = {
            day_of_year = day; year = the_year; 
            loss = rnd.NextDouble()*(20.0*1000.0*1000.0); id = anId; country = c;  risk = r;
            }
        in
            [1..n]
            |> List.map(newDayAndId >> fun (d, id) -> singleEvent d id) 
            |> List.sortBy(fun ev -> ev.day_of_year)
    
    let generateEvents2 (countries: Country list) (risks: NatCatRisk list) (theYear: int) (losses: float list) : RiskEvent list = 
        let c = selectOneOf countries
        let r = selectOneOf risks
        let makeEvent day id loss = {
            day_of_year = day; year = theYear; id = id; country = c; risk = r;
            loss = loss;
        }
        losses 
        |> List.indexed
        |> List.map (fun (k,l) -> makeEvent (rnd.Next(365) + 1) k l)
        |> List.sortBy (fun ev -> ev.day_of_year)