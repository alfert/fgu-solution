namespace Fgu 
  module Events =

    open System

    type RiskEvent = { 
        id : int;
        day_of_year : int;
        year : int;
        loss : float }

    let generateEvents (n : int) (the_year: int) : RiskEvent list= 
        let rnd = System.Random()
        let newDay() = rnd.Next(365) + 1
        let singleEvent day anId = {day_of_year = day; year = the_year; 
            loss = rnd.NextDouble()*(20.0*1000.0*1000.0); id = anId }
        in
            [1..n]
            |> List.map(fun x -> (newDay(), x))
            |> List.map(fun (d, x) -> singleEvent d x  ) 
            |> List.sortBy(fun ev -> ev.day_of_year)