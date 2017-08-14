module Fgu.Events

    open System

    type Event = { 
        day_of_year : int;
        year : int;
        loss : float }

    let generate_events n the_year = 
        let rnd = System.Random()
        let single_event day = {day_of_year = day; year = the_year; loss = rnd.NextDouble()*(20.0*1000.0*1000.0) }
        in
            [1..n] 
            |> List.map(fun x -> rnd.Next(365) + 1 )
            |> List.map(fun d -> single_event(d) )
            |> List.sortBy(fun ev -> ev.day_of_year)