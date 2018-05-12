module Output

open System

// helper function to set the console collor and automatically set it back when disposed
let consoleColor (fc : ConsoleColor) =
    let current = Console.ForegroundColor
    Console.ForegroundColor <- fc
    { new IDisposable with
          member x.Dispose() = Console.ForegroundColor <- current }

let cprintfn color str = Printf.kprintf (fun s -> use c = consoleColor color in printfn "%s" s) str

let log fmt = cprintfn ConsoleColor.White fmt

let info fmt = cprintfn ConsoleColor.Blue fmt

let warning fmt = cprintfn ConsoleColor.Yellow fmt

let error fmt = cprintfn ConsoleColor.Red fmt
