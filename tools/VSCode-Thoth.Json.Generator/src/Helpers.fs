[<AutoOpen>]
module Helpers

    module Promise =
        open VSCode
        open Fable.Import
        open Fable.PowerPack

        let inline fromThenable (thenable : Thenable<'T>) : JS.Promise<'T> = unbox<JS.Promise<'T>> thenable

        let inline toThenable (promise : JS.Promise<'T>) : Thenable<'T> = unbox<Thenable<'T>> promise

        let inline ignore promise = Promise.map ignore promise

    module IO =
        let dirSeparator = Fable.Import.Node.Exports.path.sep

        let inline normalizePath(path:string) =
            (if path.Contains(":") then path.TrimStart '/' else path)
              .Replace("\\",dirSeparator)
              .Replace("/",dirSeparator).TrimEnd('/').TrimEnd('\\')
              .Replace(dirSeparator + "." + dirSeparator, dirSeparator)

    module Process =

        open Fable.Import
        open Fable.Core.JsInterop
        open Fable.PowerPack

        let onExit (cb : int -> unit) (proc : Node.ChildProcess.ChildProcess) =
            proc.on("exit", (fun code -> code.ToString() |> int |> cb)) |> ignore
            proc

        let onError (cb : _ -> unit) (proc : Node.ChildProcess.ChildProcess) =
            proc.on("error", cb) |> ignore
            proc

        let onOutput (f : Node.Buffer.Buffer -> _) (proc : Node.ChildProcess.ChildProcess) =
            proc.stdout?on $ ("data", f |> unbox) |> ignore
            proc

        let onErrorOutput (f : Node.Buffer.Buffer -> _) (proc : Node.ChildProcess.ChildProcess) =
            proc.stderr?on $ ("data", f |> unbox) |> ignore
            proc

        let toPromise (proc : Node.ChildProcess.ChildProcess) =
            Promise.create (fun (resolve : int -> unit) reject ->

                proc
                |> onExit resolve
                |> onError reject
                |> ignore
            )
