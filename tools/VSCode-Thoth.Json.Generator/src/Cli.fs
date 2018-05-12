module Mechanic

open Fable.Core.JsInterop
open Fable.Import
open VSCode
open Fable.PowerPack

let inline (</>) a b = a + Node.Exports.path.sep + b

let pluginPath =
    match Vscode.extensions.getExtension "Ionide.mechanic" with
    | Some extension -> extension.extensionPath
    | None -> failwith "Failed to find `Ionide.mechanic` extension"

let pluginBinPath = pluginPath </> "bin" </> "Mechanic"

let private raw (projectFile : string) (outputChannel : Vscode.OutputChannel) =
    let options =
        createObj [
            "cwd" ==> Vscode.workspace.rootPath
            "detached" ==> true
        ]

    let prms =
        seq { yield pluginBinPath </> "mech.dll"
              yield projectFile } |> ResizeArray

    let progressOptions = jsOptions<Vscode.ProgressOptions> (fun o ->
        o.location <- Vscode.ProgressLocation.Window
    )

    Vscode.window.withProgress(progressOptions, fun p ->
        // Bindings don't have Vscode.ProgressMessage yet
        let pm =
            createObj [
                "message" ==> "Mechanic is running"
            ]
        p.report pm

        Node.Exports.childProcess.spawn(
            "dotnet",
            prms,
            options)
        |> Process.onOutput(fun e -> e.toString () |> outputChannel.append)
        |> Process.onError (fun e -> e.ToString () |> outputChannel.append)
        |> Process.onErrorOutput(fun e -> e.toString () |> outputChannel.append)
        |> Process.toPromise
        |> Promise.toThenable
    )
    |> Promise.fromThenable

let run projFile outputChannel =
    promise {
        let projFile = IO.normalizePath projFile
        let! result = raw projFile outputChannel
        match result with
        | 0 ->
            return! Vscode.window.showInformationMessage("Mechanic completed. Reordered files in " + projFile, [] |> ResizeArray)
                |> Promise.fromThenable
                |> Promise.ignore

        | _ ->
            let! result = Vscode.window.showErrorMessage("Mechanic failed", [ "Show output" ] |> ResizeArray)
                          |> Promise.fromThenable

            match result with
            | Some "Show output" ->
                outputChannel.show(true)
            | None | Some _ -> ()
    }
