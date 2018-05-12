module VSCodeMechanic

open Fable.Core.JsInterop
open Fable.PowerPack
open VSCode
open Model
open System.Collections.Generic

let state = Dictionary<string, Project>()
let outputChannel = Vscode.window.createOutputChannel "Mechanic"

let private askFsProjs (projFiles : string seq) = promise {
    return!
        Vscode.window.showQuickPick(!^ (projFiles |> ResizeArray))
        |> Promise.fromThenable
}

[<RequireQualifiedAccess>]
module TextEditor =

    let (|IsFsprojFile|_|) (textEditor: Vscode.TextEditor) =
        if textEditor.document.fileName.EndsWith(".fsproj") then
            Some textEditor
        else
            None

    let (|IsFsFile|_|) (textEditor: Vscode.TextEditor) =
        if textEditor.document.fileName.EndsWith(".fs") then
            Some textEditor
        else
            None

let private runInScope (input: ProjectExplorerModel option) = promise {
    match input with
    | Some (ProjectExplorerModel.Project (p,_,_,_,_,_,_)) ->
        do! Mechanic.run p outputChannel
    | _ ->
        // Decision based on the active file
        match Vscode.window.activeTextEditor with
        // Active file is an .fs file, try to find the associated project
        | Some (TextEditor.IsFsFile textEditor) ->
            let currentFile = IO.normalizePath textEditor.document.uri.path
            let projectOption =
                state
                |> Seq.map (fun keyValue -> keyValue.Value)
                |> Seq.tryFind (fun (project : Project) ->
                    project.Files 
                    |> List.map IO.normalizePath
                    |> List.contains currentFile)

            match projectOption with
            | None ->
                // Current version of Mechanic only work if the .fs file is inside a project
                let error = "Mechanic not run, there is no project associated with the active file: " + currentFile
                do! Vscode.window.showWarningMessage(error, [] |> ResizeArray)
                    |> Promise.fromThenable
                    |> Promise.ignore
            | Some project ->
                do! Mechanic.run project.Project outputChannel

        // Active file is an fsproj so we pass it to Mechanic
        | Some (TextEditor.IsFsprojFile textEditor) ->
            do! Mechanic.run textEditor.document.uri.path outputChannel

        // If no active file or not an known type, then we fallback to default case
        | Some _ | None ->
            match state.Count with
            | 0 ->
                // We didn't find an fsproj in the workspace directory
                // This case should not happen since we activate the extension only if an fsproj is found
                do! Vscode.window.showWarningMessage("Mechanic not run, no fsproj file found in the workspace", [] |> ResizeArray)
                    |> Promise.fromThenable
                    |> Promise.ignore
            | 1 ->
                // Only one fsproj found, run mechanic directly
                let projFile = state |> Seq.head |> (fun keyValue -> keyValue.Value.Project)
                do! Mechanic.run projFile outputChannel
            | _ ->
                // Several fsproj found, ask the users which one to use
                let! projFile = askFsProjs state.Keys

                match projFile with
                | None ->
                    do! Vscode.window.showInformationMessage("No project selected, command canceled", [] |> ResizeArray)
                        |> Promise.fromThenable
                        |> Promise.ignore
                | Some file ->
                    do! Mechanic.run file outputChannel
}

let activate (context : Vscode.ExtensionContext) =
    let ext = Vscode.extensions.getExtension<IonideApi> "Ionide.Ionide-fsharp"
    match ext with
    | None ->
        Vscode.window.showWarningMessage("Mechanic couldn't be activated, Ionide-fsharp is required", [] |> ResizeArray)
        |> ignore
    | Some ext ->
        ext.exports.ProjectLoadedEvent $ (fun (project : Project) ->
            state.[project.Project] <- project
        ) |> ignore

        Vscode.commands.registerCommand("mechanic.run", fun input ->
            let m = unbox<ProjectExplorerModel option> input
            Promise.start (runInScope m)
            None
        )
        |> context.subscriptions.Add
