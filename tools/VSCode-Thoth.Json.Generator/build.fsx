open System.IO
// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "FakeLib.dll"
open System
open Fake
open Fake.YarnHelper
open Fake.FileUtils
open Fake.ReleaseNotesHelper
open Fake.Git

// --------------------------------------------------------------------------------------
// Build the Generator project and run it
// --------------------------------------------------------------------------------------

let MechanicCliSource = "paket-files" </> "github.com" </> "fsprojects" </> "Mechanic" </> "src" </> "Mechanic.CommandLine" </> "bin" </> "Release" </> "netcoreapp2.0" </> "publish"
let binTools = "release" </> "bin"
let MechanicDestination = binTools </> "Mechanic"

let gitOwner = "ionide"
let gitHome = "https://github.com/" + gitOwner


// The name of the project on GitHub
let gitName = "ionide-vscode-mechanic"
Target "Clean" (fun _ ->
    CleanDir "./temp"
    CleanDir binTools
    CopyFiles "release" ["README.md"; "LICENSE.md"]
    CopyFile "release/CHANGELOG.md" "RELEASE_NOTES.md"
)

Target "YarnInstall" <| fun () ->
    Yarn (fun p -> { p with Command = Install InstallArgs.PureLockfile })

Target "DotNetRestore" <| fun () ->
    DotNetCli.Restore (fun p -> { p with WorkingDir = "src" } )

let runFable additionalArgs =
    let cmd = "fable webpack -- " + additionalArgs
    DotNetCli.RunCommand (fun p -> { p with WorkingDir = "src" } ) cmd

let runFableNoTimeout additionalArgs =
    let cmd = "fable webpack -- " + additionalArgs
    DotNetCli.RunCommand (fun p -> { p with WorkingDir = "src"
                                            TimeOut = TimeSpan.MaxValue } ) cmd

let releaseNotesData =
    File.ReadAllLines "RELEASE_NOTES.md"
    |> parseAllReleaseNotes

let release = List.head releaseNotesData


Target "Build" (fun _ ->
    // Ideally we would want a production (minized) build but UglifyJS fail on PerMessageDeflate.js as it contains non-ES6 javascript.
    runFable "-p"
)

Target "Watch" (fun _ ->
    runFableNoTimeout "--watch"
)

Target "CopyMechanic" (fun _ ->
    cp_r MechanicCliSource MechanicDestination
)

let run cmd args dir =
    if not <| execProcess( fun info ->
        info.FileName <- cmd
        if not( String.IsNullOrWhiteSpace dir) then
            info.WorkingDirectory <- dir
        info.Arguments <- args
    ) System.TimeSpan.MaxValue then
        failwithf "Error while running '%s' with args: %s" cmd args

let platformTool tool path =
    match isUnix with
    | true -> tool
    | _ ->  match ProcessHelper.tryFindFileOnPath path with
            | None -> failwithf "can't find tool %s on PATH" tool
            | Some v -> v


let vsceTool = lazy (platformTool "vsce" "vsce.cmd")


Target "SetVersion" (fun _ ->
    let fileName = "./release/package.json"
    let lines =
        File.ReadAllLines fileName
        |> Seq.map (fun line ->
            if line.TrimStart().StartsWith("\"version\":") then
                let indent = line.Substring(0,line.IndexOf("\""))
                sprintf "%s\"version\": \"%O\"," indent release.NugetVersion
            else line)
    File.WriteAllLines(fileName,lines)
)

Target "BuildPackage" ( fun _ ->
    killProcess "vsce"
    run vsceTool.Value "package" "release"
    !! "release/*.vsix"
    |> Seq.iter(MoveFile "./temp/")
)

Target "PublishToGallery" ( fun _ ->
    let token =
        match getBuildParam "vsce-token" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "VSCE Token: "

    killProcess "vsce"
    run vsceTool.Value (sprintf "publish --pat %s" token) "release"
)

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "ReleaseGitHub" (fun _ ->
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    let file = !! ("./temp" </> "*.vsix") |> Seq.head

    // release on github
    createClient user pw
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> uploadFile file
    |> releaseDraft
    |> Async.RunSynchronously
)

// --------------------------------------------------------------------------------------
// Run generator by default. Invoke 'build <Target>' to override
// --------------------------------------------------------------------------------------

Target "Default" DoNothing
Target "Release" DoNothing

"Clean"
==> "YarnInstall"
==> "CopyMechanic"
==> "DotNetRestore"
==> "Build"
==> "Default"

"Build"
==> "SetVersion"
==> "BuildPackage"
==> "ReleaseGitHub"
==> "PublishToGallery"
==> "Release"

RunTargetOrDefault "Default"
