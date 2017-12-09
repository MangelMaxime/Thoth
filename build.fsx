(* -- Fake Dependencies paket.dependencies
file ./paket.dependencies
group netcorebuild
-- Fake Dependencies -- *)

#if !DOTNETCORE
#I "./packages/netcorebuild"
#r "NETStandard.Library.NETFramework/build/net461/lib/netstandard.dll"
#r "Fake.DotNet.Paket/lib/netstandard2.0/Fake.DotNet.Paket.dll"
#r "Fake.IO.FileSystem/lib/netstandard2.0/Fake.IO.FileSystem.dll"
#r "Fake.Core.Globbing/lib/netstandard2.0/Fake.Core.Globbing.dll"
#r "Fake.Core.String/lib/netstandard2.0/Fake.Core.String.dll"
#r "Fake.Core.Target/lib/netstandard2.0/Fake.Core.Target.dll"
#r "Fake.DotNet.Cli/lib/netstandard2.0/Fake.DotNet.Cli.dll"
#r "Fake.Core.ReleaseNotes/lib/netstandard2.0/Fake.Core.ReleaseNotes.dll"
#r "Fake.Core.Process/lib/netstandard2.0/Fake.Core.Process.dll"
#r "Fake.Core.Environment/lib/netstandard2.0/Fake.Core.Environment.dll"
#r "Fake.Tools.Git/lib/netstandard2.0/Fake.Tools.Git.dll"
#endif

open System
open System.IO
open System.Text.RegularExpressions
open Fake.Core.Globbing.Operators
open Fake.Core
open Fake.Core.Process
open Fake.Core.TargetOperators
open Fake.DotNet.Cli
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Core.String
open Fake.Tools.Git

#if MONO
// prevent incorrect output encoding (e.g. https://github.com/fsharp/FAKE/issues/1196)
System.Console.OutputEncoding <- System.Text.Encoding.UTF8
#endif

let dotnetCommon = { DotnetOptions.Default with DotnetCliPath = "dotnet" }

let srcFiles =
    !! "./src/Thot.Json/Thot.Json.fsproj"
let testsGlob = "tests/**/*.fsproj"
let docsGlob = "docs/**/*.fsproj"

module Util =

    let visitFile (visitor: string->string) (fileName : string) =
        File.ReadAllLines(fileName)
        |> Array.map (visitor)
        |> fun lines -> File.WriteAllLines(fileName, lines)

    let replaceLines (replacer: string->Match->string option) (reg: Regex) (fileName: string) =
        fileName |> visitFile (fun line ->
            let m = reg.Match(line)
            if not m.Success
            then line
            else
                match replacer line m with
                | None -> line
                | Some newLine -> newLine)

// Module to print colored message in the console
module Logger =
    let consoleColor (fc : ConsoleColor) =
        let current = Console.ForegroundColor
        Console.ForegroundColor <- fc
        { new IDisposable with
              member x.Dispose() = Console.ForegroundColor <- current }

    let warn str = Printf.kprintf (fun s -> use c = consoleColor ConsoleColor.DarkYellow in printf "%s" s) str
    let warnfn str = Printf.kprintf (fun s -> use c = consoleColor ConsoleColor.DarkYellow in printfn "%s" s) str
    let error str = Printf.kprintf (fun s -> use c = consoleColor ConsoleColor.Red in printf "%s" s) str
    let errorfn str = Printf.kprintf (fun s -> use c = consoleColor ConsoleColor.Red in printfn "%s" s) str

let yarn args =
    ExecProcess
        (fun info ->
            { info with
                FileName = "yarn"
                Arguments = args
            }
        )
        (TimeSpan.FromMinutes 10.)
    |> ignore

Target.Create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "docs/**/bin"
    ++ "docs/**/obj"
    ++ "docs/**/build"
    ++ "docs/scss/extra"
    ++ "docs/public"
    |> Shell.CleanDirs
)

Target.Create "YarnInstall"(fun _ ->
    yarn "install"
)

Target.Create "DotnetRestore" (fun _ ->
    srcFiles
    ++ testsGlob
    ++ docsGlob
    |> Seq.iter (fun proj ->
        DotnetRestore (fun c ->
            { c with
                Common = dotnetCommon
            }) proj
))

Target.Create "DotnetBuild" (fun _ ->
    srcFiles
    |> Seq.iter (fun proj ->
        DotnetCompile (fun c ->
            { c with
                Common = dotnetCommon
            }) proj
))


let dotnet workingDir args =
    Dotnet
        { dotnetCommon with
            WorkingDirectory = workingDir }
         args
    |> ignore

let mocha args =
    yarn (sprintf "run mocha %s" args)

Target.Create "MochaTest" (fun _ ->
    !! testsGlob
    |> Seq.iter(fun proj ->
        let projDir = proj |> Path.getDirectory
        //Compile to JS
        dotnet projDir "fable yarn-run rollup --port free -- -c tests/rollup.config.js "

        //Run mocha tests
        let projDirOutput = projDir </> "bin"
        mocha projDirOutput
    )
)

Target.Create "DotnetPack" (fun _ ->
    srcFiles
    |> Seq.iter(fun projFile ->
        let projDir = IO.Path.GetDirectoryName(projFile)
        let release = projDir </> "RELEASE_NOTES.md" |> ReleaseNotes.LoadReleaseNotes
        let releaseNotes = sprintf "/p:PackageReleaseNotes=\"%s\"" (toLines release.Notes)

        DotnetPack (fun p ->
            { p with
                Configuration = Release
                Common = { dotnetCommon with CustomParams = Some releaseNotes } } )
            projFile
    )
)

let root = __SOURCE_DIRECTORY__
let docs = root </> "docs"
let docsContent = docs </> "src" </> "Content"
let buildMain = docs </> "build" </> "src" </> "Main.js"

let execNPX args =
    ExecProcess
        (fun info ->
            { info with
                FileName = "npx"
                Arguments = args
            }
        )
        (TimeSpan.FromSeconds 30.)
    |> ignore

let buildSass _ =
    execNPX "node-sass --output-style compressed --output docs/public/ docs/scss/main.scss"

let applyAutoPrefixer _ =
    execNPX " postcss docs/public/main.css --use autoprefixer -o docs/public/main.css"

Target.Create "Docs.Watch" (fun _ ->
    use watcher = new FileSystemWatcher(docsContent, "*.md")
    watcher.IncludeSubdirectories <- true
    watcher.EnableRaisingEvents <- true

    watcher.Changed.Add(fun _ ->
        ExecProcess
            (fun info ->
                { info with
                    FileName = "node"
                    Arguments = buildMain }
            )
            (TimeSpan.FromSeconds 30.) |> ignore
    )

    // Make sure the style is generated
    // Watch mode of node-sass don't trigger a first build
    buildSass ()

    !! docsGlob
    |> Seq.iter (fun proj ->
        let projDir = proj |> Path.getDirectory

        [ async {
            dotnet projDir "fable yarn-run fable-splitter --port free -- -c docs/splitter.config.js -w"
          }
          async {
                execNPX "node-sass --output-style compressed --watch --output docs/public/ docs/scss/main.scss"
          }
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
    )
)

Target.Create "Docs.Setup" (fun _ ->
    // Make sure directories exist
    Directory.ensure "./docs/scss/extra/highlight.js/"

    // Copy files from node_modules allow us to manage them via yarn
    Shell.CopyDir "./docs/public/fonts" "./node_modules/font-awesome/fonts" (fun _ -> true)
    Shell.CopyFile "./docs/scss/extra/highlight.js/atom-one-light.css" "./node_modules/highlight.js/styles/atom-one-light.css"
)

Target.Create "Docs.Build" (fun _ ->
    !! docsGlob
    |> Seq.iter (fun proj ->
        let projDir = proj |> Path.getDirectory

        dotnet projDir "fable yarn-run fable-splitter --port free -- -c docs/splitter.config.js -p"
        buildSass ()
        applyAutoPrefixer ()
    )
)

Target.Create "Watch" (fun _ ->
    !! testsGlob
    |> Seq.iter(fun proj ->
        let projDir = proj |> Path.getDirectory
        //Compile to JS
        dotnet projDir "fable webpack --port free -- --watch"
    )
)

let needsPublishing (versionRegex: Regex) (releaseNotes: ReleaseNotes.ReleaseNotes) projFile =
    printfn "Project: %s" projFile
    if releaseNotes.NugetVersion.ToUpper().EndsWith("NEXT")
    then
        Logger.warnfn "Version in Release Notes ends with NEXT, don't publish yet."
        false
    else
        File.ReadLines(projFile)
        |> Seq.tryPick (fun line ->
            let m = versionRegex.Match(line)
            if m.Success then Some m else None)
        |> function
            | None -> failwith "Couldn't find version in project file"
            | Some m ->
                let sameVersion = m.Groups.[1].Value = releaseNotes.NugetVersion
                if sameVersion then
                    Logger.warnfn "Already version %s, no need to publish." releaseNotes.NugetVersion
                not sameVersion

let pushNuget (releaseNotes: ReleaseNotes.ReleaseNotes) (projFile: string) =
    let versionRegex = Regex("<Version>(.*?)</Version>", RegexOptions.IgnoreCase)

    if needsPublishing versionRegex releaseNotes projFile then
        let projDir = Path.GetDirectoryName(projFile)
        let nugetKey =
            match Environment.environVarOrNone "NUGET_KEY" with
            | Some nugetKey -> nugetKey
            | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"

        (versionRegex, projFile) ||> Util.replaceLines (fun line _ ->
            versionRegex.Replace(line, "<Version>" + releaseNotes.NugetVersion + "</Version>") |> Some)

        // Directory.GetFiles(projDir </> "bin" </> "Release", "*.nupkg")
        // |> Array.find (fun nupkg -> nupkg.Contains(releaseNotes.NugetVersion))
        // |> (fun nupkg ->
        //         Paket.Push (fun p ->
        //                         { p with
        //                             PublishUrl
        //                             ApiKey = nugetKey } )
        //     )

// { ToolPath : string
//       TimeOut : TimeSpan
//       PublishUrl : string
//       EndPoint : string
//       WorkingDir : string
//       DegreeOfParallelism : int
// ApiKey : string }

            // (Path.GetFullPath nupkg, nugetKey)
            // ||> sprintf "nuget push %s -s nuget.org -k %s"
            // |> DotNetCli.RunCommand (fun c ->
            //                                 { c with ToolPath = dotnetExePath })


Target.Create "Publish" (fun _ ->
    srcFiles
    |> Seq.iter(fun s ->
        let projFile = s
        let projDir = IO.Path.GetDirectoryName(projFile)
        let release = projDir </> "RELEASE_NOTES.md" |> ReleaseNotes.LoadReleaseNotes
        pushNuget release projFile
    )
)

// Where to push generated documentation
let githubLink = "git@github.com:MangelMaxime/thot.git"
let publishBranch = "gh-pages"
let repoRoot = __SOURCE_DIRECTORY__
let temp = repoRoot </> "temp"

Target.Create "PublishDocs" (fun _ ->
    Shell.CleanDir temp
    Repository.cloneSingleBranch "" githubLink publishBranch temp

    Shell.CopyRecursive "docs/public" temp true |> printfn "%A"
    Staging.StageAll temp
    Commit.Commit temp (sprintf "Update site (%s)" (DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")))
    Branches.push temp
)

// Target "Release" (fun _ ->

//     if Git.Information.getBranchName "" <> "master" then failwith "Not on master"

//     StageAll ""
//     Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
//     Branches.push ""

//     Branches.tag "" release.NugetVersion
//     Branches.pushTag "" "origin" release.NugetVersion
// )

"Clean"
    ==> "YarnInstall"
    ==> "DotnetRestore"
    ==> "DotnetBuild"
    ==> "MochaTest"
    ==> "DotnetPack"
    ==> "Publish"
    // ==> "Release"

"Watch"
    <== [ "DotnetBuild" ]

"Docs.Build"
    <== [ "DotnetRestore"
          "Docs.Setup" ]

"Docs.Watch"
    <== [ "Docs.Setup" ]

"PublishDocs"
    ==> "Docs.Build"

Target.RunOrDefault "DotnetPack"
