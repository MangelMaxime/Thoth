#r "paket: groupref netcorebuild //"
#load ".fake/build.fsx/intellisense.fsx"
#if !FAKE
  #r "./packages/netcorebuild/NETStandard.Library.NETFramework/build/net461/lib/netstandard.dll"
#endif

#nowarn "52"

open System
open System.IO
open System.Text.RegularExpressions
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.Tools.Git
open Fake.JavaScript

let versionFromGlobalJson : DotNet.CliInstallOptions -> DotNet.CliInstallOptions = (fun o ->
        { o with Version = DotNet.Version (DotNet.getSDKVersionFromGlobalJson()) }
    )

let dotnetSdk = lazy DotNet.install versionFromGlobalJson
let inline dtntWorkDir wd =
    DotNet.Options.lift dotnetSdk.Value
    >> DotNet.Options.withWorkingDirectory wd

let srcFiles =
    !! "./src/Thoth.Json/Thoth.Json.fsproj"
    ++ "./src/Thoth.Json.Net/Thoth.Json.Net.fsproj"
    ++ "./src/Thoth.Json.Giraffe/Thoth.Json.Giraffe.fsproj"
    ++ "./src/Thoth.Elmish/Thoth.Elmish.fsproj"
    ++ "./src/Thoth.Elmish.Toast/Thoth.Elmish.Toast.fsproj"
    ++ "./src/Thoth.Elmish.FormBuilder/Thoth.Elmish.FormBuilder.fsproj"
    ++ "./src/Thoth.Elmish.FormBuilder.BasicFields/Thoth.Elmish.FormBuilder.BasicFields.fsproj"

let fableTestsGlob = "tests/fable/**/*.fsproj"
let dotnetTestsGlob = "tests/dotnet/**/*.fsproj"
let docFile = "./docs/Docs.fsproj"

let root = __SOURCE_DIRECTORY__

module Util =

    let visitFile (visitor: string -> string) (fileName : string) =
        File.ReadAllLines(fileName)
        |> Array.map (visitor)
        |> fun lines -> File.WriteAllLines(fileName, lines)

    let replaceLines (replacer: string -> Match -> string option) (reg: Regex) (fileName: string) =
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

let run (cmd:string) dir args  =
    Command.RawCommand(cmd, Arguments.OfArgs args)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory dir
    |> Proc.run
    |> ignore


    // if Process.execSimple (fun info ->
    //     { info with
    //         FileName = cmd
    //         WorkingDirectory =
    //             if not (String.IsNullOrWhiteSpace dir) then dir else info.WorkingDirectory
    //         Arguments = args
    //     }
    // ) TimeSpan.MaxValue <> 0 then
    //     failwithf "Error while running '%s' with args: %s " cmd args

let mono workingDir args =
    Command.RawCommand("mono", Arguments.OfArgs args)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> Proc.run
    |> ignore

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "tests/**/bin"
    ++ "tests/**/obj"
    ++ "docs/**/bin"
    ++ "docs/**/obj"
    ++ "docs/**/build"
    ++ "docs/scss/extra"
    ++ "docs/public"
    ++ "demos/Thoth.Elmish.Demo/bin"
    ++ "demos/Thoth.Elmish.Demo/obj"
    ++ "demos/Thoth.Elmish.Demo/output"
    |> Shell.cleanDirs
)

Target.create "YarnInstall"(fun _ ->
    Yarn.install id
    Yarn.install (fun o -> { o with WorkingDirectory = "./docs/" })
)

Target.create "DotnetRestore" (fun _ ->
    srcFiles
    ++ dotnetTestsGlob
    ++ docFile
    |> Seq.iter (fun proj ->
        DotNet.restore id proj
))


// let dotnet workingDir command args =
//     let result =
//         DotNet.exec (fun p ->
//                 { p with WorkingDirectory = workingDir
//                          DotNetCliPath = "dotnet" } )
//             command
//             args

//     if not result.OK then failwithf "dotnet failed with code %i" result.ExitCode

let build project framework =
    DotNet.build (fun p ->
        { p with Framework = Some framework } ) project

Target.create "MochaTest" (fun _ ->
    !! fableTestsGlob
    |> Seq.iter(fun proj ->
        let projDir = proj |> Path.getDirectory
        let configFile = projDir </> "splitter.config.js"
        //Compile to JS
        Yarn.exec ("fable-splitter -c " + configFile) id

        //Run mocha tests
        //TODO: Seems latest fable-splitter outputs tests Main file to "tests" subfolder, bug?
        let projDirOutput = projDir </> "bin/tests"
        Yarn.exec ("run mocha " + projDirOutput) id
    )
)

let testNetFrameworkDir = root </> "tests" </> "dotnet" </> "bin" </> "Release" </> "net461"
let testNetCoreDir = root </> "tests" </> "dotnet" </> "bin" </> "Release" </> "netcoreapp2.0"

Target.create "ExpectoTest" (fun _ ->
    build "tests/dotnet/Thoth.Tests.fsproj" "netcoreapp2.0"
    build "tests/dotnet/Thoth.Tests.fsproj" "net461"

    if Environment.isUnix then
        mono testNetFrameworkDir [ "Thoth.Tests.exe" ]
    else
        run (testNetFrameworkDir </> "Thoth.Tests.exe") root []

    let result = DotNet.exec (dtntWorkDir testNetCoreDir) "" "Thoth.Tests.dll"

    if not result.OK then failwithf "Expecto for netcore tests failed."
)

let docs = root </> "docs"
let docsContent = docs </> "src" </> "Content"
let buildMain = docs </> "build" </> "src" </> "Main.js"

let buildSass _ =
    Yarn.exec "run node-sass --output-style compressed --output docs/public/ docs/scss/main.scss" id

let applyAutoPrefixer _ =
    Yarn.exec "run postcss docs/public/main.css --use autoprefixer -o docs/public/main.css" id

Target.create "Docs.Watch" (fun _ ->
    use watcher = new FileSystemWatcher(docsContent, "*.md")
    watcher.IncludeSubdirectories <- true
    watcher.EnableRaisingEvents <- true

    watcher.Changed.Add(fun _ ->
        Command.RawCommand("node", Arguments.OfArgs [ buildMain ])
        |> CreateProcess.fromCommand
        |> Proc.run
        |> ignore
    )

    // Make sure the style is generated
    // Watch mode of node-sass don't trigger a first build
    buildSass ()

    !! docFile
    |> Seq.iter (fun proj ->
        let projDir = proj |> Path.getDirectory

        [ async {
            let result = DotNet.exec (dtntWorkDir projDir) "fable" "yarn-run fable-splitter --port free -- -c docs/splitter.config.js -w"

            if not result.OK then failwithf "Docs.Watch fable watch mode failed."
          }
          async {
            Yarn.exec "run node-sass --output-style compressed --watch --output docs/public/ docs/scss/main.scss" id
          }
          async {
            Yarn.exec "run http-server -c-1 docs/public" id
          }
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
    )
)

Target.create "Docs.Setup" (fun _ ->
    // Make sure directories exist
    Directory.ensure "./docs/scss/extra/highlight.js/"
    Directory.ensure "./docs/public/demos/"

    // Copy files from node_modules allow us to manage them via yarn
    Shell.copyDir "./docs/public/fonts" "./node_modules/font-awesome/fonts" (fun _ -> true)
    // Copy demos file
    Shell.copyDir "./docs/public/demos" "./demos/Thoth.Elmish.Demo/output/" (fun _ -> true)

    DotNet.restore id docFile
)

Target.create "Docs.Build" (fun _ ->
    !! docFile
    |> Seq.iter (fun proj ->
        let projDir = proj |> Path.getDirectory

        let result = DotNet.exec (dtntWorkDir projDir) "fable" "yarn-run fable-splitter --port free -- -c docs/splitter.config.js -p"

        if not result.OK then failwithf "Fable build failed."
        buildSass ()
        applyAutoPrefixer ()
    )
)

Target.create "Watch" (fun _ ->
    !! fableTestsGlob
    |> Seq.iter(fun proj ->
        let projDir = proj |> Path.getDirectory
        let configFile = projDir </> "splitter.config.js"
        //Compile to JS
        Yarn.exec ("fable-splitter -w -c " + configFile) id
    )
)

Target.create "Build.Demos" (fun _ ->
    Yarn.install (fun o -> { o with WorkingDirectory = "./demos/Thoth.Elmish.Demo/" })

    DotNet.restore (dtntWorkDir (root </> "demos" </> "Thoth.Elmish.Demo")) ""

    let result =
        DotNet.exec
            (dtntWorkDir (root </> "demos" </> "Thoth.Elmish.Demo"))
            "fable"
            "webpack -- -p"

    if not result.OK then failwithf "Fable build failed for demos."
)

Target.create "Watch.Demos" (fun _ ->
    Yarn.install (fun o -> { o with WorkingDirectory = "./demos/Thoth.Elmish.Demo/" })

    DotNet.restore (dtntWorkDir (root </> "demos" </> "Thoth.Elmish.Demo")) ""

    let result =
        DotNet.exec
            (dtntWorkDir (root </> "demos" </> "Thoth.Elmish.Demo"))
            "fable"
            "webpack-dev-server"

    if not result.OK then failwithf "Fable build failed for demos."
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

        DotNet.pack (fun p ->
            { p with
                Configuration = DotNet.Release
                Common = { p.Common with DotNetCliPath = "dotnet" } } )
            projFile

        let files =
            Directory.GetFiles(projDir </> "bin" </> "Release", "*.nupkg")
            |> Array.find (fun nupkg -> nupkg.Contains(releaseNotes.NugetVersion))
            |> fun x -> [x]

        Paket.pushFiles (fun o ->
            { o with ApiKey = nugetKey
                     PublishUrl = "https://www.nuget.org/api/v2/package" })
            files


Target.create "Publish" (fun _ ->
    srcFiles
    |> Seq.iter(fun s ->
        let projFile = s
        let projDir = IO.Path.GetDirectoryName(projFile)
        let release = projDir </> "RELEASE_NOTES.md" |> ReleaseNotes.load
        pushNuget release projFile
    )
)

// Where to push generated documentation
let githubLink = "git@github.com:MangelMaxime/Thoth.git"
let publishBranch = "gh-pages"
let repoRoot = __SOURCE_DIRECTORY__
let temp = repoRoot </> "temp"

Target.create "Docs.Publish" (fun _ ->
    // Clean the repo before cloning this avoid potential conflicts
    Shell.cleanDir temp
    Repository.cloneSingleBranch "" githubLink publishBranch temp

    // Copy new files
    Shell.copyRecursive "docs/public" temp true |> printfn "%A"

    // Deploy the new site
    Staging.stageAll temp
    Commit.exec temp (sprintf "Update site (%s)" (DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")))
    Branches.push temp
)

"Clean"
    ==> "YarnInstall"
    ==> "DotnetRestore"
    ==> "MochaTest"
    ==> "ExpectoTest"
    ==> "Publish"

"DotnetRestore"
    ==> "Watch"

"Docs.Setup"
    <== [ "DotnetRestore" ]

"Build.Demos"
    ==> "Docs.Setup"

"Docs.Build"
    <== [ "Docs.Setup" ]

"Docs.Watch"
    <== [ "Docs.Setup" ]

"Docs.Build"
    ==> "Docs.Publish"

Target.runOrDefault "ExpectoTest"
