module Model

type ProjectFilePath = string
type SourceFilePath = string
type ProjectReferencePath = string

[<RequireQualifiedAccess>]
type ProjectResponseInfo =
    | DotnetSdk of ProjectResponseInfoDotnetSdk
    | Verbose
    | ProjectJson

and ProjectResponseInfoDotnetSdk = {
    IsTestProject: bool
    Configuration: string
    IsPackable: bool
    TargetFramework: string
    TargetFrameworkIdentifier: string
    TargetFrameworkVersion: string
    RestoreSuccess: bool
    TargetFrameworks: string list
    RunCmd: RunCmd option
    IsPublishable: bool option }

and [<RequireQualifiedAccess>] RunCmd = { Command: string; Arguments: string }

type Project = {
    Project: ProjectFilePath
    Files: SourceFilePath list
    Output: string
    References: ProjectReferencePath list
    Logs: Map<string, string>
    OutputType: string
    Info: ProjectResponseInfo
    AdditionalInfo: Map<string, string>
}

[<RequireQualifiedAccess>]
type ProjectExplorerModel =
    | Workspace of Projects : ProjectExplorerModel list
    | Solution of path: string * name: string * items: ProjectExplorerModel list
    | WorkspaceFolder of name: string * items: ProjectExplorerModel list
    | ReferenceList of References: ProjectExplorerModel list * projectPath : string
    | ProjectReferencesList of Projects : ProjectExplorerModel list * ProjectPath : string
    | ProjectNotLoaded of path: string * name: string
    | ProjectLoading of path: string * name: string
    | ProjectFailedToLoad of path: string * name: string * error: string
    | ProjectNotRestored of path: string * name: string * error: string
    | Project of path: string * name: string * Files: ProjectExplorerModel list * ProjectReferencesList : ProjectExplorerModel  * ReferenceList: ProjectExplorerModel * isExe : bool * project : Project
    | Folder of name : string * path: string * Files : ProjectExplorerModel list
    | File of path: string * name: string * projectPath : string
    | Reference of path: string * name: string * projectPath : string
    | ProjectReference of path: string * name: string * projectPath : string

type IonideApi = {
    ProjectLoadedEvent: Event<Project>
}