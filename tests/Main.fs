module Tests.All

open Fable.Core.JsInterop

// This is necessary to make webpack collect all test files
importSideEffects "./Fable.Json.Tests.fs"
