module Tests.All

open Fable.Core.JsInterop

// This is necessary to make webpack collect all test files
importSideEffects "./Tests.Json.Decode.fs"
importSideEffects "./Tests.Json.Encode.fs"
