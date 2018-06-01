module Demos.Main

type IExposeApi =
    abstract DebouncerDemo: string -> unit

let exposeApi =
    { new IExposeApi with
        member __.DebouncerDemo (id :string) = Debouncer.start id
    }
open Fable.Core.JsInterop

Fable.Import.Browser.window?Demos <- exposeApi
