module Demos.Main

type IExposeApi =
    abstract DebouncerDemo: string -> unit
    abstract ToastDemo: string -> unit

let exposeApi =
    { new IExposeApi with
        member __.DebouncerDemo (id :string) = Debouncer.start id
        member __.ToastDemo (id :string) = Toast.start id
    }

open Fable.Core.JsInterop

Fable.Import.Browser.window?Demos <- exposeApi
