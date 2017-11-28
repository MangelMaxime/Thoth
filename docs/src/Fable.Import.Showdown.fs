namespace Fable.Import

open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS

module Showdown =
    type [<AllowNullLiteral>] Extension =
        abstract ``type``: string with get, set

    and [<AllowNullLiteral>] RegexReplaceExtension =
        inherit Extension
        abstract regex: U2<string, Regex> option with get, set
        abstract replace: obj option with get, set

    and [<AllowNullLiteral>] FilterExtension =
        inherit Extension
        abstract filter: Func<string, Converter, ConverterOptions, string> option with get, set

    and [<AllowNullLiteral>] ShowdownExtension =
        inherit RegexReplaceExtension
        inherit FilterExtension


    and [<AllowNullLiteral>] ConverterExtensions =
        abstract language: ResizeArray<ShowdownExtension> with get, set
        abstract output: ResizeArray<ShowdownExtension> with get, set

    and [<AllowNullLiteral>] ShowdownOptions =
        abstract omitExtraWLInCodeBlocks: bool option with get, set
        abstract noHeaderId: bool option with get, set
        abstract customizedHeaderId: bool option with get, set
        abstract ghCompatibleHeaderId: bool option with get, set
        abstract prefixHeaderId: U2<string, bool> option with get, set
        abstract parseImgDimensions: bool option with get, set
        abstract headerLevelStart: float option with get, set
        abstract simplifiedAutoLink: bool option with get, set
        abstract excludeTrailingPunctuationFromURLs: bool option with get, set
        abstract literalMidWordUnderscores: bool option with get, set
        abstract strikethrough: bool option with get, set
        abstract tables: bool option with get, set
        abstract tablesHeaderId: bool option with get, set
        abstract ghCodeBlocks: bool option with get, set
        abstract tasklists: bool option with get, set
        abstract smoothLivePreview: bool option with get, set
        abstract smartIndentationFix: bool option with get, set
        abstract disableForced4SpacesIndentedSublists: bool option with get, set
        abstract simpleLineBreaks: bool option with get, set
        abstract requireSpaceBeforeHeadingText: bool option with get, set
        abstract ghMentions: bool option with get, set
        abstract ghMentionsLink: string option with get, set
        abstract openLinksInNewWindow: bool option with get, set
        abstract backslashEscapesHTMLTags: bool option with get, set

    and [<AllowNullLiteral>] ConverterOptions =
        inherit ShowdownOptions
        abstract extensions: U2<string, ResizeArray<string>> option with get, set

    and [<AllowNullLiteral>] Converter =
        abstract makeHtml: text: string -> string
        abstract setOption: optionKey: string * value: string -> unit
        abstract getOption: optionKey: string -> obj
        abstract getOptions: unit -> ShowdownOptions
        abstract addExtension: extension: ShowdownExtension * name: string -> unit
        abstract useExtension: extensionName: string -> unit
        abstract getAllExtensions: unit -> ConverterExtensions
        abstract removeExtension: extensions: U2<ResizeArray<ShowdownExtension>, ShowdownExtension> -> unit
        abstract setFlavor: name: string -> unit

    and [<AllowNullLiteral>] ConverterStatic =
        [<Emit("new $0($1...)")>] abstract Create: ?converterOptions: ConverterOptions -> Converter

    and [<AllowNullLiteral>] extensionsType =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: name: string -> ShowdownExtension with get, set

    type [<Import("*","showdown")>] Globals =
        static member Converter with get(): ConverterStatic = jsNative and set(v: ConverterStatic): unit = jsNative
        static member extensions with get(): extensionsType = jsNative and set(v: extensionsType): unit = jsNative
        static member setOption(optionKey: string, value: string): unit = jsNative
        static member getOption(optionKey: string): obj = jsNative
        static member getOptions(): ShowdownOptions = jsNative
        static member resetOptions(): unit = jsNative
        static member getDefaultOptions(): ShowdownOptions = jsNative
        static member extension(name: string, extension: U3<Func<unit, ShowdownExtension>, Func<unit, ResizeArray<ShowdownExtension>>, ShowdownExtension>): unit = jsNative
        static member getAllExtensions(): obj = jsNative
        static member removeExtension(name: string): unit = jsNative
        static member resetExtensions(): unit = jsNative
        static member setFlavor(name: string): unit = jsNative
