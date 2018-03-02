module Util.Testing

    #if FABLE_COMPILER
    open Fable.Core.Testing

    let testList (name : string) (tests: seq<'b>) = name, tests
    let testCase (msg : string) (test : obj -> unit) = msg, test

    let equal expected actual: unit =
        Assert.AreEqual(expected, actual)

    type Test = string * seq<string * seq<string * (obj -> unit)>>
    #else
    open Expecto

    let testList name tests = testList name tests
    let testCase msg test = testCase msg test

    let equal expected actual: unit =
        Expect.equal actual expected ""

    type Test = Expecto.Test
    #endif
