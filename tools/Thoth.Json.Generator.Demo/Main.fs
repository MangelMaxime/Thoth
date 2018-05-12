module Main

module Extra =

    type Address =
        { Street : string
          PostalCode : string
          Country : string }

type User =
    { Firstname : string
      Surname : string
      Age : int
      Roles : string list
      Rating : float
      Address : Extra.Address }

type Article =
    { Title : string
      Description : string
      Tags : string array
      Author : User }

type EmptyClass() =
    do ()

let emptyFunction _ = ()
