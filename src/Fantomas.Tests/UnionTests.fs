﻿module Fantomas.Tests.UnionsTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``enums declaration``() =
    formatSourceString false """
    type FontVariant =
    | [<Description("small-caps")>] SmallCaps = 0""" config
    |> prepend newline
    |> should equal """
type FontVariant = 
    | [<Description("small-caps")>] SmallCaps = 0
"""

[<Test>]
let ``discriminated unions declaration``() =
    formatSourceString false "type X = private | A of AParameters | B" config
    |> prepend newline
    |> should equal """
type X = 
    private
    | A of AParameters
    | B
"""

[<Test>]
let ``enums conversion``() =
    formatSourceString false """
type uColor =
   | Red = 0u
   | Green = 1u
   | Blue = 2u
let col3 = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)""" config
    |> prepend newline
    |> should equal """
type uColor = 
    | Red = 0u
    | Green = 1u
    | Blue = 2u

let col3 = 
    Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)
"""

[<Test>]
let ``discriminated unions with members``() =
    formatSourceString false """
type Type
    = TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    with override this.ToString() =
            match this with
            | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
            | TyVar a -> a
            | TyCon (s, ts) -> s""" config
    |> prepend newline
    |> should equal """
type Type = 
    | TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    override this.ToString() = 
        match this with
        | TyLam(t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
        | TyVar a -> a
        | TyCon(s, ts) -> s
"""

[<Test>]
let ``should keep attributes on union cases``() =
    formatSourceString false """
type Argument = 
  | [<MandatoryAttribute>] Action of string
  | [<MandatoryAttribute>] ProjectFile of string
  | PackageId of string
  | Version of string""" config
    |> prepend newline
    |> should equal """
type Argument = 
    | [<MandatoryAttribute>] Action of string
    | [<MandatoryAttribute>] ProjectFile of string
    | PackageId of string
    | Version of string
"""

[<Test>]
let ``should be able to define named unions``() =
    formatSourceString false """
type Thing =
| Human of Name:string * Age:int
| Cat of Name:string * HoursSleptADay:int

type Strategy =
    | Adaptive
    | Fundamental
    | ShortAR of p:int // F# 3.1 syntax
    | BuyHold""" config
    |> prepend newline
    |> should equal """
type Thing = 
    | Human of Name : string * Age : int
    | Cat of Name : string * HoursSleptADay : int

type Strategy = 
    | Adaptive
    | Fundamental
    | ShortAR of p : int // F# 3.1 syntax
    | BuyHold
"""

[<Test>]
let ``should be able to pattern match on unions``() =
    formatSourceString false """
type TestUnion = Test of A : int * B : int
[<EntryPoint>]
let main argv =
   let d = Test(B = 1, A = 2)
   match d with
   | Test(A = a; B = b) -> a + b
   | _ -> 0""" config
    |> prepend newline
    |> should equal """
type TestUnion = Test of A : int * B : int

[<EntryPoint>]
let main argv = 
    let d = Test(B = 1, A = 2)
    match d with
    | Test (A = a; B = b) -> a + b
    | _ -> 0
"""

[<Test>]
let ``should remain one-line type definition one-line`` () =
  formatSourceString false """
type Num = Number of int""" config
  |> prepend newline
  |> should equal """
type Num = Number of int
"""

[<Test>]
let ``should remain two-line type definition two-line`` () =
  formatSourceString false """
type Num =
  | Number of int""" config
  |> prepend newline
  |> should equal """
type Num = 
    | Number of int
"""