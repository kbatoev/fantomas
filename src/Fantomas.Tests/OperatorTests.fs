﻿module Fantomas.Tests.OperatorTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

[<Test>]
let ``should keep triple ~~~ operator``() =
    formatSourceString false """x ~~~FileAttributes.ReadOnly
    """ config
    |> should equal """x ~~~FileAttributes.ReadOnly
"""

[<Test>]
let ``should keep single triple ~~~ operator``() =
    formatSourceString false """~~~FileAttributes.ReadOnly
    """ config
    |> should equal """~~~FileAttributes.ReadOnly
"""

[<Test>]
let ``should keep parens around ? operator definition``() =
    formatSourceString false """let (?) f s = f s
    """ config
    |> should equal """let (?) f s = f s
"""

[<Test>]
let ``should keep parens around ?<- operator definition``() =
    formatSourceString false """let (?<-) f s = f s
    """ config
    |> should equal """let (?<-) f s = f s
"""

[<Test>]
let ``should keep parens around !+ prefix operator definition``() =
    formatSourceString false """let (!+) x = Include x
    """ config
    |> should equal """let (!+) x = Include x
"""

[<Test>]
let ``should keep parens around ++ infix operator definition``() =
    formatSourceString false """let (++) x y = { x with Includes = y :: x.Includes }
    """ config
    |> should equal """let (++) x y = { x with Includes = y :: x.Includes }
"""

[<Test>]
let ``should keep parens around inlined ==> operator definition``() =
    formatSourceString false """let inline (==>) x y = f x y
    """ config
    |> should equal """let inline (==>) x y = f x y
"""

[<Test>]
let ``should keep parens around inlined @@ operator definition``() =
    formatSourceString false """let inline (@@) path1 path2 = Path.Combine(path1, path2)
    """ config
    |> should equal """let inline (@@) path1 path2 = Path.Combine(path1, path2)
"""

[<Test>]
let ``should pattern match on quotation expression``() =
    formatSourceString false """let rec print expr =
    match expr with
    | SpecificCall <@@ (+) @@> (_, _, exprList) ->        
        print exprList.Head
        printf " + "
        print exprList.Tail.Head
    | _ -> ()""" config
    |> should equal """let rec print expr = 
    match expr with
    | SpecificCall <@@ (+) @@> (_, _, exprList) -> 
        print exprList.Head
        printf " + "
        print exprList.Tail.Head
    | _ -> ()
"""
