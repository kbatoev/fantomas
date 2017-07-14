namespace Fantomas

open System

[<RequireQualifiedAccess>]
module String =
    let normalizeNewLine (str : string) =
        str.Replace("\r\n", "\n").Replace("\r", "\n")

    let normalizeThenSplitNewLine (str : string) =
        (normalizeNewLine str).Split('\n')

    let startsWithOrdinal (prefix : string) (str : string) =
        str.StartsWith(prefix, StringComparison.Ordinal)

    type StringLiteral = None | Normal | Verbatim | TripleQuoted
    let removeWhitespaces (str : string) =
        let mutable insideString = None
        let mutable list = []
        let mutable i = 0
        let q = '\"'
        let length = str.Length
        while i < length do
          match insideString with
          | TripleQuoted -> if str.[i] = q && str.[i + 1] = q && str.[i + 2] = q
                            then list <- q :: q :: q :: list
                                 i <- i + 3
                                 insideString <- None
                            else list <- str.[i] :: list
                                 i <- i + 1
          | Verbatim -> if str.[i] = q && str.[i - 1] <> q
                        then insideString <- None
                        list <- str.[i] :: list
                        i <- i + 1
          | Normal -> if str.[i] = q && str.[i - 1] <> '\\'
                      then list <- q :: list
                           insideString <- None
                      else list <- str.[i] :: list
                      i <- i + 1
          | None -> if str.[i] = '\n'
                    then let rec loopToRemoveWhitespaces = function
                             | [] -> []
                             | c :: cs -> if c = ' ' || c = '\r' then loopToRemoveWhitespaces cs else c :: cs
                         list <- loopToRemoveWhitespaces list
                         list <- '\n' :: list
                    elif str.[i] = q && str.[i - 1] = '@'
                    then list <- q :: list
                         insideString <- Verbatim
                    elif str.[i] = q && str.[i - 1] = q && str.[i - 2] = q
                    then list <- q :: list
                         insideString <- TripleQuoted
                    elif str.[i] = q && str.[i - 1] <> '\\' && (i + 2 >= length || str.[i + 1] <> q || str.[i + 2] <> q)
                    then insideString <- Normal
                         list <- q :: list
                    else list <- str.[i] :: list
                    i <- i + 1
        list |> List.rev |> List.toArray |> System.String
        