namespace FSharp.Data.LTSV

open System
open Basis.Core
open Basis.Core.Collections

// Ported from https://github.com/masaru-b-cl/DynamicLTSV/blob/master/DynamicLTSV/DynamicLTSV.cs

module Parser =

  let parseLine line : Result<LTSV, exn> =

    let applyOrDefault pred f a =
      if pred a then f a else a

    let toPair item =
      let collonIndex = Str.indexOf ":" item
      if collonIndex <= 0 || collonIndex >= Str.length item then
        Result.Failure (exn (sprintf "parse error: field(%s)" item))
      else
        let key = item |> Str.sub 0 collonIndex
        let value = item.Substring (collonIndex + 1)
        Result.Success (key, value)

    let inner line =
      line
      |> applyOrDefault (Str.endsWith "\n") (fun i -> i.TrimEnd('\n'))
      |> applyOrDefault (Str.endsWith "\r") (fun i -> i.TrimEnd('\r'))
      |> Str.splitBy "\t"
      |> Array.map toPair
      |> Seq.groupBy (function | Success p -> fst p | Failure (e : exn) -> e.Message)
      |> Seq.fold (fun bag (k, items) ->
        match bag with
        | Success bag ->
          match Seq.tryFind Result.isFailure items with
          | Some e -> e |> Result.getFailure |> Failure
          | None ->
            let items = items |> Seq.map (Result.get >> snd) |> Seq.toList
            Success (NameValueBag.add (k, items) bag)
        | Failure _ as x -> x
        ) (Result.Success NameValueBag.empty)

    if Str.isNullOrEmpty line then Success NameValueBag.empty
    else inner line

  let parse (lines : string) =
    lines.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)
    |> Seq.takeWhile (not << Str.isNullOrEmpty)
    |> Seq.map parseLine
    |> fun x ->
      match Seq.tryFind Result.isFailure x with
      | Some e -> e |> Result.getFailure |> Failure
      | None -> x |> Seq.map Result.get |> Success