namespace LTSVTest

open NUnit.Framework
open FsUnit

open Basis.Core
open Basis.Core.Collections
open FSharp.Data.LTSV
open FSharp.Data.LTSV.Parser

[<TestFixture>]
module ParserTest =

  let shouldHaveSuccessValue expected = function
  | Success actual -> actual |> should equal expected
  | Failure (e : exn) -> Assert.Fail(e.Message)

  let message : (obj -> string) = function
  | :? string as x -> x
  | :? exn as x -> x.Message
  | x -> x.ToString()

  let shouldHaveFailureValue<'a> expectedMessage = function
  | Success r -> Assert.Fail(sprintf "expect: Result.Failure, but was Result.Success(%s)" (r.ToString()))
  | Failure (e : 'a) -> e |> message |> should equal expectedMessage

  [<Test>]
  let ``parse one field``() =
    "label:text"
    |> parseLine
    |> shouldHaveSuccessValue (LTSV (seq { yield ("label", ["text"]) }))

  [<Test>]
  let ``parse some fields``() =
    "label:text\thoge:fuga\r\n"
    |> parseLine
    |> shouldHaveSuccessValue (LTSV (seq { yield ("label", ["text"]); yield ("hoge", ["fuga"]) }))

  [<Test>]
  let ``contains colon``() =
    "label:text:foo"
    |> parseLine
    |> shouldHaveSuccessValue (LTSV (seq { yield ("label", ["text:foo"]) }))

  [<Test>]
  let ``empty input``() =
    ""
    |> parseLine
    |> shouldHaveSuccessValue NameValueBag.empty

  [<Test>]
  let ``illegal format``() =
    "hoge"
    |> parseLine
    |> shouldHaveFailureValue<exn> "parse error: field(hoge)"

  [<Test>]
  let ``empty value``() =
    "label:"
    |> parseLine
    |> shouldHaveSuccessValue (LTSV (seq { yield ("label", [""]) }))

  [<Test>]
  let ``parse multi line``() =
    """label:text
hoge:fuga"""
    |> parse
    |> shouldHaveSuccessValue (seq {
      yield LTSV (seq { yield ("label", ["text"]) })
      yield LTSV (seq { yield ("hoge", ["fuga"]) })
      })

  [<Test>]
  let ``contain empty line``() =
    """label:text

hoge:fuga"""
    |> parse
    |> shouldHaveSuccessValue (seq {
      yield LTSV (seq { yield ("label", ["text"]) })
      })
