module DictionaryTests

open System
open Xunit
open Fela.Domain.Models.DictionaryModel

[<Theory>]
[<InlineData("PersonName")>]
[<InlineData("PersonAge")>]
[<InlineData("PersonRank")>]
let ``Name.ofString should succeed`` (value : string) =
    let n = Name.ofString value
    Assert.Equal(value, n |> Name.value)

[<Fact>]
let ``Name.ofString should fail`` () =
    Assert.Throws<ArgumentException>(fun () -> Name.ofString null |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> Name.ofString "" |> ignore) |> ignore


[<Theory>]
[<InlineData("Person name")>]
[<InlineData("Person age")>]
[<InlineData("Person rank")>]
let ``DisplayName.ofString should succeed`` (value : string) =
    let dn = DisplayName.ofString value
    Assert.Equal(value, dn |> DisplayName.value)


[<Fact>]
let ``DisplayName.ofString should fail`` () =
    Assert.Throws<ArgumentException>(fun () -> DisplayName.ofString null |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> DisplayName.ofString "" |> ignore) |> ignore

[<Fact>]
let ``Item.Create with invalid key should fail`` () =
    Assert.Throws<ArgumentException>(fun () -> Item.Create(null, "value") |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> Item.Create("", "value") |> ignore) |> ignore

[<Fact>]
let ``Item.Create with invalid value should fail`` () =
    Assert.Throws<ArgumentException>(fun () -> Item.Create("key", null) |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> Item.Create("key", "") |> ignore) |> ignore


[<Theory>]
[<InlineData("Name", "Cooper")>]
[<InlineData("Age", "25")>]
[<InlineData("False", @"Ложь")>]
let ``Item.Create should succeed``(key: string, value: string) =
    let item = Item.Create(key, value)
    Assert.Equal(key, Item.key item)
    Assert.Equal(value, Item.value item)
