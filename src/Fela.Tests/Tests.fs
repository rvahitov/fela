module Tests

open System
open Fela.Common
open Xunit

[<Fact>]
let ``ExecutionResult.Success should return instance with empty errors and IsSuccess true`` () =
    let res = ExecutionResult.Success()
    Assert.True(res.IsSuccess)
    Assert.Empty(res.Errors)

[<Fact>]
let ``ExecutionResult.Failure should return instance with not empty errors and IsSuccess false`` () =
    let res =
        ExecutionResult.Failure
            ([| ArgumentNullException()
                InvalidOperationException() |])
    Assert.False(res.IsSuccess)
    Assert.NotEmpty(res.Errors)
    Assert.IsType<ArgumentNullException>(res.Errors.[0]) |> ignore
    Assert.IsType<InvalidOperationException>(res.Errors.[1]) |> ignore
    
[<Fact>]
let ``ExecutionResult.ThrowIfFailure should not throw``() =
    let res = ExecutionResult.Success()
    ExecutionResult.ThrowIfFailure(res)

exception Ex1 
exception Ex2 
exception Ex3 

[<Fact>]
let ``ExecutionResult.ThrowIfFailure should throw``() =
    let res = ExecutionResult.Failure([|Ex1|])
    Assert.Throws<Ex1>(fun() -> res |> ExecutionResult.ThrowIfFailure) |> ignore
    let res = ExecutionResult.Failure([|Ex2|])
    Assert.Throws<Ex2>(fun() -> res |> ExecutionResult.ThrowIfFailure) |> ignore
    let res = ExecutionResult.Failure([|Ex1; Ex2; Ex3|])
    let ex = Assert.Throws<AggregateException>(fun() -> ExecutionResult.ThrowIfFailure(res))
    Assert.NotEmpty(ex.InnerExceptions)
    let errors = ex.InnerExceptions |> Seq.toArray
    Assert.IsType<Ex1>(errors.[0]) |> ignore
    Assert.IsType<Ex2>(errors.[1]) |> ignore
    Assert.IsType<Ex3>(errors.[2]) |> ignore
