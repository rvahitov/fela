namespace Fela.Common

open System
open System.Runtime.CompilerServices

type IExecutionResult =
    abstract IsSuccess : bool
    abstract Errors : exn []

[<Extension>]
module ExecutionResult =
    type private T =
        | SuccessResult
        | FailureResult of exn []
        interface IExecutionResult with

            member this.IsSuccess =
                match this with
                | SuccessResult -> true
                | FailureResult _ -> false

            member this.Errors =
                match this with
                | SuccessResult -> Array.empty
                | FailureResult errors -> errors

    let Success () = SuccessResult :> IExecutionResult

    let Failure ([<ParamArray>] errors : exn []) =
        assert (errors <> null)
        assert ((errors |> Array.length) > 0)
        FailureResult(errors) :> IExecutionResult

    [<Extension>]
    let ThrowIfFailure (er : IExecutionResult) =
        if not er.IsSuccess then
            match er.Errors with
            | null
            | [||] -> raise (InvalidOperationException())
            | [| e |] -> raise e
            | errors -> raise (AggregateException(errors))
