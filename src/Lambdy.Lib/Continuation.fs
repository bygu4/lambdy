namespace Lambdy.Lib

module Continuation =

    type ContinuationBuilder<'a, 'b>() =

        member _.Bind (inc, wrap) =
            fun cont -> inc (fun value -> wrap value cont)

        member _.ReturnFrom inc = inc

        member _.Return value = fun cont -> cont value

    let continuation<'a, 'b> = ContinuationBuilder<'a, 'b> ()

    type Computation<'a, 'b> =
        | Done of value : 'a
        | Failed of error : 'b
        | Incomplete of continuation : (unit -> Computation<'a, 'b>)

    type ComputationBuilder<'a, 'b>(stepByStep : bool) =

        member _.Bind (inc, wrap) =
            fun cont ->
                inc (
                    function
                    | Done value -> wrap value cont
                    | _ as comp -> cont comp
                )

        member _.ReturnFrom inc = inc

        member _.Return value = fun cont -> cont value

        member _.Yield value =
            fun cont ->
                match stepByStep with
                | true -> Incomplete (fun () -> cont value) |> cont
                | false -> cont value

    let computation<'a, 'b> stepByStep = ComputationBuilder<'a, 'b> stepByStep
