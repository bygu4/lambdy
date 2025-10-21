namespace Lambdy.Lib

module Continuation =

    type ContinuationBuilder<'a, 'b>() =

        member _.Bind (inc, wrap) =
            fun cont -> inc (fun value -> wrap value cont)

        member _.ReturnFrom inc = inc

        member _.Return value = fun cont -> cont value

    let continuation<'a, 'b> = ContinuationBuilder<'a, 'b> ()

    type MaybeContinuationBuilder<'a, 'b>() =

        member _.Bind (inc, wrap) =
            fun cont ->
                inc (
                    function
                    | Ok value -> wrap value cont
                    | _ as err -> cont err
                )

        member _.ReturnFrom inc = inc

        member _.Return value = fun cont -> value |> Ok |> cont

        member _.Yield value = fun cont -> value |> Error |> cont

    let maybeContinuation<'a, 'b> = MaybeContinuationBuilder<'a, 'b> ()
