namespace Lambdy.Bin

open Lambdy.Lib.Interfaces.ILogger

module Verbosity =

    type Verbosity =
        | Minimal = 0
        | Normal = 1
        | Detailed = 2
        | Diagnostic = 3

    let targetVerbosity =
        function
        | Minimal _ -> Verbosity.Minimal
        | Normal _ -> Verbosity.Normal
        | Detailed _ -> Verbosity.Detailed
        | Diagnostic _ -> Verbosity.Diagnostic

    let matchesTargetVerbosity record verbosity = verbosity >= targetVerbosity record
