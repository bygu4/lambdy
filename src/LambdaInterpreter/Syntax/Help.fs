namespace LambdaInterpreter.Syntax

open Keywords

/// Module for displaying syntax help.
module Help =

    /// A pointer indicating the start of input for the interpreter.
    [<Literal>]
    let InputPointer = ">>> "

    /// Print syntax help to the standard output.
    let printSyntaxHelp () =
        printfn $"
Syntax:
    variable\t\t {VariablePattern}
    term\t\t {{variable}}|{{abstraction}}|{{application}}|({{term}})
    application\t\t {{term}} {{term}}
    abstraction\t\t \\{{variables}}.{{term}}
    definition\t\t {DeclarationKeyword} {{variable}} = {{term}}

Examples:
    {InputPointer}{DeclarationKeyword} S = \\x y z.x z (y z)
    {InputPointer}{DeclarationKeyword} K = \\x y.x
    {InputPointer}S K K
    \\z.z

Commands:
    {ResetKeyword}\t\t Reset defined variables
    {DisplayKeyword}\t\t Display definitions in order of addition
    {HelpKeyword}\t\t Display help
    {ClearKeyword}\t\t Clear console buffer
    {ExitKeyword}\t\t Stop the execution and exit
"
