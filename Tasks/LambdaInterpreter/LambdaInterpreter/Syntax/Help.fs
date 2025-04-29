namespace LambdaInterpreter.Syntax

open Keywords

module Help =

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
    >>> {DeclarationKeyword} S = \\x y z.x z (y z)
    >>> {DeclarationKeyword} K = \\x y.x
    >>> S K K
    \\z.z

Commands:
    {ResetKeyword}\t\t Reset defined variables
    {HelpKeyword}\t\t Display help
    {ClearKeyword}\t\t Clear console buffer
    {ExitKeyword}\t\t Stop the execution and exit
"
