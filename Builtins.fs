module jsicnet.Builtins
open jsicnet.Syntax

let example = Function (
        "main",
        Unit,
        [PropType("args", List Str)],
        [Constant ("five", VNumber 5)],
        [
            Var(
                Variable ("result",
                          Number,
                          FuncCall(FunctionCall("+", [Reference "five"; Reference "five"]))
                )
            )
        ],
        []
    )

let exampleZipper = CallZipper (
    CallContext(
        "main",
        example,
        [ResolvedProp ("args", VList (ResolvedList (Str, [ VStr("main.jsic") ])))],
        []
    ),
    Root
)



