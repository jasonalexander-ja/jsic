
type Type =
    |Number
    |Character
    |Bool
    |List of Type
    |FuncRef
    |Unit
    |Union of Type list

type ResolvedValue =
    |VNumber of float
    |VCharacter of char
    |VBool of bool
    |VList of ResolvedList
    |VFuncRef of string
    |VUnit

    member this.GetType = 
        match this with 
        |VNumber _ -> Number
        |VCharacter _ -> Character
        |VBool _ -> Number
        |VList l -> List l.Type
        |VFuncRef _ -> Number
        |VUnit -> Unit

    member this.MatchesType t = 
        let thisType = this.GetType
        match t with 
        |Union types -> List.contains thisType types
        |c -> c = thisType

and ResolvedList(t, values) = 
    member this.Type: Type = t
    member this.Values: ResolvedValue list = values

type Value = 
    |FuncCall of string * Value list
    |ResolvedVal of ResolvedValue


type Constant(name, value) = 
    member this.Name: string = name
    member this.Value: ResolvedValue = value


type Variable(name, t, value) = 
    member this.Name: string = name
    member this.Type: Type = t
    member this.Value: Value = value

type ProcessNode = 
    |Var of Variable
    |Ret of Value

type Function(name, t, param, consts, nodes, funcs) = 

    member this.Name: string = name
    member this.RetType: Type = t
    member this.Params: Variable list = param
    member this.Constants: Constant list = consts
    member this.Nodes: ProcessNode list = nodes
    member this.Functions: Function list = funcs

    member this.GetParam name = 
        match List.tryFind (fun (e: Variable) -> e.Name = name) this.Params with 
        |Some var -> Some var.Value
        |None -> None

    member this.GetConstant name = 
        match List.tryFind (fun (e: Constant) -> e.Name = name) this.Constants with 
        |Some cons -> Some (ResolvedVal cons.Value)
        |None -> None

    member this.Variables = 
        List.choose (fun (e: ProcessNode) -> 
                    match e with 
                    |Var v -> Some v
                    |Ret _ -> None) this.Nodes

    member this.GetVaraible name = 
        match List.tryFind (fun (e: Variable) -> e.Name = name) this.Variables with 
        |Some var -> Some var.Value
        |None -> None

    member this.GetValue name =
        [ this.GetConstant; this.GetVaraible; this.GetParam; ]
        |> List.tryPick (fun f -> f name)

    new(parent: Function, child) = 
        Function(parent.Name,  
            parent.RetType, 
            parent.Params, 
            parent.Constants, 
            parent.Nodes, 
            child :: parent.Functions)

    new(parent: Function, name) = 
        Function(parent.Name,  
            parent.RetType, 
            parent.Params, 
            parent.Constants, 
            parent.Nodes, 
            parent.Functions |> List.filter (fun f -> f.Name <> name))
        
        
type FuncTrace = 
    |Root
    |Node of Function * FuncTrace

type FuncZipper(current, trace) = 

    member this.Current: Function = current
    member this.Trace: FuncTrace = trace

    member this.Up =
        match this.Trace with 
        |Root -> None
        |Node(p, t) -> Some (FuncZipper(Function(p, this.Current), t))

    member this.DownInto name =
        match this.Current.Functions |> List.tryFind (fun f -> f.Name = name) with
        |None -> None
        |Some f -> Some(FuncZipper(f, Node(Function(this.Current, name), this.Trace)))

    member this.GetValueLevel name: FuncZipper option = 
        match this.Current.GetValue name with
        |Some _ -> Some this
        |None -> this.Up |> Option.bind (fun f -> f.GetValueLevel name)

    member this.GetFunctionLevel name: FuncZipper option = 
        match this.Current.Functions |> List.tryFind (fun f -> f.Name = name) with
        |Some _ -> Some this
        |None -> this.Up |> Option.bind (fun f -> f.GetValueLevel name)

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
