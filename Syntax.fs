module jsicnet.Syntax


type Type =
    |Number
    |Character
    |Str
    |Bool
    |List of Type
    |FuncRef
    |Unit
    |Union of Type list

type ResolvedValue =
    |VNumber of float
    |VCharacter of char
    |VStr of string
    |VBool of bool
    |VList of ResolvedList
    |VFuncRef of string
    |VUnit

    member this.GetType = 
        match this with 
        |VNumber _ -> Number
        |VCharacter _ -> Character
        |VStr _ -> Str
        |VBool _ -> Number
        |VList l -> List l.Type
        |VFuncRef _ -> Number
        |VUnit -> Unit

    member this.MatchesType t = 
        let thisType = this.GetType
        match t with 
        |Union types -> List.contains thisType types
        |c -> c = thisType

and ResolvedList(rvalType, values) = 
    member this.Type: Type = rvalType
    member this.Values: ResolvedValue list = values


type Value = 
    |FuncCall of FunctionCall
    |ValList of ValueList
    |ResolvedVal of ResolvedValue
    |Reference of string

and FunctionCall(name, param) =
    member this.Name: string = name
    member this.Params: Value list = param

and ValueList(valType, values) = 
    member this.Type: Type = valType
    member this.Values: Value list = values


type Constant(name, value) = 
    member this.Name: string = name
    member this.Value: ResolvedValue = value


type Variable(name, varType, value) = 
    member this.Name: string = name
    member this.Type: Type = varType
    member this.Value: Value = value


type ProcessNode = 
    |Var of Variable
    |Ret of Value


type PropType(name, propType) =
    
    member this.Name: string = name
    member this.Type = propType


type Function(name, retType, propTypes, consts, nodes, funcs) = 

    member this.Name: string = name
    member this.RetType: Type = retType
    member this.PropTypes: PropType list = propTypes
    member this.Constants: Constant list = consts
    member this.Nodes: ProcessNode list = nodes
    member this.Functions: Function list = funcs
        
        
type ResolvedProp(name, value) =
    
    member this.Name: string = name
    member this.Value: ResolvedValue = value
        
        
type CallContext(name, func, props, children) =
    
    member this.Name: string = name
    member this.Function: Function = func
    member this.Props: ResolvedProp list = props
    member this.Children: CallContext list = children
    
    member this.GetParam name = 
        match List.tryFind (fun (e: ResolvedProp) -> e.Name = name) this.Props with 
        |Some var -> Some (ResolvedVal var.Value)
        |None -> None

    member this.GetConstant name = 
        match List.tryFind (fun (e: Constant) -> e.Name = name) this.Function.Constants with 
        |Some cons -> Some (ResolvedVal cons.Value)
        |None -> None

    member this.Variables = 
        List.choose (fun (e: ProcessNode) -> 
                    match e with 
                    |Var v -> Some v
                    |Ret _ -> None) this.Function.Nodes

    member this.GetVariable name = 
        match List.tryFind (fun (e: Variable) -> e.Name = name) this.Variables with 
        |Some var -> Some var.Value
        |None -> None

    member this.GetValue name =
        [ this.GetConstant; this.GetVariable; this.GetParam; ]
        |> List.tryPick (fun f -> f name)
        
    new(parent: CallContext, child) =
        CallContext(parent.Name, parent.Function, parent.Props, child :: parent.Children)
        
    new(parent: CallContext, name) =
        CallContext(parent.Name, parent.Function, parent.Props,
                    parent.Children |> List.filter (fun f -> f.Name <> name))
        
        
type CallTrace = 
    |Root
    |Node of CallContext * CallTrace

    member this.PathName =
        match this with
        |Root -> ""
        |Node (f, t) -> $".{f.Name}{t.PathName}"


type CallZipper(current, trace) = 

    member this.Current: CallContext = current
    member this.Trace: CallTrace = trace

    member this.Up =
        match this.Trace with 
        |Root -> None
        |Node(p, t) -> Some (CallZipper(CallContext(p, this.Current), t))

    member this.DownInto name =
        match this.Current.Children |> List.tryFind (fun f -> f.Name = name) with
        |None -> None
        |Some f -> Some(CallZipper(f, Node(CallContext(this.Current, name), this.Trace)))

    member this.GetValueLevel name = 
        match this.Current.GetValue name with
        |Some _ -> Some this
        |None -> this.Up |> Option.bind (fun f -> f.GetValueLevel name)

    member this.GetFunctionLevel name = 
        match this.Current.Function.Functions |> List.tryFind (fun f -> f.Name = name) with
        |Some _ -> Some this
        |None -> this.Up |> Option.bind (fun f -> f.GetValueLevel name)

