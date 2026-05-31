// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that 
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State = 
  { 
    Program : list<int * Command> 
    Variables : Map<string, Value>
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------


let printValue (value : Value) : unit =
  match value with
  | StringValue s -> printfn "%s" s
  | NumberValue n -> printfn "%d" n
  | BoolValue b -> printfn "%b" b


let getLine (state : State) (line : int) : int * Command =
  match state.Program |> List.tryFind (fun (lineNum, _) -> lineNum = line) with
  | Some l -> l
  | None -> failwith "line not found!"

let addLine state (line, cmd) : State = 
  let filteredProgram = List.filter (fun (lineNum, _) -> lineNum <> line) state.Program
  let overwrittenProgram = (line, cmd) :: filteredProgram |> List.sortBy fst
  { state with Program = overwrittenProgram }


// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression state expr = 
  match expr with
  | Const v -> v
  | Function("-", [e1; e2]) ->
    let v1 = evalExpression state e1
    let v2 = evalExpression state e2
    match (v1, v2) with
    | (NumberValue n1, NumberValue n2) -> NumberValue (n1 - n2)
    | _ -> failwith "- is only defined for integers"
  | Function("=", [e1; e2]) ->
    let v1 = evalExpression state e1
    let v2 = evalExpression state e2
    BoolValue (v1 = v2)
  | Function (funcName, _) -> failwith "unknown function called!"
  | Variable(varName) ->
    match Map.tryFind varName state.Variables with
    | Some varValue -> varValue
    | None -> failwith "variable not found!"
     
let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(expr) ->
      evalExpression state expr |> printValue
      runNextLine state line
  | Goto(ln) ->
      let nextLine = getLine state ln
      runCommand state nextLine
  
  | Assign(varName, expr) -> 
    let state' = { state with Variables = Map.add varName (evalExpression state expr) state.Variables}
    runNextLine state' line
  | If(cond, cmd') ->
    match evalExpression state cond with
    | BoolValue(true) ->
      runCommand state (line, cmd')
    | BoolValue(false) ->
      runNextLine state line
    | _ -> failwith "A conditional must be a boolean!"

and runNextLine state line = 
  match state.Program |> List.tryFind (fun (lineNum, _) -> lineNum > line) with
  | Some (ln, cmd) -> runCommand state (ln, cmd)
  | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput (state : State) (line : option<int>, cmd : Command) : State =
  match line with
  | Some ln -> addLine state (ln, cmd)
  | None -> runCommand state (System.Int32.MaxValue, cmd)
  
      

let runInputs (state : State) (cmds : list<option<int> * Command>) : State =
  List.fold runInput state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let empty : State = { 
  Program = []
  Variables = Map.empty
  }

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
