// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue (value : Value) : unit = 
  match value with
  | StringValue s -> printfn "%s" s


let getLine (state : State) (line : int) : int * Command =
  match state.Program |> List.tryFind (fun (lineNum, _) -> lineNum = line) with
  | Some l -> l
  | None -> failwith "line not found!"

let addLine state (line, cmd) : State = 
  let filteredProgram = List.filter (fun (lineNum, _) -> lineNum <> line) state.Program
  let overwrittenProgram = (line, cmd) :: filteredProgram |> List.sortBy fst
  {state with Program = overwrittenProgram }
  

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression (expr : Expression) : Value = 
  match expr with
  | Const v -> v

let rec runCommand state (line, cmd) : State =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(expr) ->
      evalExpression expr |> printValue
      runNextLine state line
  | Goto(ln) ->
      let nextLine = getLine state ln
      runCommand state nextLine

and runNextLine state line = 
  match state.Program |> List.tryFind (fun (lineNum, _) -> lineNum > line) with
  | Some (ln, cmd) -> runCommand state (ln, cmd)
  | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput (state : State) (line : option<int>, cmd : Command) : State =
  match line with
  | Some ln -> runCommand state (ln, cmd)
  | None -> runCommand state (System.Int32.MaxValue, cmd)
  
      

let runInputs (state : State) (cmds : list<option<int> * Command>) : State =
  List.fold runInput state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
