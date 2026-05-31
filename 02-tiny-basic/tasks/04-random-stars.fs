// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

open System

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression


type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value>
    // TODO: You will need to include random number generator in the state!
    Rnd : System.Random
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


let getRnd (state : State) : int = state.Rnd.Next()

let getRndFromRange (state : State) (n : int) : int =
  getRnd state % n

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let binaryNumOp f args =
  match args with
  | [NumberValue a; NumberValue b] -> NumberValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let binaryBoolOp f args =
  match args with
  | [BoolValue a; BoolValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two boolean arguments"

let unaryNumOp f args =
  match args with
  | [NumberValue a] -> NumberValue(f a)
  | _ -> failwith "expected one numerical argument"

let rec evalExpression state expr = 
  let evalArgs args = List.map (evalExpression state) args 
  match expr with
  | Const v -> v
  | Function("-", args) ->
    evalArgs args |> binaryNumOp (-)
  | Function("=", args) ->
    evalArgs args |> binaryBoolOp (=)
  | Function("<", args) -> 
    evalArgs args |> binaryRelOp (<)
  | Function(">", args) ->
    evalArgs args |> binaryRelOp (>)
  | Function("||", args) ->
    evalArgs args |> binaryBoolOp (||)
  | Function("RND", args) ->
    evalArgs args |> unaryNumOp (getRndFromRange state)
  | Function(_, _) -> failwith "unknown function called!"
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
  | Clear -> 
    System.Console.Clear()
    runNextLine state line
  | Poke(x, y, str) ->
    let xVal = evalExpression state x
    let yVal = evalExpression state y
    let strVal = evalExpression state str
    match (xVal, yVal, strVal) with
    | (NumberValue x', NumberValue y', StringValue str') ->
      System.Console.CursorLeft <- x'
      System.Console.CursorTop <- y'
      System.Console.Write(str')
      runNextLine state line

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

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Rnd = Random() } // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num 60], "RND" @ [num 20], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num 60], "RND" @ [num 20], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
