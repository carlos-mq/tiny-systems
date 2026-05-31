// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string 
  | Stop

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue (value : Value) : unit =
  match value with
  | StringValue s -> printfn "%s" s
  | NumberValue n -> printfn "%d" n
  | BoolValue b -> printfn "%b" b

let printValues (values : Value list) : unit list =
  List.map printValue values


let getLine (state : State) (line : int) : int * Command =
  match state.Program |> List.tryFind (fun (lineNum, _) -> lineNum = line) with
  | Some l -> l
  | None -> failwith "line not found!"

let addLine state (line, cmd) : State = 
  let filteredProgram = List.filter (fun (lineNum, _) -> lineNum <> line) state.Program
  let overwrittenProgram = (line, cmd) :: filteredProgram |> List.sortBy fst
  { state with Program = overwrittenProgram }


let getRnd (state : State) : int = state.Random.Next()

let getRndFromRange (state : State) (n : int) : int =
  getRnd state % n

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

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
  // TODO: We need an extra function 'MIN' that returns the smaller of
  // the two given numbers (in F#, the function 'min' does exactly this.)
  let evalArgs args = List.map (evalExpression state) args 
  match expr with
  | Const v -> v
  | Function("-", args) ->
    evalArgs args |> binaryNumOp (-)
  | Function("=", args) ->
    evalArgs args |> binaryRelOp (=)
  | Function("<", args) -> 
    evalArgs args |> binaryRelOp (<)
  | Function(">", args) ->
    evalArgs args |> binaryRelOp (>)
  | Function("||", args) ->
    evalArgs args |> binaryBoolOp (||)
  | Function("RND", args) ->
    evalArgs args |> unaryNumOp (getRndFromRange state)
  | Function ("MIN", args) ->
    evalArgs args |> binaryNumOp min
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

  | Print(exprs) ->
      List.map (evalExpression state) exprs |> printValues
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

  // TODO: Input("X") should read a number from the console using Console.RadLine
  // and parse it as a number using Int32.TryParse (retry if the input is wrong)
  // Stop terminates the execution (you can just return the 'state'.)
  | Input(varName) -> 
    let varString = System.Console.ReadLine()
    let (b, varInt) = System.Int32.TryParse(varString)
    if b 
      then
        let state' = { state with Variables = Map.add varName (NumberValue varInt) state.Variables}
        runNextLine state' line
      else
        runCommand state (line, cmd)
  | Stop _ -> state

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

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
