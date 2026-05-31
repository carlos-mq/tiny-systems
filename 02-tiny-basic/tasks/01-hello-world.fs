// ----------------------------------------------------------------------------
// 01 - Add GOTO and better PRINT for infinite loop fun!
// ----------------------------------------------------------------------------

// NOTE: You can run this using 'dotnet run' from the terminal. 
// If you want to run code in a different file, you will need to change
// the 'tinybasic.fsproj' file (which references this source file now).

// NOTE: F# code in projects is generally organized using namespaces and modules.
// Here, we declare module name for the source code in this file.
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  // NOTE: GOTO specified line number. Note that this is an integer, rather 
  // than an expression, so you cannot calculate line number dynamically. 
  // (But there are tricks to do this by direct memory access on a real C64!)
  | Goto of int


// i. e. list of line numbers along with commands.
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
  

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression (expr : Expression) : Value = 
  match expr with
  | Const v -> v

let rec runCommand state (line, cmd) : unit =
  match cmd with 
  | Print(expr) ->
      evalExpression expr |> printValue
      runNextLine state line
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Goto(l) ->
      let nextLine = getLine state l
      runCommand state nextLine

and runNextLine state line : unit = 
  match state.Program |> List.tryFind (fun (lineNum, _) -> lineNum > line) with
  | Some (l, cmd) -> runCommand state (l, cmd)
  | None -> ()

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  { Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) ] }

let helloInf = 
  { Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) 
      20, Goto 10 ] }

// NOTE: First try to get the following to work!
runCommand helloOnce (-1, Run) |> ignore

// NOTE: Then add 'Goto' and get the following to work!
runCommand helloInf (-1, Run) |> ignore

