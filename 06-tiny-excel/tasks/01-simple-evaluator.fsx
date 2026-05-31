// ----------------------------------------------------------------------------
// 01 - Simple expression evaluator
// ----------------------------------------------------------------------------

// Address represents column and row of a spreadsheet
// This is indexed from 1, so B10 would be (2, 10).
type Address = int * int

// Result of evaluating an expression. A value can be
// primitive (number or string) or an Error if things go wrong.
type Value = 
  | Number of int
  | String of string
  | Error of string
  
// Minimal formula language with just constants, references and
// functions (we will start with just functions named '+' and '*')
type Expr = 
  | Const of Value
  | Reference of Address
  | Function of string * Expr list

// A sheet is a mapping from addresses (that contain formulas)
// to expressions. Note that in real Excel, there is a difference
// between cells with data (written as just '123') and cells 
// containing formulas (written as '=123'). We ignore this.
type Sheet = Map<Address, Expr>


// ----------------------------------------------------------------------------
// Simple recursive evaluator
// ----------------------------------------------------------------------------


let rec eval (sheet:Sheet) expr = 
  match expr with
  | Const(v) -> v
  | Reference(col, row) -> 
    match Map.tryFind (col, row) sheet with
    | None -> Error "Missing value"
    | Some expr' -> eval sheet expr'
  | Function("+", [e1; e2]) ->
    match (eval sheet e1, eval sheet e2) with
    | (Number(n1), Number(n2)) -> Number(n1 + n2)
    | _ -> Error "Non-numerical addition not supported"
  | Function("*", [e1; e2]) ->
    match (eval sheet e1, eval sheet e2) with
    | (Number(n1), Number(n2)) -> Number(n1 * n2)
    | _ -> Error "Non-numerical multiplication not supported"
  | _ -> Error "Can't evaluate unknown function"




// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  let colLetter = s[0]
  let rowNumber = s[1..]
  ((int colLetter) - (int 'A'), int rowNumber)


let fib =  
  [ addr "A1", Const(Number 0) 
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")])
    addr "A4", Function("+", [Reference(addr "A2"); Reference(addr "A3")])
    addr "A5", Function("+", [Reference(addr "A3"); Reference(addr "A4")])
    addr "A6", Function("+", [Reference(addr "A4"); Reference(addr "A5")])
    addr "A7", Function("+", [Reference(addr "A5"); Reference(addr "A6")])
    addr "A8", Function("+", [Reference(addr "A6"); Reference(addr "A7")])
    addr "A9", Function("+", [Reference(addr "A7"); Reference(addr "A8")])
    addr "A10", Function("+", [Reference(addr "A8"); Reference(addr "A9")]) ]
  |> Map.ofList

// Should return: Number 13
eval fib (Reference(addr "A8"))

// Should return: Number 21
eval fib (Reference(addr "A9"))

// Should return: Number 34
eval fib (Reference(addr "A10"))

// Should return: Error "Missing value"
eval fib (Reference(addr "A11"))