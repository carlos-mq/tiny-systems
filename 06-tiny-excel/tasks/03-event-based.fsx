// ----------------------------------------------------------------------------
// 03 - Reactive event-based structure
// ----------------------------------------------------------------------------

type Address = int * int

type Value = 
  | Number of int
  | String of string
  | Error of string
  
type Expr = 
  | Const of Value
  | Reference of Address
  | Function of string * Expr list

// Node in a dependency graph that represents a spreadsheet cell
// For each cell, we store the original expression, evalauted value
// and an event to be triggered when the value changes.
type CellNode = 
  { mutable Value : Value
    mutable Expr : Expr } 

// A live spreadsheet is a mapping from addresses to graph nodes
type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet:LiveSheet) expr = 
  match expr with
  | Const(v) -> v
  | Reference(col, row) -> 
    match Map.tryFind (col, row) sheet with
    | None -> Error "Missing value"
    | Some expr' -> expr'.Value
  | Function("+", [e1; e2]) ->
    match (eval sheet e1, eval sheet e2) with
    | (Number(n1), Number(n2)) -> Number(n1 + n2)
    | _ -> Error "Non-numerical addition not supported"
  | Function("*", [e1; e2]) ->
    match (eval sheet e1, eval sheet e2) with
    | (Number(n1), Number(n2)) -> Number(n1 * n2)
    | _ -> Error "Non-numerical multiplication not supported"
  | _ -> Error "Can't evaluate unknown function"

  

let makeNode (sheet:LiveSheet) (expr:Expr) : CellNode = 
  {Value = eval sheet expr; Expr = expr}


let makeSheet (list:(Address * Expr) list) : LiveSheet =
  List.fold (fun sheet (address, expr) -> Map.add address (makeNode sheet expr) sheet) Map.empty list



// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) = 
  match srcExpr with
  | Const(v) -> srcExpr
  | Reference(col, row) -> 
    Reference(col + tgtCol - srcCol, row + tgtRow - srcRow)
  | Function (op, args) ->
    Function(op, List.map (relocateReferences (srcCol, srcRow) (tgtCol, tgtRow)) args)


let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet:LiveSheet) : LiveSheet = 
  let srcNode =
    match Map.tryFind (srcCol, srcRow) sheet with
    | Some expr -> expr
    | None -> failwith "No formula found at the source cell!"
  let newCells = seq { 
    for col in [ srcCol .. tgtCol ] do
      for row in [ srcRow .. tgtRow ] ->
        ((col, row), relocateReferences (srcCol, srcRow) (col, row) srcNode.Expr)
  }
  List.fold (fun (s:LiveSheet) ((col, row), nodeExpr) -> (Map.add (col, row) (makeNode s nodeExpr) s)) sheet (List.ofSeq newCells)


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
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> makeSheet
  |> expand (addr "A3") (addr "A10")

// Should return: Number 13
eval fib (Reference(addr "A8"))
// Should return: Number 21
eval fib (Reference(addr "A9"))
// Should return: Number 34
eval fib (Reference(addr "A10"))
// Should return: Error "Missing value"
eval fib (Reference(addr "A11"))


let fac = 
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ] 
  |> makeSheet
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")

// Should return: Number 5
eval fac (Reference(addr "A6"))
// Should return: Number 12
eval fac (Reference(addr "B6"))

// Should return: Number 10
eval fac (Reference(addr "A11"))
// Should return: Number 3628800
eval fac (Reference(addr "B11"))
