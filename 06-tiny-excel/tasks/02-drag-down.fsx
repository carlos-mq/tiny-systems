// ----------------------------------------------------------------------------
// 02 - "Drag down" formula expanding
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

type Sheet = Map<Address, Expr>

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


let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet:Sheet) : Sheet = 
  let srcExpr =
    match Map.tryFind (srcCol, srcRow) sheet with
    | Some expr -> expr
    | None -> failwith "No formula found at the source cell!"
  let newCells = seq { 
    for col in [ srcCol .. tgtCol ] do
      for row in [ srcRow .. tgtRow ] ->
        ((col, row), relocateReferences (srcCol, srcRow) (col, row) srcExpr)
  }
  List.fold (fun (s:Sheet) ((col, row), expr) -> (Map.add (col, row) expr s)) sheet (List.ofSeq newCells)



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
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> Map.ofList
  |> expand (addr "A3") (addr "A10")

// Should return: Number 13
eval fib (Reference(addr "A8"))

// Should return: Number 21
eval fib (Reference(addr "A9"))

// Should return: Number 34
eval fib (Reference(addr "A10"))

// Should return: Error "Missing value"
eval fib (Reference(addr "A11"))


// Column 'A' is a sequence of numbers increasing by 1
// Column 'B' is the factorial of the corresponding number
// i.e.: Bn = An * B(n-1) = An * A(n-1)!
let fac = 
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ] 
  |> Map.ofList
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")

// A6 should be 5, B6 should be 120
eval fac (Reference(addr "A6"))
eval fac (Reference(addr "B6"))

// A11 should be 10, B11 should be 3628800
eval fac (Reference(addr "A11"))
eval fac (Reference(addr "B11"))
