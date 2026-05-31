// ----------------------------------------------------------------------------
// 04 - Reactive event-based computation
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

type CellNode = 
  { mutable Value : Value
    mutable Expr : Expr
    // NOTE: Added event that will be triggered when the 
    // expression and value of the node is changed.
    Updated : Event<unit> } 

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
  | Function("-", [e1; e2]) ->
    match (eval sheet e1, eval sheet e2) with
    | (Number(n1), Number(n2)) -> Number(n1 - n2)
    | _ -> Error "Non-numerical subtraction not supported"
  | Function("*", [e1; e2]) ->
    match (eval sheet e1, eval sheet e2) with
    | (Number(n1), Number(n2)) -> Number(n1 * n2)
    | _ -> Error "Non-numerical multiplication not supported"
  | Function ("/", [e1; e2]) ->
    match (eval sheet e1, eval sheet e2) with
    | (Number(n1), Number(n2)) -> Number(n1 / n2)
    | _ -> Error "Non-numerical division not supported"
  | _ -> Error "Can't evaluate unknown function"
  

let rec collectReferences (expr:Expr) : Address list = 
  match expr with
  | Const(_) -> []
  | Reference(col, row) -> [(col, row)]
  | Function(op, args) -> List.collect collectReferences args


let makeNode (sheet:LiveSheet) expr = 
  let newNode = {Value = eval sheet expr; Expr = expr; Updated = Event<unit>()}
  let update = (fun () ->
    newNode.Value <- eval sheet expr
    newNode.Updated.Trigger()
  )
  let refs = collectReferences expr 
  for addr in refs do
      match sheet.TryFind(addr) with
      | None -> failwith "No cell found!"
      | Some node ->
        node.Updated.Publish.Add(update) 
  newNode

  
  


let updateNode addr (sheet:LiveSheet) expr =
  match sheet.TryFind(addr) with
  | None -> failwith "No cell found!"
  | Some node -> 
    node.Expr <- expr
    node.Value <- eval sheet expr
    node.Updated.Trigger()



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

// Simple spreadsheet that performs conversion between Celsius and Fahrenheit
// To convert F to C, we put value in F into B1 and read the result in C1
// To convert C to F, we put value in C into B2 and read the result in C2
let tempConv = 
  [ addr "A1", Const(String "F to C")
    addr "B1", Const(Number 0) 
    addr "C1", 
      Function("/", [ 
        Function("*", [ 
          Function("-", [ Reference(addr "B1"); Const(Number 32) ])
          Const(Number 5) ])
        Const(Number 9) ]) 
    addr "A2", Const(String "C to F")
    addr "B2", Const(Number 0) 
    addr "C2",
      Function("+", [
      Function("/", [Function("*", [ Reference(addr "B2"); Const(Number 9) ]); Const(Number 5)])
      Const(Number 32)
      ])
    ]
  |> makeSheet

// Fahrenheit to Celsius conversions

// Should return: -17
updateNode (addr "B1") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C1"))
// Should return: 0
updateNode (addr "B1") tempConv (Const(Number 32))
eval tempConv (Reference(addr "C1"))
// Should return: 37
updateNode (addr "B1") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C1"))

// Celsius to Fahrenheit conversions

// Should return: 32
updateNode (addr "B2") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C2"))
// Should return: 212
updateNode (addr "B2") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C2"))
// Should return: 100
updateNode (addr "B2") tempConv (Const(Number 38))
eval tempConv (Reference(addr "C2"))

