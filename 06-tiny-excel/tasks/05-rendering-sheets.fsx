// ----------------------------------------------------------------------------
// 05 - Rendering sheets as HTML
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
// Rendering sheets as HTML
// ----------------------------------------------------------------------------

open System.IO
open System.Diagnostics

let displayValue (v:Value) : string =
  // TODO: Turn the given value into a string representing HTML
  // You can use the following to create an error string in red.
  match v with
  | Number(n) -> n.ToString()
  | String(s) -> s
  | Error(e) -> "<span class='e'>" + e + "</span>"
  
let display (sheet:LiveSheet) = 
  // TODO: Find the greates row and column index
  let maxCol = Map.fold (fun n (col, _) _ -> if col > n then col else n) 1 sheet 
  let maxRow = Map.fold (fun n (_, row) _ -> if row > n then row else n) 1 sheet 

  let f = Path.GetTempFileName() + ".html"
  use wr = new StreamWriter(File.OpenWrite(f))
  wr.Write("""<html><head>
      <style>
        * { font-family:sans-serif; margin:0px; padding:0px; border-spacing:0; } 
        th, td { border:1px solid black; border-collapse:collapse; padding:4px 10px 4px 10px }
        body { padding:50px } .e { color: red; } 
        th { background:#606060; color:white; } 
      </style>
    </head><body><table>""")


  // TODO: Write column headings
  wr.Write("<tr><th></th>")
  for col in 0 .. maxCol do
    let colChar = char (int 'A' + col)
    wr.Write("<th> "+ colChar.ToString() + " </th>")
  wr.Write("</tr>")

  // TODO: Write row headings and data
  for row in 1 .. maxRow do 
    wr.Write($"<tr><th> " + row.ToString() + " </th>")
    for col in 0 .. maxCol do
      let cellRepr =
        match sheet.TryFind(col, row) with
        | None -> ""
        | Some cell -> displayValue cell.Value
      wr.Write("<td> " + cellRepr +  " </td>")
    wr.Write("</tr>")
  wr.Write("</table></body></html>")
  wr.Close()
  Process.Start("open", f)



// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  let colLetter = s[0]
  let rowNumber = s[1..]
  ((int colLetter) - (int 'A'), int rowNumber)

// NOTE: Let's visualize the Fibbonacci spreadsheet from Step 2!
let fib =  
  [ addr "A1", Const(Number 0) 
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> makeSheet
  |> expand (addr "A3") (addr "A10")
display fib

// NOTE: Let's visualize the Factorial spreadsheet from Step 2!
let fac = 
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ] 
  |> makeSheet
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")
display fac

// NOTE: Let's visualize the Temp convertor spreadsheet from Step 4! 
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
display tempConv
