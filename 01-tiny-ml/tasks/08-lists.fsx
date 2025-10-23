// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value
  // NOTE: A value that represents "empty value" and is
  // useful as the value for representing the empty list.
  | ValUnit 

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  | Recursive of string * Expression * Expression
  // NOTE: An expression that evaluates to a unit value.
  // This exists in F# too and it is written as '()'
  | Unit 

and VariableContext = 
  Map<string, Lazy<Value>>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator for numbers"
      | _, _ ->
          match op with
          | _ -> failwith "unsupported binary operator"

  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res.Value
      | _ -> failwith ("unbound variable: " + v)

  | Unary(op, e) ->
      let v = evaluate ctx e
      match v with
        | ValNum n ->
          match op with
          | "-" -> ValNum(-n)
          | _ -> failwith "unsupported unary operator for numbers"
        | _ ->
          match op with
          | _ -> failwith "unsupported unary operator"

  | If(cond, tbranch, fbranch) ->
      let condVal = evaluate ctx cond
      match condVal with
      | ValNum(n) ->
        if n = 1
          then evaluate ctx tbranch
          else evaluate ctx fbranch
      | _ ->
        failwith "condition can't evaluate to a non-number"
  
  | Lambda(v, e) ->
      ValClosure(v, e, ctx)

  | Application(e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1 with
      | ValClosure(varName, e, capturedCtxt) ->
          evaluate (Map.add varName (lazy v2) capturedCtxt ) e
      | _ -> failwith "attempted to apply to a non-function"

  | Let(v, e1, e2) ->
    let expr = Application(Lambda(v, e2), e1)
    evaluate ctx expr

  | Tuple(e1, e2) ->
      ValTuple(evaluate ctx e1, evaluate ctx e2)

  | TupleGet(b, e) ->
      let v = evaluate ctx e
      match v with
      | ValTuple(v1, v2) ->
        if b then v1 else v2
      | _ -> failwith "attempted to use tuple access on a non-tuple"

  | Match(e, v, e1, e2) ->
      let matchVal = evaluate ctx e
      match matchVal with
      | ValCase(b, caseVal) ->
        if b
          then evaluate (Map.add v (lazy caseVal) ctx) e1
          else evaluate (Map.add v (lazy caseVal) ctx) e2
      | _ -> failwith "attempted to pattern-match with a non-case value"

  | Case(b, e) ->
      ValCase(b, evaluate ctx e)

  | Recursive(v, e1, e2) ->
      let rec newCtx  = Map.add v (lazy evaluate newCtx e1) ctx
      evaluate newCtx e2

  // NOTE: This is so uninteresting I did this for you :-)
  | Unit -> ValUnit


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Ultimate functional programming - lists and List.map!
// We represent lists as cons cells using tuples, so [1,2,3]
//
// = Case(true, Tuple(Constant(1), Case(true, Tuple(Constant(2), 
//     Case(true, Tuple(Constant(3), Case(false, Unit) ))))))

// Helper function to construct lists, so that we 
// do not need to write them by hand!
let rec makeListExpr l = 
  match l with
  | x::xs -> Case(true, Tuple(x, makeListExpr xs))
  | [] -> Case(false, Unit)

let el = makeListExpr [ for i in 1 .. 5 -> Constant i ]

// List.map function in TinyML:
//
//   let rec map = (fun f -> fun l -> 
//     match l with 
//     | Case1 x -> Case1(f x#1, (map f) x#2) 
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y * 10) l
//

let em = 
  Recursive("map",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x",
        Case(true, Tuple(
          Application(Variable "f", TupleGet(true, Variable "x")),
          Application(Application(Variable "map", Variable "f"), 
            TupleGet(false, Variable "x"))
        )),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "map", 
      Lambda("y", Binary("*", Variable "y", Constant 10))), el)
  )

let mapExpr =
  Recursive("map",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x",
        Case(true, Tuple(
          Application(Variable "f", TupleGet(true, Variable "x")),
          Application(Application(Variable "map", Variable "f"), 
            TupleGet(false, Variable "x"))
        )),
        Case(false, Unit)
      )
    )),
    Variable "map"
  )
let example1 = evaluate Map.empty em


let rec showNumber (l : Value) : string =
  match l with
  | ValNum n -> string n
  | _ -> failwith "not a number"
let rec showList (l : Value) : string =
  match l with
  | ValCase(b, ValUnit) -> ""
  | ValCase(b, ValTuple(n, tail)) ->
    showNumber n + " " + showList tail
  | _ -> failwith "not a list"

showList example1
// TODO: Can you implement 'List.filter' in TinyML too??
// The somewhat silly example removes 3 from the list.
// Add '%' binary operator and you can remove odd/even numbers!
//
//   let rec filter = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> 
//          if f x#1 then Case1(x#1, (map f) x#2) 
//          else (map f) x#2
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y + (-2)) l
//

let ef =
  Recursive(
    "filter", 
    Lambda(
      "f", 
      Lambda(
        "l",
        Match(
          Variable("l"),
          "x",
          Case(
            true, 
            If(
              Application(Variable("f"), TupleGet(true, Variable "x")), 
              Case(
                true, 
                Tuple(
                  TupleGet(true, Variable "x"), 
                  Application(Application(Variable("map"), Variable("f")), TupleGet(false, Variable "x"))
                )
              ),
              Application(Application(Variable("map"), Variable("f")), TupleGet(false, Variable("x")))
              )
          ),    
          Case(false, Unit)
        )
      )
    ),
    Application(Application(Variable("map"), Lambda("y", Binary("+", Variable("y"), Constant(-2)))), el)
  )
evaluate (Map.ofList ["map", lazy (evaluate Map.empty mapExpr)]) ef |> showList
