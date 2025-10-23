// ----------------------------------------------------------------------------
// 06 - Add more data types - unions
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  // NOTE: Value representing a union case. Again, we use 'bool':
  // 'true' for 'Case1' and 'false' for 'Case2'
  | ValCase of bool * Value

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
  // NOTE: 'Case' represents creating a union value and 'Match' pattern 
  // matching. You can read 'Match(e, v, e1, e2)' as F# pattern matching 
  // of the form: 'match e with v -> e1 | v -> e2'
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

and VariableContext = 
  Map<string, Value>

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
      | Some res -> res
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
          evaluate (Map.add varName v2 capturedCtxt ) e
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
          then evaluate (Map.add v caseVal ctx) e1
          else evaluate (Map.add v caseVal ctx) e2
      | _ -> failwith "attempted to pattern-match with a non-case value"

  | Case(b, e) ->
      ValCase(b, evaluate ctx e)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 =
  Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 = 
  Match(Case(true, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec2

let ec3 = 
  Match(Case(false, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec3
