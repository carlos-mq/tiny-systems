// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
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
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  // NOTE: A recursive definition. You can think of 
  // 'Let(v, e1, e2)' as 'let rec v = e1 in e2'. 
  | Recursive of string * Expression * Expression

and VariableContext = 
  // NOTE: For recursive calls, we need to add the function
  // being defined to the variable context when defining it.
  // This can be done using 'let rec', but we need to store
  // the variables as lazy values.
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



// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x -> 
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er = 
  Recursive("factorial", 
    Lambda("x", If(
      Variable("x"),
      Constant(1),
      Binary(
        "*", Variable("x"), 
        Application(Variable("factorial"), 
          Binary("+", Constant(-1), Variable("x")))
      )
    )),  
    Application(Variable "factorial", Constant 5)
  )
evaluate Map.empty er
