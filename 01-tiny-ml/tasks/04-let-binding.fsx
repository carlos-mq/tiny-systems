// ----------------------------------------------------------------------------
// 04 - Let binding as syntactic sugar
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  // NOTE: Added. Let(v, e1, e2) stands for 'let v = e1 in e2'
  | Let of string * Expression * Expression

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
      // TODO: We added 'ValClosure' to 'Value', so this can now fail to 
      // match (if you call binary operator with functions as arguments).
      // Add a catch-all ('_') case and throw an exception using 'failwith'
      // Also do the same for 'Unary' an 'If'!
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator for numbers"
      | _, _ ->
          match op with
          | _ -> failwith "unsupported binary operator for functions"
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
        | ValClosure(varName, e', capturedCtxt) ->
          match op with
          | _ -> failwith "unsupported unary operator for functions"
  | If(cond, tbranch, fbranch) ->
      let condVal = evaluate ctx cond
      match condVal with
      | ValNum(n) ->
        if n = 1
          then evaluate ctx tbranch
          else evaluate ctx fbranch
      | ValClosure(varName, e, capturedCtxt) ->
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

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Simple let binding
//   let x = 2 in (20*x) + x
let el1 = 
  Let("x", 
    Constant(2), 
    Binary("+", Variable("x"), 
      Binary("*", Variable("x"), Constant(20)))
  )
evaluate Map.empty el1

// Function calls with let binding
//   let f = fun x -> x*2 in (f 20) + (f 1)
//
// In F#, you would write this as follows
//   let f x = x*2
//   (f 20) + (f 1)
let el2 = 
  Let("f",
    Lambda("x", Binary("*", Variable("x"), Constant(2))),
    Binary("+", 
      Application(Variable("f"), Constant(20)),
      Application(Variable("f"), Constant(1)) 
    )    
  )
evaluate Map.empty el2
