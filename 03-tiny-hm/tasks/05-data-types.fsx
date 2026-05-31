// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  match ty with
  | TyVariable(v') -> (v' = vcheck)
  | TyList(ty') -> occursCheck vcheck ty'
  | TyTuple(ty1, ty2)
  | TyFunction(ty1, ty2) -> (occursCheck vcheck ty1) || (occursCheck vcheck ty2)
  | _ -> false

let rec substType (subst:Map<_, _>) ty = 
  match ty with
  | TyVariable(v') ->
    match Map.tryFind v' subst with
    | Some ty' -> ty'
    | None -> ty
  | TyList(ty') -> TyList (substType subst ty')
  | TyFunction(ty1, ty2) -> TyFunction (substType subst ty1, substType subst ty2)
  | TyTuple(ty1, ty2) -> TyTuple (substType subst ty1, substType subst ty2)
  | _ -> ty

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  List.map (fun (t1, t2) -> (substType subst t1, substType subst t2)) cs

let rec solve cs =
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList t1, TyList t2)::cs ->
    solve ((t1,t2)::cs)
  | (TyTuple (ta1, tb1), TyTuple (ta2, tb2))::cs
  | (TyFunction (ta1, tb1), TyFunction (ta2, tb2))::cs ->
    solve ((ta1,ta2)::(tb1,tb2)::cs)
  | (t, TyVariable v)::cs
  | (TyVariable v, t)::cs ->
    if occursCheck v t then failwith "Cannot be solved (occurs check)"
    let newCs = substConstrs (Map.empty.Add(v, t)) cs
    let subst = solve newCs
    (v, substType (Map(subst)) t)::subst
  | _ -> failwith "Cannot be solved"



// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      // NOTE: If the expression is a constant number, we return
      // its type (number) and generate no further constraints.
      TyNumber, []

  | Binary("+", e1, e2)
  | Binary("*", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [t1, TyNumber; t2, TyNumber ]

  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v ->
      match Map.tryFind v ctx with
      | Some t -> t, []
      | None -> failwithf "Type of variable '%s' can't be found." v

  | If(econd, etrue, efalse) ->
      let tCond, sCond = generate ctx econd
      let tTrue, sTrue = generate ctx etrue
      let tFalse, sFalse = generate ctx efalse
      tTrue, sCond @ sTrue @ sFalse @ [ tCond, TyBool; tTrue, tFalse ]

  | Let(v, e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2 , s2 = generate (ctx.Add(v, t1)) e2
      t2, s1 @ s2
      // Perhaps I'll add v = t1 as a constraint too?
  
  | Lambda(v, e) ->
      let targ = newTyVariable()
      let tOut, sOut = generate (ctx.Add(v, targ)) e
      TyFunction(targ, tOut), sOut

  | Application(e1, e2) -> 
      let tvArg = newTyVariable()
      let tvOut = newTyVariable()
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      tvOut, s1 @ s2 @ [TyFunction(tvArg, tvOut), t1; tvArg, t2]

  | Tuple(e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyTuple(t1, t2), s1 @ s2

  | TupleGet(b, e) ->
      let t, s = generate ctx e
      let t1 = newTyVariable()
      let t2 = newTyVariable()
      (if b then t1 else t2), (TyTuple(t1, t2), t)::s


  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
