// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term =
  match term with
  | Variable var ->
    match subst.TryFind(var) with
      | Some t -> t
      | None -> Variable var
  | Predicate(pred, terms) ->
    Predicate(pred, List.map (substitute subst) terms)
  | _ -> term



let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) =
  List.map (fun (v, t) -> (v, substitute newSubst t)) subst


let substituteTerms (subst:Map<string, Term>) (terms:list<Term>) = 
  List.map (substitute subst) terms

let rec unifyList l1 l2 : option<list<string * Term>> =
  match l1, l2 with
  | [], [] -> Some []
  | h1::t1, h2::t2 ->
    match (unify h1 h2) with
    | None -> None
    | Some s1 ->
      match unifyList (substituteTerms (Map.ofList s1) t1) (substituteTerms (Map.ofList s1) t2) with
      | None -> None
      | Some s2 -> Some (s2 @ (substituteSubst (Map.ofList s2) s1))
  | _ -> None
and unify t1 t2 : option<list<string * Term>> = 
  match t1, t2 with 
  | Atom a1, Atom a2 ->
    if (a1 = a2) then Some [] else None
  | Predicate(p1, l1), Predicate(p2, l2) ->
    if (p1 = p2) then unifyList l1 l2 else None
  | Variable v, t -> Some [(v, t)]
  | t, Variable v -> Some [(v, t)]
  | _ -> None


// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  match term with
  | Variable var -> [ var ]
  | Predicate(pred, terms) ->
    List.collect freeVariables terms
  | _ -> []



let withFreshVariables (clause:Clause) : Clause =
  let distinctVariables = List.distinct ((freeVariables clause.Head) @ (List.collect freeVariables clause.Body))
  let freshSubst = Map.ofList [for var in distinctVariables -> (var, Variable (var + (nextNumber()).ToString()))]
  { Head = substitute freshSubst (clause.Head); Body = substituteTerms freshSubst (clause.Body) }


let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =
    List.choose (
      fun clause ->
      let freshClause = withFreshVariables clause 
      match unify freshClause.Head query with
      | Some(subst) -> Some(clause, subst)
      | _ -> None
      ) program

let rec solve program subst goals = 
  match goals with 
  | g::goals -> 
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = substituteTerms (Map.ofList newSubst) (clause.Body @ goals)
        solve program (newSubst @ (substituteSubst (Map.ofList newSubst) subst)) newGoals
  | [] -> 
    printfn "%A" subst

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> William, Y -> George, ... ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]

