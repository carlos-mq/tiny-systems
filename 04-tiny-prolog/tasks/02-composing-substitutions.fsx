// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
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
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
unify
  (Predicate("loves", [Atom("narcissus"); Atom("narcissus")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
unify
  (Predicate("loves", [Atom("odysseus"); Atom("penelope")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
unify
  (Predicate("add", [Atom("zero"); Predicate("succ", [Atom("zero")])]))
  (Predicate("add", [Variable("Y"); Predicate("succ", [Variable("Y")])]))

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
unify
  (Predicate("loves", [Variable("X"); Atom("narcissus")]))
  (Predicate("loves", [Variable("Y"); Variable("X")]))

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
unify
  (Predicate("add", 
      [ Predicate("succ", [Variable("X")]); 
        Variable("X") ]))
  (Predicate("add", 
      [ Variable("Y"); 
        Predicate("succ", [Variable("Z")]) ]))

