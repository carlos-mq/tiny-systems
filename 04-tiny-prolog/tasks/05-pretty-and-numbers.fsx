// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  match term with 
  | Atom("zero") -> Some(0)
  | Predicate("succ", [nTerm]) ->
    match nTerm with
    | Number n -> Some(n + 1)
    | _ -> None
  | _ -> None



let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      let formattedItems = List.map formatTerm items
      p + "(" + String.concat "," formattedItems + ")"

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
      | Some(subst) -> Some(freshClause, subst)
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
    printfn "Solution:" 
    for var, term in subst do
      printfn "%s -> %s" var (formatTerm term)

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Queries from previous step (now with readable output)
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n = 
  if n = 0 then Atom("zero") else Predicate("succ", [num(n - 1)])



// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// Query: add(2, 3, X)
// Output should include: 'X = 5' 
//   (and other variables resulting from recursive calls)
solve nums [] [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3' 
//   (we can use 'add' to calculate subtraction too!)
solve nums [] [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))' 
//   (with some number for ?? - indicating that this can be any term)
solve nums [] [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
