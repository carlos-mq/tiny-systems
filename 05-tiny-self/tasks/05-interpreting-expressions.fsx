// ----------------------------------------------------------------------------
// 05 - Representing and interpreting expressions
// ----------------------------------------------------------------------------

type Slot = 
  { Name : string
    Contents : Objekt
    IsParent : bool } 

and Objekt = 
  { mutable Slots : Slot list 
    mutable Code : Objekt option
    mutable Special : Special option }

and Special = 
  | String of string
  | Native of (Objekt -> Objekt)

#load "objekt-visualizer.fs"
open TinySelf

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

let makeCodeObject slots code = 
  { Code = Some code; Special = None; Slots = slots }
let makeObject slots = 
  { Code = None; Special = None; Slots = slots }
let makeSpecialObject special = 
  { Code = None; Special = Some special; Slots = [] }

let makeSlot (n:string) contents = 
  if n.EndsWith("*") then failwith "Non-parent slot names should not end with '*'."
  { Name = n; Contents = contents; IsParent = false }
let makeParentSlot (n:string) contents = 
  if not (n.EndsWith("*")) then failwith "Parent slot names should end with '*'."
  { Name = n; Contents = contents; IsParent = true }

let makeNativeMethod f =
  makeCodeObject [] (makeSpecialObject (Native(f)))

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

let rec lookupSlotName (name : string) (slots : list<Slot>) : option<Slot> =
  match slots with
  | [] -> None
  | s1 :: ss ->
    match s1 with
    | { Name = n} -> if (n = name) then Some s1 else (lookupSlotName name ss)

let rec findAllParentObjekts (slots : list<Slot>) : list<Objekt> =
  match slots with
  | [] -> []
  | s1 :: ss ->
    if s1.IsParent 
      then (s1.Contents :: (findAllParentObjekts ss)) 
      else (findAllParentObjekts ss)

let rec lookup (msg:string) (obj:Objekt) : list<Objekt * Slot> =
  match lookupSlotName msg obj.Slots with
  | Some s -> [(obj, s)]
  | _ ->
    let parents = findAllParentObjekts obj.Slots
    List.collect (lookup msg) parents

let eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) : Objekt =
  match slotValue with
  | { Code = None } -> slotValue
  | { Code = Some c } -> 
    match c with
    | { Special = Some (Native f)} ->
      let receiverParent = makeParentSlot "receiver*" instance
      let argsParent = makeParentSlot "args*" args
      let activationRecord = { slotValue with Slots = ([receiverParent; argsParent] @ slotValue.Slots)}
      f activationRecord
    | _ -> failwith "Non-native code not yet supported!"


let send (msg:string) (args:Objekt) (instance:Objekt) : Objekt =
  match lookup msg instance with
  | [(_, slot)] -> eval slot.Contents args instance
  | [] -> failwith "No slot with that name found!"
  | _ -> failwith "Too many slots with that name found!"

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let empty : Objekt = makeObject []

let getStringValue (obj:Objekt) : string = 
  let o = send "value" empty obj
  match o.Special with
  | Some (String s) -> s
  | _ -> failwith "The object doesn't have a string at a 'value' slot!"


let printCode = makeNativeMethod (fun arcd ->
  let s = getStringValue arcd 
  printfn "%s" s
  empty
)

// ----------------------------------------------------------------------------
// Assignment slots
// ----------------------------------------------------------------------------

let assignmentMethod n = makeNativeMethod (fun arcd -> 
  let newSlot = 
    match lookup "new" arcd with
    | [(_, s)] -> s
    | _ -> failwith "New value not found!"
  let obj =
    match lookup n arcd with
    | [(o, _)] -> o
    | _ -> failwith "Named slot not found!"
  let renamedNewSlot = { newSlot with Name = n }
  let newSlots' =
    [for s in obj.Slots -> 
      if (s.Name = n) then renamedNewSlot else s
    ]
  obj.Slots <- newSlots'
  obj
  )

let makeAssignmentSlot n = 
  { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }

// ----------------------------------------------------------------------------
// Primitive types - Booleans, strings and blocks
// ----------------------------------------------------------------------------

let makeBoolean (b:bool) = makeObject [
  makeSlot "if" (makeNativeMethod (fun arcd -> 
    let block =
      if b then
        match lookup "then" arcd with
        | [(_, s)] -> s.Contents
        | _ -> failwith "No 'then' block found!"
      else
        match lookup "else" arcd with
        | [(_, s)] -> s.Contents
        | _ -> failwith "No 'else' block found!"
    send "run" empty block
  ))
]

let trueObj = makeBoolean true
let falseObj = makeBoolean false

let equalsCode = makeNativeMethod (fun arcd -> 
  let s1 = getStringValue arcd
  let s2 = getStringValue (send "other" empty arcd)
  if (s1 = s2)
    then trueObj
    else falseObj
)

let rec appendCode = makeNativeMethod (fun arcd -> 
  let s1 = getStringValue arcd
  let s2 = getStringValue (send "other" empty arcd)
  makeString (s1 + s2)
)
and stringPrototype = makeObject [
  makeSlot "print" printCode  
  makeSlot "append" appendCode  
  makeSlot "equals" equalsCode 
]
and makeString s = 
  makeObject [ 
    makeSlot "value" (makeSpecialObject (String s)) 
    makeParentSlot "string*" stringPrototype
  ]

let makeBlock f = makeObject [ 
  makeSlot "run" (makeNativeMethod (fun _ -> f()))
]

// ----------------------------------------------------------------------------
// Representing and interpreting expressions
// ----------------------------------------------------------------------------

let exprSend (msg:string) (args:list<string * Objekt>) (target:Objekt) = 
  makeObject [
    makeSlot "exprkind" (makeString "send")
    makeSlot "message" (makeString msg)
    makeSlot "target" target
    makeSlot "args" (makeObject 
    (List.map (fun (slotName, slotValue) -> makeSlot slotName slotValue) args))
  ]
let exprConst obj = 
  makeObject [
    makeSlot "exprkind" (makeString "const")
    makeSlot "value" obj
  ]  
let exprBlock body = 
  makeObject [
    makeSlot "exprkind" (makeString "block")
    makeSlot "body" body
  ]  


let rec evalExpr expr =
  let kind = expr |> send "exprkind" empty |> getStringValue
  match kind with 
  | "const" ->
      send "value" empty expr
  | "block" ->
      let body = send "body" empty expr
      makeBlock (fun () ->
        evalExpr body
      )
  | "send" ->
      let msg = getStringValue (send "message" empty expr)
      let target = evalExpr (send "target" empty expr)
      let argsObject = send "args" empty expr
      let evaluatedArgsSlots = 
        List.map (fun slot -> makeSlot slot.Name (evalExpr slot.Contents)) argsObject.Slots
      let args = makeObject evaluatedArgsSlots
      target |> send msg args
  | _ -> 
      failwithf "Unknown expression kind: %s" kind

// ----------------------------------------------------------------------------
// Tests - trivial hello world
// ----------------------------------------------------------------------------

let helloCode1 = 
  (exprConst (makeString "Hello "))
  |> exprSend "append" [ "other", exprConst (makeString "world!") ]
  |> exprSend "print" []

// Visualise object tree to a given depth. If there are too many
// objects and the window is small, this fails. Try setting the 
// limit to smaller number if this happens!
Vis.printObjectTreeLimit 3 helloCode1
helloCode1 |> evalExpr |> ignore


// ----------------------------------------------------------------------------
// Prisoner's dilemma
// ----------------------------------------------------------------------------

let betray = makeString "betray"
let coop = makeString "cooperate"

let cc = makeString "cooperate-cooperate: each serves 1 year"
let cb = makeString "cooperate-betray: #1 gets 3 years, #2 is free"

let bc = makeString "betray-cooperate: #1 is free, #2 gets 3 years"

let bb = makeString "betray-betray: each serves 2 years"

let rnd = System.Random()
let player1 = if rnd.Next(2) = 0 then betray else coop
let player2 = if rnd.Next(2) = 0 then betray else coop


let code = 
  (exprConst player1)
  |> exprSend "equals" [ "other", exprConst coop ]
  |> exprSend "if" [
    "then", exprBlock (
      exprConst player2
      |> exprSend "equals" ["other", exprConst coop ]
      |> exprSend "if" [
        "then", exprBlock (exprConst cc)
        "else", exprBlock (exprConst cb)
      ]
    )
    "else", exprBlock (
        exprConst player2
        |> exprSend "equals" ["other", exprConst coop]
        |> exprSend "if" [
          "then", exprBlock (exprConst bc)
          "else", exprBlock (exprConst bb)
        ]
      )
  ]
  |> exprSend "print" []

  
// Vis.printObjectTreeLimit 3 code
evalExpr code
