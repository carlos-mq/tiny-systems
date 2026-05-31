// ----------------------------------------------------------------------------
// 03 - Supporting method arguments and assignment slots
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

let rec appendCode = makeNativeMethod (fun arcd -> 
  let s1 = getStringValue arcd
  let s2 = getStringValue (send "other" empty arcd)
  makeString (s1 + s2)
)
and stringPrototype = makeObject [
  makeSlot "print" printCode  
  makeSlot "append" appendCode  
]
and makeString s = 
  makeObject [ 
    makeSlot "value" (makeSpecialObject (String s)) 
    makeParentSlot "string*" stringPrototype
  ]

// ----------------------------------------------------------------------------
// Tests - printing
// ----------------------------------------------------------------------------

// Append a bunch of strings and print the result!

let s1 = makeString "hello"
let s2 = makeString " "
let s3 = makeString "world"
let s4 = makeString "!"

s1 
|> send "append" (makeObject [ makeSlot "other" s2 ])
|> send "append" (makeObject [ makeSlot "other" s3 ])
|> send "append" (makeObject [ makeSlot "other" s4 ])
|> send "print" empty
|> ignore

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


// Creates an assignment slot for a slot named 'n'
let makeAssignmentSlot n = 
  { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }


// ----------------------------------------------------------------------------
// Tests - over-engineered prototype-based Hello world
// ----------------------------------------------------------------------------

let greeter = makeObject [
  makeSlot "greeting" (makeString "Hello")
  makeSlot "greet" (makeNativeMethod (fun acrd ->
    // This is implemented as native method, but note that it is using
    // only TinySelf operations - it is creating primitive string values
    // and sending messages. We do not need any F# here!
    let who = send "who" empty acrd
    let sp, ex = makeString " ", makeString "!"
    send "greeting" empty acrd
    |> send "append" (makeObject [ makeSlot "other" sp ])
    |> send "append" (makeObject [ makeSlot "other" who ])
    |> send "append" (makeObject [ makeSlot "other" ex ])
    |> send "print" empty
  ))
]

// Hello world has an assignment slot for 'who'
let helloWorld = makeObject [
  makeParentSlot "greeter*" greeter
  makeSlot "who" (makeString "world")
  makeAssignmentSlot "who"
]
// .. but Hello Matfyz doesn't, so we cannot change its 'who'.
let helloMatfyz = makeObject [
  makeParentSlot "greeter*" greeter
  makeSlot "who" (makeString "Matfyz")
]
helloWorld |> send "greet" empty |> ignore
helloMatfyz |> send "greet" empty |> ignore

// This changes the 'who'. Run the above snippet to verify this!
helloWorld |> send "who:" (makeObject [ makeSlot "new" (makeString "svete") ]) 
// This throws an exception - there is no assignment slot.
helloMatfyz |> send "who:" (makeObject [ makeSlot "new" (makeString "CVUT") ]) 

// We can create assignment slots in derived objects and 
// the assignemtn changes the value in the parent!
let greetingSetter = makeObject [
  makeParentSlot "greeter*" greeter
  makeAssignmentSlot "greeting"
]
// We can invoke the assignment slot in 'greetingSetter'
greetingSetter |> send "greeting:" (makeObject [ makeSlot "new" (makeString "Ahoj") ]) 
// But not in another object that has 'greeter' as parent
helloMatfyz |> send "greeting:" (makeObject [ makeSlot "new" (makeString "Ahoj") ]) 

Vis.printObjectsTree [helloMatfyz; helloWorld; greetingSetter]

