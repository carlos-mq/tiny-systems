// ----------------------------------------------------------------------------
// 02 - Implementing (basic) message sending
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

// Native method has a special object (F# function) as code
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

let rec lookup (msg:string) (obj:Objekt) : list<Slot> =
  match lookupSlotName msg obj.Slots with
  | Some s -> [s]
  | _ ->
    let parents = findAllParentObjekts obj.Slots
    List.collect (lookup msg) parents


// See also §3.3.7 (https://handbook.selflanguage.org/SelfHandbook2017.1.pdf)
// Note that we do not need special "primitive sends". Instead, we have special
// objects and so we need to run the "native" method when we it is called.
//
// Also not that we do not yet support passing arguments to methods!

let eval (slotValue:Objekt) (instance:Objekt) : Objekt =
  match slotValue with
  | { Code = None } -> slotValue
  | { Code = Some c } -> 
    match c with
    | { Special = Some (Native f)} ->
      let activationRecord = { slotValue with Slots = (makeParentSlot "receiver*" instance)::(slotValue.Slots)}
      f activationRecord
    | _ -> failwith "Non-native code not yet supported!"

let send (msg:string) (instance:Objekt) : Objekt =
  match lookup msg instance with
  | [slot] -> eval slot.Contents instance
  | [] -> failwith "No slot with that name found!"
  | _ -> failwith "Too many slots with that name found!"



// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let getStringValue (obj:Objekt) : string = 
  let o = send "value" obj
  match o.Special with
  | Some (String s) -> s
  | _ -> failwith "The object doesn't have a string at a 'value' slot!"

let empty : Objekt = makeObject []

let printCode = makeNativeMethod (fun arcd ->
  let s = getStringValue arcd 
  printfn "%s" s
  empty
)


let stringPrototype = makeObject [
  makeSlot "print" printCode  
]
let makeString s = 
  makeObject [ 
    makeSlot "value" (makeSpecialObject (String s))
    makeParentSlot "parent*" stringPrototype 
  ]

// ----------------------------------------------------------------------------
// Tests - experimenting with strings
// ----------------------------------------------------------------------------

// DEMO: Create and visualize simple string object

let hello = makeString "Hello world"
hello |> send "print"

// DEMO: Create and visualize object with multiple string-object slots

let multilang = makeObject [
  makeSlot "english" (makeString "Hello world")
  makeSlot "czech" (makeString "Ahoj svete")
  makeSlot "german" (makeString "Hallo Welt")
  makeSlot "french" (makeString "Bonjour monde")
]
Vis.printObjectTree multilang


multilang |> send "english" |> send "print"
multilang |> send "czech" |> send "print"


// ----------------------------------------------------------------------------
// Tests - lookups in a hierarchy of cats!
// ----------------------------------------------------------------------------

// NOTE: Now we can do all of the below just by sending messages!
// We send message to get a slot value and then send another 
// message to invoke the printing method.

let cat = makeObject [
  makeSlot "sound" (makeString "Meow")
]
let larry = makeObject [
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Larry")
]
// Larry has name & sound, but no book!
larry |> send "name" |> send "print"
larry |> send "sound" |> send "print"
larry |> send "book" |> send "print"

let wonderland = makeObject [
  makeSlot "book" (makeString "Alice in Wonderland")
]

let cheshire = makeObject [
  makeParentSlot "parent*" cat
  makeParentSlot "fictional*" wonderland
  makeSlot "name" (makeString "Cheshire Cat")
]

// All of these should be OK!
cheshire |> send "name" |> send "print"
cheshire |> send "sound" |> send "print"
cheshire |> send "book" |> send "print"

