// ----------------------------------------------------------------------------
// 01 - Implementing slot lookup and strings
// ----------------------------------------------------------------------------

// Slots of an object have name and contents. Some can be 
// marked as parent slots (message lookup looks into them).
// In Self, parent slots have * at the end of their name.
// This type is immutable. When we want to change the value,
// we replace it in the 'slots' table of the containing object.
type Slot = 
  { Name : string
    Contents : Objekt
    IsParent : bool } 

// Object mainly has slots. It can be runnable (and have code).
// Slots of an object with code can be used to pass arguments to it.
// In TinySelf, we also have special objects (strings, native code).
and Objekt = 
  { mutable Slots : Slot list 
    mutable Code : Objekt option
    mutable Special : Special option }

// Special objects are used to represent primitive string values
// and "built-in" methods not implemented in TinySelf.
and Special = 
  | String of string
  | Native of (Objekt -> Objekt)

// Implements a simple terminal-based object visualizer for TinySelf. It 
// prints objects as boxes with numerical identifiers for tracing references.
// Use `Vis.printObjectTree obj` to visualize an object `obj`!
#load "objekt-visualizer.fs"
open TinySelf

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

// Object with a list of slots and some code
let makeCodeObject slots code = 
  { Code = Some code; Special = None; Slots = slots }
// Data object is an object that does not have code
let makeObject slots = 
  { Code = None; Special = None; Slots = slots }
// Special object such as String or Native method
let makeSpecialObject special = 
  { Code = None; Special = Some special; Slots = [] }

// Regular (non-parent) slot with a name and an object
let makeSlot (n:string) contents = 
  if n.EndsWith("*") then failwith "Non-parent slot names should not end with '*'."
  { Name = n; Contents = contents; IsParent = false }

// Parent slot (by convention the name should end with *) 
let makeParentSlot (n:string) contents = 
  if not (n.EndsWith("*")) then failwith "Parent slot names should end with '*'."
  { Name = n; Contents = contents; IsParent = true }

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

// See also §3.3.8 (https://handbook.selflanguage.org/SelfHandbook2017.1.pdf)
// Note that we do not need to keep track of visited objects as we will not
// create cyclic inheritance graphs in TinySelf.

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

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

// We represent strings as objects with a slot 'value' containing the special
// string object. This extra wrapping makes it easier to add methods to strings
// later, because the methods can get the value by sending 'value' to self.
let makeString s = 
  makeObject [ makeSlot "value" (makeSpecialObject (String s)) ]

let lookupSlotValue (msg:string) (obj:Objekt) : Objekt  =
  match lookup msg obj with
  | [slot] -> slot.Contents
  | [] -> failwith "No slots found!"
  | _ -> failwith "Too many slots found!"

// Get the actual string value from a string object (or fail)
let getStringValue (obj:Objekt) : string = 
  // TODO: Get the value of 'value' slot using 'lookupSlotValue'
  // This should be an object that has 'Special' set to 'Some str'
  // Return the string value!
  let o = lookupSlotValue "value" obj
  match o.Special with
  | Some (String s) -> s
  | _ -> failwith "The object doesn't have a string at a 'value' slot!"

// Ad-hoc helper for testing that prints a string result of 'lookup'
let printStringSlot slots = 
  match slots with 
  | [s] -> printfn "%s" (getStringValue s.Contents)
  | [] -> printfn "printStringSlot: Error - no slot found"
  | _ -> printfn "printStringSlot: Error - more than one slot found"

// ----------------------------------------------------------------------------
// Tests - experimenting with strings
// ----------------------------------------------------------------------------

// DEMO: Create and visualize simple string object

let hello = makeString "Hello world"
Vis.printObjectTree hello

hello |> lookupSlotValue "value"
hello |> getStringValue |> printfn "%s"

// DEMO: Create and visualize object with multiple string-object slots

let multilang = makeObject [
  makeSlot "english" (makeString "Hello world")
  makeSlot "czech" (makeString "Ahoj svete")
  makeSlot "german" (makeString "Hallo Welt")
  makeSlot "french" (makeString "Bonjour monde")
]
Vis.printObjectTree multilang

multilang |> lookup "english" |> printStringSlot
multilang |> lookup "czech" |> printStringSlot


// ----------------------------------------------------------------------------
// Tests - lookups in a hierarchy of cats!
// ----------------------------------------------------------------------------

let cat = makeObject [
  makeSlot "sound" (makeString "Meow")
]
let larry = makeObject [
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Larry")
]
Vis.printObjectTree larry

// Larry has name & sound, but no book!
larry |> lookup "name" |> printStringSlot
larry |> lookup "sound" |> printStringSlot
larry |> lookup "book" |> printStringSlot

// TODO: Cheshire cat has a name ("Cheshire Cat") and is 
// both a cat (with parent 'cat') and fictional character 
// from a book (with parent 'wonderland')
let wonderland = makeObject [
  makeSlot "book" (makeString "Alice in Wonderland")
]
let cheshire = makeObject [
  makeParentSlot "parent*" cat
  makeParentSlot "fictional*" wonderland
  makeSlot "name" (makeString "Cheshire Cat")
]
Vis.printObjectTree cheshire

// All of these should be OK!
cheshire |> lookup "name" |> printStringSlot
cheshire |> lookup "sound" |> printStringSlot
cheshire |> lookup "book" |> printStringSlot

