(**
- title : Lets see what we can do! with F# Computation Expressions 
- description: not sure where this goes
- author : Andrew Browne
- transition : default

***

### Lets see what we can do! with F# Computation Expressions 
By Andrew Browne

*)

(*** hide ***)
let lookupName (id : int) = Some "Andrew"
let lookupAge (id : int) = Some 32

(*** define: maybe-builder ***)
type MaybeBuilder() = 
  member this.Return(x) = Some x

  member this.Bind(m, f) = Option.bind f m

let maybe = MaybeBuilder()

(*** define: getdetails ***)
let getCompleteDetails id = maybe {  
  let! name = lookupName id
  let! age = lookupAge id
  return (name, age)
}

(*** hide ***)
let completeDetails = getCompleteDetails 1

(**
***
### Maybe 
*)
(*** include: getdetails ***)
(*** include-value: completeDetails ***)

(**
***
### Maybe Desugared
*)

let getCompleteDetails2 id = 
   maybe.Bind(lookupName id, (fun name ->
      maybe.Bind(lookupAge id, (fun age ->
        maybe.Return((name,age)
   ))))
(**
***
### Maybe Builder
*)
(*** include: maybe-builder ***)

(**
***
### Option.bind
*)
let bind f input = 
   match input with 
   | None -> None 
   | Some x -> f x