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

  member this.Bind(input, f) =
      match input with 
      | None -> None 
      | Some x -> f x 

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
       )))))
(**
***
### Maybe Builder
*)
(*** include: maybe-builder ***)

(**
***
### AsyncSeq
*)
type AsyncSeq<'T> = Async<AsyncSeqInner<'T>> 
and AsyncSeqInner<'T> = 
    internal
    | Nil
    | Cons of 'T * AsyncSeq<'T>

(*** hide ***)
[<GeneralizableValue>]
let empty<'T> : AsyncSeq<'T> = 
    async { return Nil }

let singleton (v:'T) : AsyncSeq<'T> = 
    async { return Cons(v, empty) }

let rec append (seq1: AsyncSeq<'T>) (seq2: AsyncSeq<'T>) : AsyncSeq<'T> = 
    async { let! v1 = seq1
            match v1 with 
            | Nil -> return! seq2
            | Cons (h,t) -> return Cons(h,append t seq2) }
(**
***
- class : much-code

### Async Seq Builder
*)
type AsyncSeqBuilder() =
    member x.Yield(v) = singleton v
    member x.YieldFrom (s) = s
    member x.Combine (seq1,seq2) = 
      append seq1 seq2
    member x.Bind (inp, body) = 
      async.Bind(inp, body)
    member x.Delay (f:unit -> AsyncSeq<'T>) = 
      async.Delay(f)
    member x.Zero () = empty
    
(*** hide ***)
let asyncSeq = new AsyncSeqBuilder()

/// Tries to get the next element of an asynchronous sequence
/// and returns either the value or an exception
let internal tryNext (input:AsyncSeq<_>) = async { 
    try 
      let! v = input
      return Choice1Of2 v
    with e -> 
      return Choice2Of2 e }

/// Implements the 'TryFinally' functionality for computation builder
let rec internal tryFinally (input : AsyncSeq<'T>) compensation = asyncSeq {
    let! v = tryNext input
    match v with 
    | Choice1Of2 Nil -> 
        compensation()
    | Choice1Of2 (Cons (h, t)) -> 
        yield h
        yield! tryFinally t compensation
    | Choice2Of2 e -> 
        compensation()
        yield! raise e }

let rec collect f (input : AsyncSeq<'T>) : AsyncSeq<'TResult> = asyncSeq {
    let! v = input
    match v with
    | Nil -> ()
    | Cons(h, t) ->
        yield! f h
        yield! collect f t }

type AsyncSeqBuilder with
    member x.While (gd, seq:AsyncSeq<'T>) = 
      if gd() then x.Combine(seq,x.Delay(fun () -> x.While (gd, seq))) else x.Zero()
    member x.TryFinally (body: AsyncSeq<'T>, compensation) = 
      tryFinally body compensation   
    member x.For(seq:seq<'T>, action:'T -> AsyncSeq<'TResult>) = 
      let enum = seq.GetEnumerator()
      x.TryFinally(x.While((fun () -> enum.MoveNext()), x.Delay(fun () -> 
        action enum.Current)), (fun () -> 
          if enum <> null then enum.Dispose() ))
    member x.For (seq:AsyncSeq<'T>, action:'T -> AsyncSeq<'TResult>) = 
      collect action seq
(**
***
### Async Seq Paging
*)
let getPage n : Async<seq<int>> = 
  async {
    if(n < 3) then
      return seq {
         for i in [n*10..n*10 + 9] do
           yield i
      }
    else
      return Seq.empty
  }
(**
***
### Async Seq Paging
*)
let rec resultLoop n = asyncSeq {
  let! results = getPage n
  for r in results do
    yield r
  if results <> Seq.empty then
    yield! resultLoop (n + 1)
}
let getAllResults = resultLoop 0

(**
***
### Async Seq Paging
*)
let withPageNumber = asyncSeq {
  for result in getAllResults do
    yield sprintf "Item: %d" result
}
(**
***
### Async Seq Paging
*)
let filterAsync f (input : AsyncSeq<'T>) = 
  asyncSeq {
      for v in input do
        let! b = f v
        if b then yield v }

(*** hide ***)

type WriteResult =
| WriteSuccess
| WrongExpectedVersion
| WriteError of System.Exception

type Key = string
type Version = int
(**
***
### A Simple DSL
*)
type KeyValueLanguage<'N> =
    | ReadValue of 
        Key * ((Version * string) option -> 'N)
    | WriteValue of 
        Key * Version * string * (WriteResult -> 'N)
and 
    FreeKeyValue<'F,'R> = 
    | FreeKeyValue of  
       KeyValueLanguage<FreeKeyValue<'F,'R>>
    | Pure of 'R

(*** hide ***)
let fmap f streamWorker = 
    match streamWorker with
    | ReadValue (key, streamRead) -> 
        ReadValue (key, (streamRead >> f))
    | WriteValue (key, expectedVersion, value, next) -> 
        WriteValue (key, expectedVersion, value, (next >> f))

let liftF command = FreeKeyValue (fmap Pure command)

let rec bind' f v =
    match v with
    | FreeKeyValue x -> FreeKeyValue (fmap (bind' f) x)
    | Pure r -> f r

// Return the final value wrapped in the Free type.
let result value = Pure value

// The whileLoop operator.
// This is boilerplate in terms of "result" and "bind".
let rec whileLoop pred body =
    if pred() then body |> bind' (fun _ -> whileLoop pred body)
    else result ()

// The delay operator.
let delay (func : unit -> FreeKeyValue<'a,'b>) : FreeKeyValue<'a,'b> = 
    func()

// The sequential composition operator.
// This is boilerplate in terms of "result" and "bind".
let combine expr1 expr2 =
    expr1 |> bind' (fun () -> expr2)

// The forLoop operator.
// This is boilerplate in terms of "catch", "result", and "bind".
let forLoop (collection:seq<_>) func =
    let ie = collection.GetEnumerator()
    (whileLoop (fun () -> ie.MoveNext())
        (delay (fun () -> let value = ie.Current in func value)))

(**
***
- class : dsl-functions

### DSL Functions
*)
let readValue key = 
    FreeKeyValue (ReadValue (key, Pure))
let writeValue key version value = 
    FreeKeyValue (WriteValue (key, version, value, Pure))


(*** hide ***)
type InterpeterState<'T> =
| Continue of (Map<string,(int * string)> * FreeKeyValue<obj,'T>) 
| Complete of (Map<string,(int * string)> * 'T)
(**
***
- class : dsl-interpreter

### DSL Interpreter
*)
let runStep 
    (prog : FreeKeyValue<obj,'T>)
    (dataStore : Map<Key,(Version * string)>) 
    : InterpeterState<'T> =
    match prog with
    | FreeKeyValue (ReadValue (key, f)) ->
        let value = Map.tryFind key dataStore
        let next = f value
        Continue (dataStore, next)
    | FreeKeyValue (WriteValue (key, version, value, f)) ->
        let currentValue = Map.tryFind key dataStore
        match currentValue with
        | None when version = 0 ->
           let dataStore' = Map.add key (version, value) dataStore
           Continue (dataStore', f WriteSuccess)
        | Some (currentVersion, _) when currentVersion + 1 = version -> 
           let dataStore' = Map.add key (version, value) dataStore
           Continue (dataStore', f WriteSuccess)
        | _ -> 
           Continue (dataStore, f WrongExpectedVersion)
    | Pure result ->
        Complete (dataStore,result)

(*** hide ***)
let interpret
    (prog : FreeKeyValue<obj,'T>)
    (dataStore : Map<Key,(Version * string)>) 
    : (Map<Key,(Version * string)> * 'T) =
    let rec loop 
       (state: InterpeterState<'T>) =
       match state with
       | Complete r -> r
       | Continue (state, p) ->
          loop <| runStep p state

    loop (Continue (dataStore, prog))

(**
***
- class : dsl-builder

### DSL Builder
*)
type KeyValueBuilder() =
    member x.Zero() = Pure ()
    member x.Return(r:'R) : FreeKeyValue<'F,'R> = Pure r
    member x.ReturnFrom(r:FreeKeyValue<'F,'R>) = r
    member x.Bind (inp, body) = bind' body inp
    member x.Combine(expr1, expr2) = combine expr1 expr2
    member x.For(a, f) = forLoop a f 
    member x.While(func, body) = whileLoop func body
    member x.Delay(func) = delay func

let keyValue = new KeyValueBuilder()

(**
***
- class : dsl-example

### DSL example
*)
let appendValue key value = keyValue {
     let! current = readValue key
     match current with
     | Some (version, currentValue) -> 
         let newValue = currentValue + value
         let! result = 
             writeValue key (version + 1) newValue
         return result
     | None ->
         let! result = writeValue key 0 value
         return result
   }

let appendResult = interpret (appendValue "key" "abcd") Map.empty

(*** include-value: appendResult ***)