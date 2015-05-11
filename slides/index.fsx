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
(**
***
### AsyncSeq
*)
type AsyncSeq<'T> = Async<AsyncSeqInner<'T>> 
and AsyncSeqInner<'T> = 
    internal
    | Nil
    | Cons of 'T * AsyncSeq<'T>

(**
***
- class : much-code

### Async Seq Builder
*)
module AsyncSeq =
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

  type AsyncSeqBuilder() =
    member x.Yield(v) = singleton v
    member x.YieldFrom(s) = s
    member x.Combine (seq1:AsyncSeq<'T>,seq2:AsyncSeq<'T>) = 
      append seq1 seq2
    member x.Bind (
      inp:Async<'T>, 
      body : 'T -> AsyncSeq<'U>) 
      : AsyncSeq<'U> = 
      async.Bind(inp, body)
    member x.Delay (f:unit -> AsyncSeq<'T>) = 
      async.Delay(f)
    member x.While (gd, seq:AsyncSeq<'T>) = 
      if gd() then x.Combine(seq,x.Delay(fun () -> x.While (gd, seq))) else x.Zero()
    member x.Zero () = empty
    
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
(*** hide ***)
open AsyncSeq
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
let filterAsync f (input : AsyncSeq<'T>) = asyncSeq {
  for v in input do
    let! b = f v
    if b then yield v }
