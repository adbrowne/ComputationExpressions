(**
- title : Lets see what we can do! with F# Computation Expressions 
- description: not sure where this goes
- author : Andrew Browne
- theme : solarized
- transition : none

***

### Lets see what we can do! with F# Computation Expressions 
By Andrew Browne

*)

(*** hide ***)
open System.Net
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
### Seq 
*)
let seqResult = seq {
    yield 1
    yield! seq {
        yield 2
        yield 3
    }
}
(*** include-value: seqResult ***)
(*** hide ***)
let fetchAsync (url:string) : Async<string> =
    let uri = new System.Uri(url)
    let webClient = new WebClient()
    webClient.AsyncDownloadString(uri)
(**
***
### Async
*)
let readUriAsync(name, url:string) = async { 
    try 
        let! html = fetchAsync url
        printfn 
           "Read %d characters for %s" 
           html.Length 
           name
    with
        | ex -> printfn "%s" (ex.Message);
}
(**
***
### Option
    [lang=fsharp]
    module MyOption = 
    type Option<'T> =
    | Some of 'T
    | None

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

(*** hide ***)
type CExpr =
  Yield of string
  | YieldFrom of string
  | Combine of CExpr * CExpr
  | Delay of CExpr
  | Bind of string * CExpr
  | Zero
  | Return of string
  | ReturnFrom of string
  | For of string * string
  | Run of CExpr
  | TryFinally of CExpr * string
  | TryWith of CExpr * string
  | Using of string * CExpr
  | While of string * CExpr

type CEDebugBuilder() =
   member this.Yield(i) = Yield <| sprintf "%A" i
   member this.YieldFrom(i) = YieldFrom <| sprintf "%A" i
   member this.Combine(a,b) = Combine (a, b)
   member this.Delay(f) =  Delay <| f()
   member this.Bind(a,f) = Bind (sprintf "%A" a, f())
   member this.Zero() = Zero
   member this.Return(i) = Return <| sprintf "%A" i
   member this.ReturnFrom(i) = ReturnFrom <| sprintf "%A" i
   member this.For(s,f) = For (sprintf "%A" s, sprintf "Function %A" (f))
   member this.TryFinally (f, e) = TryFinally (f,sprintf "error handler %A" e)
   member this.TryWith(expr : CExpr, handler: System.Exception -> CExpr) : CExpr = TryWith(expr, sprintf "%A" handler)
   member this.Using (v,f) = Using (sprintf "%A" v, f())
   member this.While (v,f) = While (sprintf "%A" (v()), f)

let bar = new CEDebugBuilder()

type CEDebugBuilderNoDelay() =
   member this.Yield(i) = Yield <| sprintf "%A" i
   member this.YieldFrom(i) = YieldFrom <| sprintf "%A" i
   member this.Combine(a,b) = Combine (a, b)
   member this.Bind(a,f) = Bind (sprintf "%A" a, f())
   member this.Zero() = Zero
   member this.Return(i) = Return <| sprintf "%A" i
   member this.ReturnFrom(i) = ReturnFrom <| sprintf "%A" i
   member this.For(s,f) = For (sprintf "%A" s, sprintf "Function %A" (f))
   member this.TryFinally (f,e) = TryFinally (f,sprintf "%A" e)
   member this.TryWith(expr : CExpr, handler: System.Exception -> CExpr) : CExpr = TryWith(expr, sprintf "%A" handler)
   member this.Using (v,f) = Using (sprintf "%A" v, f())
   member this.While (v,f) = While (sprintf "%A" (v()), f)

let foo = new CEDebugBuilderNoDelay()

(**
***
### Transformations: yield
*)
foo {
    yield 1
}
(**
*)
foo.Yield(1)
(**
---
### Transformations: yield!
*)
foo {
    yield! 1
}
(**
*)
foo.YieldFrom(1)
(**
---
### Transformations: return
*)
foo {
    return 1
}
(**
*)
foo.Return(1)
(**
---
### Transformations: return!
*)
foo {
    return! 1
}
(**
*)
foo.ReturnFrom(1)
(**
---
### Transformations: if else
*)
foo {
    if true then
       return 1
    else
       return 2
}
(**
*)
if true then
   foo.Return(1)
else
   foo.Return(2)
(**
---
### Transformations: if
*)
foo {
    if true then
       return 1
}
(**
*)
if false then
   foo.Return(1)
else
   foo.Zero()
(**
---
### Transformations: use
*)
foo {
    use a = 1
    return a
}
(**
*)
foo.Using(1, fun a -> foo.Return(a))
(**
---
### Transformations: use!
*)
foo {
    use! a = 1
    return a
}
(**
*)
foo.Bind(
  1, 
  fun a -> 
    foo.Using(a, 
      fun a -> foo.Return(a)))
(**
---
### Transformations: do!
*)
foo {
    do! printf "1"
    return 2
}
(**
*)
foo.Bind(
  "1",
  fun () -> foo.Return(2))
(**
---
### Transformations: combine
*)
bar {
    return "1"
    return "2"
}
(**
*)
bar.Delay(fun () ->
    bar.Combine(
        bar.Return("1"), 
        bar.Delay(fun () -> bar.Return("2"))))
(**
---
### Transformations: For
*)
bar {
    for x in [1..3] do
        return x
}
(**
*)
bar.Delay(fun () ->
    bar.For(
        [1..3],
        fun x -> bar.Return(x)))
(**
---
### Transformations: While
*)
bar {
    while true do
       return "1"
}
(**
*)
bar.Delay(fun () ->
    bar.While(
        (fun () -> true),
        bar.Delay(fun () -> bar.Return("1"))))
(**
---
### Transformations: Try Finally
*)
bar {
    try
       return "value"
    finally
       printfn "finally"
}
(**
*)
bar.Delay(fun () ->
    bar.TryFinally(
        bar.Delay(fun () -> bar.Return("value")), 
        fun () -> printfn "finally"))
(**
---
### Transformations: Try With
*)
bar {
    try
       return 1 / 0
    with | :? System.DivideByZeroException as e ->
       return 0
}
(**
*)
bar.TryWith(
    bar.Delay(fun () -> bar.Return(1/0)), 
    (fun e -> 
        match e with 
        | :? System.DivideByZeroException as e -> 
            bar.Return(0)
        | exn -> reraise() ))

(*** hide ***)
let getPage (n : int) : Async<seq<int>> 
    = raise (new System.NotImplementedException())
(**
***
### Async Seq Paging
http://tomasp.net/blog/async-sequences.aspx/

    [lang=fsharp]
    let getPage (n : int) : Async<seq<int>>

***
### Async Seq Paging
*)
let getAllPagesSeq : seq<int> = 
  let rec loop n = seq {
      let results = 
          getPage n 
          |> Async.RunSynchronously
      yield! results
      if results <> Seq.empty then
          yield! loop (n + 1)
  }
  loop 0
(**
***
### Async Seq Paging
*)
let getAllPagesAsync : Async<seq<int>> = 
  let rec loop n acc = async {
      let! results = getPage n 
      if results <> Seq.empty then
          return! 
              loop (n + 1) 
                   (Seq.append acc results)
      else
          return acc
  }
  loop 0 Seq.empty
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

let rec toAsyncSeq xs = 
    if xs = Seq.empty then
        empty
    else
        async { return Cons(Seq.head xs, toAsyncSeq <| Seq.skip 1 xs) } 
(**
***
- class : much-code

### Async Seq Builder
*)
type AsyncSeqBuilder() =
    member x.Yield(v) = singleton v
    member x.YieldFrom (s : seq<'a>) = toAsyncSeq s
    member x.YieldFrom (s : AsyncSeq<'a>) = s
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
        yield! (raise e : AsyncSeq<'T>) }

let rec collect (f : 'T -> AsyncSeq<'TResult>) (input : AsyncSeq<'T>) : AsyncSeq<'TResult> = asyncSeq {
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
let rec resultLoop n = asyncSeq {
  let! results = getPage n
  yield! results
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
(*** include: appendvalue-example ***)
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
(**
---
### DSL Interpreter
*)
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
(*** define: appendvalue-example ***)
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

(*** include: appendvalue-example ***)
let appendResult = interpret (appendValue "key" "abcd") Map.empty

(*** include-value: appendResult ***)
(**
***
- class : dsl-example

### DSL example
*)
let appendValueWithRetry key value = 
   let rec loop previousResult = keyValue {
     match previousResult with
     | WrongExpectedVersion -> 
        return! (appendValue key value)
     | _ ->
        return previousResult
   }

   loop WrongExpectedVersion
(*** hide ***)
type IRealDataStore = 
    abstract member Read : Key -> Async<(Version * string) option>
    abstract member Write : Key -> Version -> string -> Async<WriteResult>
(**
***
- class : dsl-interpreter

### Real DSL Interpreter
*)
let rec realInterpreter 
    (prog : FreeKeyValue<obj,'T>)
    (realDataStore : IRealDataStore) 
    : Async<'T> =
    match prog with
    | FreeKeyValue (ReadValue (key, f)) -> async {
        let! value = realDataStore.Read key
        return! realInterpreter (f value) realDataStore }
    | FreeKeyValue (WriteValue (key, version, value, f)) -> async {
        let! result = realDataStore.Write key version value
        return! realInterpreter (f result) realDataStore }
    | Pure result ->
        async { return result }
(**
***
- class : dsl-interpreter

### DSL Interpreter Interleaving
*)
let interpret2
    (prog1 : FreeKeyValue<obj,'T>)
    (prog2 : FreeKeyValue<obj,'T>)
    (dataStore : Map<Key,(Version * string)>) 
    : (Map<Key,(Version * string)> * 'T * 'T) =
    let runOne pRun pOther ds =
       match runStep pRun ds with
       | Continue (ds',pRun') -> (pOther, pRun', ds')
       | Complete(ds', a) -> (pOther, Pure a, ds')
        
    let rec loop 
       (p1, p2, ds) =
       match (p1,p2) with
       | (Pure r1, Pure r2) -> (ds,r1, r2)
       | (_, Pure _) -> loop <| runOne p1 p2 ds
       | _ -> loop <| runOne p2 p1 ds

    loop (prog1, prog2, dataStore)

let appendResult2 = 
   interpret2 (appendValue "key" "abcd") (appendValue "key" "abcd") Map.empty

(*** include-value: appendResult2 ***)
(**
***
### More computation expressions
* Choice
* Software Transactional Memory 
* Logging
* Parsing

***
### References
* F# Computation expression zoo http://research.microsoft.com/pubs/217375/computation-zoo.pdf
* F# Language Specification 3.0 http://fsharp.org/specs/language-spec/3.0/FSharpSpec-3.0-final.pdf
* Programming with F# asynchronous sequences http://tomasp.net/blog/async-sequences.aspx/
*)