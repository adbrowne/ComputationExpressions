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
type AsyncWriterState<'T> = AsyncWriterState of (unit -> Async<'T> * string list)

module AsyncWriterBuilderModule =
    let runWriter<'T> (AsyncWriterState w) : (Async<'T> * string list) = w()
    let ignoreLog (f : 'a -> AsyncWriterState<'B>) (a : 'a) : (Async<'B>) =
      match f a with
      | AsyncWriterState (_, r) -> r
        
    let bind (b : 'a -> AsyncWriterState<'B>) (c : AsyncWriterState<'A>) : AsyncWriterState<'B> = 
       let AsyncWriterState (log, a) = c
       let a' = async.Bind(a, ignoreLog b)
       let log' = 
          List.append

    // The rest of the operations are boilerplate.
    // The tryFinally operator.
    // This is boilerplate in terms of "result", "catch", and "bind".
    let tryFinally expr compensation =
        catch (expr)
        |> bind (fun res -> compensation();
                            match res with
                            | Ok value -> result value
                            | Exception exn -> raise exn)

    let result value = AsyncWriterState(fun () -> (async { return value }, []))

    // The whileLoop operator.
    // This is boilerplate in terms of "result" and "bind".
    let rec whileLoop pred body =
        if pred() then body |> bind (fun _ -> whileLoop pred body)
        else result ()

    let delay func = AsyncWriterState (fun () -> func())

    // The forLoop operator.
    // This is boilerplate in terms of "catch", "result", and "bind".
    let forLoop (collection:seq<_>) func =
        let ie = collection.GetEnumerator()
        tryFinally (whileLoop (fun () -> ie.MoveNext())
                     (delay (fun () -> let value = ie.Current in func value)))
                     (fun () -> ie.Dispose())

let getWords (document : string) = document.Split(' ') 

type WordStat = 
  | DocumentLength of int
  | WordLength of int

(*** define: wordLengths-seq ***)
let wordLengths document = 
   seq {
     for word in getWords document do
       yield word.Length
   }

(*** hide ***)
let getStats documents = 
   seq {
      for document in documents do
        let words = getWords document 
        yield DocumentLength words.Length
        for word in words do
            yield WordLength word.Length
   }

let downloadUrl (url : string) : Async<string> = 
  async { return sprintf "%s content" url }

let getDocumentLinks = function
  | "rootDoc content" -> ["child1"; "child2"] |> Seq.ofList
  | _ -> Seq.empty

(**
***
### Word Lengths
*)
let wordLengths' document =
    document
    |> Seq.collect getWords
    |> Seq.map (fun x -> x.Length)
(** 
---
#### seq
*)
(*** include: wordLengths-seq ***)
(**
***
### Async 
*)
let getDocumentLength url =
    async {
        let! document = downloadUrl url
        return getWords document
    }
(**
---
### myAsync
*)
type MyAsyncBuilder() = 
   member x.Bind(comp, func) 
     = async.Bind(comp, func)
   member x.Return(value) 
     = async.Return(value)

let myAsync = new MyAsyncBuilder()

let getDocumentLength' url =
  myAsync {
    let! document = downloadUrl url
    return getWords document
  }
(**
---
### Desugaring
*)
let getDocumentLength'' url =
  myAsync.Bind(downloadUrl url, fun document ->  
    myAsync.Return(getWords document))

(*** hide ***)
type AsyncWriterBuilder() =
    member x.Bind(comp, func)
      = AsyncWriterBuilderModule.bind func comp
    member x.Return(value) 
      = AsyncWriterBuilderModule.result value
    member x.For(coll:seq<_>, func) = AsyncWriterBuilderModule.forLoop coll func
    member x.Zero() = AsyncWriterBuilderModule.result ()

let async = new AsyncWriterBuilder()
       
let parseUrl url =
    async {
        let! doc = downloadUrl url
        let links = getDocumentLinks doc

        return (doc, links)
    }
(**
***
### Crawl documents
*)
let rec getDocuments startUrl = async {
    let! (doc, links) = parseUrl startUrl

    let! childDocs = 
      seq {
        for childUrl in links do 
          yield getDocuments childUrl
      } |> Async.Parallel

    return seq {
      yield doc
      for child in childDocs do
        yield! child } 
}
(**
---
### What's wrong?
*)
let countWords = 
    async {
       let! docs = getDocuments "rootDoc"
       for doc in docs do
         printfn "%A" doc
    } 
    |> Async.RunSynchronously
 
(*** include-value: countWords ***)