module CompExpression

let getWords (document : string) = document.Split(' ') 

let wordLengths documents = 
   seq {
      for document in documents do
        for word in getWords document do
            yield word.Length
   }

let wordLengths' document = 
    document
    |> Seq.collect getWords
    |> Seq.map (fun x -> x.Length)
    
let downloadUrl (url : string) : Async<string> = 
  match url with
  | "rootDoc" ->
    async { return "root doc" }
  | "child1" -> 
    async { return "child1 doc" }
  | "child2" -> 
    async { return "child2 doc" }
  | _ -> 
    async { return "other doc" }

let getDocumentLinks = function
  | "rootDoc" -> ["child1"; "child2"] |> Seq.ofList
  | _ -> Seq.empty

let rec getDocumentLength url =
    async {
        let! document = downloadUrl url
        return getWords document
    }

let rec getDocuments startUrl : Async<seq<string>> = 
    async {
        let! doc = downloadUrl startUrl
        let links = getDocumentLinks doc

        let! childDocs = 
          seq {
            for childUrl in links do 
                yield getDocuments childUrl
          }
          |> Async.Parallel

        return seq {
            yield doc

            for child in childDocs do
                yield! child
        }
    }