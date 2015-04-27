(**
- title : Lets see what we can do! with F# Computation Expressions 
- description: not sure where this goes
- author : Andrew Browne
- transition : default

****
### Lets see what we can do! with F# Computation Expressions 
By Andrew Browne
***
*)

(*** hide ***)
let getWords (document2 : string) = document2.Split(' ') 

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
(**
****
### Some word lengths
*)
let wordLengths' document =
    document
    |> Seq.collect getWords
    |> Seq.map (fun x -> x.Length)
(**
****
### Some word lengths - using seq
*)
(*** include: wordLengths-seq ***)
(**
***

### Reveal.js

- A framework for easily creating beautiful presentations using HTML.  
  
> **Atwood's Law**: any application that can be written in JavaScript, will eventually be written in JavaScript.

***

### FSharp.Formatting

- F# tools for generating documentation (Markdown processor and F# code formatter).
- It parses markdown and F# script file and generates HTML or PDF.
- Code syntax highlighting support.
- It also evaluates your F# code and produce tooltips.

***

### Syntax Highlighting

#### F# (with tooltips)

*)
let a = 5
let factorial x = [1..x] |> List.reduce (*)
let c = factorial a
(** 
`c` is evaluated for you
*)
(*** include-value: c ***)
(**

--- 

#### More F#

*)
[<Measure>] type sqft
[<Measure>] type dollar
let sizes = [|1700<sqft>;2100<sqft>;1900<sqft>;1300<sqft>|]
let prices = [|53000<dollar>;44000<dollar>;59000<dollar>;82000<dollar>|] 
(**

#### `prices.[0]/sizes.[0]`

*)
(*** include-value: prices.[0]/sizes.[0] ***)
(**

---

#### C#

    [lang=cs]
    using System;


    class Program
    {
        static void Main()
        {
            Console.WriteLine("Hello, world!");
        }
    }


---

#### JavaScript

    [lang=js]
    function copyWithEvaluation(iElem, elem) {
      return function (obj) {
          var newObj = {};
          for (var p in obj) {
              var v = obj[p];
              if (typeof v === "function") {
                  v = v(iElem, elem);
              }
              newObj[p] = v;
          }
          if (!newObj.exactTiming) {
              newObj.delay += exports._libraryDelay;
          }
          return newObj;
      };
    }

---

#### Haskell
 
    [lang=haskell]
    recur_count k = 1 : 1 : zipWith recurAdd (recur_count k) (tail (recur_count k))
            where recurAdd x y = k * x + y

    main = do
      argv <- getArgs
      inputFile <- openFile (head argv) ReadMode
      line <- hGetLine inputFile
      let [n,k] = map read (words line)
      printf "%d\n" ((recur_count k) !! (n-1))


*code from [NashFP/rosalind](https://github.com/NashFP/rosalind/blob/master/mark_wutka%2Bhaskell/FIB/fib_ziplist.hs)*

---

### SQL
 
    [lang=sql]
    select * 
    from 
      (select 1 as Id union all select 2 union all select 3) as X 
    where Id in (@Ids1, @Ids2, @Ids3)

*sql from [Dapper](https://code.google.com/p/dapper-dot-net/)* 

***

**Bayes' Rule in LaTeX**

$ \Pr(A|B)=\frac{\Pr(B|A)\Pr(A)}{\Pr(B|A)\Pr(A)+\Pr(B|\neg A)\Pr(\neg A)} $

***

### The Reality of a Developer's Life 

**When I show my boss that I've fixed a bug:**
  
![When I show my boss that I've fixed a bug](http://www.topito.com/wp-content/uploads/2013/01/code-07.gif)
  
**When your regular expression returns what you expect:**
  
![When your regular expression returns what you expect](http://www.topito.com/wp-content/uploads/2013/01/code-03.gif)
  
*from [The Reality of a Developer's Life - in GIFs, Of Course](http://server.dzone.com/articles/reality-developers-life-gifs)*

*)