
module DirSum.ReportTests

open Xunit
open DirSum.Report

let assertEqualSeqs (xs : seq<'T>, ys : seq<'T>) =
    Assert.Equal<'T>(List.ofSeq xs, List.ofSeq ys)

[<Fact>]
let findPairsTest () =
    assertEqualSeqs(
        ["aa", "vv"; "aaa", "uuu"; "bb", "ww"; "d", "w"; "", ""],
        findPairs
            (fun s -> s.Length)
            ["aa"; "aaa"; "bb"; "bb"; "cc"; "ccc"; "d"; ""]
            [""; "uuu"; "vv"; "w"; "ww"])

[<Fact>]
let analyzeReportsTest () =
    let rep =
        set [ { Path = "animal.png"; Size = 920UL; Hash = "x" }
              { Path = "dog.png"; Size = 750UL; Hash = "y" }
              { Path = "labrador.png"; Size = 750UL; Hash = "y" }
              { Path = "dir/cat.png"; Size = 920UL; Hash = "x" }
              { Path = "dir/info.txt"; Size = 0UL; Hash = "a" }
              { Path = "dir/pig.png"; Size = 800UL; Hash = "z" } ]
    let zeroSize = set [ { Path = "dir/info.txt"; Size = 0UL; Hash = "a" } ]
    let duplicates =
        set [
            set [ { Path = "animal.png"; Size = 920UL; Hash = "x" }
                  { Path = "dir/cat.png"; Size = 920UL; Hash = "x" } ]
            set [ { Path = "dog.png"; Size = 750UL; Hash = "y" }
                  { Path = "labrador.png"; Size = 750UL; Hash = "y" } ]
        ]
    let expected = { ZeroSize = zeroSize; Duplicates = duplicates }
    Assert.Equal(expected, analyzeReport rep)

[<Fact>]
let compareReportsTest () =
    let oldRep =
        set [ { Path = "dir/cat.png"; Size = 920UL; Hash = "x" }
              { Path = "dir/dog.png"; Size = 750UL; Hash = "y" }
              { Path = "dir/info.txt"; Size = 20UL; Hash = "a" }
              { Path = "dir/pig.png"; Size = 800UL; Hash = "z" } ]
    let newRep =
        set [ { Path = "animal.png"; Size = 920UL; Hash = "x" }
              { Path = "dog.png"; Size = 750UL; Hash = "y" }
              { Path = "dir/cat.png"; Size = 920UL; Hash = "x" }
              { Path = "dir/info.txt"; Size = 80UL; Hash = "b" } ]
    let expected =
        { Moved = set [ { Path = "dir/dog.png"; Size = 750UL; Hash = "y" },
                        { Path = "dog.png"; Size = 750UL; Hash = "y" } ]
          Modified = set [ { Path = "dir/info.txt"; Size = 20UL; Hash = "a" },
                           { Path = "dir/info.txt"; Size = 80UL; Hash = "b" } ]
          Added = set [ { Path = "animal.png"; Size = 920UL; Hash = "x" } ]
          Deleted = set [ { Path = "dir/pig.png"; Size = 800UL; Hash = "z" } ]
        }
    Assert.Equal(expected, compareReports oldRep newRep)

[<Fact>]
let isValidBookFileNameTest () =
    let validBookFileNames =
        [ // Book name - only 1 word.
          "Dasgupta - Algorithms (2006).pdf"
          // Book name - only 2 words.
          "Mitchell - Machine Learning (1997).pdf"
          // Book name - comma after words.
          "Arbib - Arrows, Structures, and Functors (1975).djvu"
          // Book name - dash between words.
          "Graham - Concrete Mathematics - A Foundation for Computer Science (1994).pdf"
          // Book name - special word ".NET".
          "Lidin - .NET IL Assembler (2014).pdf"
          // Book name - special word "F#" and version "3.0".
          "Syme - Expert F# 3.0 (2012).pdf"
          // Author name - 2 words.
          "Van Le - Techniques of Prolog Programming (1993).djvu"
          // Author name - dash inside word.
          "Ben-Ari - Ada For Software Engineers (2009).pdf"
        ]
    let invalidBookFileNames =
        [ // Book name - colon is not allowed.
          "Graham - Concrete Mathematics: A Foundation for Computer Science (1994).pdf"
          // Book name - comma cannot be after the last word.
          "Author - Book name, (2000).pdf"
          // Author name - numbers are not allowed.
          "Author 2 - Book name (2000).pdf"
          // Author name - at most 2 words are allowed.
          "A B C - Book name (2000).pdf"
          // Author name - missing.
          "Book name (2000).pdf"
          // Year - must be in parens.
          "Arbib - Arrows, Structures, and Functors 1975.djvu"
          // Year - must consist of 4 digits.
          "Mitchell - Machine Learning (97).pdf"
          // Extension - must be lower case.
          "Syme - Expert F# 3.0 (2012).PDF"
          // Extension - only ".pdf" or ".djvu" is allowed.
          "Lidin - .NET IL Assembler (2014).epub"
        ]
    validBookFileNames
    |> List.iter (fun f -> Assert.True(isValidBookFileName f, sprintf "Valid: %s" f))
    invalidBookFileNames
    |> List.iter (fun f -> Assert.False(isValidBookFileName f, sprintf "Invalid: %s" f))
