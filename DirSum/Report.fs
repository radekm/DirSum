
module DirSum.Report

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Xml.Linq

/// Relative path.
/// Platform native representation for relative paths.
type RelPath = string

/// Normalized relative path is a representation for relative paths
/// which doesn't change with a platform. Note: This representation
/// of paths may not work with the stuff from the library (eg. `System.IO`).
///
/// Since reports contain paths in this representation,
/// the platforms where the reports are created don't matter
/// and reports created on different platforms can be easily compared
/// (standard string equality is used to determine whether two strings
/// represent the same path).
///
/// An alternative to using the normalized representation for paths
/// in reports is to use the platform native representation.
/// Unfortunately comparing reports possibly created on different platforms
/// involves determining whether two strings represent the same path
/// and this is harder with the platform native representation
/// since the characters which act as the directory separators are not known
/// (report doesn't contain the platform where it was created).
type NRelPath = string

/// The returned paths are relative to `baseDir`.
let rec listFiles (baseDir : string) : Set<RelPath> =
    let arr = ResizeArray()
    let rec list relPath =
        let toRelPath x = Path.Combine(relPath, Path.GetFileName x)
        let path = Path.Combine(baseDir, relPath)

        Directory.GetFiles path
        |> Array.map toRelPath
        |> Array.sort
        |> arr.AddRange

        Directory.GetDirectories path
        |> Array.map toRelPath
        |> Array.sort
        |> Array.iter list

    list ""
    set arr

let computeHash (file : string) =
    let sha1 = System.Security.Cryptography.SHA1.Create()
    use stream = new FileStream(file, FileMode.Open, FileAccess.Read)
    let hash = sha1.ComputeHash stream
    hash |> Seq.map (fun b -> b.ToString "x2") |> String.concat ""

let private normalizePath (path : RelPath) : NRelPath =
    path.Replace(Path.DirectorySeparatorChar, '/')
        .Replace(Path.AltDirectorySeparatorChar, '/')

type File =
    { Path : NRelPath
      Size : uint64
      Hash : string
    }

type Report = Set<File>

let generateReport
        (baseDir : string)
        (files : Set<RelPath>)
        (fileProcessed : File -> unit)
        : Report =
    files
    |> Set.map (fun relPath ->
        let path = Path.Combine(baseDir, relPath)
        let file = { Path = normalizePath relPath
                     Size = uint64 <| FileInfo(path).Length
                     Hash = computeHash path
                   }
        fileProcessed file
        file)

type ReportAnalysisResult =
    { ZeroSize : Set<File>
      Duplicates : Set<Set<File>>
    }

let analyzeReport (rep : Report) =
    let zeroSize =
        rep
        |> Set.filter (fun f -> f.Size = 0UL)

    let duplicates =
        rep
        |> Seq.filter (fun f -> f.Size > 0UL)
        |> Seq.groupBy (fun f -> f.Size, f.Hash)
        |> Seq.map (snd >> set)
        |> Seq.filter (fun fs -> fs.Count > 1)
        |> set

    { ZeroSize = zeroSize; Duplicates = duplicates }

/// Produces a sequence of pairs where the first element of each pair
/// is from `xs`, the second element is from `ys` and both elements
/// have equal `'Key`. Additionally each element of `xs` and `ys`
/// is used in at most one pair and the produced sequence is maximal
/// (ie. no pair can be added).
///
/// Example:
/// ```
/// findPairs
///     (fun (s : string) -> s.Length)
///     ["aa"; "aaa"; "bb"; "bb"; "cc"; "ccc"; "d"; ""]
///     [""; "uuu"; "vv"; "w"; "ww"])
/// ```
/// pairs strings by their length. The results is
/// `["aa", "vv"; "aaa", "uuu"; "bb", "ww"; "d", "w"; "", ""]`.
///
/// Another example:
/// ```
/// findPairs (fun x -> x) [1; 1; 1; 4] [4; 1; 1]
/// ```
/// produces `[1, 1; 1, 1; 4, 4]` since the third `1` in `xs`
/// doesn't have corresponding `1` in `ys`.
let findPairs (projection : 'T -> 'Key) (xs : seq<'T>) (ys : seq<'T>) : seq<'T * 'T> =
    let map =
        ys
        |> Seq.groupBy projection
        |> Seq.map (fun (key, ys') -> key, Queue(ys'))
        |> Map.ofSeq
    let assignYToX x =
        let key = projection x
        if map.ContainsKey key && map.[key].Count > 0 then
            let y = map.[key].Dequeue()
            Some (x, y)
        else None
    xs
    |> Seq.choose assignYToX

let extractPairs
        (projection : 'T -> 'Key)
        (xs : Set<'T>)
        (ys : Set<'T>)
        : Set<'T * 'T> * Set<'T> * Set<'T> =
    let pairs = findPairs projection xs ys |> set
    let xs' = pairs |> Set.map fst
    let ys' = pairs |> Set.map snd
    pairs, Set.difference xs xs', Set.difference ys ys'

type ReportComparisonResult =
    { Moved : Set<File * File>
      Modified : Set<File * File>
      Added : Set<File>
      Deleted : Set<File>
    }

let compareReports (oldRep : Report) (newRep : Report) =
    let same, oldRep, newRep = extractPairs (fun f -> f) oldRep newRep
    let moved, oldRep, newRep = extractPairs (fun f -> f.Size, f.Hash) oldRep newRep
    let modified, oldRep, newRep = extractPairs (fun f -> f.Path) oldRep newRep
    { Moved = moved
      Modified = modified
      Added = newRep
      Deleted = oldRep
    }

let isValidBookFileName =
    let c = "[A-Za-z]"
    // Dash is surrounded by different characters.
    // Singular possessive nouns are allowed.
    // Digits inside words aren't allowed.
    let word = String.Format("(?:{0}|(?<={0})-(?={0}))+(?:'s)?", c)
    // One or two words. The first word may start with `O'` (eg. `O'Brien`).
    let author = String.Format("(?:O')?{0}(?: {0})?", word)
    // Number consists of one or more digits.
    // Version is a number which contains dots
    // and each dot is surrounded by digits.
    let numberOrVersion = @"(?:\d|(?<=\d)\.(?=\d))+"
    // Special words are not covered by `word` but occur in book names.
    let specialWord = @"(?:C#|F#|\.NET|ASP\.NET|C\+\+|HTML5)"
    let wordEx = sprintf "(?:%s|%s|%s)" word numberOrVersion specialWord
    let bookName = String.Format("(?:{0}(?: |, | - ))*{0}", wordEx)
    // Extension is lower case.
    let ext = "\.(?:pdf|djvu)"
    let pattern = sprintf @"^%s - %s \(\d{4}\)%s$" author bookName ext
    let regex = Regex(pattern, RegexOptions.Compiled ||| RegexOptions.CultureInvariant)
    regex.IsMatch

let private xname (str : string) = XName.Get(str)
let private xattr (name : string) (value : string) =
    XAttribute(xname name, value)

let saveReport (rep : Report) (file : string) =
    let root = XElement(xname "files", xattr "version" "1")
    let doc = XDocument(root)
    for f in rep do
        root.Add(
            XElement(xname "file",
                xattr "path" f.Path,
                xattr "size" (string f.Size),
                xattr "hash" f.Hash))
    doc.Save file

let private attrVal (attrName : string) (el : XElement) = el.Attribute(xname attrName).Value

let loadReport (file : string) =
    let report = ResizeArray()
    let doc = XDocument.Load(file)
    if doc.Root |> attrVal "version" <> "1" then
        failwith "Invalid version."
    for el in doc.Element(xname "files").Elements() do
        let f = { Path = el |> attrVal "path"
                  Hash = el |> attrVal "hash"
                  Size = el |> attrVal "size" |> UInt64.Parse }
        report.Add f
    set report
