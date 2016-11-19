
module DirSum.Program

#nowarn "52"

open System
open System.IO
open System.Text

module Format =

    let private log (sb : StringBuilder) (msg : string) = sb.AppendLine(msg) |> ignore

    let private logStartSection (sb : StringBuilder) (title : string) =
        log sb (String.replicate 90 "-")
        log sb ("-- " + title + "\n")

    let private logEndSection (sb : StringBuilder) = log sb ""

    let private logFileSet (sb : StringBuilder) (title : string) (files : Set<Report.File>) =
        logStartSection sb title
        files |> Set.iter (fun f -> log sb f.Path)
        logEndSection sb

    let invalidFileNames (files : Set<string>) =
        let sb = StringBuilder()
        logStartSection sb "Invalid file names"
        files |> Set.iter (log sb)
        logEndSection sb
        sb.ToString()

    let reportAnalysis (a : Report.ReportAnalysisResult) =
        let sb = StringBuilder()

        if not a.ZeroSize.IsEmpty then
            logFileSet sb "Files with size 0" a.ZeroSize

        if not a.Duplicates.IsEmpty then
            logStartSection sb "Files which have same content"
            a.Duplicates
            |> Seq.map (Seq.map (fun f -> f.Path) >> String.concat "\n")
            |> String.concat "\n\n"
            |> log sb
            logEndSection sb

        if a.ZeroSize.IsEmpty && a.Duplicates.IsEmpty then
            logStartSection sb "Analysis - OK"
            logEndSection sb

        sb.ToString()

    let reportComparison (comp : Report.ReportComparisonResult) =
        let sb = StringBuilder()

        if not comp.Moved.IsEmpty then
            logStartSection sb "Moved files"
            comp.Moved
            |> Seq.map (fun (src, dest) -> sprintf "File '%s'\nmoved to '%s'" src.Path dest.Path)
            |> String.concat "\n\n"
            |> log sb
            logEndSection sb

        if not comp.Modified.IsEmpty then
            logFileSet sb "Modified files" (comp.Modified |> Set.map fst)

        if not comp.Added.IsEmpty then
            logFileSet sb "Added files" comp.Added

        if not comp.Deleted.IsEmpty then
            logFileSet sb "Deleted files" comp.Deleted

        if
            comp.Moved.IsEmpty &&
            comp.Modified.IsEmpty &&
            comp.Added.IsEmpty &&
            comp.Deleted.IsEmpty
        then
            logStartSection sb "Report comparison - SAME"
            logEndSection sb

        sb.ToString()

    let error (e : exn) =
        let sb = StringBuilder()
        logStartSection sb "Exception"
        log sb ("Type: " + e.GetType().Name)
        log sb ("Message: " + e.Message + "\n")
        log sb ("Stack trace:\n" + e.StackTrace)
        logEndSection sb
        sb.ToString()

module Table =
    type Layout = Eto.Forms.TableLayout
    type Row = Eto.Forms.TableRow
    type Cell = Eto.Forms.TableCell

type MainForm() as me =
    inherit Eto.Forms.Form()

    let reportFileFilter = Eto.Forms.FileDialogFilter("Reports (*.xml)", "xml")

    // ---------------------------------------------------------------------------------------------
    // Controls

    let currentRep = new Eto.Forms.Label()

    let newRepFolder = new Eto.Forms.TextBox(ReadOnly = true)
    let selectNewRepFolder = new Eto.Forms.Button(Text = "Select…")
    let ensureBookFileNames = new Eto.Forms.CheckBox(Checked = Nullable true)

    let createRep = new Eto.Forms.Button(Text = "Create")
    let openRep = new Eto.Forms.Button(Text = "Open…")
    let saveRepAs = new Eto.Forms.Button(Text = "Save as…")
    let compareRepWith = new Eto.Forms.Button(Text = "Compare with…")

    let log = new Eto.Forms.TextArea(ReadOnly = true)

    // ---------------------------------------------------------------------------------------------
    // State

    let mutable report = None

    let refreshButtons () =
        createRep.Enabled <- newRepFolder.Text <> ""
        saveRepAs.Enabled <- report.IsSome
        compareRepWith.Enabled <- report.IsSome

    // Called at the beginning of an operation which replaces the current report
    // (there are 2 such operations: create report and open report).
    let resetReport () =
        report <- None
        currentRep.Text <- "(no report)"
        log.Text <- ""
        refreshButtons ()

    let error (e : exn) msg =
        log.Append(Format.error e, true)
        Eto.Forms.MessageBox.Show(me, msg, Eto.Forms.MessageBoxType.Error) |> ignore

    // ---------------------------------------------------------------------------------------------
    // Layout

    do
        resetReport ()

        let layout = new Table.Layout()
        layout.Spacing <- Eto.Drawing.Size(10, 10)
        layout.Padding <- Eto.Drawing.Padding(15)

        let labelRow (label : string) =
            Table.Row(Table.Cell(new Eto.Forms.Label(Text = label)))
            |> layout.Rows.Add

        let contentRow (content : Eto.Forms.Control) =
            Table.Row(
                Table.Cell(
                    new Table.Layout(
                        Table.Row(Table.Cell(content)),
                        Padding = Eto.Drawing.Padding(20, 0, 0, 0))))
            |> layout.Rows.Add

        labelRow "Current report:"
        contentRow currentRep

        labelRow "Settings when creating report:"
        new Table.Layout(
            Table.Row(
                Table.Cell(new Eto.Forms.Label(Text = "Folder:")),
                Table.Cell(
                    new Table.Layout(
                        Table.Row(
                            Table.Cell(newRepFolder, true),
                            Table.Cell(selectNewRepFolder)),
                        Spacing = Eto.Drawing.Size(5, 5)))),
            Table.Row(
                Table.Cell(new Eto.Forms.Label(Text = "Ensure book file names:")),
                Table.Cell(ensureBookFileNames)),
            Spacing = Eto.Drawing.Size(5, 5))
        |> contentRow

        labelRow "Report:"
        new Table.Layout(
            Table.Row(
                Table.Cell(createRep),
                Table.Cell(openRep),
                Table.Cell(saveRepAs),
                Table.Cell(compareRepWith),
                Table.Cell(null)),
            Spacing = Eto.Drawing.Size(5, 5))
        |> contentRow

        labelRow "Log:"
        contentRow log

        me.Title <- "DirSum"
        me.ClientSize <- Eto.Drawing.Size(700, 500)
        me.Content <- layout

    // ---------------------------------------------------------------------------------------------
    // Event handlers

    let onSelectNewRepFolder _ =
        let dlg = new Eto.Forms.SelectFolderDialog()
        dlg.Directory <- newRepFolder.Text
        match dlg.ShowDialog me with
        | Eto.Forms.DialogResult.Ok ->
            newRepFolder.Text <- dlg.Directory
            refreshButtons ()
        | Eto.Forms.DialogResult.Cancel -> ()
        | _ -> failwith "Unexpected dialog result."

    let onCreateRep _ =
        try
            resetReport ()
            let baseDir = newRepFolder.Text
            let files = Report.listFiles baseDir
            let invalidFileNames =
                if ensureBookFileNames.Checked.Value then
                    files
                    |> Set.filter (Path.GetFileName >> Report.isValidBookFileName >> not)
                    // Note: Path normalization is not needed here
                    // since these paths won't be stored in a report.
                else Set.empty

            if invalidFileNames.IsEmpty then
                let rep = Report.generateReport baseDir files
                report <- Some rep
                currentRep.Text <- "(in memory report)"
                log.Text <- Format.reportAnalysis <| Report.analyzeReport rep
            else
                log.Text <- Format.invalidFileNames invalidFileNames

            refreshButtons ()
        with
            | e -> error e "Cannot create report."

    let onOpenRep _ =
        let dlg = new Eto.Forms.OpenFileDialog()
        dlg.Filters.Add reportFileFilter
        match dlg.ShowDialog me with
        | Eto.Forms.DialogResult.Ok ->
            try
                resetReport ()
                let rep = Report.loadReport dlg.FileName
                report <- Some rep
                currentRep.Text <- dlg.FileName
                log.Text <- Format.reportAnalysis <| Report.analyzeReport rep
                refreshButtons ()
            with
                | e -> error e "Cannot open report."
        | Eto.Forms.DialogResult.Cancel -> ()
        | _ -> failwith "Unexpected dialog result."

    let onSaveRepAs _ =
        let dlg = new Eto.Forms.SaveFileDialog()
        dlg.Filters.Add reportFileFilter
        match dlg.ShowDialog me with
        | Eto.Forms.DialogResult.Ok ->
            try
                Report.saveReport report.Value dlg.FileName
                currentRep.Text <- dlg.FileName
                refreshButtons ()
            with
                | e -> error e "Cannot save report."
        | Eto.Forms.DialogResult.Cancel -> ()
        | _ -> failwith "Unexpected dialog result."

    let onCompareRepWith _ =
        let dlg = new Eto.Forms.OpenFileDialog()
        dlg.Filters.Add reportFileFilter
        match dlg.ShowDialog me with
        | Eto.Forms.DialogResult.Ok ->
            try
                let oldRep = Report.loadReport dlg.FileName
                let info = Format.reportComparison <| Report.compareReports oldRep report.Value
                log.Append(info, true)
            with
                | e -> error e "Comparison failed."
        | Eto.Forms.DialogResult.Cancel -> ()
        | _ -> failwith "Unexpected dialog result."

    do
       selectNewRepFolder.Click.Add onSelectNewRepFolder
       createRep.Click.Add onCreateRep
       openRep.Click.Add onOpenRep
       saveRepAs.Click.Add onSaveRepAs
       compareRepWith.Click.Add onCompareRepWith

[<STAThread>]
[<EntryPoint>]
let main argv =
    use app = new Eto.Forms.Application()
    app.Run(new MainForm())
    0
