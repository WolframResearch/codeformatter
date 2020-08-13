BeginPackage["CodeFormatter`Generate`Palette`"]

Begin["`Private`"]

Needs["CodeFormatter`Generate`GenerateSources`"]

Print["Generating Palette..."]

generatePalette[] :=
Module[{nb},
  
  UsingFrontEnd[

    nb = CreatePalette[
      {
        Button["Format current cell", Needs["CodeFormatter`Notebooks`"];CodeFormatter`Notebooks`formatCurrentCell[], Method -> "Queued"],
        Button["Format current notebook", Needs["CodeFormatter`Notebooks`"];CodeFormatter`Notebooks`formatCurrentNotebook[], Method -> "Queued"],
        Spacer[{0, 20}],
        Button["Selection: Strip newlines", Null, Method -> "Queued", Enabled -> False],
        Button["Selection: Shift left", Null, Method -> "Queued", Enabled -> False],
        Spacer[{0, 20}],
        OpenerView[{
          "Settings",
          Column[{
            Labeled[RadioButtonBar["space", {"tab", "space"}, Enabled -> False], "Indentation character"],
            Spacer[{0, 20}],
            Labeled[RadioButtonBar["4", {"2", "4", "6"}, Enabled -> False], "Tab width"],
            Spacer[{0, 20}],
            Labeled[RadioButtonBar["\\n", {"\\n", "\\r\\n"}, Enabled -> False], "Newline string"],
            Spacer[{0, 20}],
            Labeled[Checkbox[Enabled -> False], "Reparse boxes before formatting"],
            Spacer[{0, 20}],
            Labeled[Slider[Dynamic[CodeFormatter`$DefaultAirynessLevel, Initialization :> (Needs["CodeFormatter`"])], {-1, 1}], "Airyness"],
            Spacer[{0, 20}]
          }]
        }]
      }, WindowTitle->"CodeFormatter"];

    NotebookSave[nb, FileNameJoin[{generatedWLDir, "CodeFormatter.nb"}]]
  ]
]

Print["UsingFrontEnd... \[WatchIcon]"]

generatePalette[]

Print["Done Palette"]

End[]

EndPackage[]
