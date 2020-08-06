BeginPackage["CodeFormatter`Generate`Palette`"]

Begin["`Private`"]

Print["Generating Palette..."]


buildDirFlagPosition = FirstPosition[$CommandLine, "-buildDir"]

If[MissingQ[buildDirFlagPosition],
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]

buildDir = $CommandLine[[buildDirFlagPosition[[1]] + 1]]

If[!DirectoryQ[buildDir],
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]



generatedWLDir = FileNameJoin[{buildDir, "generated", "wl"}]

Print["Clean..."]

Quiet[DeleteDirectory[generatedWLDir, DeleteContents -> True], DeleteDirectory::nodir]

Quiet[CreateDirectory[generatedWLDir], CreateDirectory::filex]

Print["Done Clean"]







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
            (*Labeled[Slider[Dynamic[CodeFormatter`$DefaultLineWidth, Initialization :> (Needs["CodeFormatter`"])], {40, 200, 1}], "Line width"],*)
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
