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

    nb = CreatePalette[{
      Button["Format current cell",
        Needs["CodeFormatter`Notebooks`"];
        CodeFormatter`Notebooks`formatCurrentCell[]]}, WindowTitle->"CodeFormatter"];

    NotebookSave[nb, FileNameJoin[{generatedWLDir, "CodeFormatter.nb"}]]
  ]
]

generatePalette[]

Print["Done Palette"]

End[]

EndPackage[]
