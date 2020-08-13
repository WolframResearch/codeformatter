BeginPackage["CodeFormatter`Generate`GenerateSources`"]

buildDirFlagPosition

buildDir

srcDirFlagPosition

srcDir

generatedWLDir

dataDir


importedPrefixParselets

importedInfixParselets


Begin["`Private`"]

Print["Generating additional required source files..."]


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

srcDirFlagPosition = FirstPosition[$CommandLine, "-srcDir"]

If[MissingQ[srcDirFlagPosition],
  Print["Cannot proceed; Unsupported src directory"];
  Quit[1]
]

srcDir = $CommandLine[[srcDirFlagPosition[[1]] + 1]]

If[!DirectoryQ[srcDir],
  Print["Cannot proceed; Unsupported src directory"];
  Quit[1]
]



generatedWLDir = FileNameJoin[{buildDir, "generated", "wl"}]


dataDir = FileNameJoin[{srcDir, "CodeFormatter", "Data"}]

PrependTo[$Path, srcDir]

If[FailureQ[FindFile["CodeFormatter`Generate`GenerateSources`"]],
  Print["CodeFormatter`Generate`GenerateSources` could not be found."];
  Quit[1]
]

Print["Clean..."]

Quiet[DeleteDirectory[generatedWLDir, DeleteContents -> True], DeleteDirectory::nodir]

Quiet[CreateDirectory[generatedWLDir], CreateDirectory::filex]

Print["Done Clean"]


importedPrefixParselets = Get[FileNameJoin[{dataDir, "PrefixParselets.wl"}]]

importedInfixParselets = Get[FileNameJoin[{dataDir, "InfixParselets.wl"}]]


Get["CodeFormatter`Generate`AcceptableOperators`"]

Get["CodeFormatter`Generate`Palette`"]

Print["Done generating additional required source files"]

End[]

EndPackage[]
