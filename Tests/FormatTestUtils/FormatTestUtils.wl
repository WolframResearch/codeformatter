BeginPackage["FormatTestUtils`"]



formatTest

ok


Begin["`Private`"]

Needs["Format`"]
Needs["AST`"]
Needs["AST`Utils`"]
Needs["PacletManager`"]






Options[formatTest] = {"FileSizeLimit" -> {0, Infinity}};

formatTest[fileIn_String, i_Integer, OptionsPattern[]] :=
 Module[{ast},
  Catch[
   
   limit = OptionValue["FileSizeLimit"];

    file = fileIn;
    If[$Debug, Print["file1: ", file]];
    
    If[FileType[file] === File,
     If[FileByteCount[file] > limit[[2]],
      ast =
      Failure["FileTooLarge", <|"FileName" -> file,
        "FileSize" -> FileSize[file]|>];
     Throw[ast]
     ];
     If[FileByteCount[file] < limit[[1]],
      ast =
      Failure["FileTooSmall", <|"FileName" -> file,
        "FileSize" -> FileSize[file]|>];
     Throw[ast]
     ];
     ];

  FormatFile[file, AirynessLevel -> 1.0]

]]




End[]

EndPackage[]
