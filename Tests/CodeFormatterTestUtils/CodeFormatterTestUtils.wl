BeginPackage["CodeFormatterTestUtils`"]



formatTest

ok


Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["PacletManager`"]






Options[formatTest] = {
  "FileNamePrefixPattern" -> "",
  "FileSizeLimit" -> {0, Infinity},
  "DryRun" -> False
}

formatTest[fileIn_String, i_Integer, OptionsPattern[]] :=
 Module[{ast, dryRun, prefix, res},
  Catch[
   
   prefix = OptionValue["FileNamePrefixPattern"];
   limit = OptionValue["FileSizeLimit"];
   dryRun = OptionValue["DryRun"];

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

  Quiet[
  Check[
    res = CodeFormat[File[file], AirynessLevel -> 1.0, "DryRun" -> dryRun];
    If[res == {},
      Print[Style[Row[{"index: ", i, " ", StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], Darker[Orange]]];
      Print[Style["empty file", Darker[Orange]]]
    ]
    ,
    Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[fileIn, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[
       Style[$MessageList, Darker[Orange]]];
  ]]
]]




End[]

EndPackage[]
