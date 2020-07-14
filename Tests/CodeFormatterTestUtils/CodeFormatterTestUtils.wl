BeginPackage["CodeFormatterTestUtils`"]



formatTest

$HandledException


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

formatTest[file_String, i_Integer, OptionsPattern[]] :=
 Module[{ast, dryRun, prefix, res, lines},
  Catch[
   
   prefix = OptionValue["FileNamePrefixPattern"];
   limit = OptionValue["FileSizeLimit"];
   dryRun = OptionValue["DryRun"];

    If[$Debug, Print["file1: ", File[file]]];
    
    If[FileType[file] === File,
     If[FileByteCount[file] > limit[[2]],
      ast =
      Failure["FileTooLarge", <|"File" -> File[file],
        "FileSize" -> FileSize[file]|>];
     Throw[ast]
     ];
     If[FileByteCount[file] < limit[[1]],
      ast =
      Failure["FileTooSmall", <|"File" -> File[file],
        "FileSize" -> FileSize[file]|>];
     Throw[ast]
     ];
     ];

  Quiet[
  Check[
    res = CodeFormat[File[file]];

    If[FailureQ[res],

      Switch[res,
        Failure["TooLong", _],
          Print[
             Style[Row[{"index: ", i, " ", File[file]}], 
              Darker[Orange]]];
            Print[Style[Row[{"index: ", i, " ", res}], Darker[Orange]]];
          Throw[$HandledException]
        ,
        _,
          Print[
             Style[Row[{"index: ", i, " ", File[file]}], 
              Red]];
            Print[Style[Row[{"index: ", i, " ", res}], Red]];
          Throw[res, "Uncaught"]
      ]
    ];

    If[!StringQ[res],
      Print[
         Style[Row[{"index: ", i, " ", File[file]}], 
          Red]];
        Print[Style[Row[{"index: ", i, " ", res}], Red]];
      Throw[res, "Uncaught"]
    ];

  lines = StringSplit[res, {"\r\n", "\n", "\r"}, All];

  (*
  If[AnyTrue[lines, StringLength[#] > 120 &],
    Print[
       Style[Row[{"index: ", i, " ", 
          StringReplace[file, StartOfString ~~ prefix -> ""]}], 
        Darker[Orange]]];
      Print[
       Style[Row[{"Too Long"}], Darker[Orange]]];
  ];
  *)
  
    If[dryRun === False,
      Export[file, res, "Text"]
    ]
    ,
    Print[
       Style[Row[{"index: ", i, " ", File[file]}], 
        Darker[Orange]]];
      Print[
       Style[$MessageList, Darker[Orange]]];
  ]]
]]




End[]

EndPackage[]
