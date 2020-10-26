BeginPackage["CodeFormatterTestUtils`"]

formatTest

formatPackageEditorTest

Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`Notebooks`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["PacletManager`"]


Options[formatTest] = {
  "FileNamePrefixPattern" -> "",
  "FileSizeLimit" -> {0, Infinity},
  "DryRun" -> False,
  "LineWidth" -> 120,
  "SafetyMargin" -> 10,
  Airiness -> 0
}

formatTest[file_String, i_Integer, OptionsPattern[]] :=
  Catch[
 Module[{dryRun, prefix, res, lineWidth, airiness, margin},
   
   prefix = OptionValue["FileNamePrefixPattern"];
   limit = OptionValue["FileSizeLimit"];
   dryRun = OptionValue["DryRun"];
   lineWidth = OptionValue["LineWidth"];
   margin = OptionValue["SafetyMargin"];
   airiness = OptionValue[Airiness];

    If[$Debug, Print["file1: ", File[file]]];
    
    If[FileType[file] === File,
     If[FileByteCount[file] > limit[[2]],
     Throw[Null]
     ];
     If[FileByteCount[file] < limit[[1]],
     Throw[Null]
     ];
     ];

  (*
  figure out if first line is special
  *)
  If[FileByteCount[file] > 0,
    Quiet[
      (*
      Importing a file containing only \n gives a slew of different messages and fails
      bug 363161
      Remove this Quiet when bug is resolved
      *)
      firstLine = Import[file, {"Lines", 1}];
      If[FailureQ[firstLine],
        firstLine = "";
      ]
    ];
    Which[
      (* special encoded file format *)
      StringMatchQ[firstLine, "(*!1"~~("A"|"B"|"C"|"D"|"H"|"I"|"N"|"O")~~"!*)mcm"],
      Throw[Failure["EncodedFile", <|"File" -> File[file]|>]]
      ,
      (* wl script *)
      StringStartsQ[firstLine, "#!"],
      Throw[Failure["WLScript", <|"File" -> File[file]|>]]
    ];
  ];

  Check[
    Check[
    implicitTimesInserted = False;
    res = CodeFormat[File[file], "LineWidth" -> lineWidth, "SafetyMargin" -> margin, Airiness -> airiness];
    ,
    implicitTimesInserted = True;
    ,
    {CodeFormat::implicittimesaftercontinuation}
    ];

    If[FailureQ[res],

      If[implicitTimesInserted,
        Throw[Failure["ImplicitTimesAfterLineContinuation", <|"File" -> File[file]|>]]
      ];
      Print[
         Style[Row[{"index: ", i, " ", File[file]}], 
          Red]];
        Print[Style[Row[{"index: ", i, " ", res}], Red]];
      Throw[res, "Uncaught"]
    ];

    If[!StringQ[res],
      Print[
         Style[Row[{"index: ", i, " ", File[file]}], 
          Red]];
      Throw[res, "UncaughtNotAString"]
    ];
  
    If[dryRun === False,
      Export[file, res, "Text"]
    ]
    ,
    If[!implicitTimesInserted,
      (*
      only fire if not handled by other check
      *)
      Print[
         Style[Row[{"index: ", i, " ", File[file]}], 
          Darker[Orange]]];
        Print[
         Style[$MessageList, Darker[Orange]]];
    ]
  ]
]]






Options[formatPackageEditorTest] = {
  "FileNamePrefixPattern" -> "",
  "FileSizeLimit" -> {0, Infinity},
  "DryRun" -> False,
  "LineWidth" -> 120,
  "SafetyMargin" -> 10,
  Airiness -> 0
}

formatPackageEditorTest[file_String, i_Integer, OptionsPattern[]] :=
  Catch[
 Module[{dryRun, prefix, res, lineWidth, airiness, margin, nbObj, nb, boxs},
   
   prefix = OptionValue["FileNamePrefixPattern"];
   limit = OptionValue["FileSizeLimit"];
   dryRun = OptionValue["DryRun"];
   lineWidth = OptionValue["LineWidth"];
   margin = OptionValue["SafetyMargin"];
   airiness = OptionValue[Airiness];

    If[$Debug, Print["file1: ", File[file]]];
    
    If[FileType[file] === File,
     If[FileByteCount[file] > limit[[2]],
     Throw[Null]
     ];
     If[FileByteCount[file] < limit[[1]],
     Throw[Null]
     ];
     ];

  (*
  figure out if first line is special
  *)
  If[FileByteCount[file] > 0,
    Quiet[
      (*
      Importing a file containing only \n gives a slew of different messages and fails
      bug 363161
      Remove this Quiet when bug is resolved
      *)
      firstLine = Import[file, {"Lines", 1}];
      If[FailureQ[firstLine],
        firstLine = "";
      ]
    ];
    Which[
      (* special encoded file format *)
      StringMatchQ[firstLine, "(*!1"~~("A"|"B"|"C"|"D"|"H"|"I"|"N"|"O")~~"!*)mcm"],
      Throw[Failure["EncodedFile", <|"File" -> File[file]|>]]
      ,
      (* wl script *)
      StringStartsQ[firstLine, "#!"],
      Throw[Failure["WLScript", <|"File" -> File[file]|>]]
    ];
  ];


  nbObj = NotebookOpen[file, Visible -> False];
  nb = NotebookGet[nbObj];

  boxs = If[#[[2]] == "Code", #[[1, 1]], Nothing]& /@ nb[[1]];


  Check[
    Do[

      res = CodeFormatter`Notebooks`Private`formatInputContents[box];

      If[FailureQ[res],

        If[TrueQ[implicitTimesInserted],
          Throw[Failure["ImplicitTimesAfterLineContinuation", <|"File" -> File[file]|>]]
        ];
        Print[
           Style[Row[{"index: ", i, " ", File[file]}], 
            Red]];
          Print[Style[Row[{"index: ", i, " ", res}], Red]];
        Throw[res, "Uncaught"]
      ];

      (* If[!StringQ[res],
        Print[
           Style[Row[{"index: ", i, " ", File[file]}], 
            Red]];
        Throw[res, "UncaughtNotAString"]
      ] *)
    ,
    {box, boxs}
  ]
  ,
    If[!TrueQ[implicitTimesInserted],
      (*
      only fire if not handled by other check
      *)
      Print[
         Style[Row[{"index: ", i, " ", File[file]}], 
          Darker[Orange]]];
        Print[
         Style[$MessageList, Darker[Orange]]];
    ]
  ];

  NotebookClose[nbObj];

  res
]]




End[]

EndPackage[]
