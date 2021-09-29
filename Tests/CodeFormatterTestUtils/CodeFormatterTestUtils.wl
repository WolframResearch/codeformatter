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
  Airiness -> 0,
  PerformanceGoal -> "Speed"
}

formatTest[file_String, i_Integer, OptionsPattern[]] :=
  Catch[
 Module[{dryRun, prefix, limit, res, lineWidth, airiness, margin, performanceGoal, actual, firstLine, implicitTimesInserted},
   
   prefix = OptionValue["FileNamePrefixPattern"];
   limit = OptionValue["FileSizeLimit"];
   dryRun = OptionValue["DryRun"];
   lineWidth = OptionValue["LineWidth"];
   margin = OptionValue["SafetyMargin"];
   airiness = OptionValue[Airiness];
   performanceGoal = OptionValue[PerformanceGoal];

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
    CodeFormatter`Private`$LastExtent = Indeterminate;

    res = CodeFormat[File[file], "LineWidth" -> lineWidth, "SafetyMargin" -> margin, Airiness -> airiness, PerformanceGoal -> performanceGoal];
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
    ];

    If[CodeFormatter`Private`$LastExtent === CodeFormatter`Private`$OutOfDate,
      Throw[Failure["OutOfDateExtent", <|"File" -> File[file]|>]]
    ];

    If[!MatchQ[CodeFormatter`Private`$LastExtent, {_Integer, _Integer, _Integer, _Integer}],
      Print[
         Style[Row[{"index: ", i, " ", File[file]}], 
          Red]];
      Throw[CodeFormatter`Private`$LastExtent, "UncaughtBadExtent"]
    ];

    actual = actualExtent[res];
    If[!(actual[[1]] === CodeFormatter`Private`$LastExtent[[1]] && actual[[2]] === CodeFormatter`Private`$LastExtent[[2]]),
      Print[
         Style[Row[{"index: ", i, " ", File[file]}], 
          Red]];
      Throw[<| "PrecomputedExtent" -> CodeFormatter`Private`$LastExtent, "ActualExtent" -> actual|>, "UncaughtMiscalculatedExtent"]
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
    ];
  ]
]]


actualExtent[s_String] :=
Module[{split, width, height, firstWidth, lastWidth},
  split = StringSplit[s, "\n", All];
  width = Max[StringLength /@ split];
  height = Length[split];
  firstWidth = StringLength[First[split]];
  lastWidth = StringLength[Last[split]];
  {width, height, firstWidth, lastWidth}
]




Options[formatPackageEditorTest] = {
  "FileNamePrefixPattern" -> "",
  "FileSizeLimit" -> {0, Infinity},
  "DryRun" -> False,
  "LineWidth" -> 120,
  "SafetyMargin" -> 10,
  Airiness -> 0,
  PerformanceGoal -> "Speed"
}

formatPackageEditorTest[file_String, i_Integer, OptionsPattern[]] :=
  Catch[
 Module[{dryRun, prefix, res, lineWidth, airiness, margin, nbObj, nb, cells, cell1, performaceGoal},
   
   prefix = OptionValue["FileNamePrefixPattern"];
   limit = OptionValue["FileSizeLimit"];
   dryRun = OptionValue["DryRun"];
   lineWidth = OptionValue["LineWidth"];
   margin = OptionValue["SafetyMargin"];
   airiness = OptionValue[Airiness];
   performaceGoal = OptionValue[PerformanceGoal];

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

  cells = If[Length[#] >= 2 && #[[2]] == "Code", #, Nothing]& /@ nb[[1]];


  Check[
    Do[

      (*
      work around very common FE bug

      Related bugs: 395301
      *)
      cell1 = FrontEndExecute[FrontEnd`ReparseBoxStructurePacket[cell]];

      box = cell1[[1, 1]];

      res = CodeFormatter`Notebooks`Private`formatInputContents[box];

      If[FailureQ[res],

        If[TrueQ[implicitTimesInserted],
          Throw[Failure["ImplicitTimesAfterLineContinuation", <|"File" -> File[file]|>]]
        ];

        If[MatchQ[res, _Failure] && res[[1]] == "SanityCheckFailed",
          If[MemberQ[sanityCheckExceptions, FileNameTake[file]],
            Print[
             Style[Row[{"index: ", i, " ", File[file]}]]];
            Print["sanity check exception"]
          ]
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
    {cell, cells}
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


sanityCheckExceptions = {

  (*
  FE parses trailing ; differently than kernel

  Related bugs: 400268
  *)
  "PlanetaryAstronomy.m"
}


End[]

EndPackage[]
