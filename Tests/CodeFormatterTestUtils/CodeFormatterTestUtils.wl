BeginPackage["CodeFormatterTestUtils`"]



formatTest


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
  Catch[
 Module[{dryRun, prefix, res},
   
   prefix = OptionValue["FileNamePrefixPattern"];
   limit = OptionValue["FileSizeLimit"];
   dryRun = OptionValue["DryRun"];

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
    res = CodeFormat[File[file]];

    If[FailureQ[res],

      Switch[res,
        Failure["TooLong", _],
          Print[
             Style[Row[{"index: ", i, " ", File[file]}], 
              Darker[Orange]]];
            Print[Style[Row[{"index: ", i, " ", res}], Darker[Orange]]];
          Throw[res]
        ,
        _,
          (*
          implicit Times after a line continuation

          must make an explicit *, so this is now not the same aggregate syntax as the input and the sanity check fails
          *)
          If[MemberQ[{
            prefix <> "StartUp/Biology/MoleculePlot.m",
            prefix <> "StartUp/DataPaclets/TideData.m",
            prefix <> "StartUp/DataPaclets/UniverseModelData.m",
            prefix <> "StartUp/DSolve/DSolveNonlinear2ndOrderODE.m",
            prefix <> "StartUp/DSolve/DSolveSecondOrderODE.m",
            prefix <> "StartUp/ImageProcessing/Interactive/DynamicImage.m",
            prefix <> "StartUp/Network/GraphElementLibrary.m",
            prefix <> "StartUp/NKSSpecialFunctions/RulePlot.m",
            prefix <> "StartUp/PlaneGeometry/TriangleData.m",
            prefix <> "StartUp/RandomProcesses/Library.m",
            prefix <> "StartUp/RandomProcesses/TimeSeriesAnalysis/TimeSeriesConvert.m",
            prefix <> "StartUp/Regions/RegionFunctions/Measure.m",
            prefix <> "StartUp/Regions/RegionFunctions/Perimeter.m",
            prefix <> "StartUp/SpecialSimplifiers/PolyGamma.m",
            prefix <> "StartUp/SpecialSimplifiers/Recurrence.m",
            prefix <> "StartUp/Statistics/RandomMatrices/TracyWidomDistribution.m",
            Nothing
            }, file],
            Throw[Failure["ImplcitTimesAfterLineContinuation", <|"File" -> File[file]|>]]
          ];
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
      Throw[res, "UncaughtNotAString"]
    ];
  
    If[dryRun === False,
      Export[file, res, "Text"]
    ]
    ,
    Print[
       Style[Row[{"index: ", i, " ", File[file]}], 
        Darker[Orange]]];
      Print[
       Style[$MessageList, Darker[Orange]]];
  ]
]]




End[]

EndPackage[]
