BeginPackage["CodeFormatter`"]

(*
Functions
*)
CodeFormat


(*
Options
*)
Airiness


(*
Undocumented functions
*)
CodeFormatCST


(*
Formatter-only nodes
*)

GroupSquareSquare

Token`OpenSquareOpenSquare

Token`CloseSquareCloseSquare


$DefaultIndentationString

$DefaultTabWidth


$DefaultLineWidth

$DefaultBreakLinesMethod

(*
Line Breaker V1
*)
$DefaultSafetyMargin


(*
Messages
*)
CodeFormatter


(*
Dynamic variables
*)
$CurrentIndentationString

$CurrentIndentationStringLength

$CurrentTabWidth

$CurrentIndentationNodeList

$CurrentIndentationLevelNodeList

$CurrentIndentationCommentFragmentNodeList

$CurrentNewlineString

$CurrentLineContinuationString

$CurrentStyle

$Toplevel


Begin["`Private`"]

Needs["CodeFormatter`Absorb`"]
Needs["CodeFormatter`Abstract`"]
Needs["CodeFormatter`AnchoredComments`"]
Needs["CodeFormatter`Fragmentize`"]
Needs["CodeFormatter`Indent`"]
Needs["CodeFormatter`LineBreakerV1`"]
(*
Needs["CodeFormatter`Notebooks`"]

CodeFormatter`Notebooks` is not used here, so do not need to load
*)
Needs["CodeFormatter`RemoveLineContinuations`"]
Needs["CodeFormatter`Standardize`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Folds`"] (* for linearize *)
Needs["CodeParser`Utils`"]
Needs["PacletManager`"] (* for PacletInformation *)


CodeFormatter::old = "The old Format paclet has been renamed to CodeFormatter. Uninstall Format paclet from your system."

If[PacletFind["Format"] != {},
  Message[CodeFormatter::old]
]


CodeFormatter::versions = "CodeParser version `1` and CodeFormatter version `2` are different. There may be unexpected problems."

codeParserVersion = "Version" /. PacletInformation["CodeParser"]
codeFormatterVersion = "Version" /. PacletInformation["CodeFormatter"]
If[StringSplit[codeParserVersion, "."][[1;;2]] != StringSplit[codeFormatterVersion, "."][[1;;2]],
  Message[CodeFormatter::versions, codeParserVersion, codeFormatterVersion]
]



$DefaultIndentationString := StringRepeat[" ", $DefaultTabWidth]

$DefaultNewlineString = "\n"

$DefaultTabWidth = 4

(*
Discussed in Live CEOing Ep 392

This RFC:
https://tools.ietf.org/html/rfc2822.html#section-2.1.1

states:

There are two limits that this standard places on the number of
characters in a line. Each line of characters MUST be no more than
998 characters, and SHOULD be no more than 78 characters, excluding
the CRLF.

And SW thought it was a good idea to follow RFC 2822 for the default
*)
$DefaultLineWidth = 78

$DefaultBreakLinesMethod = "LineBreakerV2"

(*
Line Breaker V1
*)
$DefaultSafetyMargin = 10


CodeFormat::implicittimesaftercontinuation = "Replaced implicit Times with explicit * to remove ambiguity."

CodeFormat::syntaxissues = "Input to CodeFormat has syntax issues: `1`"

CodeFormat::cellfailure = "This cell cannot currently be formatted."


CodeFormat::usage = "CodeFormat[code] returns a string of formatted WL code. \
code can be a string, a file, or a list of bytes."

Options[CodeFormat] = {
  "IndentationString" :> $DefaultIndentationString,
  "NewlineString" :> $DefaultNewlineString,
  "LineWidth" :> $DefaultLineWidth,

  Airiness -> Automatic,
  "NewlinesBetweenCommas" -> Automatic,
  "NewlinesBetweenSemicolons" -> Automatic,
  "NewlinesBetweenOperators" -> Automatic,
  "NewlinesInComments" -> Automatic,
  "NewlinesInControl" -> Automatic,
  "NewlinesInGroups" -> Automatic,
  "NewlinesInScoping" -> Automatic,

  (*
  Undocumented options
  *)
  
  "TabWidth" :> $DefaultTabWidth,
  "BreakLinesMethod" :> $DefaultBreakLinesMethod,
  (* Undocumented options for  LineBreakerV1 *)
  "SafetyMargin" :> $DefaultSafetyMargin,

  PerformanceGoal -> "Speed"
}


CodeFormat[file_File, opts:OptionsPattern[]] :=
Catch[
Module[{cst, tabWidth, formattedStr, agg, cst2, agg2, aggToCompare, agg2ToCompare, newline, performanceGoal},

  tabWidth = OptionValue["TabWidth"];
  newline = OptionValue["NewlineString"];
  performanceGoal = OptionValue[PerformanceGoal];

  cst = CodeConcreteParse[file, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
  ];

  formattedStr = CodeFormatCST[cst, opts];

  (*
  PerformanceGoal -> "Speed" will skip sanity checking
  *)
  If[TrueQ[$DisableSanityChecking] || (performanceGoal == "Speed"),
    Throw[formattedStr]
  ];

  (*
  Sanity Checking
  *)
  
  agg = CodeParser`Abstract`Aggregate[cst];
  agg = normalizeTokens[agg, "FormatOnly" -> True, "NewlineString" -> newline, "TabWidth" -> tabWidth];

  cst2 = CodeConcreteParse[formattedStr];

  If[MatchQ[cst2[[3]], KeyValuePattern[SyntaxIssues -> {___, SyntaxIssue["UnrecognizedCharacter", _, _, _], ___}]],
    Message[CodeFormat::syntaxissues, Lookup[cst2[[3]], SyntaxIssues]];
  ];

  agg2 = CodeParser`Abstract`Aggregate[cst2];
  agg2 = normalizeTokens[agg2, "FormatOnly" -> True, "NewlineString" -> newline, "TabWidth" -> tabWidth];

  agg2[[1]] = File;

  aggToCompare = agg /. _Association -> <||>;
  agg2ToCompare = agg2 /. _Association -> <||>;

  If[aggToCompare =!= agg2ToCompare,
    Throw[Failure["SanityCheckFailed", <||>]]
  ];

  formattedStr
]]


CodeFormat[str_String, opts:OptionsPattern[]] :=
Catch[
Module[{cst, tabWidth, newline, formattedStr, agg, cst2, agg2, aggToCompare, agg2ToCompare, performanceGoal},

  tabWidth = OptionValue["TabWidth"];
  newline = OptionValue["NewlineString"];
  performanceGoal = OptionValue[PerformanceGoal];

  cst = CodeConcreteParse[str, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
  ];

  formattedStr = CodeFormatCST[cst, opts];

  If[!StringQ[formattedStr],
    Throw[formattedStr]
  ];

  formattedStr = StringTrim[formattedStr];

  (*
  PerformanceGoal -> "Speed" will skip sanity checking
  *)
  If[TrueQ[$DisableSanityChecking] || (performanceGoal == "Speed"),
    Throw[formattedStr]
  ];

  (*
  Sanity Checking
  *)

  agg = CodeParser`Abstract`Aggregate[cst];
  agg = normalizeTokens[agg, "FormatOnly" -> True, "NewlineString" -> newline, "TabWidth" -> tabWidth];

  cst2 = CodeConcreteParse[formattedStr];

  If[MatchQ[cst2[[3]], KeyValuePattern[SyntaxIssues -> {___, SyntaxIssue["UnrecognizedCharacter", _, _, _], ___}]],
    Message[CodeFormat::syntaxissues, Lookup[cst2[[3]], SyntaxIssues]];
  ];

  agg2 = CodeParser`Abstract`Aggregate[cst2];
  agg2 = normalizeTokens[agg2, "FormatOnly" -> True, "NewlineString" -> newline, "TabWidth" -> tabWidth];

  aggToCompare = agg /. _Association -> <||>;
  agg2ToCompare = agg2 /. _Association -> <||>;

  If[aggToCompare =!= agg2ToCompare,
    If[$Debug,
      Print["aggToCompare: ", aggToCompare];
      Print["agg2ToCompare: ", agg2ToCompare];
    ];
    Throw[Failure["SanityCheckFailed", <||>]]
  ];

  formattedStr
]]


CodeFormat[bytes:{_Integer, _Integer...}, opts:OptionsPattern[]] :=
Catch[
Module[{cst, tabWidth, formattedStr, agg, cst2, agg2, aggToCompare, agg2ToCompare, newline, performanceGoal},

  tabWidth = OptionValue["TabWidth"];
  newline = OptionValue["NewlineString"];
  performanceGoal = OptionValue[PerformanceGoal];

  If[bytes == {},
    Throw[""]
  ];

  cst = CodeConcreteParse[bytes, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
  ];

  formattedStr = CodeFormatCST[cst, opts];

  If[!StringQ[formattedStr],
    Throw[formattedStr]
  ];

  formattedStr = StringTrim[formattedStr];

  (*
  PerformanceGoal -> "Speed" will skip sanity checking
  *)
  If[TrueQ[$DisableSanityChecking] || (performanceGoal == "Speed"),
    Throw[formattedStr]
  ];

  (*
  Sanity Checking
  *)
  
  agg = CodeParser`Abstract`Aggregate[cst];
  agg = normalizeTokens[agg, "FormatOnly" -> True, "NewlineString" -> newline, "TabWidth" -> tabWidth];

  cst2 = CodeConcreteParse[formattedStr];

  If[MatchQ[cst2[[3]], KeyValuePattern[SyntaxIssues -> {___, SyntaxIssue["UnrecognizedCharacter", _, _, _], ___}]],
    Message[CodeFormat::syntaxissues, Lookup[cst2[[3]], SyntaxIssues]];
  ];
  
  agg2 = CodeParser`Abstract`Aggregate[cst2];
  agg2 = normalizeTokens[agg2, "FormatOnly" -> True, "NewlineString" -> newline, "TabWidth" -> tabWidth];

  agg2[[1]] = Byte;

  aggToCompare = agg /. _Association -> <||>;
  agg2ToCompare = agg2 /. _Association -> <||>;

  If[aggToCompare =!= agg2ToCompare,
    Throw[Failure["SanityCheckFailed", <||>]]
  ];

  formattedStr
]]


CodeFormat[{}, opts:OptionsPattern[]] := ""


Options[CodeFormatCST] = Options[CodeFormat]

CodeFormatCST[cstIn_, opts:OptionsPattern[]] :=
Catch[
Module[{
  indentationString, newline, tabWidth, breakLinesMethod, lineWidth, safetyMargin, style, airiness,
  cst, gst, changed,
  tmp,
  indented, linearized, merged, absorbed, spaced, lineWidth1, lineWidth2, breaked,
  strs, formattedStr},

  indentationString = OptionValue["IndentationString"];
  newline = OptionValue["NewlineString"];
  tabWidth = OptionValue["TabWidth"];

  breakLinesMethod = OptionValue["BreakLinesMethod"];
  lineWidth = OptionValue["LineWidth"];
  safetyMargin = OptionValue["SafetyMargin"];

  style = <||>;

  style["LineWidth"] = lineWidth;

  style["NewlinesBetweenCommas"] = OptionValue["NewlinesBetweenCommas"];
  style["NewlinesBetweenSemicolons"] = OptionValue["NewlinesBetweenSemicolons"];
  style["NewlinesBetweenOperators"] = OptionValue["NewlinesBetweenOperators"];
  style["NewlinesInComments"] = OptionValue["NewlinesInComments"];
  style["NewlinesInControl"] = OptionValue["NewlinesInControl"];
  style["NewlinesInGroups"] = OptionValue["NewlinesInGroups"];
  style["NewlinesInScoping"] = OptionValue["NewlinesInScoping"];

  airiness = OptionValue[Airiness];
  If[airiness === Automatic,
    airiness = 0.0
  ];
  
  If[airiness == -1,
    style["NewlinesInComments"] = Delete
  ];
  If[airiness <= -0.85,
    style["NewlinesBetweenCommas"] = Delete;
    style["NewlinesInScoping"] = Delete;
    style["NewlinesInControl"] = Delete
  ];
  If[airiness <= -0.75,
    style["NewlinesInGroups"] = Delete
  ];
  If[airiness <= -0.5,
    style["NewlinesBetweenOperators"] = Delete
  ];
  If[airiness <= -0.25,
    style["NewlinesBetweenSemicolons"] = Delete
  ];
  If[airiness >= 0.25,
    style["NewlinesBetweenSemicolons"] = Insert
  ];
  If[airiness >= 0.5,
    style["NewlinesBetweenOperators"] = Insert
  ];
  If[airiness >= 0.75,
    style["NewlinesInGroups"] = Insert
  ];
  If[airiness >= 0.85,
    style["NewlinesBetweenCommas"] = Insert;
    style["NewlinesInScoping"] = Insert;
    style["NewlinesInControl"] = Insert
  ];
  If[airiness == 1,
    style["NewlinesInComments"] = Insert
  ];

  cst = cstIn;

  If[$Debug,
    Print["CodeFormatCST: ", cst];
  ];

  Block[{$CurrentIndentationString, $CurrentIndentationStringLength, $CurrentTabWidth, $CurrentIndentationNodeList, $CurrentIndentationLevelNodeList,
    $CurrentIndentationCommentFragmentNodeList, $CurrentNewlineString, $CurrentLineContinuationString, $CurrentStyle, $Toplevel},

    $CurrentIndentationString = indentationString;

    $CurrentIndentationStringLength = StringLength[$CurrentIndentationString];

    $CurrentTabWidth = tabWidth;

    $CurrentIndentationNodeList = LeafNode[Whitespace, #, <| "Extent" -> {1, 1, 1, 1} |>]& /@ Characters[$CurrentIndentationString];

    $CurrentIndentationLevelNodeList = {};

    $CurrentIndentationCommentFragmentNodeList = FragmentNode[Token`Comment, #, <| "Extent" -> {1, 1, 1, 1} |>]& /@ Characters[$CurrentIndentationString];

    $CurrentNewlineString = newline;

    $CurrentLineContinuationString = "\\" <> $CurrentNewlineString;

    $CurrentStyle = style;

    $Toplevel = True;


    (*
    Must be before StandardizeEmbeddedNewlines
    depends on "EmbeddedNewlines" data
    *)
    cst = RemoveSimpleLineContinuations[cst];

    If[$Debug,
      Print["after RemoveSimpleLineContinuations: ", cst];
    ];

    cst = RemoveComplexLineContinuations[cst];

    If[$Debug,
      Print["after RemoveComplexLineContinuations: ", cst];
    ];

    cst = RemoveRemainingSimpleLineContinuations[cst];

    If[$Debug,
      Print["after RemoveRemainingSimpleLineContinuations: ", cst];
    ];

    (*
    Post condition: "EmbeddedNewlines" data is removed
    *)
    cst = StandardizeEmbeddedNewlines[cst, newline];

    If[$Debug,
      Print["after StandardizeEmbeddedNewlines: ", cst];
    ];

    cst = StandardizeEmbeddedTabs[cst, newline, tabWidth];

    If[$Debug,
      Print["after StandardizeEmbeddedTabs: ", cst];
    ];

    cst = Fragmentize[cst];

    If[$Debug,
      Print["after Fragmentize: ", cst];
    ];

    cst = InsertNewlineAnchorInformationIntoComments[cst];

    If[$Debug,
      Print["after InsertNewlineAnchorInformationIntoComments: ", cst];
    ];

    (*
    Canonicalize to Graphical Syntax
    
    It is nice to go to Graphical Syntax before abstracting

    graphical is Graphical Syntax == Concrete Syntax - (whitespace + newlines) == Aggregate Syntax + Comments
    *)
    gst = removeWhitespaceAndNewlines[cst];

    gst = AbstractFormatNodes[gst];

    If[$Debug,
      Print["after AbstractFormatNodes: ", gst];
    ];

    If[FailureQ[gst],
      Throw[gst]
    ];

    (*
    indented is Graphical Syntax == Concrete Syntax - (whitespace + newlines) == Aggregate Syntax + Comments
    *)
    indented = IndentCST[gst, "LineWidth" -> lineWidth, "BreakLinesMethod" -> breakLinesMethod];

    If[$Debug,
      Print["after indent: ", indented];
    ];

    If[FailureQ[indented],
      Throw[indented]
    ];

    $LastExtent = indented[[3, Key["Extent"]]];

    (*
    linearized is a list of leafs
    *)
    linearized = linearize[indented];

    If[$Debug,
      Print["after linearize: ", linearized];
    ];

    If[FailureQ[linearized],
      Throw[linearized]
    ];

    (*
    merged is a list of leafs
    *)
    merged = mergeTemporaryLineContinuations[linearized];

    If[$Debug,
      Print["after mergeTemporaryLineContinuations: ", merged];
    ];

    {absorbed, changed} = absorbNewlinesIntoComments[merged];

    If[$Debug,
      Print["after absorbNewlinesIntoComments: ", absorbed];
    ];

    If[changed,
      $LastExtent = $OutOfDate
    ];

    absorbed = absorbNewlinesIntoSemis[absorbed];

    If[$Debug,
      Print["after absorbNewlinesIntoSemis: ", absorbed];
    ];

    (*
    spaced is a list of leafs
    *)
    {spaced, changed} = insertNecessarySpaces[absorbed];

    If[$Debug,
      Print["after insertNecessarySpaces: ", spaced];
    ];

    If[changed,
      $LastExtent = $OutOfDate
    ];
    

    tmp = spaced;
    
    If[breakLinesMethod === "LineBreakerV1",
      
      (*
      Line Breaker V1
      *)

      lineWidth1 = lineWidth - safetyMargin;
      lineWidth2 = lineWidth;

      (*
      breaked is a list of leafs
      *)
      breaked = breakLinesV1[tmp, lineWidth1, lineWidth2];

      If[$Debug,
        Print["after breakLinesV1: ", breaked];
      ];

      If[!ListQ[breaked],
        Throw[breaked]
      ];

      tmp = breaked
    ]
  ]; (* Block *)

  strs = collectStr /@ tmp;

  formattedStr = StringJoin[strs];

  formattedStr
]]


collectStr[LeafNode[_, s_String, _]] := s

collectStr[ErrorNode[_, s_String, _]] := s

collectStr[FragmentNode[_, s_String, _]] := s


End[]

EndPackage[]
