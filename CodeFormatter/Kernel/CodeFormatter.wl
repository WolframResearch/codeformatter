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




$DefaultAiriness

$DefaultIndentationString

$DefaultTabWidth


(*
Messages
*)
CodeFormatter


$CurrentIndentationString

$CurrentNewline

$CurrentStyle

$Toplevel


Begin["`Private`"]

Needs["CodeFormatter`BreakLines`"]
Needs["CodeFormatter`Indent`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Folds`"] (* for linearize *)
Needs["CodeParser`Utils`"] (* for empty *)
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



$DefaultAiriness = 0

$DefaultIndentationString := StringRepeat[" ", $DefaultTabWidth]

$DefaultNewline = "\n"

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

$DefaultSafetyMargin = 10



CodeFormat::implicittimesaftercontinuation = "Replaced implicit Times with explicit * to remove ambiguity."

CodeFormat::syntaxissues = "Input to CodeFormat has syntax issues: `1`"

CodeFormat::cellfailure = "This cell cannot currently be formatted."



CodeFormat::usage = "CodeFormat[code] returns a string of formatted WL code. \
code can be a string, a file, or a list of bytes."

Options[CodeFormat] = {
  Airiness :> $DefaultAiriness,
  "IndentationString" :> $DefaultIndentationString,
  "Newline" :> $DefaultNewline,
  "TabWidth" :> $DefaultTabWidth,
  "LineWidth" :> $DefaultLineWidth,
  "SafetyMargin" :> $DefaultSafetyMargin,

  "NewlinesBetweenCommas" -> Automatic,
  "NewlinesBetweenCompoundExpressions" -> Automatic,
  "NewlinesBetweenOperators" -> Automatic,
  "NewlinesInComments" -> Automatic,
  "NewlinesInControl" -> Automatic,
  "NewlinesInGroups" -> Automatic,
  "NewlinesInScoping" -> Automatic
}


CodeFormat[file_File, opts:OptionsPattern[]] :=
Catch[
Module[{cst, tabWidth, formattedStr, agg, cst2, agg2, aggToCompare, agg2ToCompare, newline},

  tabWidth = OptionValue["TabWidth"];
  newline = OptionValue["Newline"];

  cst = CodeConcreteParse[file, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
  ];

  formattedStr = CodeFormatCST[cst, opts];

  If[$DisableSanityChecking,
    Throw[formattedStr]
  ];

  agg = CodeParser`Abstract`Aggregate[cst];
  agg = normalizeTokens[agg, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  cst2 = CodeConcreteParse[formattedStr];

  If[MatchQ[cst2[[3]], KeyValuePattern[SyntaxIssues -> {___, SyntaxIssue["UnrecognizedCharacter", _, _, _], ___}]],
    Message[CodeFormat::syntaxissues, Lookup[cst2[[3]], SyntaxIssues]];
  ];

  agg2 = CodeParser`Abstract`Aggregate[cst2];
  agg2 = normalizeTokens[agg2, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

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
Module[{cst, tabWidth, formattedStr, agg, cst2, agg2, aggToCompare, agg2ToCompare},

  tabWidth = OptionValue["TabWidth"];
  newline = OptionValue["Newline"];

  cst = CodeConcreteParse[str, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
  ];

  formattedStr = CodeFormatCST[cst, opts];

  If[!StringQ[formattedStr],
    Throw[formattedStr]
  ];

  formattedStr = StringTrim[formattedStr];

  If[$DisableSanityChecking,
    Throw[formattedStr]
  ];

  agg = CodeParser`Abstract`Aggregate[cst];
  agg = normalizeTokens[agg, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  cst2 = CodeConcreteParse[formattedStr];

  If[MatchQ[cst2[[3]], KeyValuePattern[SyntaxIssues -> {___, SyntaxIssue["UnrecognizedCharacter", _, _, _], ___}]],
    Message[CodeFormat::syntaxissues, Lookup[cst2[[3]], SyntaxIssues]];
  ];

  agg2 = CodeParser`Abstract`Aggregate[cst2];
  agg2 = normalizeTokens[agg2, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

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


CodeFormat[bytes_List, opts:OptionsPattern[]] :=
Catch[
Module[{cst, tabWidth, formattedStr, agg, cst2, agg2, aggToCompare, agg2ToCompare, newline},

  tabWidth = OptionValue["TabWidth"];
  newline = OptionValue["Newline"];

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

  If[$DisableSanityChecking,
    Throw[formattedStr]
  ];

  agg = CodeParser`Abstract`Aggregate[cst];
  agg = normalizeTokens[agg, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  cst2 = CodeConcreteParse[formattedStr];

  If[MatchQ[cst2[[3]], KeyValuePattern[SyntaxIssues -> {___, SyntaxIssue["UnrecognizedCharacter", _, _, _], ___}]],
    Message[CodeFormat::syntaxissues, Lookup[cst2[[3]], SyntaxIssues]];
  ];
  
  agg2 = CodeParser`Abstract`Aggregate[cst2];
  agg2 = normalizeTokens[agg2, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  agg2[[1]] = Byte;

  aggToCompare = agg /. _Association -> <||>;
  agg2ToCompare = agg2 /. _Association -> <||>;

  If[aggToCompare =!= agg2ToCompare,
    Throw[Failure["SanityCheckFailed", <||>]]
  ];

  formattedStr
]]



Options[CodeFormatCST] = Options[CodeFormat]

CodeFormatCST[cstIn_, opts:OptionsPattern[]] :=
Catch[
Module[{indentationString, cst, newline, tabWidth, indented, airiness, formattedStr, merged,
  linearized, strs, spaced, breaked, lineWidth1, lineWidth2, lineWidth, safetyMargin,
  style},

  indentationString = OptionValue["IndentationString"];
  newline = OptionValue["Newline"];
  tabWidth = OptionValue["TabWidth"];

  style = <||>;

  style["NewlinesBetweenCommas"] = OptionValue["NewlinesBetweenCommas"];
  style["NewlinesBetweenCompoundExpressions"] = OptionValue["NewlinesBetweenCompoundExpressions"];
  style["NewlinesBetweenOperators"] = OptionValue["NewlinesBetweenOperators"];
  style["NewlinesInComments"] = OptionValue["NewlinesInComments"];
  style["NewlinesInControl"] = OptionValue["NewlinesInControl"];
  style["NewlinesInGroups"] = OptionValue["NewlinesInGroups"];
  style["NewlinesInScoping"] = OptionValue["NewlinesInScoping"];

  airiness = OptionValue[Airiness];

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
    style["NewlinesBetweenCompoundExpressions"] = Delete
  ];
  If[airiness >= 0.25,
    style["NewlinesBetweenCompoundExpressions"] = Insert
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




  safetyMargin = OptionValue["SafetyMargin"];
  lineWidth = OptionValue["LineWidth"];

  lineWidth1 = lineWidth - safetyMargin;
  lineWidth2 = lineWidth;

  cst = cstIn;

  If[$Debug,
    Print["CodeFormatCST: ", cst];
  ];

  Block[{$CurrentIndentationString, $CurrentNewline, $CurrentStyle, $Toplevel},

    $CurrentIndentationString = indentationString;

    $CurrentNewline = newline;

    $CurrentStyle = style;

    $Toplevel = True;

    cst = StandardizeCommentGroups[cst];

    If[$Debug,
      Print["after StandardizeCommentGroups: ", cst];
    ];

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

    cst = IntroduceRowNodes[cst];

    If[$Debug,
      Print["after IntroduceRowNodes: ", cst];
    ];
    
    cst = AbstractFormatNodes[cst];

    If[$Debug,
      Print["after AbstractFormatNodes: ", cst];
    ];

    indented = indent[cst, 0];

    If[$Debug,
      Print["after indent: ", indented];
    ];

    If[FailureQ[indented],
      Throw[indented]
    ];

    linearized = linearize[indented];

    If[$Debug,
      Print["after linearize: ", linearized];
    ];

    If[FailureQ[linearized],
      Throw[linearized]
    ];

    merged = mergeLineContinuations[linearized];

    If[$Debug,
      Print["after mergeLineContinuations: ", merged];
    ];

    spaced = insertNecessarySpaces[merged];

    If[$Debug,
      Print["after insertNecessarySpaces: ", spaced];
    ];

    breaked = breakLines[spaced, lineWidth1, lineWidth2];

    If[$Debug,
      Print["after breaked: ", breaked];
    ];

    If[!ListQ[breaked],
      Throw[breaked]
    ];

    strs = collectStr /@ breaked;

    formattedStr = StringJoin[strs];

    formattedStr
  ]
]]



collectStr[LeafNode[_, s_String, _]] := s

collectStr[ErrorNode[_, s_String, _]] := s

collectStr[FragmentNode[_, s_String, _]] := s



(*
Flatten comment groups
*)
StandardizeCommentGroups::usage = "StandardizeCommentGroups[cst] standardizes comment groups."

StandardizeCommentGroups[cstIn_] :=
  Module[{cst},

    cst = cstIn;

    poss = Position[cst, GroupNode[Comment, _, _]];

    cst = MapAt[convertCommentGroups, cst, poss];

    cst
  ]

convertCommentGroups[c:GroupNode[Comment, boxs_, data_]] :=
  Module[{leafs, src, origSpaces},
    src = data[Source];
    leafs = Cases[boxs, LeafNode[_, _, _], Infinity];

    If[MemberQ[leafs, LeafNode[String, "\[IndentingNewLine]" | "\n", _]], 
      origSpaces = 0;
      (*
      Do a little hack here

      It is hard to tell how many spaces were in front of the multiline comment

      so count how many spaces were before the closer and use that
      *)
      Do[
        If[MatchQ[leafs[[i]], LeafNode[String, "\[IndentingNewLine]" | "\n", _]],
          Break[]
        ];
        If[MatchQ[leafs[[i]], LeafNode[String, s_ /; StringMatchQ[s, " "..], _]],
          origSpaces += StringLength[leafs[[i]][[2]]]
        ];
        ,
        {i, Length[leafs] - 1, 2, -1}
      ];

      LeafNode[Token`Comment,
        {FragmentNode[Token`Comment, "\\" <> $CurrentNewline, <||>]} ~Join~
        Table[FragmentNode[String, " ", <||>], origSpaces] ~Join~
        (FragmentNode[Token`Comment, #[[2]], <||>]& /@ leafs), <|data, "InsertedFragmentNodes" -> 1 + origSpaces|>
      ]
      ,
      (* single line *)
      LeafNode[Token`Comment,
        (FragmentNode[Token`Comment, #[[2]], <||>]& /@ leafs)
        ,
        data
      ]
    ]
  ]



RemoveSimpleLineContinuations::usage = "RemoveSimpleLineContinuations[cst] removes simple line continuations from cst."

RemoveSimpleLineContinuations[cstIn_] :=
  Catch[
  Module[{data, cst, tokStartLocs, simpleLineContinuations, grouped, poss, tuples, mapSpecs,
    embeddedNewlineStartLocs, extracted, complexLineContinuations, childData},

    cst = cstIn;

    data = cst[[3]];

    If[empty[cst[[2]]],
      Throw[cst]
    ];

    childData = cst[[2, 1, 3]];

    (*
    Only proceed if LineColumn convention
    *)
    If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
      Throw[cst]
    ];

    If[KeyExistsQ[data, "SimpleLineContinuations"],

      (*
      -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

      -3 is where LeafNode[xxx, xxx, <||>] is

      There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
      poss = Cases[poss, {___Integer}];

      extracted = Extract[cst, poss];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ extracted;

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];



      (*
      Filter out multiline strings and multiline comments: they are not simple
      *)
      If[KeyExistsQ[data, "EmbeddedNewlines"],

        embeddedNewlineStartLocs = data["EmbeddedNewlines"];

        KeyDropFrom[grouped, embeddedNewlineStartLocs]
      ];

      If[KeyExistsQ[data, "ComplexLineContinuations"],

        complexLineContinuations = data["ComplexLineContinuations"];

        KeyDropFrom[grouped, complexLineContinuations]
      ];



      simpleLineContinuations = data["SimpleLineContinuations"];

      mapSpecs = Map[
        Function[{contLoc},

          tuples = grouped[contLoc];

          (*
          The token associated with this location may have been processed away and now missing
          *)
          If[!MissingQ[tuples],
            tuples
            ,
            Nothing
          ]
        ]
        ,
        simpleLineContinuations
      ];

      mapSpecs = Flatten[mapSpecs, 1];

      (*
      There may be more than 1 simple line continuation, so use FixedPoint
      *)
      cst = MapAt[FixedPoint[removeSimpleLineContinuation, #]&, cst, mapSpecs[[All, 2]]];

      If[empty[simpleLineContinuations],
        KeyDropFrom[data, "SimpleLineContinuations"]
        ,
        data["SimpleLineContinuations"] = simpleLineContinuations
      ];

      cst[[3]] = data;
    ];

    cst
  ]]

RemoveComplexLineContinuations::usage = "RemoveComplexLineContinuations[cst] removes complex line continuations from cst."

RemoveComplexLineContinuations[cstIn_] :=
  Catch[
  Module[{data, cst, tokStartLocs, grouped, poss, tuples, mapSpecs,
    extracted, complexLineContinuations, childData},

    cst = cstIn;

    data = cst[[3]];

    If[empty[cst[[2]]],
      Throw[cst]
    ];

    childData = cst[[2, 1, 3]];

    (*
    Only proceed if LineColumn convention
    *)
    If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
      Throw[cst]
    ];

    If[KeyExistsQ[data, "ComplexLineContinuations"],

      (*
      -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

      -3 is where LeafNode[xxx, xxx, <||>] is

      There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
      poss = Cases[poss, {___Integer}];

      extracted = Extract[cst, poss];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ extracted;

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

      complexLineContinuations = data["ComplexLineContinuations"];

      mapSpecs = Map[
        Function[{contLoc},

          tuples = grouped[contLoc];

          (*
          The token associated with this location may have been processed away and now missing
          *)
          If[!MissingQ[tuples],
            tuples
            ,
            Nothing
          ]
        ]
        ,
        complexLineContinuations
      ];

      mapSpecs = Flatten[mapSpecs, 1];

      cst = MapAt[removeComplexLineContinuations, cst, mapSpecs[[All, 2]]];

      KeyDropFrom[data, "ComplexLineContinuations"];

      cst[[3]] = data;
    ];

    cst
  ]]

(*
There may be "simple line continuations" that were left behind because they belonged to a multiline string or comment

Ex:

x + \
 "abc
 def"

We remove the simple line continuation here, but we DO NOT remove the preceding whitespace

Note: we only care about the LAST preceding whitespace

Ex:

x + \
 \
   "abc
   def"

the last preceding whitespace is 3 spaces, so keep that

Also recompute the Source of the multiline token to reflect the new whitespace start

Need to recompute Source because it is used later
*)
RemoveRemainingSimpleLineContinuations::usage =
  "RemoveRemainingSimpleLineContinuations[cst] removes simple line continuations from cst."

RemoveRemainingSimpleLineContinuations[cstIn_] :=
  Catch[
  Module[{data, cst, tokStartLocs, simpleLineContinuations, grouped, poss, tuples, mapSpecs, extracted, childData},

    cst = cstIn;

    data = cst[[3]];

    If[empty[cst[[2]]],
      Throw[cst]
    ];

    childData = cst[[2, 1, 3]];

    (*
    Only proceed if LineColumn convention
    *)
    If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
      Throw[cst]
    ];

    If[KeyExistsQ[data, "SimpleLineContinuations"],

      (*
      -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

      -3 is where LeafNode[xxx, xxx, <||>] is

      There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
      poss = Cases[poss, {___Integer}];

      extracted = Extract[cst, poss];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ extracted;

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

      simpleLineContinuations = data["SimpleLineContinuations"];

      mapSpecs = Map[
        Function[{contLoc},

          tuples = grouped[contLoc];

          (*
          The token associated with this location may have been processed away and now missing
          *)
          If[!MissingQ[tuples],
            tuples
            ,
            Nothing
          ]
        ]
        ,
        simpleLineContinuations
      ];

      mapSpecs = Flatten[mapSpecs, 1];

      (*
      There may be more than 1 simple line continuation, so use FixedPoint
      *)
      cst = MapAt[FixedPoint[removeRemainingSimpleLineContinuation, #]&, cst, mapSpecs[[All, 2]]];

      KeyDropFrom[data, "SimpleLineContinuations"];

      cst[[3]] = data;
    ];

    cst
  ]]


StandardizeEmbeddedNewlines::usage = "StandardizeEmbeddedNewlines[cst, newline] standardizes the newlines in cst."

StandardizeEmbeddedNewlines[cstIn_, newline_String] :=
  Catch[
  Module[{cst, data, embeddedNewlines, mapSpecs, tuples, poss, tokStartLocs, grouped, childData},

    cst = cstIn;

    data = cst[[3]];

    If[empty[cst[[2]]],
      Throw[cst]
    ];

    childData = cst[[2, 1, 3]];

    (*
    Only proceed if LineColumn convention
    *)
    If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
      Throw[cst]
    ];

    If[KeyExistsQ[data, "EmbeddedNewlines"],

      (*
      -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

      -3 is where LeafNode[xxx, xxx, <||>] is

      There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
      poss = Cases[poss, {___Integer}];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ Extract[cst, poss];

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

      embeddedNewlines = data["EmbeddedNewlines"];

      mapSpecs = Map[
        Function[{newlineLoc},

          tuples = grouped[newlineLoc];

          (*
          The token associated with this location may have been abstracted away and now missing
          *)
          If[!MissingQ[tuples],
            tuples
            ,
            Nothing
          ]
        ]
        ,
        embeddedNewlines
      ];

      mapSpecs = Flatten[mapSpecs, 1];

      cst = MapAt[convertEmbeddedNewlines[#, "FormatOnly" -> True, "Newline" -> newline]&, cst, mapSpecs[[All, 2]]];

      (*

      To be used later

      KeyDropFrom[data, "EmbeddedNewlines"];
      *)

      cst[[3]] = data;
    ];

    cst
  ]]



StandardizeEmbeddedTabs::usage = "StandardizeEmbeddedTabs[cst, newline, tabWidth] standardizes tabs in cst."

StandardizeEmbeddedTabs[cstIn_, newline_String, tabWidth_Integer] :=
  Catch[
  Module[{cst, data, embeddedTabs, mapSpecs, tuples, poss, tokStartLocs, grouped, childData},

    cst = cstIn;

    data = cst[[3]];

    If[empty[cst[[2]]],
      Throw[cst]
    ];

    childData = cst[[2, 1, 3]];

    (*
    Only proceed if LineColumn convention
    *)
    If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
      Throw[cst]
    ];

    If[KeyExistsQ[data, "EmbeddedTabs"],

      (*
      -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

      -3 is where LeafNode[xxx, xxx, <||>] is

      There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
      poss = Cases[poss, {___Integer}];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ Extract[cst, poss];

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

      embeddedTabs = data["EmbeddedTabs"];

      mapSpecs = Map[
        Function[{newlineLoc},

          tuples = grouped[newlineLoc];

          (*
          The token associated with this location may have been abstracted away and now missing
          *)
          If[!MissingQ[tuples],
            tuples
            ,
            Nothing
          ]
        ]
        ,
        embeddedTabs
      ];

      mapSpecs = Flatten[mapSpecs, 1];

      cst = MapAt[convertEmbeddedTabs[#, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth]&, cst, mapSpecs[[All, 2]]];

      (*
      
      To be used later

      KeyDropFrom[data, "EmbeddedTabs"];
      *)

      cst[[3]] = data;
    ];

    cst
  ]]



(*
Fragmentize leaf nodes

Fragmentizing is converting:

LeafNode[String, "abc", <||>]

into

LeafNode[String, {FragmentNode[String, "abc", <||>]}, <||>]

We do NOT want to fragmentize ALL LeafNodes, that would not be intuitive

Actual leaves will be fragmentized in various ways:

overlong leafs split on line breaks

multiline strings have temporary line continuations introduced

multiline comments have temporary line continuations introduced

others?

*)
Fragmentize[cstIn_] :=
  Catch[
  Module[{poss, cst, tokStartLocs, grouped, data, embeddedNewlines, mapSpecs, tuples, allOtherPoss, childData},

    cst = cstIn;
    
    data = cst[[3]];

    If[empty[cst[[2]]],
      Throw[cst]
    ];

    childData = cst[[2, 1, 3]];

    (*
    Only proceed if LineColumn convention
    *)
    If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
      Throw[cst]
    ];

    (*
    Special case multiline strings and multiline comments with LineColumn convention

    Must preserve the original number of columns preceding the string

    We do this by inserting a newline, then the original number of spaces.

    A line continuation is inserted to help with semantics of inserting a newline (this may not ultimately be needed)
    Also, the line continuation helps to communicate the "separateness" of the string or comment
    *)
    poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
    poss = Cases[poss, {___Integer}];
    
    tokStartLocs = #[[3, Key[Source], 1]]& /@ Extract[cst, poss];

    (*
    Group by starting SourceLocation
    *)
    grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

    (*
    "EmbeddedNewlines" may not exist
    *)
    embeddedNewlines = Lookup[data, "EmbeddedNewlines", {}];

    mapSpecs = Map[
      Function[{newlineLoc},

        tuples = grouped[newlineLoc];

        (*
        The token associated with this location may have been abstracted away and now missing
        *)
        If[!MissingQ[tuples],
          tuples
          ,
          Nothing
        ]
      ]
      ,
      embeddedNewlines
    ];

    mapSpecs = Flatten[mapSpecs, 1];
    embeddedNewlinePoss =  mapSpecs[[All, 2]];

    cst = MapAt[fragmentizeMultilineLeafNode, cst, embeddedNewlinePoss];

    If[$Debug,
      Print["after fragmentizeMultilineLeafNode: ", cst];
    ];

    allOtherPoss = Complement[poss, embeddedNewlinePoss];
    (*
    Now also fragmentize the remaining comments
    *)
    cst = MapAt[fragmentizeAllOtherLeafs, cst, allOtherPoss];

    If[$Debug,
      Print["after fragmentizeAllOtherLeafs: ", cst];
    ];

    cst
  ]]

fragmentizeMultilineLeafNode[LeafNode[String, s_, data:KeyValuePattern[Source -> {{_, _}, {_, _}}]]] :=
Module[{origSpaces},
  origSpaces = data[[Key[Source], 1, 2]]-1;
  LeafNode[String,
    Flatten[{
      FragmentNode[String, "\\" <> $CurrentNewline, <||>],
      Table[FragmentNode[String, " ", <||>], origSpaces],
      FragmentNode[String, #, <||>]& /@ DeleteCases[StringSplit[s, x:$CurrentNewline :> x], ""]
    }], <|data, "InsertedFragmentNodes" -> 1 + origSpaces|>]
]

(*
Make sure to fragmentize ( * and * ) here

And fragmentize nested comments also
*)
fragmentizeMultilineLeafNode[LeafNode[Token`Comment, s_, data:KeyValuePattern[Source -> {{_, _}, {_, _}}]]] :=
Module[{origSpaces},
  origSpaces = data[[Key[Source], 1, 2]]-1;
  LeafNode[Token`Comment,
    Flatten[{
      FragmentNode[Token`Comment, "\\" <> $CurrentNewline, <||>],
      Table[FragmentNode[Token`Comment, " ", <||>], origSpaces],
      FragmentNode[Token`Comment, #, <||>]& /@ DeleteCases[StringSplit[s, x:"(*"|"*)"|$CurrentNewline :> x], ""]
    }], <|data, "InsertedFragmentNodes" -> 1 + origSpaces|>]
]

(*
Could be processed because it has the same Source as a multiline leaf node
*)
fragmentizeMultilineLeafNode[n:LeafNode[Token`Fake`ImplicitNull, _, _]] :=
	n

fragmentizeMultilineLeafNode[args___] := Failure["InternalUnhandled", <|"Function"->fragmentizeMultilineLeafNode, "Arguments"->{args}|>]


(*
Make sure to fragmentize ( * and * ) here

And fragmentize nested comments also
*)
fragmentizeAllOtherLeafs[LeafNode[Token`Comment, s_, data_]] :=
  LeafNode[Token`Comment,
    FragmentNode[Token`Comment, #, <||>]& /@ DeleteCases[StringSplit[s, x:"(*"|"*)" :> x], ""]
    ,
    data
  ]

fragmentizeAllOtherLeafs[l:LeafNode[_, _, _]] :=
  l




(*
RowNodes are used to enforce nodes being on the same line

Right now this is only done for comments

when there is this code:

f[
1;
2; (*2a*)
3;
4;
]

then the (*2a*) comment must be constrained to stay on the same line as 2;

Currently, this is implemented by joining the string "(*2a*)" to ";" in the Token`Semi leaf node.
*)
IntroduceRowNodes[cst_] :=
  Module[{commentPoss, isCommentOrWhitespacePos, ranges, commentsOnNewlinesOrStartOfFile, subsumedRanges, extracted,
    last, pos},
    
    commentPoss = Position[cst, LeafNode[Token`Comment, _, _]];
    
    isCommentOrWhitespacePos[pos1_] :=
      ListQ[pos1] && MatchQ[Extract[cst, pos1], LeafNode[Token`Comment | Whitespace | Token`Boxes`MultiWhitespace, _, _]];

    ranges = Function[{commentPos},
      Reverse[NestWhileList[ Join[Most[#], {Last[#] - 1}]&, commentPos, (Last[#] >= 1 && isCommentOrWhitespacePos[#])&]]
    ] /@ commentPoss;

    If[$Debug2,
      Print["ranges: ", ranges];
    ];

    ranges = DeleteCases[ranges, {{___, 0}, ___}];

    If[$Debug2,
      Print["ranges: ", ranges];
    ];

    (*
    in a situation like this:
    x (*1*) (*2*)
    there are 2 ranges: x-to-comment1 and x-to-comment2
    But x-to-comment2 is the only one we care about. x-to-comment1 is subsumed by x-to-comment2
    *)
    subsumedRanges = {};
    Do[
      If[ranges[[i, 1]] == ranges[[j, 1]],
        If[Length[ranges[[i]]] < Length[ranges[[j]]],
          AppendTo[subsumedRanges, ranges[[i]]]
          ,
          AppendTo[subsumedRanges, ranges[[j]]]
        ]
      ]
      ,
      {i, 1, Length[ranges]}
      ,
      {j, i+1, Length[ranges]}
    ];
    ranges = Complement[ranges, subsumedRanges];

    If[$Debug2,
      Print["ranges: ", ranges];
    ];

    (*
    if comments are on their own newlines, then do not introduce a RowNode
    *)
    commentsOnNewlinesOrStartOfFile = Select[ranges, MatchQ[Extract[cst, #[[1]]], LeafNode[Token`Newline, _, _]]&];

    ranges = Complement[ranges, commentsOnNewlinesOrStartOfFile];

    If[$Debug2,
      Print["ranges: ", ranges];
    ];

    (*
    Complement[] sorts in standard order (where smaller ranges come before larger ranges, regardless of their positions),
    which is not what we want

    Resort into lex order
    *)
    ranges = lexSort[ranges];

    If[$Debug2,
      Print["ranges: ", ranges];
    ];

    (*
    ok, now replace each range with RowNode[range]
    *)
    Fold[
      Function[{cst1, range},
        (*
        The effect of replacing f[1, 2, 3] with f[1, RowNode[2, 3] ]
        *)
        extracted = Extract[cst1, range];
        last = lastLeafNode[extracted[[1]]];
        pos = Position[cst1, last][[1]];

        If[$Debug,
          Print["extracted: ", extracted];
        ];

        If[ListQ[last[[2]]],
          (*
          May have already been fragmentized
          *)
          last[[2]] = last[[2]] ~Join~
            Flatten[extractFragmentNodes /@ extracted[[2;;]]];
          ,
          last[[2]] = {FragmentNode[last[[1]], last[[2]], <||>]} ~Join~
            Flatten[extractFragmentNodes /@ extracted[[2;;]]];
        ];

        Insert[Delete[Delete[cst1, range[[2;;]]], pos], last, pos]
      ]
      ,
      cst
      ,
      (*
      Go in reverse so as to not have earlier changes affect later changes in the same node
      *)
      ranges //Reverse
    ]
  ]

extractFragmentNodes[LeafNode[_, fs_List, _]] := fs

extractFragmentNodes[LeafNode[tag_, s_String, data_]] := {FragmentNode[tag, s, data]}



(*
Some nodes can be "abstracted" for formatting
e.g.,

f[] := f[] = x

is a SetDelayed node, with a Set node on the RHS

But this really should be formatted as if it were a single TernaryNode
*)
AbstractFormatNodes[cst_] :=
  abstractFormatNodes[cst]

(*
abstract

a := a = b

into a single TernaryNode
*)
abstractFormatNodes[
  BinaryNode[SetDelayed, {
    lhs1_,
    trivia1:trivia...,
    op1:LeafNode[Token`ColonEqual, _, _],
    trivia2:trivia...,
    BinaryNode[Set, {
      lhs2_,
      trivia3:trivia...,
      op2:LeafNode[Token`Equal, _, _],
      trivia4:trivia...,
      rhs_}, _]}, data_]] /; DeclarationName[lhs1] === DeclarationName[lhs2] :=
  TernaryNode[MemoizedSetDelayed, abstractFormatNodes /@ {lhs1, trivia1, op1, trivia2, lhs2, trivia3, op2, trivia4, rhs}, data]

(*
abstract

a /: b := b = c

into a single QuaternaryNode
*)
abstractFormatNodes[
  TernaryNode[TagSetDelayed, {
    lhs1_,
    trivia1:trivia...,
    op1:LeafNode[Token`SlashColon, _, _],
    trivia2:trivia...,
    lhs2_,
    trivia3:trivia...,
    op2:LeafNode[Token`ColonEqual, _, _],
    trivia4:trivia...,
    BinaryNode[Set, {
      lhs3_,
      trivia5:trivia...,
      op3:LeafNode[Token`Equal, _, _],
      trivia6:trivia...,
      rhs_}, _]}, data_]] /; DeclarationName[lhs2] === DeclarationName[lhs3] :=
  QuaternaryNode[MemoizedTagSetDelayed, abstractFormatNodes /@ {lhs1, trivia1, op1, trivia2, lhs2, trivia3, op2, trivia4, lhs3, trivia5, op3, trivia6, rhs}, data]

abstractFormatNodes[node_LeafNode] := node

abstractFormatNodes[CallNode[head_, ts_, data_]] := CallNode[abstractFormatNodes /@ head, abstractFormatNodes /@ ts, data]

abstractFormatNodes[BoxNode[RowBox, {ts_}, data_]] := BoxNode[RowBox, {abstractFormatNodes /@ ts}, data]

abstractFormatNodes[node:CodeNode[_, _, _]] := node

abstractFormatNodes[head_[tag_, ts_, data_]] := head[tag, abstractFormatNodes /@ ts, data]


mergeLineContinuations[fs_] :=
  Module[{poss, lc, onePast, numberOfOriginalSpaces, numberOfBeforeChars, originalSpacesSpec, dropSpecs, takeSpec,
    newFs, commentReplaceSpecs},

    If[$Debug,
      Print["fs: ", fs];
    ];

    lc = FragmentNode[_, "\\" <> $CurrentNewline, _];

    poss = Position[fs, lc];

    dropSpecs = {};
    commentReplaceSpecs = {};
    Function[{pos},

      If[$Debug,
        Print["pos: ", pos];
      ];

      (*
      Count how many spaces after the line continuation
      *)
      onePast = NestWhile[# + 1 &, pos[[1]] + 1, # <= Length[fs] && MatchQ[fs[[#]], (LeafNode|FragmentNode)[_, " ", _]] &];

      originalSpacesSpec = {pos[[1]] + 1, onePast};

      If[$Debug,
        Print["originalSpacesSpec: ", originalSpacesSpec];
      ];

      numberOfOriginalSpaces = (onePast) - (pos[[1]] + 1);

      (*
      Count how many characters before the line continuation (but after any previous newline)
      *)
      onePast = NestWhile[# - 1 &, pos[[1]] - 1, # >= 1 && !MatchQ[fs[[#]], (LeafNode|FragmentNode)[_, $CurrentNewline, _]] &];

      takeSpec = {onePast + 1, pos[[1]] - 1};

      If[$Debug,
        Print["takeSpec: ", takeSpec];
      ];

      numberOfBeforeChars =
        Total[
          (*
          Make sure to treat implicit Times as " "
          *)
          StringLength /@ (Take[fs, takeSpec] /. LeafNode[Token`Fake`ImplicitTimes, _, _] -> LeafNode[Whitespace, " ", <||>])[[All, 2]]
        ];

      If[$Debug,
        Print["numberOfBeforeChars: ", numberOfBeforeChars];
        Print["numberOfOriginalSpaces: ", numberOfOriginalSpaces];
      ];

      Which[
        (*
        numberOfBeforeChars == 0 && numberOfOriginalSpaces == 0,
          (*
          not mergeable; the line continuation will be dropped later 
          *)
          If[$Debug,
            Print["not mergeable 1"];
          ];
          Nothing
        ,
        *)
        (*
        either there are more original spaces (so fine to merge back)

        a::usage = \
                   "text
        text"

        turns into =>

        a::usage = "text
        text"

        *)
        numberOfBeforeChars <= numberOfOriginalSpaces,
          (*
          make sure to include the line continuation itself to be removed
          *)
          If[$Debug,
            Print["mergeable 1"];
          ];

          AppendTo[dropSpecs, {pos[[1]], originalSpacesSpec[[2]] - (numberOfOriginalSpaces - numberOfBeforeChars) - 1}]
        ,
        (*
        or whatever is overlapping is spaces, so also fine to merge back

        a::usage =
              \
        "text
        text"

        turns into =>

        a::usage =
        "text
        text"

        *)
        MatchQ[Take[fs, takeSpec], {(LeafNode|FragmentNode)[_, " ", _]...}],
          If[$Debug,
            Print["mergeable 2"];
          ];
          (*
          make sure to include the line continuation itself
          *)
          AppendTo[dropSpecs, {takeSpec[[1]], pos[[1]]}]
        ,
        (*

        If the string itself starts with " and then immediately a newline, then it is fine to merge
        (we will not mess up any alignment, because there is no alignment on the first line)

        a::usage = \
                 "
        xxx"

        turns into =>

        a::usage = "
        xxx"

        *)
        MatchQ[fs[[originalSpacesSpec[[2]]]], FragmentNode[String, "\"", _] | FragmentNode[Token`Comment, "(*", _]],
          (*
          make sure to include the line continuation itself to be removed
          *)
          If[$Debug,
            Print["mergeable 3"];
          ];
          AppendTo[dropSpecs, {pos[[1]], originalSpacesSpec[[2]] - 1}]
        ,
        MatchQ[Take[fs, {Max[pos[[1]] - (numberOfBeforeChars - numberOfOriginalSpaces), 1], pos[[1]] - 1}], {(LeafNode|FragmentNode)[_, " ", _]...}],
          (*
          
          If[  \
              "x
          x"
          ]

          turns into

          If[ "x
          x"
          ]

          *)
          If[$Debug,
            Print["mergeable 4"];
          ];
          AppendTo[dropSpecs, {Max[pos[[1]] - (numberOfBeforeChars - numberOfOriginalSpaces), 1], pos[[1]] + numberOfOriginalSpaces}]
        ,
        MatchQ[fs[[pos[[1]]]], FragmentNode[Token`Comment, "\\" <> $CurrentNewline, _]],
          (*
          Always ok to remove line continuation from a comment
          But make sure to leave a newline
          *)
          (*
          AppendTo[commentReplaceSpecs, pos]
          *)
          Null
        ,
        MatchQ[fs[[pos[[1]]]], FragmentNode[Token`String, "\\" <> $CurrentNewline, _]],
          (*

          Something like:

          a::usage = \
                   "xx
          xx"

          turns into

          a::usage = "xx
          xx"

          The internal alignment of the string is changed.
          Give a message.

          *)
          If[TrueQ[$DisableBadMerging],
            Message[CodeFormat::multiline];
            ,
            AppendTo[dropSpecs, {pos[[1]], originalSpacesSpec[[2]] - 1}]
          ]
      ]

    ] /@ poss;

    If[$Debug,
      Print["commentReplaceSpecs: ", commentReplaceSpecs];
      Print["dropSpecs: ", dropSpecs];
    ];

    newFs = fs;
    newFs = ReplacePart[newFs, commentReplaceSpecs -> FragmentNode[Token`Comment, $CurrentNewline, <||>]];
    newFs = Fold[Drop, newFs, dropSpecs // Reverse];

    If[$Debug,
      Print["newFs: ", newFs];
    ];

    (*
    cleanup any remaining line continuations
    *)
    (*
    newFs = newFs /. {
      lc :> Sequence @@ line[0],
      (*
      space fragments are now orphaned, so need to convert back to LeafNodes
      *)
      FragmentNode[_, " ", data_] :> LeafNode[Whitespace, " ", data],
      FragmentNode[tag_, str_, data_] :> LeafNode[tag, str, data]
    };
    *)

    newFs
  ]


End[]

EndPackage[]
