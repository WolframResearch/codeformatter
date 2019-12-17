BeginPackage["Format`"]

(*
Functions
*)
FormatFile

FormatString

FormatBytes




(*
Options
*)
AirynessLevel



CodeTextAction




Begin["`Private`"]

Needs["Format`AggregateRules`"]
Needs["Format`Utils`"]
Needs["AST`"]
Needs["AST`Abstract`"]
Needs["AST`CodeAction`"]
Needs["AST`Utils`"]



$AirynessLevel = 1.0

(*
Number of characters per line to consider "long"
*)
$lineLengthLimit = 200

(*
Number of lines to consider "long"
*)
$lineLimit = 5000





$existsTest = Not @* KeyExistsQ[AirynessLevel]



FormatFile::usage = "FormatFile[file] formats the WL input file."

FormatFile::long = "File `1` is long. Formatting will be truncated."

FormatFile::longlines = "File `1` has long lines. Formatting will be truncated."

Options[FormatFile] = {
  AirynessLevel :> $AirynessLevel,
  "DryRun" -> False,
  PerformanceGoal -> "Speed",
  "Tau" -> 2
}

FormatFile[file_String | File[file_String], opts:OptionsPattern[]] :=
  formatFile[file, opts]



Options[formatFile] = Options[FormatFile]

formatFile[file_String, opts:OptionsPattern[]] :=
Catch[
Module[{cst, cstAndIssues, issues, last, lastSrc, lastSrcLine, actions, str, bytes,
  actionfulIssues, actionlessIssues, badIssues, airynessTest, airyness,
  groupedActions, lines, newLines, dryRun, performanceGoal, tau},

  airyness = OptionValue[AirynessLevel];
  dryRun = OptionValue["DryRun"];
  performanceGoal = OptionValue[PerformanceGoal];
  tau = OptionValue["Tau"];

  bytes = Import[file, "Byte"];

  cstAndIssues = ConcreteParseBytes[bytes, {FileNode[File, #[[1]], <||>], Cases[#[[2]], FormatIssue[___]]}&];

  If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
   ];

   {cst, issues} = cstAndIssues;

  issues = formatCST[cst, issues, "Tau" -> tau];

  (*
  File only
  insert trailing \n if missing
  *)
  If[!empty[ cst[[2]] ],

    last = cst[[2, -1]];

    If[!MatchQ[last, LeafNode[Token`Newline, _, _]],

      lastSrc = last[[3, Key[Source] ]];
      lastSrcLine = lastSrc[[2, 1]];

      AppendTo[issues, FormatIssue["MissingTrailingNewline", "Missing trailing newline", "Formatting",
        <|  Source -> {{lastSrcLine + 1, 0}, {lastSrcLine + 1, 0}},
          CodeActions -> { CodeAction["Insert", InsertNodeAfter,
            <|Source -> lastSrc, "InsertionNode" -> LeafNode[Token`Newline, "\n", <||>]|>] },
          AirynessLevel -> 0.0|>] ];
    ];
  ];

  badIssues = Cases[issues, FormatIssue[_, _, _, _?$existsTest]];
  If[!empty[badIssues],
   Message[FormatFile::airyness, badIssues]
  ];

    airynessTest = LessEqualThan[airyness];
    issues = Cases[issues, FormatIssue[_, _, _, KeyValuePattern[AirynessLevel -> _?airynessTest]]];

    If[$Debug,
    Print["issues: ", issues];
  ];

  actionfulIssues = Select[issues, KeyExistsQ[#[[4]], CodeActions]&];
  actionlessIssues = Complement[issues, actionfulIssues];

  Scan[(Message[FormatFile::noaction, #])&, actionlessIssues];


  (*
  Actions
  *)
  actions = #[[4, Key[CodeActions], 1 ]]& /@ actionfulIssues;

  actions = Flatten[lowerToText[#, cst]& /@ actions];

  groupedActions = GroupBy[actions, #[[3, Key[Source], 1, 1]]&];

  If[$Debug,
    Print["actions Length: ", Length[actions]];
    If[$Debug2,
      Print["actions: ", actions];
    ];
  ];



  str = FromCharacterCode[bytes, "UTF8"];

  lines = ("\n" <> #)& /@ StringSplit[str, {"\r\n", "\n", "\r"}, All];

  If[performanceGoal == "Speed",

    If[Length[lines] > $lineLimit,
      Message[FormatFile::long, file]
    ];

    If[AnyTrue[lines, StringLength[#] > $lineLengthLimit&],
      Message[FormatFile::longlines, file]
    ];
  ];

  If[$Debug,
    xPrint["lines: ", lines // InputForm];
  ];

  newLines = ApplyCodeTextActions[groupedActions, lines, performanceGoal];

  str = StringDrop[StringJoin[newLines], 1];

  bytes = ToCharacterCode[str, "UTF8"];

  If[!MatchQ[bytes, {_Integer...}],
    Throw[Failure["CannotExportBytes", <|"Bytes" -> Shallow[bytes], "File" -> file|>]]
  ];

  If[dryRun,
    Throw[Null]
  ];

  Export[file, bytes, "Byte"]
]]







FormatString::usage = "FormatString[string] formats the WL input string."

FormatString::long = "String is long. Formatting will be truncated."

FormatString::longlines = "String has long lines. Formatting will be truncated."

Options[FormatString] = {
  AirynessLevel :> $AirynessLevel,
  PerformanceGoal -> "Speed",
  "Tau" -> 2
}

FormatString[str_String, opts:OptionsPattern[]] :=
  formatString[str, opts]


Options[formatString] = Options[FormatString]

formatString[strIn_String, opts:OptionsPattern[]] :=
Module[{cst, cstAndIssues, issues, actions, str,
  actionfulIssues, actionlessIssues, badIssues, airynessTest, airyness,
  lines, newLines, groupedActions, newStr, performanceGoal, tau},

  str = strIn;

  airyness = OptionValue[AirynessLevel];
  performanceGoal = OptionValue[PerformanceGoal];
  tau = OptionValue["Tau"];

  cstAndIssues = ConcreteParseString[str, {StringNode[String, #[[1]], <||>], Cases[#[[2]], FormatIssue[___]]}&];

  If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
   ];

   {cst, issues} = cstAndIssues;

  issues = formatCST[cst, issues, "Tau" -> tau];

  If[$Debug,
    Print["issues: ", issues];
  ];

  badIssues = Cases[issues, FormatIssue[_, _, _, _?$existsTest]];
  If[!empty[badIssues],
   Message[FormatFile::airyness, badIssues]
  ];

    airynessTest = LessEqualThan[airyness];
    issues = Cases[issues, FormatIssue[_, _, _, KeyValuePattern[AirynessLevel -> _?airynessTest]]];

  actionfulIssues = Select[issues, KeyExistsQ[#[[4]], CodeActions]&];
  actionlessIssues = Complement[issues, actionfulIssues];

  Scan[(Message[FormatString::noaction, #])&, actionlessIssues];

  If[$Debug,
    Print["actionfulIssues: ", actionfulIssues];
  ];

  (*
  Actions
  *)
  actions = #[[4, Key[CodeActions], 1 ]]& /@ actionfulIssues;

  actions = Flatten[lowerToText[#, cst]& /@ actions];

    groupedActions = GroupBy[actions, #[[3, Key[Source], 1, 1]]&];

    (*
  actions = SortBy[actions, -#[[3, Key[Source]]]&];
  *)

  If[$Debug,
    Print["actions Length: ", Length[actions]];
    If[$Debug2,
      Print["actions: ", actions];
    ];
  ];



  lines = ("\n" <> #)& /@ StringSplit[str, {"\r\n", "\n", "\r"}, All];

  If[performanceGoal == "Speed",

    If[Length[lines] > $lineLimit,
      Message[FormatString::long]
    ];

    If[AnyTrue[lines, StringLength[#] > $lineLengthLimit&],
      Message[FormatString::longlines]
    ];
  ];

  If[$Debug,
    xPrint["lines: ", lines // InputForm];
  ];

  newLines = ApplyCodeTextActions[groupedActions, lines, performanceGoal];

  newStr = StringDrop[StringJoin[newLines], 1];

  newStr
]






FormatBytes::usage = "FormatBytes[bytes] formats the WL input bytes."

FormatBytes::long = "Bytes is long. Formatting will be truncated."

FormatBytes::longlines = "Bytes has long lines. Formatting will be truncated."

Options[FormatBytes] = {
  AirynessLevel :> $AirynessLevel,
  PerformanceGoal -> "Speed",
  "Tau" -> 2
}

FormatBytes[bytes_List, opts:OptionsPattern[]] :=
  formatBytes[bytes, opts]


Options[formatBytes] = Options[FormatBytes]

formatBytes[bytesIn_List, opts:OptionsPattern[]] :=
Catch[
Module[{cst, cstAndIssues, issues, actions, str,
  actionfulIssues, actionlessIssues, bytes, bytesOut, badIssues, airynessTest, airyness,
  groupedActions, performanceGoal, tau},

  bytes = bytesIn;

  airyness = OptionValue[AirynessLevel];
  performanceGoal = OptionValue[PerformanceGoal];
  tau = OptionValue["Tau"];

  If[$Debug,
    Print["concrete parse"];
  ];
  cstAndIssues = ConcreteParseBytes[bytes, {FileNode[File, #[[1]], <||>], Cases[#[[2]], FormatIssue[___]]}&];

    If[$Debug,
    Print["concrete parse done"];
  ];

  If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
   ];

   {cst, issues} = cstAndIssues;

    If[$Debug,
    Print["formatCST"];
  ];

  issues = formatCST[cst, issues, "Tau" -> tau];

    If[$Debug,
    Print["formatCST done"];
  ];

  badIssues = Cases[issues, FormatIssue[_, _, _, _?$existsTest]];
  If[!empty[badIssues],
   Message[FormatFile::airyness, badIssues]
  ];

    airynessTest = LessEqualThan[airyness];
    issues = Cases[issues, FormatIssue[_, _, _, KeyValuePattern[AirynessLevel -> _?airynessTest]]];

  actionfulIssues = Select[issues, KeyExistsQ[#[[4]], CodeActions]&];
  actionlessIssues = Complement[issues, actionfulIssues];

  Scan[(Message[FormatBytes::noaction, #])&, actionlessIssues];


  (*
  Actions
  *)
  actions = #[[4, Key[CodeActions], 1 ]]& /@ actionfulIssues;

  actions = Flatten[lowerToText[#, cst]& /@ actions];

  If[$Debug,
    Print["actions Length: ", Length[actions]];
    If[$Debug2,
      Print["actions: ", actions];
    ];
  ];

  groupedActions = GroupBy[actions, #[[3, Key[Source], 1, 1]]&];




  
  str = ToSourceCharacterString[cst];

  lines = ("\n" <> #)& /@ StringSplit[str, {"\r\n", "\n", "\r"}, All];

  If[performanceGoal == "Speed",

    If[Length[lines] > $lineLimit,
      Message[FormatBytes::long]
    ];

    If[AnyTrue[lines, StringLength[#] > $lineLengthLimit&],
      Message[FormatBytes::longlines]
    ];
  ];

  If[$Debug,
    xPrint["lines: ", lines // InputForm];
  ];

  newLines = ApplyCodeTextActions[groupedActions, lines, performanceGoal];

  str = StringDrop[StringJoin[newLines], 1];

  bytesOut = ToCharacterCode[str, "UTF8"];

  bytesOut
]]




Options[formatCST] = {
  "Tau" -> 2
}

(*
  gather concrete issues

  gather abstract issues

  
  noncontroversial issues:
  
  remove trailing whitespace at end of lines

  *)
formatCST[cstIn_, issuesIn_, OptionsPattern[]] :=
Catch[
Module[{cst, issues, agg, ast, trailing, trailingIssues, astIssues, rulesIssues, tabIssues, tabs,
  tau, tabReplacement},

  cst = cstIn;
  issues = issuesIn;

  tau = OptionValue["Tau"];

  tabReplacement = StringJoin[Table[" ", tau]];

  rulesIssues = {};

  agg = Aggregate[cst];

  If[FailureQ[agg],
    Throw[agg]
  ];

  KeyValueMap[Function[{pat, func},
      AppendTo[rulesIssues, Map[func[#, agg]&, Position[agg, pat]]];], $DefaultAggregateRules];

  rulesIssues = Flatten[rulesIssues];

  If[$Debug,
    Print["rulesIssues: ", rulesIssues];
  ];
  
  ast = Abstract[agg];

  astIssues = Cases[ast, FormatIssue[___], Infinity];

  If[$Debug,
    xPrint["astIssues: ", astIssues];
  ];

  trailing = trailingWhitespace[cst];

  If[$Debug2,
    Print["trailing: ", trailing]
  ];

  trailingIssues = FormatIssue["TrailingWhitespace", "Trailing whitespace", "Formatting",
              <|  Source -> #[[3, Key[Source] ]],
                CodeActions -> { CodeAction["Remove", DeleteTriviaNode,
                            <|Source -> #[[3, Key[Source] ]]|>] },
                AirynessLevel -> 0.0|>]& /@ trailing;

  If[$Debug2,
    Print["trailingIssues: ", trailingIssues]
  ];



  tabs = Cases[cst, LeafNode[Token`WhiteSpace, "\t", _], -1];

  (*
  if a tab is also trailing, then just let trailingIssues remove it
  *)
  tabs = Complement[tabs, trailing];

  tabIssues = FormatIssue["TabsToSpaces", "Tabs to spaces", "Formatting",
            <| Source -> #[[3, Key[Source] ]],
              CodeActions -> { CodeAction["replace", ReplaceText,
                          <| Source -> #[[3, Key[Source] ]],
                            "ReplacementText" -> tabReplacement |>] },
              AirynessLevel -> 0.1
            |>]& /@ tabs;
  If[$Debug2,
    Print["tabIssues: ", tabIssues]
  ];


  issues = issues ~Join~
          rulesIssues ~Join~
          astIssues ~Join~
          trailingIssues ~Join~
          tabIssues;

  If[$Debug,
    Print["issues: ", issues];
  ];

  (*
  str = ToSourceCharacterString[cst];

  str*)

  issues
]]





(*
Return WhiteSpace nodes that are before Newline nodes
*)
trailingWhitespace[cstIn_] :=
Catch[
Module[{cst, toks, lines},

  cst = cstIn;

  (* linearize *)
  toks = Cases[cst, _LeafNode, -1];

  lines = Split[toks, #1[[1]] =!= Token`Newline && #1[[1]] =!= Token`LineContinuation&];

  reaped = Reap[
  Scan[sowLine, lines]
  ][[2]];

  If[$Debug,
    xPrint["reaped: ", reaped];
  ];

  If[empty[reaped],
    Throw[{}]
  ];

  reaped[[1]]
]]


sowLine[line_] :=
Module[{cases},
Switch[line,

  (* only whitespace, then \n
    also catches case of only \n

    if toplevel, then remove
  *)
  {LeafNode[Token`WhiteSpace, _, _]..., LeafNode[Token`Newline | Token`LineContinuation, _, _]},
    
    (*
    super slow:
    cases = SequenceCases[line, {ws:LeafNode[Token`WhiteSpace, _, _]..., LeafNode[Token`Newline | Token`LineContinuation, _, _]} :> ws];
    *)
    cases = Reverse[TakeWhile[Reverse[line[[1 ;; -2]]], MatchQ[#, LeafNode[Token`WhiteSpace, _, _]]&]];

    cases = Select[cases, toplevelQ[#, cst]&];

    Scan[Sow, cases]
  ,

  (* something, then whitespace, then \n
    
    remove whitespace after something
  *)
  {___, LeafNode[Except[Token`WhiteSpace], _, _], LeafNode[Token`WhiteSpace, _, _]..., LeafNode[Token`Newline | Token`LineContinuation, _, _]},
    
    (*
    super slow:
    cases = SequenceCases[line, {___, LeafNode[Except[Token`WhiteSpace], _, _], ws:LeafNode[Token`WhiteSpace, _, _]..., LeafNode[Token`Newline | Token`LineContinuation, _, _]} :> ws];
    *)
    cases = Reverse[TakeWhile[Reverse[line[[1 ;; -2]]], MatchQ[#, LeafNode[Token`WhiteSpace, _, _]]&]];

    Scan[Sow, cases]
  ,

  (* only whitespace, at EOF

    if toplevel, then remove
  *)
  {LeafNode[Token`WhiteSpace, _, _]...},
    
    (*
    super slow:
    cases = SequenceCases[line, {ws:LeafNode[Token`WhiteSpace, _, _]...} :> ws];
    *)
    cases = Reverse[TakeWhile[Reverse[line[[1 ;; -1]]], MatchQ[#, LeafNode[Token`WhiteSpace, _, _]]&]];

    cases = Select[cases, toplevelQ[#, cst]&];

    Scan[Sow, cases]
  ,

  (* something, then whitespace, at EOF

    remove whitespace after something
  *)
  {___, LeafNode[Except[Token`WhiteSpace], _, _], LeafNode[Token`WhiteSpace, _, _]...},
    
    (*
    super slow:
    cases = SequenceCases[line, {___, LeafNode[Except[Token`WhiteSpace], _, _], ws:LeafNode[Token`WhiteSpace, _, _]...} :> ws];
    *)
    cases = Reverse[TakeWhile[Reverse[line[[1 ;; -1]]], MatchQ[#, LeafNode[Token`WhiteSpace, _, _]]&]];

    Scan[Sow, cases]
]]


toplevelQ[tok_, cst_] := MatchQ[FirstPosition[cst, tok], {2, _}]




(*
leave text-based actions alone
*)
lowerToText[CodeAction[label_, DeleteText, data_], _] := { CodeTextAction[label, DeleteText, data] }

lowerToText[CodeAction[label_, InsertText, data_], _] := { CodeTextAction[label, InsertText, data] }

lowerToText[CodeAction[label_, ReplaceText, data_], _] := { CodeTextAction[label, ReplaceText, data] }


(*
lower DeleteTrivia to { DeleteText }
*)
lowerToText[CodeAction[label_, DeleteTrivia, data_], cstIn_] :=
Module[{trivia, actionSrc, cst},
  
  cst = cstIn;

  actionSrc = data[Source];

  trivia = Cases[cst,
          LeafNode[Token`WhiteSpace | Token`Newline | Token`Comment | Token`LineContinuation, _,
            KeyValuePattern[Source -> triviaSrc_ /; SourceMemberQ[actionSrc, triviaSrc]]], -1];

  CodeTextAction[label, DeleteText, <| Source -> #[[3, Key[Source] ]] |>]& /@ trivia
]



cacheToSourceCharacterString[insertionNode_] :=
  cacheToSourceCharacterString[insertionNode] =
  ToSourceCharacterString[insertionNode]


(*
lower InsertNode to { InsertText }
*)
lowerToText[CodeAction[label_, InsertNode, data_], _] :=
Module[{actionSrc, insertionText, insertionNode},

  actionSrc = data[Source];

  insertionNode = data["InsertionNode"];

  insertionText = cacheToSourceCharacterString[insertionNode];

  { CodeTextAction[label, InsertText,
    <| Source -> { actionSrc[[1]], actionSrc[[1]] + StringLength[insertionText] - 1 },
      "InsertionText" -> insertionText |>] }
]


(*
lower InsertNode to { InsertText }
*)

lowerToText[CodeAction[label_, InsertNodeAfter, data_], _] :=
Module[{actionSrc, insertionText, insertionNode},

  actionSrc = data[Source];

  insertionNode = data["InsertionNode"];

  insertionText = cacheToSourceCharacterString[insertionNode];

  { CodeTextAction[label, InsertTextAfter,
    <| Source -> { { actionSrc[[2, 1]], actionSrc[[2, 2]] + 1 },
              { actionSrc[[2, 1]], actionSrc[[2, 2]] + 1 + StringLength[insertionText] - 1 } },
      "InsertionText" -> insertionText |>] }
]



(*
lower DeleteTriviaNode to { DeleteText }
*)
lowerToText[CodeAction[label_, DeleteTriviaNode, data_], _] :=
Module[{actionSrc},

  actionSrc = data[Source];

  { CodeTextAction[label, DeleteText, <| Source -> actionSrc |>] }
]







ApplyCodeTextActions[groupedActions_, lines_, performanceGoal_] :=
Module[{},
  
  If[$Debug2,
    Print["lines: ", Length[lines]];
    MapIndexed[Print["line ", #2[[1]], " ", #1 // InputForm]&, lines];
  ];

  MapIndexed[(If[$Debug, xPrint["line ", #2[[1]]]]; apply[Lookup[groupedActions, #2[[1]], {}], #1, performanceGoal])&, lines]
]


apply[actionsIn_, line_, performanceGoal_] :=
Module[{sorted, shadowing, actions},

  If[$Debug2,
    Print["apply: ", {actionsIn, line}];
  ];

  actions = actionsIn;

  (*
  Disregard label when deleting duplicates
  *)
  actions = DeleteDuplicatesBy[actions, {#[[2]], #[[3]]}&];

  If[performanceGoal == "Speed",

    actions = DeleteCases[actions,
            CodeTextAction[_, _, KeyValuePattern[Source -> {{line1_ /; line1 > $lineLimit, _}, {_, _}}]]];

    actions = DeleteCases[actions,
            CodeTextAction[_, _, KeyValuePattern[Source -> {{_, _}, {_, col2_ /; col2 > $lineLengthLimit}}]]];
  ];

  If[$Debug2,
      Print["actions: ", actions];
    ];

  shadowing = Select[actions, Function[action, AnyTrue[actions, shadows[action, #]&]]];

    If[$Debug2,
      Print["shadowing: ", shadowing];
    ];

    actions = Complement[actions, shadowing];

  sorted = SortBy[actions, -#[[3, Key[Source]]]&];

  Fold[Switch[#2[[2]],
      InsertText,      StringInsert[     #1,   #2[[3, Key["InsertionText"] ]], #2[[3, Key[Source],   1, 2]] + 1 ],
      InsertTextAfter, StringInsert[     #1,   #2[[3, Key["InsertionText"] ]], #2[[3, Key[Source],   2, 2]] + 1 ],
      DeleteText,      StringReplacePart[#1,                               "", #2[[3, Key[Source], All, 2]] + 1 ],
      ReplaceText,     StringReplacePart[#1, #2[[3, Key["ReplacementText"] ]], #2[[3, Key[Source], All, 2]] + 1 ]]&, line, sorted]
]



End[]

EndPackage[]
