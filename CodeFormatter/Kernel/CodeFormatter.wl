BeginPackage["CodeFormatter`"]

(*
Functions
*)
CodeFormat



(*
Options
*)
AirynessLevel



(*
Undocumented functions
*)
CodeFormatCST




$DefaultAirynessLevel




Begin["`Private`"]

Needs["CodeFormatter`Utils`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


If[PacletFind["Format"] != {},
  Message[General::obspkg, "Format`"]
]



$DefaultAirynessLevel = 0

$DefaultIndentationString := StringRepeat[" ", $DefaultTabWidth]

$DefaultNewline = "\n"

$DefaultTabWidth = 4



CodeFormat::usage = "CodeFormat[code] returns a string of formatted WL code."

Options[CodeFormat] = {
  AirynessLevel :> $DefaultAirynessLevel,
  "IndentationString" :> $DefaultIndentationString,
  "Newline" :> $DefaultNewline,
  "TabWidth" :> $DefaultTabWidth
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

  agg2 = CodeParser`Abstract`Aggregate[cst2];

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
Module[{cst, tabWidth, formattedStr, agg, cst2, agg2, aggToCompare, agg2ToCompare, newline},

  tabWidth = OptionValue["TabWidth"];
  newline = OptionValue["Newline"];

  cst = CodeConcreteParse[str, "TabWidth" -> tabWidth];

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
  agg2 = CodeParser`Abstract`Aggregate[cst2];

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

  If[$DisableSanityChecking,
    Throw[formattedStr]
  ];

  agg = CodeParser`Abstract`Aggregate[cst];
  agg = normalizeTokens[agg, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  cst2 = CodeConcreteParse[formattedStr];
  agg2 = CodeParser`Abstract`Aggregate[cst2];

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
Module[{indentationString, cst, newline, tabWidth, indented, airyness, formattedStr, merged,
  linearized, strs, spaced},

  indentationString = OptionValue["IndentationString"];
  newline = OptionValue["Newline"];
  tabWidth = OptionValue["TabWidth"];
  airyness = OptionValue[AirynessLevel];

  cst = cstIn;

  If[$Debug,
    Print["CodeFormatCST: ", cst];
  ];

  (*
  Must be before StandardizeEmbeddedNewlines
  depends on "EmbeddedNewlines" data
  *)
  cst = RemoveSimpleLineContinuations[cst];

  If[$Debug,
    Print["after RemoveSimpleLineContinuations: ", cst];
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

  cst = IntroduceRowNodes[cst];

  If[$Debug,
    Print["after IntroduceRowNodes: ", cst];
  ];

  cst = AbstractFormatNodes[cst];

  If[$Debug,
    Print["after AbstractFormatNodes: ", cst];
  ];

  Block[{blockedIndentationString, blockedNewline, blockedAiryness},

    blockedIndentationString[] = indentationString;

    blockedNewline[] = newline;

    blockedAiryness[] = airyness;

    indented = indent[cst, 0];

    If[$Debug,
      Print["after indent: ", indented];
    ];

    linearized = linearize[indented];

    If[$Debug,
      Print["after linearize: ", linearized];
    ];

    merged = mergeLineContinuations[linearized];

    If[$Debug,
      Print["after mergeLineContinuations: ", merged];
    ];
  ];

  spaced = insertNecessarySpaces[merged];

  If[$Debug,
    Print["after insertNecessarySpaces: ", merged];
  ];

  strs = spaced[[All, 2]];

  formattedStr = StringJoin[strs];

  formattedStr
]]




trivia = LeafNode[Whitespace | Token`Boxes`MultiWhitespace | Token`Comment | Token`Newline, _, _]

ws = LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]

nl = LeafNode[Token`Newline, _, _]

comment = LeafNode[Token`Comment, _, _]

matchNewlineQ = MatchQ[nl]




RemoveSimpleLineContinuations::usage = "RemoveSimpleLineContinuations[cst] removes simple line continuations from cst."

RemoveSimpleLineContinuations[cstIn_] :=
  Module[{data, cst, tokStartLocs, simpleLineContinuations, grouped, poss, tuples, mapSpecs,
    embeddedNewlineStartLocs, extracted, complexLineContinuations},

    cst = cstIn;

    data = cst[[3]];

    If[KeyExistsQ[data, "SimpleLineContinuations"],

      (*
      -5 is where LeafNodes[] are

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5}];

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
  ]


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
  Module[{data, cst, tokStartLocs, simpleLineContinuations, grouped, poss, tuples, mapSpecs, extracted},

    cst = cstIn;

    data = cst[[3]];

    If[KeyExistsQ[data, "SimpleLineContinuations"],

      (*
      -5 is where LeafNodes[] are

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5}];

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
  ]


StandardizeEmbeddedNewlines::usage = "StandardizeEmbeddedNewlines[cst, newline] standardizes the newlines in cst."

StandardizeEmbeddedNewlines[cstIn_, newline_String] :=
  Module[{cst, data, embeddedNewlines, mapSpecs, tuples, poss, tokStartLocs, grouped},

    cst = cstIn;

    data = cst[[3]];

    If[KeyExistsQ[data, "EmbeddedNewlines"],

      (*
      -5 is where LeafNodes[] are
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5}];

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

      KeyDropFrom[data, "EmbeddedNewlines"];

      cst[[3]] = data;
    ];

    cst
  ]



StandardizeEmbeddedTabs::usage = "StandardizeEmbeddedTabs[cst, newline, tabWidth] standardizes tabs in cst."

StandardizeEmbeddedTabs[cstIn_, newline_String, tabWidth_Integer] :=
  Module[{cst, data, embeddedTabs, mapSpecs, tuples, poss, tokStartLocs, grouped},

    cst = cstIn;

    data = cst[[3]];

    If[KeyExistsQ[data, "EmbeddedTabs"],

      (*
      -5 is where LeafNodes[] are
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5}];

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

      cst = MapAt[convertEmbeddedTabs[#, "Newline" -> newline, "TabWidth" -> tabWidth]&, cst, mapSpecs[[All, 2]]];

      KeyDropFrom[data, "EmbeddedTabs"];

      cst[[3]] = data;
    ];

    cst
  ]



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
    
    commentPoss = Position[cst, LeafNode[Token`Comment, _, _] | GroupNode[Comment, _, _]];
    
    isCommentOrWhitespacePos[pos1_] :=
      ListQ[pos1] && MatchQ[Extract[cst, pos1], LeafNode[Token`Comment | Whitespace | Token`Boxes`MultiWhitespace, _, _] | GroupNode[Comment, _, _]];

    ranges = Function[{commentPos},
      Reverse[NestWhileList[decrementLast, commentPos, isCommentOrWhitespacePos]]
    ] /@ commentPoss;

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

    (*
    if comments are their own newlines, then do not introduce a RowNode
    *)
    commentsOnNewlinesOrStartOfFile = Select[ranges, FailureQ[#[[1]]] || MatchQ[Extract[cst, #[[1]]], LeafNode[Token`Newline, _, _]]&];

    ranges = Complement[ranges, commentsOnNewlinesOrStartOfFile];

    (*
    Complement sorts in standard order (where smaller ranges come before larger ranges, regardless of their positions),
    which is not what we want

    Resort into lex order
    *)
    ranges = lexSort[ranges];

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
        AssociateTo[last[[3]], "RowNode" -> {last[[2]], ToSourceCharacterString /@ extracted[[2;;]]}];
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

decrementLast[{most___, 1}] := $Failed
decrementLast[{most___, last_}] := {most, last - 1}


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
      rhs_}, _]}, data_]] /; ToSourceCharacterString[lhs1] === ToSourceCharacterString[lhs2] :=
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
      rhs_}, _]}, data_]] /; ToSourceCharacterString[lhs2] === ToSourceCharacterString[lhs3] :=
  QuaternaryNode[MemoizedTagSetDelayed, abstractFormatNodes /@ {lhs1, trivia1, op1, trivia2, lhs2, trivia3, op2, trivia4, lhs3, trivia5, op3, trivia6, rhs}, data]

abstractFormatNodes[node_LeafNode] := node

abstractFormatNodes[CallNode[head_, ts_, data_]] := CallNode[abstractFormatNodes /@ head, abstractFormatNodes /@ ts, data]

abstractFormatNodes[BoxNode[RowBox, {ts_}, data_]] := BoxNode[RowBox, {abstractFormatNodes /@ ts}, data]

abstractFormatNodes[node:CodeNode[_, _, _]] := node

abstractFormatNodes[head_[tag_, ts_, data_]] := head[tag, abstractFormatNodes /@ ts, data]



line[level_] :=
  {LeafNode[Token`Newline, blockedNewline[], <||>]} ~Join~
    Table[LeafNode[Token`Whitespace, #, <||>]& /@ Characters[blockedIndentationString[]], level]

space[] = LeafNode[Token`WhiteSpace, " ", <||>]

tab[] = LeafNode[Token`WhiteSpace, "\t", <||>]

nil[] = {}



indent[LeafNode[Token`Newline, _, _], level_] :=
  line[level]

(*
Special case multiline strings with LineColumn convention

Must preserve the original number of columns preceding the string

We do this by inserting a newline, then the original number of spaces.

A line continuation is inserted to help with semantics of inserting a newline (this may not be needed)
Also, the line continuation helps to communicate the "separateness" of the string
*)
indent[LeafNode[String, s_ /; StringContainsQ[s, blockedNewline[]], data:KeyValuePattern[Source -> {{_, _}, {_, _}}]], level_] :=
Module[{origSpaces},
  origSpaces = data[[Key[Source], 1, 2]]-1;
  LeafNode[String, {"\\" <> blockedNewline[], Table[" ", origSpaces], s}, data]
]

(*
Special case multiline comments with LineColumn convention

It is ok to change the internal indentation of comments
*)
indent[LeafNode[Token`Comment, s_ /; StringContainsQ[s, blockedNewline[]], data:KeyValuePattern[Source -> {{_, _}, {_, _}}]], level_] :=
Module[{min, replaced, origSpaces},
  origSpaces = data[[Key[Source], 1, 2]]-1;
  (*
  Correctly indent comment, taking into account the original indentation
  *)
  (*
  min is the smallest number of irrelevant characters before any of the lines of the comment
  This is the amount of whitespace that is safe to remove from each line.

  We do not count completely empty lines within the comment here,
  because that would always result in a min of 0,
  so we require that there must be *some* whitespace starting the line
  *)
  min = Min[
    StringCases[s, (blockedNewline[] ~~ ws:" "... ~~ Except[" " | blockedNewline[]]) :> StringLength[ws]]
    ,
    origSpaces
  ];
  replaced = StringReplace[s, blockedNewline[] ~~ StringJoin[Table[" ", min]] :> blockedNewline[]];
  LeafNode[Token`Comment, StringReplace[replaced, blockedNewline[] -> blockedNewline[] <> StringJoin[Table[blockedIndentationString[], level]]], data]
]


(*
All other leafs:
  integers,
  reals,
  symbols,
  singleline strings,
  singleline comments,
  multiline string from FE,
  multiline comments from FE, setc.
*)
indent[LeafNode[tag_, _, data:KeyValuePattern["RowNode" -> row_]], level_] :=
  LeafNode[tag, row, data]

indent[n:LeafNode[_, _, _], level_] :=
  n

indent[n:ErrorNode[_, _, _], level_] :=
  n


indent[CompoundNode[tag_, {rand1_, rand2_}, data_], level_] :=
  CompoundNode[tag, {indent[rand1, level], indent[rand2, level]}, data]


(*
"FrontEnd"-style:
indentPostfixRator[Function][rator_, level_] :=
  {space[], indent[rator, level]}
*)

indentPostfixRator[_][rator_, level_] :=
  indent[rator, level]


indent[PrefixNode[tag_, {rator_, trivia..., rand_}, data_], level_] :=
  PrefixNode[tag, {indent[rator, level], indent[rand, level]}, data]

indent[PostfixNode[tag_, {rand_, trivia..., rator_}, data_], level_] :=
  PostfixNode[tag, {indent[rand, level], indentPostfixRator[tag][rator, level]}, data]


(*
Special case CompoundExpression at top-level

Completely preserve newlines for CompoundExpression at top-level

Breaking up lines or gluing lines together may change the actual CompoundExpressions and
how expressions are parsed
*)
indent[InfixNode[CompoundExpression, ts_, data_], level:0] :=
Catch[
Module[{aggs, rands, rators, graphs, lastRator, lastRand, 
  ratorsPat, randsPat},
  aggs = DeleteCases[ts, trivia];
  graphs = DeleteCases[ts, ws];
  rands = aggs[[1 ;; All ;; 2]];
  rators = aggs[[2 ;; All ;; 2]];
  lastRator = Last[rators];
  lastRand = Last[rands];
  ratorsPat = Alternatives @@ rators;
  randsPat = Alternatives @@ rands;

  InfixNode[CompoundExpression,
    Replace[graphs, {
      lastRator /; MatchQ[lastRand, LeafNode[Token`Fake`ImplicitNull, _, _]] :> indent[lastRator, level], 
      rator : ratorsPat :> {indent[rator, level], space[]},
      rand : randsPat :> indent[rand, level], 
      other_ :> indent[other, level]
    }, {1}]
    ,
    data
  ]
]]

(*
special casing CompoundExpression:

shouldStayOnSingleLine =
matches these cases:
  singleLineExpr;
  singleLineExpr; singleLineExpr
and does NOT match these cases:
  <newline> anywhere

if shouldStayOnSingleLine:
  do not insert space before semi
else:
  do not insert space before or after semi
  completely redo newlines
*)
indent[InfixNode[CompoundExpression, ts_, data_], level_] :=
Catch[
Module[{aggs, rands, rators, graphs, lastRator, lastRand, 
  ratorsPat, randsPat, shouldStayOnSingleLine},
  aggs = DeleteCases[ts, trivia];
  graphs = DeleteCases[ts, ws | nl];
  rands = aggs[[1 ;; All ;; 2]];
  rators = aggs[[2 ;; All ;; 2]];
  lastRator = Last[rators];
  lastRand = Last[rands];
  ratorsPat = Alternatives @@ rators;
  randsPat = Alternatives @@ rands;

  Which[
    blockedAiryness[] <= -0.25,
      shouldStayOnSingleLine = True
    ,
    !FreeQ[ts, LeafNode[Token`Newline, _, _]],
      (*
      CompoundExpression is on multiple lines
      *)
      shouldStayOnSingleLine = False
    ,
    Length[rands] <= 2,
      shouldStayOnSingleLine = True
    ,
    True,
      shouldStayOnSingleLine = False
  ];

  InfixNode[CompoundExpression,
    Which[
      blockedAiryness[] >= 0.25,
        Replace[graphs, {
            lastRator /; MatchQ[lastRand, LeafNode[Token`Fake`ImplicitNull, _, _]] :> indent[lastRator, level], 
            rator : ratorsPat :> {indent[rator, level], line[level], line[level]}, 
            rand : randsPat :> indent[rand, level], 
            comm_ :> {indent[comm, level], line[level], line[level]}
          }, {1}
        ]
      ,
      shouldStayOnSingleLine,
        Replace[graphs, {
            lastRator /; MatchQ[lastRand, LeafNode[Token`Fake`ImplicitNull, _, _]] :> indent[lastRator, level], 
            rator : ratorsPat :> {indent[rator, level], space[]},
            rand : randsPat :> indent[rand, level], 
            comm_ :> indent[comm, level]
          }, {1}
        ]
      ,
      True,
        Replace[graphs, {
            lastRator /; MatchQ[lastRand, LeafNode[Token`Fake`ImplicitNull, _, _]] :> indent[lastRator, level], 
            rator : ratorsPat :> {indent[rator, level], line[level]}, 
            rand : randsPat :> indent[rand, level], 
            comm_ :> {indent[comm, level], line[level]}
          }, {1}
        ]
    ]
    ,
    data
  ]
]]


(*
no spaces around Pattern:
a:b

no spaces around Optional:
1:2
a:b:c

no spaces around PatternTest:
a?b

no spaces around MessageName:
a::b

no spaces around Power:
a^b

*)
indentInfixRator[Pattern | Optional | PatternTest | MessageName | Power][rator_, level_] :=
  indent[rator, level]

indentInfixRator[Pattern | Optional | PatternTest | MessageName | Power][rator_, level_] :=
  indent[rator, level]


(*
No spaces before Comma
*)
indentInfixRator[Comma][rator_, level_] /; blockedAiryness[] >= 0.85 :=
  {line[level], indent[rator, level], line[level]}

indentInfixRatorNoTrailingSpace[Comma][rator_, level_] /; blockedAiryness[] >= 0.85 :=
  {line[level], indent[rator, level], line[level]}

indentInfixRator[Comma][rator_, level_] :=
  {indent[rator, level], space[]}

indentInfixRatorNoTrailingSpace[Comma][rator_, level_] :=
  indent[rator, level]

(*
special case implicit Times
*)
indentInfixRator[Times][LeafNode[Token`Fake`ImplicitTimes, _, _], level_] :=
  space[]

indentInfixRatorNoTrailingSpace[Times][LeafNode[Token`Fake`ImplicitTimes, _, _], level_] :=
  nil[]


indentInfixRator[_][rator_, level_] :=
  {space[], indent[rator, level], space[]}

indentInfixRatorNoTrailingSpace[_][rator_, level_] :=
  {space[], indent[rator, level]}


(*
Do not indent Comma
*)
indentIncrement[Comma, level_] := level

indentIncrement[_, level_] := level + 1


$AlwaysLineBreak = {

  (*
  BinaryNode
  *)
  SetDelayed,

  (*
  TernaryNode
  *)
  TagSetDelayed, MemoizedSetDelayed,

  (*
  QuaternaryNode
  *)
  MemoizedTagSetDelayed
}


(*

This is the big function for all BinaryNodes, InfixNodes, and TernaryNodes

The logic for all 3 is so similar, it should all be in a single function

*)
indent[(head:(BinaryNode|InfixNode|TernaryNode|QuaternaryNode))[tag_, ts_, data_], level_] :=
  Catch[
  Module[{aggs, rators, graphs, ratorsPat, split, lastGraph},

    aggs = DeleteCases[ts, trivia];
    rators = aggs[[2 ;; All ;; 2]];
    ratorsPat = Alternatives @@ rators;

    If[MemberQ[$AlwaysLineBreak, tag],
      (*
      Always line break, so redo newlines
      *)
      graphs = DeleteCases[ts, ws | nl];
      lastGraph = Last[graphs];
      If[MatchQ[lastGraph, GroupNode[_, _, _]],
        (*
        we want the opener of a group to be on same line as everything else
        Newlines are inserted inside the group itself
        *)
        lastGraph = redoGroupNewlines[lastGraph];
        split = {Most[graphs] ~Join~ {lastGraph}};
        ,
        split = {Most[graphs], {lastGraph}};
      ];
      ,
      (*
      split graphs around existing newline tokens 
      *)
      graphs = DeleteCases[ts, ws];
      split = Split[graphs, (matchNewlineQ[#1] == matchNewlineQ[#2])&];
      (*
      Forget about the actual newline tokens
      *)
      split = Take[split, {1, -1, 2}];
    ];
    
    If[blockedAiryness[] <= -0.5 || (Length[split] == 1 && blockedAiryness[] <= 0.5),
      (*
      There were no newline tokens
      *)
      Throw[
        head[tag,
          Map[
            Function[{grouped},
              Replace[grouped, {
                rator : ratorsPat :> indentInfixRator[tag][rator, level], 
                randOrComment_ :> indent[randOrComment, level]
              }, {1}]
            ]
            ,
            split
          ]
          ,
          data
        ]
      ]
    ];

    head[tag,
      With[{newLevel = indentIncrement[tag, level]},
        Riffle[
          Map[
            (*
            grouped is a list of tokens with no newline tokens
            *)
            Function[{grouped},
              {
                Replace[Most[grouped], {
                  rator : ratorsPat :> indentInfixRator[tag][rator, newLevel], 
                  randOrComment_ :> indent[randOrComment, newLevel]
                }, {1}]
                ,
                Replace[{Last[grouped]}, {
                  (* do not insert space after rator if immediately followed by newline *)
                  rator : ratorsPat :> indentInfixRatorNoTrailingSpace[tag][rator, newLevel], 
                  randOrComment_ :> indent[randOrComment, newLevel]
                }, {1}]
              }
            ]
            ,
            split
          ]
          ,
          {line[newLevel]}
        ]
      ]
      ,
      data
    ]
  ]]


redoGroupNewlines[GroupNode[tag_, {
  opener_,
  trivia...,
  ts:Except[trivia]...,
  trivia...,
  closer_
  }, data_]] :=
  GroupNode[tag, {
    opener,
    LeafNode[Token`Newline, blockedNewline[], <||>],
    ts,
    LeafNode[Token`Newline, blockedNewline[], <||>],
    closer
  }, data]


indent[GroupNode[tag_, {
      opener_, 
      trivia : trivia...,
      closer_
    }, data_], level_] :=
  Module[{triviaAggs, triviaGraphs, condition},

    triviaAggs = DeleteCases[{trivia}, ws];

    triviaGraphs = DeleteCases[{trivia}, ws | nl];

    Which[
      blockedAiryness[] >= 0.75,
        condition = MultiLineEnum
      ,
      blockedAiryness[] <= -0.75,
        condition = SingleLineEnum
      ,
      !FreeQ[triviaAggs, LeafNode[Token`Newline, _, _]],
        condition = MultiLineEnum
      ,
      True,
        condition = SingleLineEnum
    ];

    GroupNode[tag,
      Which[
        condition === MultiLineEnum,
        (*
        if multiline, then keep as multiline, but redo newlines
        *)
        {
          indent[opener, level], 
          indent[#, level + 1]& /@ triviaGraphs,
          line[level],
          indent[closer, level]
        }
        ,
        condition === SingleLineEnum,
        (*
        if will NOT make multiline, then keep everything on 1 line,
        and do not indent
        *)
        {
          indent[opener, level], 
          indent[#, level]& /@ triviaGraphs, 
          indent[closer, level]
        }
      ]
      ,
      data
    ]
  ]

indent[GroupNode[tag_, {
      opener_, 
      trivia1 : trivia...,
      ts:PatternSequence[Except[trivia], ___, Except[trivia]] | Except[trivia],
      trivia2 : trivia...,
      closer_
    }, data_], level_] :=
  Module[{aggs, trivia1Aggs, trivia2Aggs, trivia1Graphs, trivia2Graphs, condition, x},

    trivia1Aggs = DeleteCases[{trivia1}, ws];
    aggs = DeleteCases[{ts}, ws];
    trivia2Aggs = DeleteCases[{trivia2}, ws];

    trivia1Graphs = DeleteCases[{trivia1}, ws | nl];
    trivia2Graphs = DeleteCases[{trivia2}, ws | nl];

    Which[
      blockedAiryness[] >= 0.75,
        condition = MultiLineEnum
      ,
      blockedAiryness[] <= -0.75,
        condition = SingleLineEnum
      ,
      !FreeQ[trivia1Aggs, LeafNode[Token`Newline, _, _]],
        condition = MultiLineEnum
      ,
      FreeQ[aggs, LeafNode[Token`Newline, _, _]],
        condition = SingleLineEnum
      ,
      MatchQ[aggs, {(GroupNode|CallNode)[_, _, _]}],
        condition = SingleLineEnum
      ,
      MatchQ[aggs, {InfixNode[Comma, {(GroupNode|CallNode)[_, _, _], ___}, _]}] && FreeQ[trivia2Aggs, LeafNode[Token`Newline, _, _]],
        condition = SingleLineEnum
      ,
      (*
      MatchQ[aggs, {InfixNode[Comma, {___, GroupNode[_, _, _]}, _]}] && FreeQ[trivia2Aggs, LeafNode[Token`Newline, _, _]],
        condition = SingleLineEnum
      ,
      *)
      True,
        condition = WeirdMiddleEnum
    ];

    GroupNode[tag,
      Which[
        condition === WeirdMiddleEnum,

        (*
        Comma does not indent, so must indent manually here
        *)
        If[MatchQ[aggs, {InfixNode[Comma | CompoundExpression, _, _]}],
          x = 1
          ,
          x = 0
        ];
        (*
        format as single line, except there are newlines inside, so also line break before the closer
        *)
        {
          indent[opener, level], 
          indent[#, level]& /@ trivia1Graphs, 
          indent[#, level + x]& /@ aggs, 
          indent[#, level]& /@ trivia2Graphs,
          line[level],
          indent[closer, level]
        }
        ,
        condition === MultiLineEnum,
        (*
        if multiline, then keep as multiline, but redo newlines
        *)
        {
          indent[opener, level], 
          indent[#, level + 1]& /@ trivia1Graphs,
          line[level + 1],
          indent[#, level + 1]& /@ aggs, 
          indent[#, level]& /@ trivia2Graphs,
          line[level],
          indent[closer, level]
        }
        ,
        condition === SingleLineEnum,
        (*
        if will NOT make multiline, then keep everything on 1 line,
        and do not indent
        *)
        {
          indent[opener, level], 
          indent[#, level]& /@ trivia1Graphs, 
          indent[#, level]& /@ aggs, 
          indent[#, level]& /@ trivia2Graphs, 
          indent[closer, level]
        }
      ]
      ,
      data
    ]
  ]

indent[GroupMissingCloserNode[tag_, ts_, data_], level_] :=
  indent[GroupNode[tag, ts ~Join~ { implicitCloser[] }, data], level]

indent[implicitCloser[], level_] :=
  nil[]


(*
special casing Module | With | Block
*)
indent[CallNode[{head : LeafNode[Symbol, "Module" | "With" | "Block", _], trivia1:trivia...}, {
      GroupNode[GroupSquare, {
          opener_, 
          trivia2:trivia..., 
          InfixNode[
            Comma, {varsIn_, trivia3:trivia..., comma1:LeafNode[Token`Comma, _, _], trivia4:trivia..., 
              body_
            }, _
          ], 
          trivia5:trivia..., 
          closer_
        }, _]
    }, data_], level_] :=
  Module[{vars, comments1, comments2, comments3, comments4, comments5},

    vars = varsIn;

    comments1 = Cases[{trivia1}, comment];
    comments2 = Cases[{trivia2}, comment];
    comments3 = Cases[{trivia3}, comment];
    comments4 = Cases[{trivia4}, comment];
    comments5 = Cases[{trivia5}, comment];

    (*
    remove newlines from variable lists

    This is a kind of "pre-processing" that undoes previous line breaks that were inserted just for column limits

    Level -5 is where LeafNodes[] are
    *)
    vars = DeleteCases[vars, LeafNode[Token`Newline, _, _], {-5}];

    CallNode[{
        indent[head, level],
        indent[#, level + 1]& /@ comments1
      },
      {
        indent[opener, level + 1],
        indent[#, level + 1]& /@ comments2,
        indent[vars, level + 1],
        indent[#, level + 1]& /@ comments3,
        indent[comma1, level + 1], 
        line[level + 1], 
        indent[#, level + 1]& /@ comments4,
        indent[body, level + 1],
        If[!empty[comments5], line[level + 1], nil[]],
        Riffle[indent[#, level + 1]& /@ comments5, {space[]}],
        line[level],
        indent[closer, level]
      }
      ,
      data
    ]
  ]


(*
special casing Function
*)
indent[CallNode[{head:LeafNode[Symbol, "Function", _], trivia1:trivia...}, {
      GroupNode[GroupSquare, {
          opener_, 
          trivia2:trivia..., 
          InfixNode[
            Comma, {varsIn_, rest___}, data2_
          ], 
          trivia4:trivia..., 
          closer_
        }, data1_]
    }, data_], level_] :=
  Module[{vars, comments1, comments2},

    vars = varsIn;

    comments1 = Cases[{trivia1}, comment];
    comments2 = Cases[{trivia2}, comment];

    (*
    remove newlines from variable lists

    This is a kind of "pre-processing" that undoes previous line breaks that were inserted just for column limits
    
    Level -5 is where LeafNodes[] are
    *)
    vars = DeleteCases[{varsIn}, LeafNode[Token`Newline, _, _], {-5}];

    CallNode[{
        indent[head, level],
        indent[#, level]& /@ comments1},
      {
        indent[GroupNode[GroupSquare,
            {opener} ~Join~
            comments2 ~Join~
            {InfixNode[Comma, vars ~Join~ {rest}, data2]} ~Join~
            {trivia4, closer}, data1], level]
      }
      ,
      data
    ]
  ]


(*
special casing Switch

completely redo newlines
*)
indent[CallNode[{tag:LeafNode[Symbol, "Switch", _], trivia1:trivia...}, {
      GroupNode[GroupSquare, {
          opener_,
          trivia2:trivia...,
          InfixNode[
            Comma, {
              firstRand_,
              trivia3:trivia...,
              firstRator:Except[trivia],
              trivia4:trivia...,
              middle:Repeated[_, {2, Infinity}],
              trivia5:trivia...,
              lastRand_
            },
            _
          ], 
          trivia6:trivia..., 
          closer_
        }, _]
    }, data_], level_] :=
  Module[{aggs, rands, rators, tests, bodies, testsPat, bodiesPat, comments1, comments2, comments3, comments4, comments5, comments6},
    comments1 = Cases[{trivia1}, comment];
    comments2 = Cases[{trivia2}, comment];
    comments3 = Cases[{trivia3}, comment];
    comments4 = Cases[{trivia4}, comment];
    comments5 = Cases[{trivia5}, comment];
    comments6 = Cases[{trivia6}, comment];
    aggs = DeleteCases[{middle}, trivia];
    graphs = DeleteCases[{middle}, ws | nl];
    rands = aggs[[1 ;; -1 ;; 2]];
    tests = rands[[1;;All;;2]];
    bodies = rands[[2;;All;;2]];
    rators = aggs[[2 ;; -2 ;; 2]];
    ratorsPat = Alternatives @@ rators;
    testsPat = Alternatives @@ tests;
    bodiesPat = Alternatives @@ bodies;

    CallNode[{
        indent[tag, level + 1],
        indent[#, level + 1]& /@ comments1
      },
      {
        indent[opener, level + 1],
        indent[#, level + 1]& /@ comments2, 
        indent[firstRand, level + 1],
        indent[#, level + 1]& /@ comments3,
        indent[firstRator, level + 1],
        indent[#, level + 1]& /@ comments4,
        Replace[graphs, {
            rator : ratorsPat :> indent[rator, level + 1],
            test:testsPat :> {line[level + 1], indent[test, level + 1]},
            body:bodiesPat :> {line[level + 2], indent[body, level + 2], line[level + 1]},
            other_ :> indent[other, level + 1]
          }, {1}],
        line[level + 2],
        indent[#, level + 1]& /@ comments5,
        indent[lastRand, level + 2],
        indent[#, level + 1]& /@ comments6,
        line[level],
        indent[closer, level]
      }
      ,
      data
    ]
  ]


(*
special casing Which

completely redo newlines
*)
indent[CallNode[{tag:LeafNode[Symbol, "Which", _], trivia1:trivia...}, {
      GroupNode[GroupSquare, {
          opener_,
          trivia2:trivia..., 
          InfixNode[
            Comma, {
              most___,
              lastRand_
            },
            _
          ], 
          trivia3:trivia..., 
          closer_
        }, _]
    }, data_], level_] :=
  Module[{aggs, rands, rators, tests, bodies, testsPat, bodiesPat,
    comments1, comments2, comments3},
    comments1 = Cases[{trivia1}, comment];
    comments2 = Cases[{trivia2}, comment];
    comments3 = Cases[{trivia3}, comment];
    aggs = DeleteCases[{most}, trivia];
    graphs = DeleteCases[{most}, ws | nl];
    rands = aggs[[1 ;; -1 ;; 2]];
    tests = rands[[1;;All;;2]];
    bodies = rands[[2;;All;;2]];
    rators = aggs[[2 ;; -2 ;; 2]];
    ratorsPat = Alternatives @@ rators;
    testsPat = Alternatives @@ tests;
    bodiesPat = Alternatives @@ bodies;

    CallNode[{
        indent[tag, level + 1],
        indent[#, level + 1]& /@ comments1},
      {
        indent[opener, level + 1],
        indent[#, level + 1]& /@ comments2, 
        Replace[graphs, {
            rator : ratorsPat :> indent[rator, level + 1],
            test:testsPat :> {line[level + 1], indent[test, level + 1]},
            body:bodiesPat :> {line[level + 2], indent[body, level + 2], line[level + 1]},
            other_ :> indent[other, level + 1]
          }, {1}],
        line[level + 2],
        indent[lastRand, level + 2],
        indent[#, level + 1]& /@ comments3,
        line[level],
        indent[closer, level]
      }
      ,
      data
    ]
  ]


(*
special casing If

completely redo newlines
*)
indent[CallNode[{tag : LeafNode[Symbol, "If", _], trivia1 : trivia...}, {
      GroupNode[_, {
          opener_, 
          trivia2 : trivia..., 
          InfixNode[
            Comma, {
              firstRand_, 
              trivia3 : trivia..., 
              firstRator:Except[trivia], 
              rest___
            }, _
          ], 
          trivia4 : trivia..., 
          closer_
        }, _]
    }, data_], level_] :=
  Module[{graphs, comments1, comments2, comments3, comments4, aggs, rators, 
    rands, ratorsPat, condition},

    comments1 = Cases[{trivia1}, comment];
    comments2 = Cases[{trivia2}, comment];
    aggs = DeleteCases[{rest}, trivia];
    graphs = DeleteCases[{rest}, ws | nl];
    comments3 = Cases[{trivia3}, comment];
    rands = aggs[[1 ;; All ;; 2]];
    rators = aggs[[2 ;; All ;; 2]];
    comments4 = Cases[{trivia4}, comment];
    ratorsPat = Alternatives @@ rators;

    Which[
      (*
      If[anything, symbol, symbol] => single line
      *)
      MatchQ[aggs, {PatternSequence[LeafNode[Symbol, _, _], LeafNode[Token`Comma, _, _]]..., LeafNode[Symbol, _, _]}],
        condition = SingleLine
      ,
      True,
        condition = RedoNewlines
    ];

    CallNode[{
        indent[tag, level + 1], 
        indent[#, level + 1]& /@ comments1},
      Switch[condition,
        SingleLine,
          {
            indent[opener, level + 1], 
            indent[#, level + 1]& /@ comments2, 
            indent[firstRand, level + 1], 
            indent[#, level + 1]& /@ comments3, 
            indent[firstRator, level + 1],
            Replace[graphs, {
                rator : ratorsPat :> indent[rator, level + 1], 
                other_ :> indent[other, level + 1]
              }, {1}], 
            indent[#, level]& /@ comments4,
            indent[closer, level]
          }
        ,
        RedoNewlines,
          {
            indent[opener, level + 1], 
            indent[#, level + 1]& /@ comments2, 
            indent[firstRand, level + 1], 
            indent[#, level + 1]& /@ comments3, 
            indent[firstRator, level + 1], 
            line[level + 1], 
            Replace[graphs, {
                rator : ratorsPat :> {line[level + 1], indent[rator, level + 1], line[level + 1]}, 
                other_ :> indent[other, level + 1]
              }, {1}], 
            indent[#, level]& /@ comments4, 
            line[level], 
            indent[closer, level]
          }
      ]
      ,
      data
    ]
  ]

(*
special casing For

completely redo newlines
*)
indent[CallNode[{tag : LeafNode[Symbol, "For", _], trivia1 : trivia...}, {
      GroupNode[_, {
          opener_, 
          trivia2 : trivia..., 
          InfixNode[
            Comma, {
              rand1_,
              trivia3 : trivia...,
              rator1:Except[trivia],
              trivia4:trivia...,
              rand2:Except[trivia],
              trivia5:trivia...,
              rator2:Except[trivia],
              trivia6:trivia...,
              rand3:Except[trivia],
              trivia7:trivia...,
              rator3:Except[trivia],
              trivia8:trivia...,
              rand4_
            }, _
          ], 
          trivia9 : trivia..., 
          closer_
        }, _]
    }, data_], level_] :=
  Module[{comments1, comments2, comments3, comments4, comments5, comments6, comments7, comments8, comments9},
    comments1 = Cases[{trivia1}, comment];
    comments2 = Cases[{trivia2}, comment];
    comments3 = Cases[{trivia3}, comment];
    comments4 = Cases[{trivia4}, comment];
    comments5 = Cases[{trivia5}, comment];
    comments6 = Cases[{trivia6}, comment];
    comments7 = Cases[{trivia7}, comment];
    comments8 = Cases[{trivia8}, comment];
    comments9 = Cases[{trivia9}, comment];

    CallNode[{
        indent[tag, level],
        Riffle[indent[#, level]& /@ comments1, {space[]}]
      },
      {
        indent[opener, level],
        Riffle[indent[#, level]& /@ comments2, {space[]}],
        indent[rand1, level],
        Riffle[indent[#, level]& /@ comments3, {space[]}],
        indent[rator1, level], space[],
        Riffle[indent[#, level]& /@ comments4, {space[]}],
        indent[rand2, level],
        Riffle[indent[#, level]& /@ comments5, {space[]}],
        indent[rator2, level], space[],
        Riffle[indent[#, level]& /@ comments6, {space[]}],
        indent[rand3, level],
        Riffle[indent[#, level]& /@ comments7, {space[]}],
        indent[rator3, level], space[],
        Riffle[indent[#, level]& /@ comments8, {space[]}],
        line[level + 1],
        indent[rand4, level + 1],
        If[!empty[comments9], line[level + 1], nil[]],
        Riffle[indent[#, level + 1]& /@ comments9, {space[]}],
        line[level], 
        indent[closer, level]
      }
      ,
      data
    ]
  ]

indent[CallNode[{tag_, trivia...}, ts_, data_], level_] :=
  CallNode[{indent[tag, level]}
    ,
    indent[#, level]& /@ ts
    ,
    data
  ]


indent[SyntaxErrorNode[tag_, ts_, data_], level_] :=
  SyntaxErrorNode[tag, indent[#, level]& /@ ts, data]


(*
special casing ContainerNode

redo newlines
*)
indent[ContainerNode[File, ts_, data_], level_] :=
  Module[{graphs},

    graphs = DeleteCases[ts, ws];

    If[Length[graphs] >= 1 && !MatchQ[graphs[[-1]], nl],
      AppendTo[graphs, LeafNode[Token`Newline, "", <||>]]
    ];

    ContainerNode[File,
      indent[#, level]& /@ graphs
      ,
      data
    ]
  ]

indent[ContainerNode[Box, ts_, data_], level_] :=
  Module[{graphs},

    graphs = DeleteCases[ts, ws];
    
    ContainerNode[Box,
      indent[#, level]& /@ graphs
      ,
      data
    ]
  ]

indent[ContainerNode[tag_, ts_, data_], level_] :=
  Module[{graphs},

    graphs = DeleteCases[ts, ws];
    
    ContainerNode[tag,
      indent[#, level]& /@ graphs
      ,
      data
    ]
  ]



indent[BoxNode[RowBox, {ts_}, data_], level_] :=
  Module[{graphs},

    graphs = DeleteCases[ts, ws];

    BoxNode[RowBox,
      indent[#, level]& /@ graphs
      ,
      data
    ]
  ]

indent[node:BoxNode[_, _, _], level_] :=
  node





linearize[node_] := Flatten[linearize0[node]]

linearize0[(ContainerNode|GroupNode|
  BinaryNode|InfixNode|TernaryNode|QuaternaryNode|
  PrefixNode|PostfixNode|
  CompoundNode|
  BoxNode)[_, fs_List, _]] :=
  linearize0 /@ fs

linearize0[(CallNode)[head_List, fs_List, _]] :=
  {linearize0 /@ head, linearize0 /@ fs}

linearize0[n:LeafNode[_, _String, _]] :=
  n

linearize0[n:ErrorNode[_, _String, _]] :=
  n

linearize0[LeafNode[tag_, fs_List, _]] :=
  FragmentNode[tag, #, <||>]& /@ Flatten[linearize0[#]& /@ fs]

linearize0[ErrorNode[tag_, fs_List, _]] :=
  FragmentNode[tag, #, <||>]& /@ Flatten[linearize0[#]& /@ fs]

linearize0[fs_List] :=
  linearize0 /@ fs

linearize0[f_String] :=
  f




mergeLineContinuations[fs_] :=
  Module[{poss, lc, onePast, numberOfOriginalSpaces, numberOfBeforeChars, originalSpacesSpec, dropSpecs, takeSpec,
    newFs},

    lc = FragmentNode[String, "\\" <> blockedNewline[], <||>];

    poss = Position[fs, lc];

    dropSpecs = Function[{pos},

      (*
      Count how many spaces after the line continuation
      *)
      onePast = NestWhile[# + 1 &, pos[[1]] + 1, # <= Length[fs] && MatchQ[fs[[#]], (LeafNode|FragmentNode)[_, " ", _]] &];

      originalSpacesSpec = {pos[[1]] + 1, onePast};

      numberOfOriginalSpaces = (onePast) - (pos[[1]] + 1);

      (*
      Count how many characters before the line continuation (but after any previous newline)
      *)
      onePast = NestWhile[# - 1 &, pos[[1]] - 1, # >= 1 && !MatchQ[fs[[#]], (LeafNode|FragmentNode)[_, blockedNewline[], _]] &];

      takeSpec = {onePast, pos[[1]] - 1};

      numberOfBeforeChars = (pos[[1]] - 1) - (onePast);

      Which[
        numberOfBeforeChars == 0 && numberOfOriginalSpaces == 0,
          (*
          not mergeable; the line continuation will be dropped later 
          *)
          Nothing
        ,
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
          {pos[[1]], originalSpacesSpec[[2]] - (numberOfOriginalSpaces - numberOfBeforeChars) - 1}
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
        (*
        AllTrue[Take[fs, {takeSpec[[2]] - (numberOfBeforeChars - numberOfOriginalSpaces) + 1, takeSpec[[2]]}], MatchQ[#, (LeafNode|FragmentNode)[_, " ", _]]&] &&
          True,
          If[$Debug,
            Print["mergeable 2"];
            Print["tested this spec: ", {takeSpec[[2]] - (numberOfBeforeChars - numberOfOriginalSpaces) + 1, takeSpec[[2]]}]
          ];
          (*
          make sure to include the line continuation itself
          *)
          {takeSpec[[2]] - (numberOfBeforeChars - numberOfOriginalSpaces) + 1, originalSpacesSpec[[2]] - 1}
        ,
        *)
        True,
          (*
          not mergeable; the line continuation will be dropped later 
          *)
          Nothing
      ]

    ] /@ poss;

    newFs = Fold[Drop, fs, dropSpecs // Reverse];

    (*
    cleanup any remaining line continuations
    *)
    newFs = newFs /. {
      lc :> Sequence @@ line[0],
      (*
      space fragments are now orphaned, so need to convert back to LeafNodes
      *)
      FragmentNode[String, " ", data_] :> LeafNode[Token`WhiteSpace, " ", data],
      FragmentNode[String, str_, data_] :> LeafNode[String, str, data]
    };

    newFs
  ]




End[]

EndPackage[]
