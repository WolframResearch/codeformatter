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



Begin["`Private`"]

Needs["CodeParser`"]


If[PacletFind["Format"] != {},
  Message[General::obspkg, "Format`"]
]



$AirynessLevel = 1.0

$TabWidth = 4



CodeFormat::usage = "CodeFormat[code] returns a string of formatted WL code."

Options[CodeFormat] = {
  AirynessLevel :> $AirynessLevel,
  "IndentationString" -> "    ",
  "Newline" -> "\n",
  "TabWidth" -> $TabWidth
}


CodeFormat[f:File[_String], opts:OptionsPattern[]] :=
  formatFile[f, opts]

Options[formatFile] = Options[CodeFormat]

formatFile[File[file_String], opts:OptionsPattern[]] :=
Catch[
Module[{cst, bytes, tabWidth},

  tabWidth = OptionValue["TabWidth"];

  bytes = Import[file, "Byte"];

  If[bytes == {},
    Throw[""]
  ];

  cst = CodeConcreteParse[bytes, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
   ];

  formatCST[cst, opts]
]]


CodeFormat[str_String, opts:OptionsPattern[]] :=
  formatString[str, opts]

Options[formatString] = Options[CodeFormat]

formatString[str_String, opts:OptionsPattern[]] :=
Module[{cst, tabWidth},

  tabWidth = OptionValue["TabWidth"];

  cst = CodeConcreteParse[str, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
   ];

   formatCST[cst, opts]
]


CodeFormat[bytes_List, opts:OptionsPattern[]] :=
  formatBytes[bytes, opts]

Options[formatBytes] = Options[CodeFormat]

formatBytes[bytes_List, opts:OptionsPattern[]] :=
Catch[
Module[{cst, tabWidth},

  tabWidth = OptionValue["TabWidth"];

  If[bytes == {},
    Throw[""]
  ];

  cst = CodeConcreteParse[bytes, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
   ];

   formatCST[cst, opts]
]]



Options[CodeFormatCST] = Options[CodeFormat]

CodeFormatCST[cst_, opts:OptionsPattern[]] :=
  formatCST[cst, opts]



Options[formatCST] = Options[CodeFormat]

formatCST[cstIn_, OptionsPattern[]] :=
Module[{indentationString, cst, newline, tabWidth},

  indentationString = OptionValue["IndentationString"];
  newline = OptionValue["Newline"];
  tabWidth = OptionValue["TabWidth"];

  cst = cstIn;

  cst = removeLineContinuations[cst];

  cst = standardizeNewlines[cst, newline];

  cst = standardizeTabs[cst, newline, tabWidth];

  Block[{blockedIndentationString, blockedNewline},

    blockedIndentationString[] = indentationString;

    blockedNewline[] = newline;

    fmt[cst, 0]
  ]
]


(*
Level -5 is where LeafNodes[] are
*)
removeLineContinuations[cst_] :=
  Replace[cst, {
    (*
    quickly filter down to LeafNodes that may have line continuations
    *)
    LeafNode[tag_, s_ /; StringContainsQ[s, "\\"], data_] :>
     LeafNode[tag, 
      StringReplace[s, {"\\" ~~ ("\r\n" | "\n" | "\r") ~~ WhitespaceCharacter... -> ""}], data]}, {-5}]


(*
Level -5 is where LeafNodes[] are
*)
standardizeNewlines[cst_, newline_] :=
  Replace[cst, {
    LeafNode[tag : String | Token`Comment, s_, data_] :>
     LeafNode[tag, 
      StringReplace[s, {"\r\n" -> newline, "\n" -> newline, "\r" -> newline}], data]}, {-5}]


(*
Level -5 is where LeafNodes[] are
*)
standardizeTabs[cst_, newline_, tabWidth_] :=
  Replace[cst, {
    LeafNode[Whitespace, "\t", data_] :> 
      LeafNode[Whitespace, replacementFunc[data[[Key[Source], 1, 2]], tabWidth], data],
    LeafNode[tag : String | Token`Comment, s_ /; StringContainsQ[s, "\t"], data_] :> 
      LeafNode[tag, replaceTabs[s, data[[Key[Source], 1, 2]], newline, tabWidth], data]}, {-5}]


(*
Memoizing function that returns the number of spaces that a tab should be replaced with

replacementFunc[1, 4] => "    "
replacementFunc[2, 4] => "   "
replacementFunc[3, 4] => "  "
replacementFunc[4, 4] => " "
replacementFunc[5, 4] => "    "
replacementFunc[6, 4] => "   "
replacementFunc[7, 4] => "  "
replacementFunc[8, 4] => " "
replacementFunc[9, 4] => "    "

*)
replacementFunc[col_, tabWidth_] :=
  replacementFunc[col, tabWidth] =
  StringJoin[Table[" ", Mod[1 - col, tabWidth, 1]]]


replaceTabs[str_String, startingColumn_Integer, newline_String, tabWidth_Integer] :=
Module[{pos, lines},
  lines = StringSplit[str, newline, All];
  (*
  Pad the first line with the correct number of characters from its origin
  *)
  lines[[1]] = StringJoin[Table["!", startingColumn - 1], lines[[1]]];
  lines = Map[
    Function[{line},
      (*
      for each line, accumulate a string by replacing each tab with its equivalent spaces
      *)
      NestWhile[
        Function[{lineAccum},
          pos = StringPosition[lineAccum, "\t"][[1, 1]];
          StringReplacePart[lineAccum, replacementFunc[pos, tabWidth], {pos, pos}]
        ]
        ,
        line
        ,
        StringContainsQ[#, "\t"]&
      ]
    ]
    ,
    lines
  ];
  (*
  Remove padding
  *)
  lines[[1]] = StringDrop[lines[[1]], startingColumn - 1];
  StringJoin[Riffle[lines, newline]]
]



trivia = LeafNode[Whitespace | Token`Comment | Token`Newline, _, _]

ws = LeafNode[Whitespace, _, _]

nl = LeafNode[Token`Newline, _, _]

comment = LeafNode[Token`Comment, _, _]

matchNewlineQ = MatchQ[nl]


cat[docs___] :=
  StringJoin[docs]

line[indent_] := blockedNewline[] <> StringJoin[Table[blockedIndentationString[], indent]]

space[] = " "

nil = ""

nest[i_][doc_String] :=
  StringReplace[doc, blockedNewline[] -> blockedNewline[] <> StringJoin[Table[blockedIndentationString[], i]]]


fmt[LeafNode[Token`Newline, s_, _], indent_] :=
  line[indent]

fmt[LeafNode[Token`Comment, s_, data_], indent_] :=
Module[{min, replaced},
  (*
  Correctly indent comment, taking into account the original indentation
  *)
  (*
  min is the smallest number of irrelevant characters before any of the lines of the comment
  This is the amount of whitespace that is safe to remove form each line.

  We do not count completely empty lines within the comment here,
  because that would always result in a min of 0,
  so we require that there must be *some* whitespace starting the line

  *)
  min = Min[StringCases[s, (blockedNewline[] ~~ ws:" ".. ~~ Except[" "]) :> StringLength[ws]], data[[Key[Source], 1, 2]]-1];
  replaced = StringReplace[s, blockedNewline[] ~~ StringJoin[Table[" ", min]] :> blockedNewline[]];
  nest[indent][replaced]
]

fmt[LeafNode[_, s_, _], indent_] :=
  s


fmt[ErrorNode[_, s_, _], indent_] :=
  s


fmt[CompoundNode[_, {rand1_, rand2_}, _], indent_] :=
  cat[fmt[rand1, indent], fmt[rand2, indent]]


fmt[PrefixNode[_, {rator_, trivia..., rand_}, _], indent_] :=
  cat[fmt[rator, indent], fmt[rand, indent]]

fmt[PostfixNode[_, {rator_, trivia..., rand_}, _], indent_] :=
  cat[fmt[rator, indent], fmt[rand, indent]]



(*
special casing CompoundExpression:

shouldStayOnSingleLine =
matches these cases:
  singleLineExpr;
  singleLineExpr; singleLineExpr
does NOT match these cases:
  <newline> anywhere

if shouldStayOnSingleLine:
  do not insert space before semi
else:
  do not insert space before or after semi
  completely redo newlines
*)
fmt[InfixNode[CompoundExpression, ts_, data_], indent_] :=
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
    data[[Key[Source], 1, 1]] != data[[Key[Source], 2, 1]],
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

  If[shouldStayOnSingleLine,

    Throw[
    cat[Replace[graphs, {
          lastRator /; MatchQ[lastRand, LeafNode[Token`Fake`ImplicitNull, _, _]] :> fmt[lastRator, indent], 
          rator : ratorsPat :> fmt[rator, indent],
          rand : randsPat :> fmt[rand, indent], 
          comm_ :> fmt[comm, indent]
        }, {1}]]
    ]
  ];

  cat[Replace[graphs, {
        lastRator /; MatchQ[lastRand, LeafNode[Token`Fake`ImplicitNull, _, _]] :> fmt[lastRator, indent], 
        rator : ratorsPat :> cat[fmt[rator, indent], line[indent]], 
        rand : randsPat :> fmt[rand, indent], 
        comm_ :> cat[fmt[comm, indent], line[indent]]
      }, {1}]]
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

*)
fmtRator[Pattern | Optional | PatternTest | MessageName][rator_, indent_] :=
  fmt[rator, indent]

fmtRatorNoTrailingSpace[Pattern | Optional | PatternTest | MessageName][rator_, indent_] :=
  fmt[rator, indent]


(*
No spaces before Comma
*)
fmtRator[Comma][rator_, indent_] :=
  cat[fmt[rator, indent], space[]]

fmtRatorNoTrailingSpace[Comma][rator_, indent_] :=
  fmt[rator, indent]

(*
special case implicit Times
*)
fmtRator[Times][LeafNode[Token`Fake`ImplicitTimes, _, _], indent_] :=
  space[]

fmtRatorNoTrailingSpace[Times][LeafNode[Token`Fake`ImplicitTimes, _, _], indent_] :=
  nil[]


fmtRator[_][rator_, indent_] :=
  cat[space[], fmt[rator, indent], space[]]

fmtRatorNoTrailingSpace[_][rator_, indent_] :=
  cat[space[], fmt[rator, indent]]



(*
Do not indent Comma
*)
indentIncrement[Comma, indent_] := indent

indentIncrement[_, indent_] := indent + 1



$AlwaysIndent = {

  (*
  BinaryNode
  *)
  SetDelayed,

  (*
  TernaryNode
  *)
  TagSetDelayed
}


(*

This is the big function for all BinaryNodes, InfixNodes, and TernaryNodes

The logic for all 3 is so similar, it should all be in a single function

*)
fmt[(BinaryNode|InfixNode|TernaryNode)[tag_, ts_, _], indent_] :=
  Catch[
  Module[{aggs, rators, graphs, ratorsPat, split},

    aggs = DeleteCases[ts, trivia];
    graphs = DeleteCases[ts, ws];
    rators = aggs[[2 ;; All ;; 2]];
    ratorsPat = Alternatives @@ rators;

    If[MemberQ[$AlwaysIndent, tag],
      (*
      Always indent, so redo newlines
      *)
      graphs = DeleteCases[ts, ws | nl];
      split = {Most[graphs], {Last[graphs]}};
      ,
      (*
      split graphs around existing newline tokens 
      *)
      split = Split[graphs, ((!matchNewlineQ[#1] && !matchNewlineQ[#2]) || (matchNewlineQ[#1] && matchNewlineQ[#2]))&];
      (*
      Forget about the actual newline tokens
      *)
      split = Take[split, {1, -1, 2}];
    ];

    If[Length[split] == 1,
      (*
      There were no newline tokens
      *)
      Throw[
        cat[
          Map[
            (*
            grouped is a list of tokens with no newline tokens
            *)
            Function[{grouped},
              cat[Replace[grouped, {
                rator : ratorsPat :> fmtRator[tag][rator, indent], 
                randOrComment_ :> fmt[randOrComment, indent]
              }, {1}]]
            ]
            ,
            split
          ]
        ]
      ]
    ];

    cat[
    Riffle[
      Map[
        (*
        grouped is a list of tokens with no newline tokens
        *)
        Function[{grouped},
          cat[
            cat[Replace[Most[grouped], {
              rator : ratorsPat :> fmtRator[tag][rator, indentIncrement[tag, indent]], 
              randOrComment_ :> fmt[randOrComment, indentIncrement[tag, indent]]
            }, {1}]]
            ,
            cat[Replace[{Last[grouped]}, {
              (* do not insert space after rator if immediately followed by newline *)
              rator : ratorsPat :> fmtRatorNoTrailingSpace[tag][rator, indentIncrement[tag, indent]], 
              randOrComment_ :> fmt[randOrComment, indentIncrement[tag, indent]]
            }, {1}]]
          ]
        ]
        ,
        split
      ]
      ,
      line[indentIncrement[tag, indent]]
    ]]
  ]]



fmt[GroupNode[_, {
      opener_, 
      trivia1 : trivia..., 
      ts : Except[trivia]..., 
      trivia2 : trivia...,
      closer_
    }, _], indent_] :=
  Module[{aggs, trivia1Aggs, trivia2Aggs, trivia1HasNewline, 
    trivia2HasNewline},
    trivia1Aggs = DeleteCases[{trivia1}, ws];
    aggs = DeleteCases[{ts}, ws];
    trivia2Aggs = DeleteCases[{trivia2}, ws];

    trivia1HasNewline = !FreeQ[trivia1Aggs, LeafNode[Token`Newline, _, _]];
    trivia2HasNewline = !FreeQ[trivia2Aggs, LeafNode[Token`Newline, _, _]];

    Which[
      trivia1HasNewline && !trivia2HasNewline,
        cat[
          fmt[opener, indent], 
          fmt[#, indent + 1]& /@ trivia1Aggs, 
          fmt[#, indent + 1]& /@ aggs, 
          fmt[#, indent]& /@ trivia2Aggs, line[indent],
          fmt[closer, indent]
        ]
      ,
      trivia1HasNewline && trivia2HasNewline,
        cat[
          fmt[opener, indent], 
          fmt[#, indent + 1]& /@ trivia1Aggs, 
          fmt[#, indent + 1]& /@ aggs, 
          fmt[#, indent]& /@ trivia2Aggs, 
          fmt[closer, indent]
        ]
      ,
      !trivia1HasNewline && trivia2HasNewline,
        cat[
          fmt[opener, indent], 
          fmt[#, indent + 1]& /@ trivia1Aggs, line[indent + 1],
          fmt[#, indent + 1]& /@ aggs, 
          fmt[#, indent]& /@ trivia2Aggs,
          fmt[closer, indent]
        ]
      ,
      !trivia1HasNewline && !trivia2HasNewline,
        cat[
          fmt[opener, indent], 
          fmt[#, indent]& /@ trivia1Aggs, 
          fmt[#, indent]& /@ aggs, 
          fmt[#, indent]& /@ trivia2Aggs, 
          fmt[closer, indent]
        ]
    ]
  ]

fmt[GroupMissingCloserNode[tag_, ts_, data_], indent_] :=
  fmt[GroupNode[tag, ts ~Join~ { implicitCloser[] }, data], indent]

fmt[implicitCloser[], indent_] :=
  nil[]



fmt[CallNode[{head : LeafNode[Symbol, "Module" | "With" | "Block", _], trivia...}, {
      GroupNode[GroupSquare, {
          LeafNode[Token`OpenSquare, "[", _], 
          trivia..., 
          InfixNode[
            Comma, {vars_, trivia..., LeafNode[Token`Comma, _, _], trivia..., 
              body_
            }, _
          ], 
          trivia..., 
          LeafNode[Token`CloseSquare, "]", _]
        }, _]
    }, _], indent_] :=
  cat[
    fmt[head, indent], "[", fmt[vars, indent], ",", 
    line[indent + 1], 
    fmt[body, indent + 1], 
    line[indent],
    "]"
  ]

fmt[CallNode[{tag:LeafNode[Symbol, "Switch", _], trivia1:trivia...}, {
      GroupNode[GroupSquare, {
          opener_,
          trivia2:trivia..., 
          InfixNode[
            Comma, {
              firstRand_, 
              trivia3:trivia..., 
              firstRator:Except[trivia], 
              middle___,
              lastRand_
            },
            _
          ], 
          trivia4:trivia..., 
          closer_
        }, _]
    }, _], indent_] :=
  Module[{aggs, rands, rators, tests, bodies, testsPat, bodiesPat,
    comments1, comments2, comments3, comments4},
    comments1 = Cases[{trivia1}, comment];
    comments2 = Cases[{trivia2}, comment];
    comments3 = Cases[{trivia3}, comment];
    comments4 = Cases[{trivia4}, comment];
    aggs = DeleteCases[{middle}, trivia];
    graphs = DeleteCases[{middle}, ws | nl];
    rands = aggs[[1 ;; -1 ;; 2]];
    tests = rands[[1;;All;;2]];
    bodies = rands[[2;;All;;2]];
    rators = aggs[[2 ;; -2 ;; 2]];
    ratorsPat = Alternatives @@ rators;
    testsPat = Alternatives @@ tests;
    bodiesPat = Alternatives @@ bodies;
    cat[
      fmt[tag, indent + 1],
      fmt[#, indent + 1]& /@ comments1,
      fmt[opener, indent + 1],
      fmt[#, indent + 1]& /@ comments2, 
      fmt[firstRand, indent + 1],
      fmt[#, indent + 1]& /@ comments3,
      fmt[firstRator, indent + 1],
      Replace[graphs, {
          rator : ratorsPat :> fmt[rator, indent + 1],
          test:testsPat :> cat[line[indent + 1], fmt[test, indent + 1]],
          body:bodiesPat :> cat[line[indent + 2], fmt[body, indent + 2], line[indent + 1]],
          other_ :> fmt[other, indent + 1]
        }, {1}],
      line[indent + 2],
      fmt[lastRand, indent + 2],
      fmt[#, indent + 1]& /@ comments4,
      line[indent],
      fmt[closer, indent]
    ]
  ]

(*
special casing If

completely redo newlines
*)
fmt[CallNode[{tag : LeafNode[Symbol, "If", _], trivia1 : trivia...}, {
      GroupNode[_, {
          opener_, 
          trivia2 : trivia..., 
          InfixNode[
            Comma, {
              firstRand_, 
              trivia3 : trivia..., 
              firstRator_, 
              rest___
            }, _
          ], 
          trivia4 : trivia..., 
          closer_
        }, _]
    }, _], indent_] :=
  Module[{graphs, comments1, comments2, comments3, comments4, aggs, rators, 
    rands, ratorsPat},
    comments1 = Cases[{trivia1}, comment];
    comments2 = Cases[{trivia2}, comment];
    aggs = DeleteCases[{rest}, trivia];
    graphs = DeleteCases[{rest}, ws | nl];
    comments3 = Cases[{trivia3}, comment];
    rands = aggs[[1 ;; All ;; 2]];
    rators = aggs[[2 ;; All ;; 2]];
    comments4 = Cases[{trivia4}, comment];
    ratorsPat = Alternatives @@ rators;
    cat[
      fmt[tag, indent + 1], 
      fmt[#, indent + 1]& /@ comments1, 
      fmt[opener, indent + 1], 
      fmt[#, indent + 1]& /@ comments2, 
      fmt[firstRand, indent + 1], 
      fmt[#, indent + 1]& /@ comments3, 
      fmt[firstRator, indent + 1], 
      line[indent + 1], 
      Replace[graphs, {
          rator : ratorsPat :> cat[line[indent + 1], fmt[rator, indent + 1], line[indent + 1]], 
          other_ :> fmt[other, indent + 1]
        }, {1}], 
      fmt[#, indent]& /@ comments4, 
      line[indent], 
      fmt[closer, indent]
    ]
  ]

fmt[CallNode[{tag_, trivia..., ___}, ts_, _], indent_] :=
  cat[fmt[tag, indent], fmt[#, indent]& /@ ts]


fmt[ContainerNode[_, ts_, _], indent_] :=
  Module[{graphs},
    graphs = DeleteCases[ts, ws | nl];
    cat[
      cat[fmt[#, indent], line[indent], line[indent]]& /@ graphs
    ]
  ]


End[]

EndPackage[]
