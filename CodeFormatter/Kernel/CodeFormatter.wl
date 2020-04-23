BeginPackage["CodeFormatter`"]

(*
Functions
*)
CodeFormat



(*
Options
*)
AirynessLevel



Begin["`Private`"]

Needs["CodeParser`"]


If[PacletFind["Format"] != {},
  Message[General::obspkg, "Format`"]
]



$AirynessLevel = 1.0



CodeFormat::usage = "CodeFormat[code] returns a string of formatted WL code."

Options[CodeFormat] = {
  AirynessLevel :> $AirynessLevel,
  "IndentationString" -> "  "
}


CodeFormat[f:File[_String], opts:OptionsPattern[]] :=
  formatFile[f, opts]

Options[formatFile] = Options[CodeFormat]

formatFile[File[file_String], opts:OptionsPattern[]] :=
Catch[
Module[{cst, bytes},

  bytes = Import[file, "Byte"];

  cst = CodeConcreteParse[bytes];

  If[FailureQ[cst],
    Throw[cst]
   ];

  formatCST[cst, opts]
]]


CodeFormat[str_String, opts:OptionsPattern[]] :=
  formatString[str, opts]

Options[formatString] = Options[CodeFormat]

formatString[str_String, opts:OptionsPattern[]] :=
Module[{cst},

  cst = CodeConcreteParse[str];

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
Module[{cst},

  cst = CodeConcreteParse[bytes];

  If[FailureQ[cst],
    Throw[cst]
   ];

   formatCST[cst, opts]
]]




Options[formatCST] = Options[CodeFormat]

formatCST[cst_, OptionsPattern[]] :=
Module[{indentationString},

  indentationString = OptionValue["IndentationString"];

  Block[{blockedIndentationString},

    blockedIndentationString[] = indentationString;

    fmt[cst, 0]
  ]
]


trivia = LeafNode[
    Whitespace | Token`Comment | Token`Newline | Token`LineContinuation | 
      Token`Boxes`MultiWhitespace, _, _]

ws = LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]

nl = LeafNode[Token`Newline, _, _]

comment = LeafNode[Token`Comment, _, _]

cat[docs___] :=
  StringJoin[docs]

line[] = "\n"

space[] = " "

nest[i_][doc_String] :=
  StringReplace[doc, "\n" -> "\n" <> StringJoin[Table[blockedIndentationString[], i]]]


fmt[LeafNode[Token`Newline, s_, _], indent_] :=
  nest[indent][line[]]

fmt[LeafNode[Token`Comment, s_, data_], indent_] :=
  nest[indent][s]

fmt[LeafNode[_, s_, _], indent_] :=
  s


fmt[CompoundNode[_, {rand1_, rand2_}, _], indent_] :=
  cat[fmt[rand1, indent], fmt[rand2, indent]]


fmt[PrefixNode[_, {rator_, trivia..., rand_}, _], indent_] :=
  cat[fmt[rator, indent], fmt[rand, indent]]

fmt[PostfixNode[_, {rator_, trivia..., rand_}, _], indent_] :=
  cat[fmt[rator, indent], fmt[rand, indent]]


fmt[BinaryNode[SetDelayed, {
      lhs_, 
      trivia..., 
      _, 
      trivia..., 
      rhs_
    }, _], indent_] :=
  cat[fmt[lhs, indent], space[], ":=", 
    nest[indent + 1][cat[line[], fmt[rhs, indent]]]
  ]

fmt[BinaryNode[_, {
      rand1_, 
      trivia1 : trivia..., 
      rator : Except[trivia], 
      trivia2 : trivia..., 
      rand2_
    }, _], indent_] :=
  Module[{graphs1, graphs2},
    graphs1 = DeleteCases[{trivia1}, ws];
    graphs2 = DeleteCases[{trivia2}, ws];
    cat[
      fmt[rand1, indent], 
      fmt[#, indent + 1]& /@ graphs1, 
      space[], fmt[rator, indent + 1], space[], 
      fmt[#, indent + 1]& /@ graphs2, 
      fmt[rand2, indent + 1]
    ]
  ]


(*
special casing CompoundExpression:
do not insert space before or after semi

  completely redo newlines
*)
fmt[InfixNode[CompoundExpression, ts_, _], indent_] :=
  Catch[
    Module[{aggs, rands, rators, graphs, lastRator, lastRand, 
      ratorsPat, randsPat},
      aggs = DeleteCases[ts, trivia];
      graphs = DeleteCases[ts, ws | nl];
      rands = aggs[[1 ;; All ;; 2]];
      rators = aggs[[2 ;; All ;; 2]];
      lastRator = Last[rators];
      lastRand = Last[rands];
      ratorsPat = Alternatives @@ rators;
      randsPat = Alternatives @@ rands;
      cat[Replace[graphs, {
            lastRator /; 
              MatchQ[lastRand, LeafNode[Token`Fake`ImplicitNull, _, _]] :> 
              fmt[lastRator, indent], 
            rator : ratorsPat :> cat[fmt[rator, indent], nest[indent][line[]]], 
            rand : randsPat :> fmt[rand, indent], 
            comm_ :> cat[fmt[comm, indent], nest[indent][line[]]]
          }, {1}]]
    ]
  ]

(*
special casing Comma:
do not insert space before comma
*)
fmt[InfixNode[Comma, ts_, _], indent_] :=
  Catch[
    Module[{aggs, rators, graphs, ratorsPat},
      aggs = DeleteCases[ts, trivia];
      graphs = DeleteCases[ts, ws];
      rators = aggs[[2 ;; All ;; 2]];
      ratorsPat = Alternatives @@ rators;
      cat[Replace[graphs, {
            rator : ratorsPat :> cat[fmt[rator, indent], space[]], 
            rand_ :> cat[fmt[rand, indent]]
          }, {1}]]
    ]
  ]

fmt[InfixNode[MessageName, ts_, _], indent_] :=
  Module[{rands, rators},
    rands = ts[[1 ;; All ;; 2]];
    rators = ts[[2 ;; All ;; 2]];
    cat[Riffle[fmt[#, indent]& /@ rands, cat[fmt[#, indent]]& /@ rators]]
  ]

fmt[InfixNode[Times, ts_, _], indent_] :=
  Catch[
    Module[{aggs, rands, rators, graphs, ratorsPat},
      aggs = DeleteCases[ts, trivia];
      graphs = DeleteCases[ts, ws];
      rands = aggs[[1 ;; All ;; 2]];
      rators = aggs[[2 ;; All ;; 2]];
      ratorsPat = Alternatives @@ rators;
      cat[Replace[graphs, {
            rator : LeafNode[Token`Fake`ImplicitTimes, _, _] :> space[], 
            rator : ratorsPat :> cat[space[], fmt[rator, indent + 1], space[]], 
            other_ :> cat[fmt[other, indent + 1]]
          }, {1}]]
    ]
  ]

(*
behavior of InfixNode:

*)
fmt[InfixNode[_, ts_, _], indent_] :=
  Catch[
    Module[{aggs, rands, rators, graphs},
      aggs = DeleteCases[ts, trivia];
      graphs = DeleteCases[ts, ws];
      rands = aggs[[1 ;; All ;; 2]];
      rators = aggs[[2 ;; All ;; 2]];
      cat[Replace[graphs, {
            rator : Alternatives @@ rators :> 
              cat[space[], fmt[rator, indent + 1], space[]], 
            other_ :> cat[fmt[other, indent + 1]]
          }, {1}]]
    ]
  ]


fmt[TernaryNode[_, {
      rand1_, 
      trivia..., 
      rator1_, 
      trivia..., 
      rand2_, 
      trivia..., 
      rator2_, 
      trivia..., 
      rand3_
    }, _], indent_] :=
  cat[
    fmt[rand1, indent], 
    space[], fmt[rator1, indent], space[], 
    fmt[rand2, indent], 
    space[], fmt[rator2, indent], space[], 
    fmt[rand3, indent]
  ]


fmt[GroupNode[_, {
      opener_, 
      trivia1 : trivia..., 
      ts : Except[trivia]..., 
      trivia2 : trivia..., closer_
    }, _], indent_] :=
  Catch[
    Module[{aggs, trivia1Aggs, trivia2Aggs, trivia1HasNewline, 
      trivia2HasNewline},
      trivia1Aggs = DeleteCases[{trivia1}, ws];
      aggs = DeleteCases[{ts}, ws];
      trivia2Aggs = DeleteCases[{trivia2}, ws];
      trivia1HasNewline = !FreeQ[trivia1Aggs, LeafNode[Token`Newline, _, _]];
      trivia2HasNewline = !FreeQ[trivia2Aggs, LeafNode[Token`Newline, _, _]];
      If[trivia1HasNewline,
        If[!trivia2HasNewline,
          Throw[
            cat[
              fmt[opener, indent], 
              fmt[#, indent + 1]& /@ trivia1Aggs, 
              fmt[#, indent + 1]& /@ aggs, 
              fmt[#, indent]& /@ trivia2Aggs, nest[indent][line[]], 
              fmt[closer, indent]
            ]
          ]
        ]
      ];
      cat[
        fmt[opener, indent], 
        fmt[#, indent + 1]& /@ trivia1Aggs, 
        fmt[#, indent + 1]& /@ aggs, 
        fmt[#, indent]& /@ trivia2Aggs, 
        fmt[closer, indent]
      ]
    ]
  ]


fmt[CallNode[{head : LeafNode[Symbol, "Module" | "With" | "Block", _], trivia...}, {
      GroupNode[GroupSquare, {
          LeafNode[Token`OpenSquare, "[", _], 
          trivia..., 
          InfixNode[
            Comma, {vars_, trivia..., LeafNode[Token`Comma, _, _], trivia..., 
              body_}, _
          ], 
          trivia..., 
          LeafNode[Token`CloseSquare, "]", _]
        }, _]
    }, _], indent_] :=
  cat[fmt[head, indent], "[", fmt[vars, indent], ",", 
    nest[indent + 1][line[]], 
    fmt[body, indent + 1], 
    nest[indent][cat[line[], "]"]]
  ]

fmt[CallNode[{LeafNode[Symbol, "Switch", _], trivia...}, {
      GroupNode[GroupSquare, {
          LeafNode[Token`OpenSquare, "[", _], 
          trivia..., 
          InfixNode[Comma, args_, _], 
          trivia..., 
          LeafNode[Token`CloseSquare, "]", _]
        }, _]
    }, _], indent_] :=
  Module[{aggs, rands, rators, restRands, restRators},
    aggs = DeleteCases[args, trivia];
    rands = aggs[[1 ;; All ;; 2]];
    rators = aggs[[2 ;; All ;; 2]];
    restRands = Rest[rands];
    restRators = Rest[rators];
    cat["Switch", "[", fmt[First[rands], indent], fmt[First[rators], indent], 
      nest[indent + 1][
        cat[line[], 
          Riffle[fmt[#, indent]& /@ restRands, 
            cat[fmt[#, indent], line[]]& /@ restRators]]
      ], 
      nest[indent][cat[line[], "]"]]
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
      fmt[#, indent]& /@ comments1, 
      fmt[opener, indent + 1], 
      fmt[#, indent]& /@ comments2, 
      fmt[firstRand, indent + 1], 
      fmt[#, indent]& /@ comments3, 
      fmt[firstRator, indent + 1], 
      nest[indent + 2][line[]], 
      Replace[graphs, {
          rator : ratorsPat :> 
            cat[nest[indent + 1][line[]], fmt[rator, indent + 1], 
              nest[indent + 1][line[]]], 
          other_ :> cat[fmt[other, indent + 1]]
        }, {1}], 
      fmt[#, indent]& /@ comments4, 
      nest[indent][line[]], 
      fmt[closer, indent]
    ]
  ]

fmt[CallNode[{tag_, trivia..., ___}, ts_, _], indent_] :=
  cat[fmt[tag, indent], fmt[#, indent]& /@ ts]


fmt[ContainerNode[_, ts_, _], indent_] :=
  Module[{graphs},
    graphs = DeleteCases[ts, ws | nl];
    cat[cat[fmt[#, indent], nest[indent][line[]], nest[indent][line[]]]& /@ 
        graphs]
  ]


End[]

EndPackage[]
