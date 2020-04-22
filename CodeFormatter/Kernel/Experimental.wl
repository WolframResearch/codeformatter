BeginPackage["CodeFormatter`Experimental`"]

CodeFormat

Begin["`Private`"]

Needs["CodeParser`"]

(*

Usage:

cst = CodeConcreteParse[File[file]];
str = CodeFormat[cst]

*)

CodeFormat[cst_] :=
  fmt[cst, 0]

trivia = LeafNode[
    Whitespace | Token`Comment | Token`Newline | Token`LineContinuation | 
      Token`Boxes`MultiWhitespace, _, _
  ]

ws = LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]

nl = LeafNode[Token`Newline, _, _]

comment = LeafNode[Token`Comment, _, _]

cat[docs___] :=
  StringJoin[docs]

line[] = "\n"

space[] = " "

nest[i_][doc_String] :=
  StringReplace[doc, "\n" -> "\n" <> StringJoin[Table[space[], i]]]

nil[] = ""

fmt[BinaryNode[SetDelayed, {
      lhs_, 
      trivia..., 
      _, 
      trivia..., 
      rhs_
    }, _], indent_] :=
  cat[fmt[lhs, indent], space[], ":=", 
    nest[indent + 2][cat[line[], fmt[rhs, indent]]]
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
    nest[indent + 2][line[]], 
    fmt[body, indent + 2], 
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
      nest[indent + 2][
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
      fmt[tag, indent + 2], 
      fmt[#, indent]& /@ comments1, 
      fmt[opener, indent + 2], 
      fmt[#, indent]& /@ comments2, 
      fmt[firstRand, indent + 2], 
      fmt[#, indent]& /@ comments3, 
      fmt[firstRator, indent + 2], 
      nest[indent + 2][line[]], 
      Replace[graphs, {
          rator : ratorsPat :> 
            cat[nest[indent + 2][line[]], fmt[rator, indent + 2], 
              nest[indent + 2][line[]]], 
          other_ :> cat[fmt[other, indent + 2]]
        }, {1}], 
      fmt[#, indent]& /@ comments4, 
      nest[indent][line[]], 
      fmt[closer, indent]
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

(*
General rules
*)

fmt[ContainerNode[_, ts_, _], indent_] :=
  Module[{graphs},
    graphs = DeleteCases[ts, ws | nl];
    cat[cat[fmt[#, indent], nest[indent][line[]], nest[indent][line[]]]& /@ 
        graphs]
  ]

fmt[CallNode[{tag_, trivia..., ___}, ts_, _], indent_] :=
  cat[fmt[tag, indent], fmt[#, indent]& /@ ts]

fmt[LeafNode[Token`Newline, s_, _], indent_] :=
  nest[indent][line[]]

fmt[LeafNode[Token`Comment, s_, data_], indent_] :=
  nest[indent][s]

fmt[LeafNode[_, s_, _], indent_] :=
  s

fmt[SlotNode[_, ts_, _], indent_] :=
  cat[fmt[#, indent]& /@ ts]

fmt[SlotSequenceNode[_, ts_, _], indent_] :=
  cat[fmt[#, indent]& /@ ts]

fmt[OutNode[_, ts_, _], indent_] :=
  cat[fmt[#, indent]& /@ ts]

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
              fmt[#, indent + 2]& /@ trivia1Aggs, 
              fmt[#, indent + 2]& /@ aggs, 
              fmt[#, indent]& /@ trivia2Aggs, nest[indent][line[]], 
              fmt[closer, indent]
            ]
          ]
        ]
      ];
      cat[
        fmt[opener, indent], 
        fmt[#, indent + 2]& /@ trivia1Aggs, 
        fmt[#, indent + 2]& /@ aggs, 
        fmt[#, indent]& /@ trivia2Aggs, 
        fmt[closer, indent]
      ]
    ]
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
            rator : ratorsPat :> cat[space[], fmt[rator, indent + 2], space[]], 
            other_ :> cat[fmt[other, indent + 2]]
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
              cat[space[], fmt[rator, indent + 2], space[]], 
            other_ :> cat[fmt[other, indent + 2]]
          }, {1}]]
    ]
  ]

fmt[BlankNode[Blank, {under_, head_}, _], indent_] :=
  cat[fmt[under, indent], fmt[head, indent]]

fmt[BlankSequenceNode[BlankSequence, {under_, head_}, _], indent_] :=
  cat[fmt[under, indent], fmt[head, indent]]

fmt[BlankNullSequenceNode[BlankNullSequence, {under_, head_}, _], indent_] :=
  cat[fmt[under, indent], fmt[head, indent]]

fmt[PatternBlankNode[PatternBlank, {sym_, blank_}, _], indent_] :=
  cat[fmt[sym, indent], fmt[blank, indent]]

fmt[PatternBlankSequenceNode[PatternBlankSequence, {sym_, blank_}, _], indent_] :=
  cat[fmt[sym, indent], fmt[blank, indent]]

fmt[PatternBlankNullSequenceNode[PatternBlankNullSequence, {sym_, blank_}, _], indent_] :=
  cat[fmt[sym, indent], fmt[blank, indent]]

fmt[PatternOptionalDefaultNode[PatternOptionalDefault, {sym_, blank_}, _], indent_] :=
  cat[fmt[sym, indent], fmt[blank, indent]]

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
      fmt[#, indent + 2]& /@ graphs1, 
      space[], fmt[rator, indent + 2], space[], 
      fmt[#, indent + 2]& /@ graphs2, 
      fmt[rand2, indent + 2]
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

fmt[PrefixNode[_, {rator_, trivia..., rand_}, _], indent_] :=
  cat[fmt[rator, indent], fmt[rand, indent]]

fmt[PostfixNode[_, {rator_, trivia..., rand_}, _], indent_] :=
  cat[fmt[rator, indent], fmt[rand, indent]]

End[]

EndPackage[]

