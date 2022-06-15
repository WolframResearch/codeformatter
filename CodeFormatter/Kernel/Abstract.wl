(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["CodeFormatter`Abstract`"]

AbstractFormatNodes

cleanLexicalVariables

Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Definitions`"] (* for DefinitionSymbols *)
Needs["CodeParser`Utils`"]


(*
Some nodes can be "abstracted" for formatting
e.g.,

f[] := f[] = x

is a SetDelayed node, with a Set node on the RHS

But this really should be formatted as if it were a single TernaryNode
*)
AbstractFormatNodes[gstIn_] :=
Module[{gst},
  
  gst = gstIn;

  (*
  rely on abstractFormatNodes not yet run, and CompoundNode[tag, {child1, child2}] is still preserved
  *)
  If[$CleanLexicalVariables,
    gst = cleanLexicalVariables[gst];
  ];

  (*
  Now run and convert CompoundNode[tag, {child1, child2}] into LeafNode[tag, child1child2]
  *)
  gst = abstractFormatNodes[gst];
  
  gst
]

(*
abstract

a := a = b

into a single TernaryNode
*)
abstractFormatNodes[
  BinaryNode[SetDelayed, {
    lhs1_,
    comment1:comment...,
    op1:LeafNode[Token`ColonEqual, _, _],
    comment2:comment...,
    BinaryNode[Set, {
      lhs2_,
      comment3:comment...,
      op2:LeafNode[Token`Equal, _, _],
      comment4:comment...,
      rhs_}, _]}, data_]] /; sameDefinitionSymbols[lhs1, lhs2] :=
  TernaryNode[MemoizedSetDelayed, abstractFormatNodes /@ {lhs1, comment1, op1, comment2, lhs2, comment3, op2, comment4, rhs}, data]

abstractFormatNodes[
  BinaryNode[UpSetDelayed, {
    lhs1_,
    comment1:comment...,
    op1:LeafNode[Token`CaretColonEqual, _, _],
    comment2:comment...,
    BinaryNode[UpSet, {
      lhs2_,
      comment3:comment...,
      op2:LeafNode[Token`CaretEqual, _, _],
      comment4:comment...,
      rhs_}, _]}, data_]] /; sameDefinitionSymbols[lhs1, lhs2] :=
  TernaryNode[MemoizedUpSetDelayed, abstractFormatNodes /@ {lhs1, comment1, op1, comment2, lhs2, comment3, op2, comment4, rhs}, data]

(*
abstract

a /: b := b = c

into a single QuaternaryNode
*)
abstractFormatNodes[
  TernaryNode[TagSetDelayed, {
    lhs1_,
    comment1:comment...,
    op1:LeafNode[Token`SlashColon, _, _],
    comment2:comment...,
    lhs2_,
    comment3:comment...,
    op2:LeafNode[Token`ColonEqual, _, _],
    comment4:comment...,
    BinaryNode[Set, {
      lhs3_,
      comment5:comment...,
      op3:LeafNode[Token`Equal, _, _],
      comment6:comment...,
      rhs_}, _]}, data_]] /; sameDefinitionSymbols[lhs2, lhs3] :=
  QuaternaryNode[MemoizedTagSetDelayed, abstractFormatNodes /@ {lhs1, comment1, op1, comment2, lhs2, comment3, op2, comment4, lhs3, comment5, op3, comment6, rhs}, data]

(*
wrap ClauseNode around clauses
*)
abstractFormatNodes[CallNode[{head:LeafNode[Symbol, "Switch", _], headSeq___},
  GroupNode[GroupSquare, {
      opener_, openerSeq:comment...,
      InfixNode[Comma, {expr:Except[LeafNode[Token`Comma, _, _]], exprSeq:Except[LeafNode[Token`Comma, _, _]]..., rest___}, data2_],
      closerSeq:comment..., closer_}, data1_]
    , data_]] :=
Module[{replaced},

  replaced =
    SequenceReplace[{rest}, {
        test:Except[LeafNode[Token`Comma, _, _]],
        testSeq:Except[LeafNode[Token`Comma, _, _]]...,
        comm:LeafNode[Token`Comma, ",", _],
        valSeq:Except[LeafNode[Token`Comma, _, _]]...
      } :>
        ClauseNode[Switch, {test, testSeq, comm, valSeq}, <||>]
    ];

  CallNode[{head, headSeq}, 
    GroupNode[GroupSquare, {
      opener, openerSeq,
      InfixNode[Comma, abstractFormatNodes /@ ({expr, exprSeq} ~Join~ replaced), data2],
      closerSeq, closer
    }, data1]
  , data]
]

(*
wrap ClauseNode around clauses
*)
abstractFormatNodes[CallNode[{head:LeafNode[Symbol, "Which", _], headSeq___}, 
  GroupNode[GroupSquare, {
      opener_, openerSeq:comment...,
      InfixNode[Comma, commaChildren_, data2_],
      closerSeq:comment..., closer_}, data1_]
    , data_]] :=
Module[{replaced},

  replaced =
    SequenceReplace[commaChildren, {
        test:Except[LeafNode[Token`Comma, _, _]],
        testSeq:Except[LeafNode[Token`Comma, _, _]]...,
        comm:LeafNode[Token`Comma, ",", _],
        valSeq:Except[LeafNode[Token`Comma, _, _]]...
      } :>
        ClauseNode[Which, {test, testSeq, comm, valSeq}, <||>]
    ];

  CallNode[{head, headSeq},
    GroupNode[GroupSquare, {
      opener, openerSeq,
      InfixNode[Comma, abstractFormatNodes /@ replaced, data2],
      closerSeq, closer
    }, data1]
  , data]
]


(*
may have already been aggregated, so preserve
*)
cleanLexicalVariables[n:CallNode[head:LeafNode[Symbol, "Module", _], 
  GroupNode[GroupSquare, {
      opener_, 
      InfixNode[Comma, commaChildren_, data2_],
      closer_}, data1_]
    , data_]] :=
Catch[
Module[{ast, children, params, vars, rules},

  ast = CodeParser`Abstract`Abstract[n];

  children = ast[[2]];

  Which[
    Length[children] != 2,
      rules = {}
    ,
    !MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
      rules = {}
    ,
    True,
      params = children[[1, 2]];
      vars = # /. {
        CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _],
          sym:LeafNode[Symbol, _, _], _, _] :> sym,
        sym:LeafNode[Symbol, _, _] :> sym,
        _ :> Nothing
      }& /@ params;
      vars = Cases[vars, LeafNode[Symbol, name_ /; StringContainsQ[name, "`"], _]];
      rules = (LeafNode[Symbol, #[[2]], _] -> LeafNode[Symbol, Last[StringSplit[#[[2]], "`"]], <||>])& /@ vars;
  ];

  CallNode[head,
    GroupNode[GroupSquare, {
      opener,
      InfixNode[Comma, cleanLexicalVariables /@ (commaChildren /. rules), data2],
      closer
    }, data1]
  , data]
]]

(*
may have NOT YET been aggregated, so preserve
*)
cleanLexicalVariables[n:CallNode[{head:LeafNode[Symbol, "Module", _], headSeq___},
  GroupNode[GroupSquare, {
      opener_, openerSeq:comment...,
      InfixNode[Comma, commaChildren_, data2_],
      closerSeq:comment..., closer_}, data1_]
    , data_]] :=
Catch[
Module[{agg, ast, children, params, vars, rules},

  agg = CodeParser`Abstract`Aggregate[n];
  ast = CodeParser`Abstract`Abstract[agg];

  children = ast[[2]];

  Which[
    Length[children] != 2,
      rules = {}
    ,
    !MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
      rules = {}
    ,
    True,
      params = children[[1, 2]];
      vars = # /. {
        CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {
          sym:LeafNode[Symbol, _, _], _}, _] :> sym,
        sym:LeafNode[Symbol, _, _] :> sym,
        _ :> Nothing
      }& /@ params;
      vars = Cases[vars, LeafNode[Symbol, name_ /; StringContainsQ[name, "`"], _]];
      rules = (LeafNode[Symbol, #[[2]], _] -> LeafNode[Symbol, Last[StringSplit[#[[2]], "`"]], <||>])& /@ vars;
  ];

  CallNode[{head, headSeq},
    GroupNode[GroupSquare, {
      opener, openerSeq,
      InfixNode[Comma, cleanLexicalVariables /@ (commaChildren /. rules), data2],
      closerSeq, closer
    }, data1]
  , data]
]]


(*
may have already been aggregated, so preserve
*)
cleanLexicalVariables[n:CallNode[head:LeafNode[Symbol, "With", _], 
  GroupNode[GroupSquare, {
      opener_, 
      InfixNode[Comma, commaChildren_, data2_],
      closer_}, data1_]
    , data_]] :=
Catch[
Module[{ast, children, params, vars, rules},

  ast = CodeParser`Abstract`Abstract[n];

  children = ast[[2]];

  Which[
    Length[children] != 2,
      rules = {}
    ,
    !MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
      rules = {}
    ,
    True,
      params = children[[1, 2]];
      vars = # /. {
        CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {
          sym:LeafNode[Symbol, _, _], _}, _] :> sym,
        _ :> Nothing
      }& /@ params;
      vars = Cases[vars, LeafNode[Symbol, name_ /; StringContainsQ[name, "`"], _]];
      rules = (LeafNode[Symbol, #[[2]], _] -> LeafNode[Symbol, Last[StringSplit[#[[2]], "`"]], <||>])& /@ vars;
  ];

  CallNode[head,
    GroupNode[GroupSquare, {
      opener,
      InfixNode[Comma, cleanLexicalVariables /@ (commaChildren /. rules), data2],
      closer
    }, data1]
  , data]
]]

(*
may have NOT YET been aggregated, so preserve
*)
cleanLexicalVariables[n:CallNode[{head:LeafNode[Symbol, "With", _], headSeq___},
  GroupNode[GroupSquare, {
      opener_, openerSeq:comment...,
      InfixNode[Comma, commaChildren_, data2_],
      closerSeq:comment..., closer_}, data1_]
    , data_]] :=
Catch[
Module[{agg, ast, children, params, vars, rules},

  agg = CodeParser`Abstract`Aggregate[n];
  ast = CodeParser`Abstract`Abstract[agg];

  children = ast[[2]];

  Which[
    Length[children] != 2,
      rules = {}
    ,
    !MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
      rules = {}
    ,
    True,
      params = children[[1, 2]];
      vars = # /. {
        CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {
          sym:LeafNode[Symbol, _, _], _}, _] :> sym,
        _ :> Nothing
      }& /@ params;
      vars = Cases[vars, LeafNode[Symbol, name_ /; StringContainsQ[name, "`"], _]];
      rules = (LeafNode[Symbol, #[[2]], _] -> LeafNode[Symbol, Last[StringSplit[#[[2]], "`"]], <||>])& /@ vars;
  ];

  CallNode[{head, headSeq},
    GroupNode[GroupSquare, {
      opener, openerSeq,
      InfixNode[Comma, cleanLexicalVariables /@ (commaChildren /. rules), data2],
      closerSeq, closer
    }, data1]
  , data]
]]


(*
may have already been aggregated, so preserve
*)
cleanLexicalVariables[n:CallNode[head:LeafNode[Symbol, "Function", _],
  GroupNode[GroupSquare, {
      opener_, 
      InfixNode[Comma, commaChildren_, data2_],
      closer_}, data1_]
    , data_]] :=
Catch[
Module[{ast, children, param, params, rules},

  ast = CodeParser`Abstract`Abstract[n];

  children = ast[[2]];

  Which[
    !(2 <= Length[children] <= 3),
      rules = {}
    ,
    MatchQ[children[[1]], LeafNode[Symbol, "Null", _]],
      rules = {}
    ,
    MatchQ[children[[1]], LeafNode[Symbol, _, _]],
      param = children[[1]];
      params = Cases[{param}, LeafNode[Symbol, name_ /; StringContainsQ[name, "`"], _]];
      rules = (LeafNode[Symbol, #[[2]], _] -> LeafNode[Symbol, Last[StringSplit[#[[2]], "`"]], <||>])& /@ params;
    ,
    !MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
      rules = {}
    ,
    True,
      params = children[[1, 2]];
      params = # /. {
        sym:LeafNode[Symbol, _, _] :> sym,
        _ :> Nothing
      }& /@ params;
      params = Cases[params, LeafNode[Symbol, name_ /; StringContainsQ[name, "`"], _]];
      rules = (LeafNode[Symbol, #[[2]], _] -> LeafNode[Symbol, Last[StringSplit[#[[2]], "`"]], <||>])& /@ params;
  ];

  CallNode[head,
    GroupNode[GroupSquare, {
      opener,
      InfixNode[Comma, cleanLexicalVariables /@ (commaChildren /. rules), data2],
      closer
    }, data1]
  , data]
]]

(*
may have NOT YET been aggregated, so preserve
*)
cleanLexicalVariables[n:CallNode[{head:LeafNode[Symbol, "Function", _], headSeq___},
  GroupNode[GroupSquare, {
      opener_, openerSeq:comment...,
      InfixNode[Comma, commaChildren_, data2_],
      closerSeq:comment..., closer_}, data1_]
    , data_]] :=
Catch[
Module[{agg, ast, children, param, params, rules},

  agg = CodeParser`Abstract`Aggregate[n];
  ast = CodeParser`Abstract`Abstract[agg];

  children = ast[[2]];

  Which[
    !(2 <= Length[children] <= 3),
      rules = {}
    ,
    MatchQ[children[[1]], LeafNode[Symbol, "Null", _]],
      rules = {}
    ,
    MatchQ[children[[1]], LeafNode[Symbol, _, _]],
      param = children[[1]];
      params = Cases[{param}, LeafNode[Symbol, name_ /; StringContainsQ[name, "`"], _]];
      rules = (LeafNode[Symbol, #[[2]], _] -> LeafNode[Symbol, Last[StringSplit[#[[2]], "`"]], <||>])& /@ params;
    ,
    !MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
      rules = {}
    ,
    True,
      params = children[[1, 2]];
      params = # /. {
        sym:LeafNode[Symbol, _, _] :> sym,
        _ :> Nothing
      }& /@ params;
      params = Cases[params, LeafNode[Symbol, name_ /; StringContainsQ[name, "`"], _]];
      rules = (LeafNode[Symbol, #[[2]], _] -> LeafNode[Symbol, Last[StringSplit[#[[2]], "`"]], <||>])& /@ params;
  ];

  CallNode[{head, headSeq},
    GroupNode[GroupSquare, {
      opener, openerSeq,
      InfixNode[Comma, cleanLexicalVariables /@ (commaChildren /. rules), data2],
      closerSeq, closer
    }, data1]
  , data]
]]


cleanLexicalVariables[BinaryNode[SetDelayed, {
  lhs_,
  seq1___,
  op:LeafNode[Token`ColonEqual, _, _],
  seq2___,
  rhs_ }, data_]] :=
Catch[
Module[{potential, params, rules},

  Which[
    True,
      potential = Cases[lhs, CompoundNode[PatternBlank, {LeafNode[Symbol, _, _], _}, _], Infinity];
      params = # /. {
        CompoundNode[PatternBlank, {
          sym:LeafNode[Symbol, _, _], _}, _] :> sym
      }& /@ potential;
      params = Cases[params, LeafNode[Symbol, name_ /; StringContainsQ[name, "`"], _]];
      rules = (LeafNode[Symbol, #[[2]], _] -> LeafNode[Symbol, Last[StringSplit[#[[2]], "`"]], <||>])& /@ params;
  ];
  BinaryNode[SetDelayed, cleanLexicalVariables /@ {
    lhs /. rules,
    seq1,
    op,
    seq2,
    rhs /. rules
  }, data]
]]


cleanLexicalVariables[BinaryNode[RuleDelayed, {
  lhs_,
  seq1___,
  op:LeafNode[Token`ColonGreater | Token`LongName`RuleDelayed, _, _],
  seq2___,
  rhs_ }, data_]] :=
Catch[
Module[{potential, params, rules},

  Which[
    True,
      potential = Cases[lhs, CompoundNode[PatternBlank, {LeafNode[Symbol, _, _], _}, _], Infinity];
      params = # /. {
        CompoundNode[PatternBlank, {
          sym:LeafNode[Symbol, _, _], _}, _] :> sym
      }& /@ potential;
      params = Cases[params, LeafNode[Symbol, name_ /; StringContainsQ[name, "`"], _]];
      rules = (LeafNode[Symbol, #[[2]], _] -> LeafNode[Symbol, Last[StringSplit[#[[2]], "`"]], <||>])& /@ params;
  ];
  
  BinaryNode[RuleDelayed, cleanLexicalVariables /@ {
    lhs /. rules,
    seq1,
    op,
    seq2,
    rhs /. rules
  }, data]
]]


(*
Convert CompoundNode[tag, {child1, child2}] into LeafNode[tag, child1child2]
*)

abstractFormatNodes[CompoundNode[tag_, children_, data_]] :=
  LeafNode[tag, StringJoin[abstractFormatNodes[#][[2]]& /@ children], data]


(*
Abstract the outer [] and inner [] groups into a single [[]] group

Do not worry about mis-labeling bad syntax such as a[[[[]]]]
*)
abstractFormatNodes[
  GroupNode[GroupSquare, {
    LeafNode[Token`OpenSquare, str6_, _],
    GroupNode[GroupSquare, {
      LeafNode[Token`OpenSquare, str5_, _],
      contentsSeq___,
      LeafNode[Token`CloseSquare, str4_, _]},
      _
    ],
    LeafNode[Token`CloseSquare, str2_, _]},
    data1_
  ]
] :=
  GroupNode[GroupSquareSquare, {
    LeafNode[Token`OpenSquareOpenSquare, str6 <> str5, <||>],
    Sequence @@ (abstractFormatNodes /@ {contentsSeq}),
    LeafNode[Token`CloseSquareCloseSquare, str4 <> str2, <||>]}, data1
  ]


abstractFormatNodes[InfixNode[CompoundExpression, children_, data_]] :=
Module[{},
  If[MatchQ[children, {___, LeafNode[Token`Semi, _, _], LeafNode[Token`Fake`ImplicitNull, _, _]}],
    (*
    rename the ; before implicit null for easier processing later
    *)
    InfixNode[CompoundExpression, abstractFormatNodes /@ (Drop[children, -2] ~Join~ {LeafNode[Token`Fake`SemiBeforeImplicitNull, children[[-2, 2]], children[[-2, 3]]], children[[-1]]}), data]
    ,
    InfixNode[CompoundExpression, abstractFormatNodes /@ children, data]
  ]
]


(*
Abstract binary  (a // b) // c  into  infix  a // b // c
*)
abstractFormatNodes[n:BinaryNode[BinarySlashSlash, {BinaryNode[BinarySlashSlash, _, _], _, _}, _]] :=
Module[{flattenedChildren},

  flattenedChildren = List @@ Flatten[flattenBinarySlashSlash[n], Infinity, binarySlashSlashHead];

  abstractFormatNodes[InfixNode[InfixSlashSlash, flattenedChildren, <||>]]
]


flattenBinarySlashSlash[BinaryNode[BinarySlashSlash, {rand1_, rator_, rand2_}, _]] :=
  binarySlashSlashHead[flattenBinarySlashSlash[rand1], rator, rand2]

flattenBinarySlashSlash[n_] := n


abstractFormatNodes[node_LeafNode] :=
  node

abstractFormatNodes[CallNode[head_, t_, data_]] :=
  CallNode[abstractFormatNodes /@ head, abstractFormatNodes[t], data]

abstractFormatNodes[BoxNode[RowBox, {ts_}, data_]] :=
  BoxNode[RowBox, {abstractFormatNodes /@ ts}, data]

abstractFormatNodes[node:CodeNode[_, _, _]] :=
  node

abstractFormatNodes[m_?MissingQ] :=
  m

abstractFormatNodes[f_?FailureQ] :=
  f

(*
Bad args such as List[1, 2, 3] used to match this, so changed data_ => data_Association to try to reduce bad hits
*)
abstractFormatNodes[head_[tag_, ts_, data_Association]] :=
  head[tag, abstractFormatNodes /@ ts, data]

abstractFormatNodes[args___] :=
  Failure["Unhandled", <| "Function" -> abstractFormatNodes, "Arguments" -> HoldForm[{args}] |>]


cleanLexicalVariables[node_LeafNode] :=
  node

(*
maybe being called in formatter pass sequence, where whitespace has been removed and heads still have list
*)
cleanLexicalVariables[CallNode[{head_, headSeq___}, t_, data_]] :=
  CallNode[{cleanLexicalVariables[head], headSeq}, cleanLexicalVariables[t], data]

(*
maybe being called in sanity check, where proper aggregation has happened, and head is a node
*)
cleanLexicalVariables[CallNode[head_, t_, data_]] :=
  CallNode[cleanLexicalVariables[head], cleanLexicalVariables[t], data]

cleanLexicalVariables[BoxNode[RowBox, {ts_}, data_]] :=
  BoxNode[RowBox, {cleanLexicalVariables /@ ts}, data]

cleanLexicalVariables[node:CodeNode[_, _, _]] :=
  node

cleanLexicalVariables[m_?MissingQ] :=
  m

(*
Bad args such as List[1, 2, 3] used to match this, so changed data_ => data_Association to try to reduce bad hits
*)
cleanLexicalVariables[head_[tag_, ts_, data_Association]] :=
  head[tag, cleanLexicalVariables /@ ts, data]

cleanLexicalVariables[args___] :=
  Failure["Unhandled", <| "Function" -> cleanLexicalVariables, "Arguments" -> HoldForm[{args}] |>]


sameDefinitionSymbols[lhs1_, lhs2_] :=
  Function[{syms1, syms2},
    !FailureQ[syms1] &&
    !FailureQ[syms2] &&
    (#[[2]]& /@ syms1) === (#[[2]]& /@ syms2)
  ][
    (* must do concrete -> abstract before calling DefinitionSymbols *)
    DefinitionSymbols[CodeParser`Abstract`Abstract @ CodeParser`Abstract`Aggregate @ lhs1],
    DefinitionSymbols[CodeParser`Abstract`Abstract @ CodeParser`Abstract`Aggregate @ lhs2]
  ]


End[]

EndPackage[]
