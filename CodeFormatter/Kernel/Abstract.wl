BeginPackage["CodeFormatter`Abstract`"]

AbstractFormatNodes

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
AbstractFormatNodes[gst_] :=
Catch[
  abstractFormatNodes[gst]
  ,
  abstractFormatNodesTag
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
abstractFormatNodes[CallNode[{head:LeafNode[Symbol, "Switch", _], headSeq___}, {
  GroupNode[GroupSquare, {
      opener_, openerSeq:comment...,
      InfixNode[Comma, {expr:Except[LeafNode[Token`Comma, _, _]], exprSeq:Except[LeafNode[Token`Comma, _, _]]..., rest___}, data2_],
      closerSeq:comment..., closer_}, data1_]
    }, data_]] :=
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

  CallNode[{head, headSeq}, {
    GroupNode[GroupSquare, {
      opener, openerSeq,
      InfixNode[Comma, abstractFormatNodes /@ ({expr, exprSeq} ~Join~ replaced), data2],
      closerSeq, closer
    }, data1]
  }, data]
]

(*
wrap ClauseNode around clauses
*)
abstractFormatNodes[CallNode[{head:LeafNode[Symbol, "Which", _], headSeq___}, {
  GroupNode[GroupSquare, {
      opener_, openerSeq:comment...,
      InfixNode[Comma, commaChildren_, data2_],
      closerSeq:comment..., closer_}, data1_]
    }, data_]] :=
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

  CallNode[{head, headSeq}, {
    GroupNode[GroupSquare, {
      opener, openerSeq,
      InfixNode[Comma, abstractFormatNodes /@ replaced, data2],
      closerSeq, closer
    }, data1]
  }, data]
]



(*
Convert CompoundNode[tag, {child1, child2}] into LeafNode[tag, child1child2]
*)

abstractFormatNodes[CompoundNode[tag_, children_, data_]] :=
  LeafNode[tag, StringJoin[abstractFormatNodes[#][[2]]& /@ children], data]


abstractFormatNodes[node_LeafNode] :=
  node

abstractFormatNodes[CallNode[head_, ts_, data_]] :=
  CallNode[abstractFormatNodes /@ head, abstractFormatNodes /@ ts, data]

abstractFormatNodes[BoxNode[RowBox, {ts_}, data_]] :=
  BoxNode[RowBox, {abstractFormatNodes /@ ts}, data]

abstractFormatNodes[node:CodeNode[_, _, _]] :=
  node

(*
Bad args such as List[1, 2, 3] used to match this, so changed data_ => data_Association to try to reduce bad hits
*)
abstractFormatNodes[head_[tag_, ts_, data_Association]] :=
  head[tag, abstractFormatNodes /@ ts, data]

abstractFormatNodes[args___] :=
  Throw[Failure["InternalUnhandled", <| "Function" -> abstractFormatNodes, "Args" -> {args} |>], abstractFormatNodesTag]


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
