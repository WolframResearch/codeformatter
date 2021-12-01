(* ::Package::"Tags"-><|"NoParameters" -> <|"With" -> <|Enabled -> False|>|>, "NoVariables" -> <|"Module" -> <|Enabled -> False|>|>, "SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

BeginPackage["CodeFormatter`Indent`"]

IndentCST

Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
Would like to use symbol Indent, but Indent is an undocumented System` symbol
*)

Options[IndentCST] = {
  "LineWidth" :> $DefaultLineWidth,
  "BreakLinesMethod" :> $DefaultBreakLinesMethod
}

IndentCST[gst_, OptionsPattern[]] :=
Module[{indented, lineWidth, breakLinesMethod},

  If[$Debug,
    Print["Inside IndentCST"];
  ];

  lineWidth = OptionValue["LineWidth"];
  breakLinesMethod = OptionValue["BreakLinesMethod"];

  Internal`InheritedBlock[{$CurrentStyle},

    (*
    Only use provided lineWidth in indent[] if using LineBreakerV2

    allow mixing both LineBreakerV1 and LineBreakerV2
    *)
    If[StringContainsQ[breakLinesMethod, "LineBreakerV2"],
      $CurrentStyle["LineWidth"] = lineWidth
      ,
      $CurrentStyle["LineWidth"] = Infinity
    ];

    (*
    indented has symbolic expressions:
    IndentationNode[]
    line[]
    *)
    indented = indent[gst];
  ];

  If[$Debug,
    Print["before symbolic render: ", indented];
  ];
  
  (*
  Now render the symbolic values down to actual values
  *)
  Block[{IndentationNode, line},

    (*
    Make IndentationNode HoldAll so that we can achieve top-down evaluation
    *)
    Attributes[IndentationNode] = {HoldAll};

    IndentationNode[Block, childrenIn_, data_] :=
    Module[{children, blocked},

      Internal`InheritedBlock[{$CurrentIndentationLevelNodeList = $CurrentIndentationLevelNodeList ~Join~ $CurrentIndentationNodeList},

        children = childrenIn;

        blocked =
          {line[]} ~Join~
          children
      ];
      
      If[Lookup[data, "Toplevel", False],
        (*
        do not add trailing newline if top-level
        *)
        Sequence @@ blocked
        ,
        Sequence @@ (
          blocked ~Join~
          {line[]}
        )
      ]
    ];
    
    IndentationNode[Increment, children_, _] :=
    Module[{blocked},

      Internal`InheritedBlock[{$CurrentIndentationLevelNodeList = $CurrentIndentationLevelNodeList ~Join~ $CurrentIndentationNodeList},
        blocked = children
      ];

      Sequence @@ blocked
    ];
    
    line[] :=
      Sequence @@ ({LeafNode[Token`Newline, $CurrentNewlineString, <| "Extent" -> {0, 2, 0, 0} |>]} ~Join~ $CurrentIndentationLevelNodeList);
    
    (* :!CodeAnalysis::BeginBlock:: *)
    (* :!CodeAnalysis::Disable::SelfAssignment:: *)
    indented = indented;
    (* :!CodeAnalysis::EndBlock:: *)

    Null
  ];

  If[$Debug,
    Print["after symbolic render: ", indented];
  ];

  indented
]


(* line[] :=
  {LeafNode[Token`Newline, $CurrentNewlineString, <| "Extent" -> {0, 2, 0, 0} |>], $CurrentIndentationLevelNodeList}
*)

space[] =
  LeafNode[Whitespace, " ", <| "Extent" -> {1, 1, 1, 1} |>]


(*
Replace all newline leafs with the sequence: newline + indentation
*)
(*
needs to be s_ /; ... because we do not want to use the value of $CurrentNewlineString at definition time
*)
(* increment[nl:LeafNode[Token`Newline, s_ /; s == $CurrentNewlineString, _]] :=
  Sequence @@ ({nl} ~Join~ $CurrentIndentationNodeList) *)

(*
Make sure to handle the cases of newlines inside of multiline comments
*)
(*
needs to be s_ /; ... because we do not want to use the value of $CurrentNewlineString at definition time
*)
(* increment[c:FragmentNode[Token`Comment, s_ /; s == "\\" <> $CurrentNewlineString, _]] :=
  Sequence @@ ({c} ~Join~ $CurrentIndentationCommentFragmentNodeList) *)

(*
needs to be s_ /; ... because we do not want to use the value of $CurrentNewlineString at definition time
*)
(* increment[nl:FragmentNode[Token`Comment, s_ /; s == $CurrentNewlineString, _]] :=
  Sequence @@ ({nl} ~Join~ $CurrentIndentationCommentFragmentNodeList) *)

(* increment[CallNode[headIn_, childrenIn_, data_]] :=
Module[{head, children,
  extent},

  If[$Debug,
    Print["calling increment: ", {CallNode, headIn}];
  ];

  head = headIn;

  children = childrenIn;

  head = Flatten[{increment /@ head}];

  children = Flatten[{increment /@ children}];

  extent = computeExtent[head ~Join~ children];

  CallNode[head, children, <| data, "Extent" -> extent |>]
] *)

(* increment[n:LeafNode[_, _, _]] :=
Module[{},
  n
] *)

(* increment[type_[tag_, childrenIn_, data_]] :=
Module[{children,
  extent},

  If[$Debug,
    Print["calling increment: ", {type, tag}];
  ];

  children = childrenIn;

  children = Flatten[{increment /@ children}];

  extent = computeExtent[children];

  type[tag, children, <| data, "Extent" -> extent |>]
] *)


(*
short-hand for IndentationNode[Increment, ...]
*)
increment[n_] :=
Catch[
Module[{multiline, children,
  extent},

  If[$Debug,
    Print["inside increment"];
  ];

  multiline = Lookup[n[[3]], "Multiline", False];

  children = n[[2]];
  
  (*
  ignore pre-computed extent of n
  recompute extent of n with newlines properly indented
  *)
  
  extent = computeIndentedExtent[children];

  IndentationNode[Increment, {n}, <| "Multiline" -> multiline, "Extent" -> extent |>]
]]


(*
short-hand for IndentationNode[Block, ...]
*)
block[childrenIn_List] :=
Module[{children, len,
  extent},

  children = childrenIn;

  If[$Debug,
    Print["inside block"];
    Print["children: ", children];
    Print["$Toplevel: ", $Toplevel];
  ];

  (*
  There may be leading whitespace from breaking after an operator such as :=, so make sure to remove
  *)
  len = LengthWhile[children, MatchQ[#, ws]&];
  children = Drop[children, len];

  extent = computeExtent[children];

  If[$Toplevel,
    (*
    do not add trailing newline if top-level
    *)
    extent = {extent[[1]] + $CurrentIndentationStringLength, extent[[2]] + 1, 0, extent[[4]] + $CurrentIndentationStringLength}
    ,
    extent = {extent[[1]] + $CurrentIndentationStringLength, extent[[2]] + 2, 0, 0}
  ];

  IndentationNode[Block, children, <| "Multiline" -> True, "Toplevel" -> $Toplevel, "Extent" -> extent |>]
]

blockAfterFirstRator[type_[tag_, graphs_, data_]] :=
Catch[
Module[{aggs, rators, firstRatorPos, first, rest, children,
  extent},

  If[$Debug,
    Print["inside blockAfterFirstRator"];
    Print["graphs: ", graphs]
  ];

  (*
  blockAfterFirstRator is called after ws has been reintroduced, so must check for both comments AND ws
  *)
  aggs = DeleteCases[graphs, trivia];

  rators = aggs[[2;;;;2]];

  firstRatorPos = Position[graphs, rators[[1]]][[1]];

  {first, rest} = TakeDrop[graphs, firstRatorPos[[1]]];

  children = first ~Join~ {block[rest]};

  extent = computeExtent[children];

  type[tag, children, <| data, "Extent" -> extent |>]
]]

blockAfterLastRator[type_[tag_, graphs_, data_]] :=
Catch[
Module[{aggs, rators, lastRatorPos, first, rest, children, extent},

  If[$Debug,
    Print["inside blockAfterLastRator"];
    Print["graphs: ", graphs]
  ];

  (*
  blockAfterLastRator is called after ws has been reintroduced, so must check for both comments AND ws
  *)
  aggs = DeleteCases[graphs, trivia];

  rators = aggs[[2;;;;2]];

  lastRatorPos = Position[graphs, rators[[-1]]][[1]];

  {first, rest} = TakeDrop[graphs, lastRatorPos[[1]]];

  children = first ~Join~ {block[rest]};

  extent = computeExtent[children];

  type[tag, children, <| data, "Extent" -> extent |>]
]]



Options[indent] = {
  "NewlinesBetweenCommas" -> Automatic,
  "NewlinesBetweenSemicolons" -> Automatic,
  "NewlinesBetweenOperators" -> Automatic,
  "NewlinesInControl" -> Automatic,
  "NewlinesInGroups" -> Automatic,
  "NewlinesInScoping" -> Automatic
}



(* indent[LeafNode[Token`Newline, _, _], level_] :=
  line[level] *)

(*
Special case multiline comments

It is ok to change the internal indentation of comments, as long as everything is still aligned
*)
indent[LeafNode[Token`Comment, fsIn:{
  (*
  needs to be s_ /; ... because we do not want to use the value of $CurrentNewlineString at definition time
  *)
  FragmentNode[Token`Comment, s_ /; s == $CurrentLineContinuationString, _],
  ___,
  FragmentNode[Token`Comment, "(*", _],
  ___,
  FragmentNode[Token`Comment, "*)", _]}, data_], OptionsPattern[]] :=
Module[{fs, min, replaced, origSpaces, strs, minStr, indentStr, frags, inserted, split, fragGroups, nlGroups, firstStrs, replacedStrs, replacedFirstStrs,
  replacedOrigSpaces, level,
  children,
  extent},

  If[$Debug,
    Print["calling indent multiline Comment"];
  ];

  fs = fsIn;

  fs = indent /@ fs;

  origSpaces = Count[fs, FragmentNode[Token`Comment, " ", KeyValuePattern["Temporary" -> True]]];

  If[$Debug,
    Print["origSpaces: ", origSpaces //InputForm];
  ];

  (*
  There is also the line continuation that was inserted
  *)
  inserted = origSpaces + 1;

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

  (*
  inserted + 1 is the opener ( *
  *)
  split = Split[fs[[inserted + 1;;]], (matchCommentFragmentNewlineQ[#1] == matchCommentFragmentNewlineQ[#2])&];
  fragGroups = split[[1;;-1;;2]];
  nlGroups = split[[2;;-2;;2]];

  strs = fragGroups[[All, All, 2]];

  firstStrs = strs[[All, 1]];

  If[$Debug,
    Print["fragGroups: ", fragGroups];
    Print["nlGroups: ", nlGroups];
    Print["strs: ", strs];
    Print["firstStrs: ", firstStrs];
  ];


  min = Min[
    (*
    do not count the first group that starts with ( *, this is already being counted by inserted
    *)
    StringCases[firstStrs[[2;;]],
      StartOfString ~~ ws:(" "...) ~~ Except[" "] :> StringLength[ws]]
    ,
    origSpaces
  ];

  If[$Debug,
    Print["min: ", min];
  ];

  level = 0;

  minStr = StringJoin[Table[" ", min]];
  indentStr = StringJoin[Table[$CurrentIndentationString, level]];
  
  replacedFirstStrs = StringReplace[firstStrs[[2;;]], StartOfString ~~ minStr :> indentStr];

  replacedStrs = MapThread[({#1} ~Join~ Rest[#2])&, {replacedFirstStrs, strs[[2;;]]}];

  replacedOrigSpaces =
    Join[
      Drop[fs[[2;;inserted]], -min]
      ,
      Flatten[Table[indent[FragmentNode[Token`Comment, #, <||>]], level]& /@ Characters[$CurrentIndentationString]]
    ];

  If[$Debug,
    Print["minStr: ", minStr //InputForm];
    Print["indentStr: ", indentStr //InputForm];
    Print["replacedOrigSpaces: ", replacedOrigSpaces //InputForm];
    Print["replacedStrs: ", replacedStrs //InputForm];
  ];

  If[$CurrentStyle["NewlinesInComments"] === Delete,

    children =
      Flatten[
        replacedOrigSpaces ~Join~
        {fragGroups[[1]]} ~Join~
        (Function[{replacedStr}, indent[FragmentNode[Token`Comment, #, <||>]]& /@ replacedStr] /@ replacedStrs)
      ];

    extent = computeExtent[children];

    LeafNode[
      Token`Comment
      ,
      children
      ,
      <| data, "Extent" -> extent |>
    ]
    ,

    children =
      Flatten[
        {indent[fs[[1]]]} ~Join~
        replacedOrigSpaces ~Join~
        betterRiffle[{fragGroups[[1]]} ~Join~ (Function[{replacedStr}, indent[FragmentNode[Token`Comment, #, <||>]]& /@ replacedStr] /@ replacedStrs), nlGroups]
      ];
    
    extent = computeExtent[children];

    LeafNode[
      Token`Comment
      ,
      children
      ,
      <| data, "Multiline" -> True, "Extent" -> extent |>
    ]
  ]
]


indent[LeafNode[Token`Comment, str_String, data_], OptionsPattern[]] :=
With[{len = StringLength[str]},

  If[$Debug,
    Print["indent comment"];
  ];

  LeafNode[Token`Comment, str, <| data, "Extent" -> {len, 1, len, len} |>]
]

(*
newlines may be introduced and result in:

f[args___] :=
    $Failed /; (g[])
;

so keep track of top-level ; and absorb newlines later as needed

*)
indent[LeafNode[Token`Semi, str_String, data_], OptionsPattern[]] :=
With[{},
  LeafNode[Token`Semi, str, <| data, "Toplevel" -> $Toplevel, "Extent" -> {1, 1, 1, 1} |>]
]

(*
All other leafs:
  integers,
  reals,
  symbols,
  singleline strings,
  singleline comments,
  multiline string from FE,
  multiline comments from FE, etc.
*)
indent[LeafNode[tag_, str_String, data_], OptionsPattern[]] :=
With[{len = StringLength[str]},
  LeafNode[tag, str, <| data, "Extent" -> {len, 1, len, len} |>]
]

indent[LeafNode[tag_, frags_, data_], OptionsPattern[]] :=
With[
  {children = indent /@ frags}
  ,
  {extent = computeExtent[children]}
  ,
  LeafNode[tag, children, <| data, "Extent" -> extent |>]
]

(*
Temporary line continuation, do not give a real extent, just use identity
*)
(*
needs to be s_ /; ... because we do not want to use the value of $CurrentNewlineString at definition time
*)
indent[FragmentNode[Token`Comment, s_ /; s == $CurrentLineContinuationString, data_]] :=
  FragmentNode[Token`Comment, s, <| data, "Extent" -> identityExtent |>]

indent[FragmentNode[Token`Comment, s_ /; s == $CurrentNewlineString, data_]] :=
  FragmentNode[Token`Comment, s, <| data, "Extent" -> {0, 2, 0, 0} |>]

indent[FragmentNode[String, s_ /; s == $CurrentLineContinuationString, data_]] :=
  FragmentNode[String, s, <| data, "Extent" -> identityExtent |>]

indent[FragmentNode[String, s_ /; s == $CurrentNewlineString, data_]] :=
  FragmentNode[String, s, <| data, "Extent" -> {0, 2, 0, 0} |>]

indent[FragmentNode[tag_, str_String, data_], OptionsPattern[]] :=
With[{len = StringLength[str]},
  FragmentNode[tag, str, <| data, "Extent" -> {len, 1, len, len} |>]
]

indent[ErrorNode[tag_, str_String, data_], OptionsPattern[]] :=
With[{len = StringLength[str]},
  ErrorNode[tag, str, <| data, "Extent" -> {len, 1, len, len} |>]
]


(*
"FrontEnd"-style:
indentPostfixRator[Function][rator_, level_] :=
  {space[], indent[rator, level]}
*)

(* indentPostfixRator[_][rator_, level_] :=
  indent[rator, level] *)

indent[PrefixNode[tag_, {rator_, randSeq:comment..., rand_}, data_], OptionsPattern[]] :=
Module[{indentedRator, indentedRandSeq, indentedRand, indentedRandMultiline,
  children,
  extent},

  indentedRator = indent[rator];
  indentedRandSeq = indent /@ {randSeq};
  indentedRand = indent[rand];

  indentedRandMultiline = Lookup[indentedRand[[3]], "Multiline", False];

  If[$Debug,
    Print["enter Prefix"];
    Print["rand: ", indentedRand];
  ];

  children =
    Flatten[{
      indentedRator,
      indentedRandSeq,
      indentedRand
    }];

  extent = computeExtent[children];

  PrefixNode[tag,
    children
    ,
    <| data, "Multiline" -> indentedRandMultiline, "Extent" -> extent |>
  ]
]

indent[PostfixNode[tag_, {rand_, randSeq:comment..., rator_}, data_], OptionsPattern[]] :=
Module[{indentedRand, indentedRandSeq, indentedRator, indentedRandMultiline,
  children,
  extent},

  indentedRand = indent[rand];
  indentedRandSeq = indent /@ {randSeq};
  indentedRator = indent[rator];

  indentedRandMultiline = Lookup[indentedRand[[3]], "Multiline", False];

  If[$Debug,
    Print["enter Postfix"];
    Print["rand: ", indentedRand];
  ];

  children =
    Flatten[{
      indentedRand,
      indentedRandSeq,
      indentedRator
    }];

  extent = computeExtent[children];

  PostfixNode[tag,
    children
    ,
    <| data, "Multiline" -> indentedRandMultiline, "Extent" -> extent |>
  ]
]


indent[PrefixBinaryNode[tag_, {rator_, rand1Seq:comment..., rand1_, rand2Seq:comment..., rand2_}, data_], OptionsPattern[]] :=
Module[{indentedRator, indentedRand1Seq, indentedRand1, indentedRand2Seq, indentedRand2, indentedRandMultiline,
  children,
  extent},

  indentedRator = indent[rator];
  indentedRand1Seq = indent /@ {rand1Seq};
  indentedRand1 = indent[rand1];
  indentedRand2Seq = indent /@ {rand2Seq};
  indentedRand2 = indent[rand2];

  indentedRandMultiline = Lookup[indentedRand1[[3]], "Multiline", False] || Lookup[indentedRand2[[3]], "Multiline", False];

  If[$Debug,
    Print["enter PrefixBinary"];
    Print["rand1: ", indentedRand1];
    Print["rand2: ", indentedRand2];
  ];

  children =
    Flatten[{
      indentedRator,
      indentedRand1Seq,
      indentedRand1,
      indentedRand2Seq,
      indentedRand2
    }];
  
  extent = computeExtent[children];
  
  PrefixBinaryNode[tag,
    children
    ,
    <| data, "Multiline" -> indentedRandMultiline, "Extent" -> extent |>
  ]
]


(*
special casing semicolons:

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
indent[InfixNode[CompoundExpression, graphs_, data_], OptionsPattern[]] :=
Catch[
Module[{ratorsPat, definitelyDelete, definitelyInsert, definitelyAutomatic, split, indentedGraphs, anyIndentedGraphsMultiline},

  If[$Debug,
    Print["indent CompoundExpression"];
  ];

  indentedGraphs = indent /@ graphs;

  anyIndentedGraphsMultiline = AnyTrue[indentedGraphs, Lookup[#[[3]], "Multiline", False]&];

  ratorsPat = LeafNode[Token`Semi | Token`Fake`SemiBeforeImplicitNull, _, _];

  (*
  Override settings and always definitely delete newlines for CompoundExpression at top-level

  If we are at top-level, then we know there must not be any newlines, other-wise this would not be a single CompoundExpression
  e.g.:
  
  a;
  b

  at top-level is 2 different expressions.

  Breaking up lines or gluing lines together may change the actual CompoundExpressions and
  how expressions are parsed
  *)
  Which[
    TrueQ[$Toplevel],
      definitelyDelete = True;
    ,
    anyIndentedGraphsMultiline,
      definitelyInsert = True;
  ];

  If[!TrueQ[(definitelyDelete || definitelyInsert)],
    definitelyDelete = OptionValue["NewlinesBetweenSemicolons"] === Delete || $CurrentStyle["NewlinesBetweenSemicolons"] === Delete;
    definitelyInsert = OptionValue["NewlinesBetweenSemicolons"] === Insert || $CurrentStyle["NewlinesBetweenSemicolons"] === Insert;
  ];

  If[!TrueQ[(definitelyDelete || definitelyInsert)],
    definitelyAutomatic = True
  ];

  If[$Debug,
    Print["NewlinesBetweenSemicolons choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}];
  ];

  Which[
    TrueQ[definitelyInsert],
      (*
      special case the implicit Null at the end:
      a;implicit Null
      leave it on same line as ;
      *)
      split = Split[indentedGraphs, MatchQ[#2, LeafNode[Token`Semi | Token`Fake`SemiBeforeImplicitNull | Token`Fake`ImplicitNull, _, _]]&];
    ,
    TrueQ[definitelyDelete],
      split = {indentedGraphs};
    ,
    TrueQ[definitelyAutomatic],
      (*
      special case the implicit Null at the end:
      a;implicit Null
      leave it on same line as ;
      *)
      split = Split[indentedGraphs, MatchQ[#2, LeafNode[Token`Semi | Token`Fake`SemiBeforeImplicitNull | Token`Fake`ImplicitNull, _, _]]&];
  ];

  Which[
    TrueQ[definitelyInsert],
      baseOperatorNodeIndent[InfixNode, CompoundExpression, data, split, ratorsPat, anyIndentedGraphsMultiline, False]
    ,
    TrueQ[definitelyDelete],
      baseOperatorNodeIndent[InfixNode, CompoundExpression, data, split, ratorsPat, anyIndentedGraphsMultiline, False]
    ,
    TrueQ[definitelyAutomatic],
      (*
      no special line-breaking behavior here

      NewlinesBetweenSemicolons -> Automatic is same as NewlinesBetweenSemicolons -> Insert, so there is nothing to do
      *)
      baseOperatorNodeIndent[InfixNode, CompoundExpression, data, split, ratorsPat, anyIndentedGraphsMultiline, False]
  ]
]]


(*
special casing commas:

Automatic behavior for Comma:

if fewer than 10 elements (not counting Token`Comma), then just do Delete

else:
format with commas on same line as preceding element:

a,
b,
c,
d,
e

*)

indent[node:InfixNode[Comma, graphs_, data_], OptionsPattern[]] :=
  Catch[
  Module[{aggs, ratorsPat, split, definitelyDelete, definitelyInsert, definitelyAutomatic,
    indentedGraphs, anyIndentedGraphsMultiline, res, extent},

    If[$Debug,
      Print["enter comma"];
    ];

    ratorsPat = LeafNode[Token`Comma, _, _];

    indentedGraphs = indent /@ graphs;

    aggs = DeleteCases[indentedGraphs, comment];

    anyIndentedGraphsMultiline = AnyTrue[indentedGraphs, Lookup[#[[3]], "Multiline", False]&];

    If[$Debug,
      Print["comma anyIndentedGraphsMultiline: ", anyIndentedGraphsMultiline];
    ];

    If[anyIndentedGraphsMultiline,
      definitelyInsert = True;
    ];

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyDelete = OptionValue["NewlinesBetweenCommas"] === Delete || $CurrentStyle["NewlinesBetweenCommas"] === Delete;
      definitelyInsert = OptionValue["NewlinesBetweenCommas"] === Insert || $CurrentStyle["NewlinesBetweenCommas"] === Insert;
    ];

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True
    ];

    If[$Debug,
      Print["NewlinesBetweenCommas choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}];
    ];

    Which[
      TrueQ[definitelyInsert],
        split = {#}& /@ indentedGraphs;
      ,
      TrueQ[definitelyDelete],
        split = {indentedGraphs};
      ,
      TrueQ[definitelyAutomatic],
        split = {indentedGraphs};
    ];
    
    If[$Debug,
      Print["Comma split: ", split];
    ];

    Which[
      TrueQ[definitelyInsert],
        baseOperatorNodeIndent[InfixNode, Comma, data, split, ratorsPat, anyIndentedGraphsMultiline, False]
      ,
      TrueQ[definitelyDelete],
        baseOperatorNodeIndent[InfixNode, Comma, data, split, ratorsPat, anyIndentedGraphsMultiline, False]
      ,
      TrueQ[definitelyAutomatic],
        res = baseOperatorNodeIndent[InfixNode, Comma, data, split, ratorsPat, anyIndentedGraphsMultiline, False];
        extent = res[[3, Key["Extent"]]];
        If[extent[[1]] >= $CurrentStyle["LineWidth"],
          (*
          exceeding LineWidth, so re-indent with "NewlinesBetweenCommas" -> Insert
          *)
          Throw[indent[node, "NewlinesBetweenCommas" -> Insert]]
        ];
        res
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
indentInfixRatorSurroundedByLeafs[Pattern | Optional | PatternTest | MessageName | Power][rator_] :=
  rator

indentInfixRatorSurroundedByLeafs[tag_][rator_] :=
  indentInfixRator[tag][rator]


(*
special case implicit Times
*)
indentInfixRator[Times][l:LeafNode[Token`Fake`ImplicitTimes, _, _]] :=
  l

indentInfixRatorGrouped[Times][l:LeafNode[Token`Fake`ImplicitTimes, _, _]] :=
  l

indentInfixRatorGroupedLast[Times][LeafNode[Token`Fake`ImplicitTimes, _, _]] :=
  {}

indentInfixRatorGroupedOnly[Times][LeafNode[Token`Fake`ImplicitTimes, _, _]] :=
  {}


(*
no space before or after Token`Fake`SemiBeforeImplicitNull
*)
indentInfixRator[Comma | CompoundExpression][rator:LeafNode[Token`Fake`SemiBeforeImplicitNull, _, _]] :=
  rator

(*
no space before commas or semi
*)
indentInfixRator[Comma | CompoundExpression][rator_] :=
  {rator, space[]}

indentInfixRatorGrouped[Comma | CompoundExpression][rator:LeafNode[Token`Fake`SemiBeforeImplicitNull, _, _]] :=
  rator

indentInfixRatorGrouped[Comma | CompoundExpression][rator_] :=
  {rator, space[]}

indentInfixRatorGroupedLast[Comma | CompoundExpression][rator_] :=
  rator


(*
no space after = in Unset:

a =.
*)
indentInfixRator[Unset][rator_] :=
  {space[], rator}

(*
even with non-leafs on LHS, MessageName must always format with no spaces

a[]::b  must format as  a[]::b
*)
indentInfixRator[MessageName][rator_] :=
  rator

indentInfixRator[_][rator_] :=
  {space[], rator, space[]}

indentInfixRatorGrouped[_][rator_] :=
  {space[], rator, space[]}

indentInfixRatorGroupedLast[_][rator_] :=
  {space[], rator}

indentInfixRatorGroupedOnly[_][rator_] :=
  rator


(*
if top-level, then newline after last rator
*)
$SpecialBreakAfterLastRator = {

  (*
  BinaryNode
  *)
  SetDelayed, UpSetDelayed,

  (*
  TernaryNode
  *)
  TagSetDelayed, MemoizedSetDelayed, MemoizedUpSetDelayed,

  (*
  QuaternaryNode
  *)
  MemoizedTagSetDelayed
}


(*

This is the big function for all BinaryNodes, InfixNodes, and TernaryNodes

The logic for all 3 is so similar, it should all be in a single function

*)
indent[node:(type:BinaryNode|InfixNode|TernaryNode|QuaternaryNode)[tag_, graphs_, data_], OptionsPattern[]] :=
  Catch[
  Module[{aggs, rators, ratorsPat, split,
    definitelyDelete, definitelyInsert, definitelyAutomatic, indentedGraphs, anyIndentedGraphsMultiline,
    infixRatorSurroundedByLeafs,
    res, extent},

    If[$Debug,
      Print["inside indent operator"]
    ];

    indentedGraphs = indent /@ graphs;

    anyIndentedGraphsMultiline = AnyTrue[indentedGraphs, Lookup[#[[3]], "Multiline", False]&];

    If[$Debug,
      Print["anyIndentedGraphsMultiline: ", anyIndentedGraphsMultiline]
    ];

    infixRatorSurroundedByLeafs = MatchQ[type, BinaryNode | InfixNode] && MatchQ[graphs, {_LeafNode, _LeafNode, _LeafNode}];

    aggs = DeleteCases[indentedGraphs, comment];

    rators = aggs[[2;;;;2]];

    (*
    TODO: this can be derived directly from tag as a function
    *)
    ratorsPat = LeafNode[Alternatives @@ DeleteDuplicates[#[[1]]& /@ rators], _, _];

    (*
    Override settings and always definitely delete newlines for operators at top-level

    If we are at top-level, then we know there must not be any newlines, other-wise this would not be a single operator
    e.g.:
    
    a
    +
    b

    at top-level is 2 different expressions.

    Breaking up lines or gluing lines together may change the actual operators and
    how expressions are parsed
    *)

    Which[
      TrueQ[$Toplevel],
        definitelyDelete = OptionValue["NewlinesBetweenOperators"] === Delete || $CurrentStyle["NewlinesBetweenOperators"] === Delete;

        If[!TrueQ[(definitelyDelete || definitelyInsert)],
          definitelyAutomatic = True;
        ];
      ,
      True,
        definitelyDelete = OptionValue["NewlinesBetweenOperators"] === Delete || $CurrentStyle["NewlinesBetweenOperators"] === Delete;
        definitelyInsert = OptionValue["NewlinesBetweenOperators"] === Insert || $CurrentStyle["NewlinesBetweenOperators"] === Insert;

        If[!TrueQ[(definitelyDelete || definitelyInsert)],
          definitelyAutomatic = True;
        ];
    ];

    If[$Debug,
      Print["NewlinesBetweenOperators choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}]
    ];

    Which[
      TrueQ[definitelyInsert],
        split = {#}& /@ indentedGraphs;
      ,
      TrueQ[definitelyDelete],
        split = {indentedGraphs};
      ,
      TrueQ[definitelyAutomatic],
        Which[
          (*
          Special hard-coded

          Always line break after last rator, so redo newlines
          *)
          MemberQ[$SpecialBreakAfterLastRator, tag] && $Toplevel,
            (* lastRator = rators[[-1]];
            lastRatorPos = Position[indentedGraphs, lastRator][[1]];
            split = TakeDrop[indentedGraphs, lastRatorPos[[1]]]; *)
            split = {indentedGraphs};
          ,
          True,
            split =
              Split[indentedGraphs,
                  Which[
                    (*
                    Do not split lines if $Toplevel and either side of implicit Times
                    This would change the expression!!

                    So return True if either is implicit Times
                    *)
                    $Toplevel && (MatchQ[#1, LeafNode[Token`Fake`ImplicitTimes, _,_]] || MatchQ[#2, LeafNode[Token`Fake`ImplicitTimes, _,_]]),
                      True
                    ,
                    (*
                    If both are single line, then keep on single line
                    *)
                    !Lookup[#1[[3]], "Multiline", False] && !Lookup[#2[[3]], "Multiline", False],
                      True
                    ,
                    (*
                    If second arg is a rator, then do not split
                    *)
                    MatchQ[#2, ratorsPat],
                      True
                    ,
                    True,
                      False
                  ]&
              ]
        ]
    ];
    
    If[$Debug,
      Print["Operators split: ", split];
    ];

    Which[
      TrueQ[definitelyInsert],
        baseOperatorNodeIndent[type, tag, data, split, ratorsPat, anyIndentedGraphsMultiline, infixRatorSurroundedByLeafs]
      ,
      TrueQ[definitelyDelete],
        baseOperatorNodeIndent[type, tag, data, split, ratorsPat, anyIndentedGraphsMultiline, infixRatorSurroundedByLeafs]
      ,
      TrueQ[definitelyAutomatic],
        Which[
          MemberQ[$SpecialBreakAfterLastRator, tag] && $Toplevel,
            blockAfterLastRator @
              baseOperatorNodeIndent[type, tag, data, split, ratorsPat, anyIndentedGraphsMultiline, infixRatorSurroundedByLeafs]
            (*
            $Toplevel, so do not re-format if exceeding LineWidth, cannot re-indent safely at $Toplevel
            *)
          ,
          $Toplevel,
            increment @
              baseOperatorNodeIndent[type, tag, data, split, ratorsPat, anyIndentedGraphsMultiline, infixRatorSurroundedByLeafs]
            (*
            $Toplevel, so do not re-format if exceeding LineWidth, cannot re-indent safely at $Toplevel
            *)
          ,
          True,
            res = increment @
              baseOperatorNodeIndent[type, tag, data, split, ratorsPat, anyIndentedGraphsMultiline, infixRatorSurroundedByLeafs];
            extent = res[[3, Key["Extent"]]];
            If[extent[[1]] >= $CurrentStyle["LineWidth"],
              (*
              exceeding LineWidth, so re-indent with "NewlinesBetweenOperators" -> Insert
              *)
              Throw[indent[node, "NewlinesBetweenOperators" -> Insert]]
            ];
            res
        ]
    ]
  ]]


baseOperatorNodeIndent[type_, tag_, data_, split_, ratorsPat_, anyIndentedGraphsMultiline_, infixRatorSurroundedByLeafs_] :=
Catch[
Module[{children, grouped,
  extent},

  If[$Debug,
    Print["inside baseOperatorNodeIndent"];
    Print["type: ", type];
    Print["tag: ", tag];
    Print["split: ", split];
    Print["ratorsPat: ", ratorsPat];
    Print["infixRatorSurroundedByLeafs: ", infixRatorSurroundedByLeafs];
  ];

  If[Length[split] == 1,
    (*
    There were no newline tokens
    *)

    grouped = split[[1]];

    children =
      Flatten[
        Replace[grouped, {
          rator:ratorsPat :>
            If[infixRatorSurroundedByLeafs,
              indentInfixRatorSurroundedByLeafs[tag][rator]
              ,
              indentInfixRator[tag][rator]
            ]
        }, {1}]
      ];

    extent = computeExtent[children];

    Throw[
      type[tag,
        children
        ,
        <| data, "Multiline" -> anyIndentedGraphsMultiline, "Extent" -> extent |>
      ]
    ]
  ];

  children =
    Flatten[
    betterRiffle[
      Map[
        (*
        grouped is a list of tokens with no newline tokens
        *)
        Function[{grouped},
          Which[
            Length[grouped] == 0,
            {}
            ,
            (*
            Single token on a line
            *)
            Length[grouped] == 1,
              Replace[grouped, {
                rator:ratorsPat :> indentInfixRatorGroupedOnly[tag][rator]
              }, {1}]
            ,
            (*
            Multiple tokens on a line
            *)
            True,
              {
                Replace[Most[grouped], {
                  rator:ratorsPat :> indentInfixRatorGrouped[tag][rator]
                }, {1}]
                ,
                Replace[{Last[grouped]}, {
                  rator:ratorsPat :> indentInfixRatorGroupedLast[tag][rator]
                }, {1}]
              }
          ]
        ]
        ,
        split
      ]
      ,
      line[]
    ]
    ];

  extent = computeExtent[children];

  type[tag,
    children
    ,
    <| data, "Multiline" -> True, "Extent" -> extent |>
  ]
]]


(*
Empty group
*)
indent[GroupNode[tag_, {
      opener_,
      closer_
    }, data_], OptionsPattern[]] :=
Module[{children,
  extent},

  children =
    {
      indent[opener],
      indent[closer]
    };

  extent = computeExtent[children];

  Block[{$Toplevel = False},
    GroupNode[tag,
      children
      ,
      <| data, "Extent" -> extent |>
    ]
  ]
]

indent[node:GroupNode[tag_, {
      opener_, 
      graphSeq___,
      closer_
    }, data_], OptionsPattern[]] :=
Catch[
  Module[{definitelyDelete, definitelyInsert, definitelyAutomatic,
    indentedOpener, indentedGraphs, indentedCloser,
    anyIndentedGraphsMultiline,
    children,
    extent},
  
  Block[{$Toplevel = False},

    indentedOpener = indent[opener];
    indentedGraphs = indent /@ {graphSeq};
    indentedCloser = indent[closer];

    If[$Debug,
      Print["enter indent Group"];
      Print["graphs to test: ", indentedGraphs];
    ];

    anyIndentedGraphsMultiline = AnyTrue[indentedGraphs, Lookup[#[[3]], "Multiline", False]&];

    If[$Debug,
      Print["anyIndentedGraphsMultiline: ", anyIndentedGraphsMultiline];
    ];

    If[anyIndentedGraphsMultiline,
      definitelyInsert = True;
    ];

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyDelete = OptionValue["NewlinesInGroups"] === Delete || $CurrentStyle["NewlinesInGroups"] === Delete;
      definitelyInsert = OptionValue["NewlinesInGroups"] === Insert || $CurrentStyle["NewlinesInGroups"] === Insert;
    ];

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True
    ];

    If[$Debug,
      Print["NewlinesInGroups choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}];
    ];

    Which[
      TrueQ[definitelyInsert],

        (* children =
          betterRiffle[{indentedOpener} ~Join~ indentedGraphs ~Join~ {indentedCloser}, line[]]; *)
        children =
          Flatten[{
            indentedOpener,
            block @ indentedGraphs,
            indentedCloser
          }];

        extent = computeExtent[children];

        GroupNode[tag,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
      ,
      TrueQ[definitelyDelete],

        children =
          Flatten[{
            indentedOpener,
            indentedGraphs,
            indentedCloser
          }];

        extent = computeExtent[children];

        GroupNode[tag,
          children
          ,
          (*
          may want to do:
          <| data, "Multiline" -> anyIndentedGraphsMultiline |>
          *)
          <| data, "Extent" -> extent |>
        ]
      ,
      TrueQ[definitelyAutomatic],

        children =
          Flatten[{
            indentedOpener,
            indentedGraphs,
            indentedCloser
          }];

        extent = computeExtent[children];

        If[extent[[1]] >= $CurrentStyle["LineWidth"],
          (*
          exceeding LineWidth, so re-indent with "NewlinesInGroups" -> Insert
          *)
          Throw[indent[node, "NewlinesInGroups" -> Insert]]
        ];

        GroupNode[tag,
          children
          ,
          (*
          may want to do:
          <| data, "Multiline" -> anyIndentedGraphsMultiline |>
          *)
          <| data, "Extent" -> extent |>
        ]
    ]
  ]]
]

indent[n:GroupMissingCloserNode[_, _, _], OptionsPattern[]] :=
  n

indent[n:UnterminatedGroupNode[_, _, _], OptionsPattern[]] :=
  n



(*
special casing Module | Block | With | Function:

Module[{x},
  x + 1
]

*)
indent[node:CallNode[head:{LeafNode[Symbol, "Module" | "Block" | "With" | "Function" | {FragmentNode[Symbol, "Module" | "Block" | "With" | "Function", _], ___}, _], ___}, {
      GroupNode[GroupSquare, {
          opener_,
          openerSeq:comment...,
          commaNode:InfixNode[Comma, {
              varsSeq:Except[LeafNode[Token`Comma, _, _]]...,
              comma1:LeafNode[Token`Comma, _, _],
              bodySeq:Except[LeafNode[Token`Comma, _, _]...]
            }
            ,
            data2_
          ],
          closerSeq:comment...,
          closer_
        }
        ,
        data1_
      ]
    }, data_], OptionsPattern[]] :=
Catch[
  Module[{indentedHead, indentedCommaNode,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    commaChildren, groupChildren, children,
    commaExtent, groupExtent, extent},

    definitelyDelete = OptionValue["NewlinesInScoping"] === Delete || $CurrentStyle["NewlinesInScoping"] === Delete;
    definitelyInsert = OptionValue["NewlinesInScoping"] === Insert || $CurrentStyle["NewlinesInScoping"] === Insert;

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True
    ];

    If[$Debug,
      Print["NewlinesInScoping choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}];
    ];

    Which[
      TrueQ[definitelyDelete],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyInsert],

        (*
          NewlinesInScoping -> Insert is equivalent to:
            NewlinesBetweenCommas -> Insert for the Comma node child of the scoping construct
            And also block the Comma node child
        *)

        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          indentedCommaNode = indent[commaNode, "NewlinesBetweenCommas" -> Insert];

          groupChildren =
            Flatten[{
              indent[opener],
              indent[Insert[#, EndOfLine -> True, {3, 1}]]& /@ {openerSeq},
              block @ {
                indentedCommaNode
              },
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          groupExtent = computeExtent[groupChildren];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1, "Extent" -> groupExtent |>
              ]
            };

          extent = computeExtent[indentedHead ~Join~ children];
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
      ,
      TrueQ[definitelyAutomatic],

        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          commaChildren =
            Flatten[{
              indent /@ {varsSeq},
              indent[comma1],
              indent /@ {bodySeq}
            }];

          commaExtent = computeExtent[commaChildren];

          groupChildren =
            Flatten[{
              indent[opener],
              indent /@ {openerSeq},
              blockAfterFirstRator @
                InfixNode[Comma,
                  commaChildren
                  ,
                  <| data2, "Extent" -> commaExtent |>
                ],
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          groupExtent = computeExtent[groupChildren];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1, "Extent" -> groupExtent |>
              ]
            };

          extent = computeExtent[indentedHead ~Join~ children];
        ];

        If[extent[[1]] >= $CurrentStyle["LineWidth"],
          (*
          exceeding LineWidth, so re-indent with "NewlinesInScoping" -> Insert
          *)
          Throw[indent[node, "NewlinesInScoping" -> Insert]]
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
    ]
  ]
]


(*
special casing multi-arg With:

With[
  {a = 1}
  ,
  {b = a + 1}
  ,
  a + b
]

This form is currently undocumented but is nice to support

*)
indent[node:CallNode[head:{LeafNode[Symbol, "With" | {FragmentNode[Symbol, "With", _], ___}, _], ___}, {
      GroupNode[GroupSquare, {
          opener_, 
          openerSeq:comment..., 
          InfixNode[Comma, {
              varsFirstSeq:Except[LeafNode[Token`Comma, _, _]]...,
              comma1:LeafNode[Token`Comma, _, _],
              varsSecondSeq:Except[LeafNode[Token`Comma, _, _]]...,
              comma2:LeafNode[Token`Comma, _, _],
              varsRestSeq:PatternSequence[Except[LeafNode[Token`Comma, _, _]]..., LeafNode[Token`Comma, _, _]]...,
              bodySeq:Except[LeafNode[Token`Comma, _, _]...]
            }
            ,
            data2_
          ],
          closerSeq:comment..., 
          closer_
        }
        ,
        data1_
      ]
    }, data_], OptionsPattern[]] :=
Catch[
  Module[{indentedHead,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    commaChildren, groupChildren, children,
    commaExtent, groupExtent, extent},

    definitelyDelete = OptionValue["NewlinesInScoping"] === Delete || $CurrentStyle["NewlinesInScoping"] === Delete;
    definitelyInsert = OptionValue["NewlinesInScoping"] === Insert || $CurrentStyle["NewlinesInScoping"] === Insert;

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True
    ];

    If[$Debug,
      Print["NewlinesInScoping choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}];
    ];

    Which[
      TrueQ[definitelyDelete],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyInsert] || TrueQ[definitelyAutomatic],

        (*
          Use same rules for NewlinesInScoping -> Insert and NewlinesInScoping -> Automatic
        *)

        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          commaChildren =
            Flatten[betterRiffle[Flatten[indent /@ {varsFirstSeq, comma1, varsSecondSeq, comma2, varsRestSeq, bodySeq}], line[]]];
          
          commaExtent = computeExtent[commaChildren];

          groupChildren =
            Flatten[{
              indent[opener],
              indent[Insert[#, EndOfLine -> True, {3, 1}]]& /@ {openerSeq},
              block @ {
                InfixNode[Comma,
                  commaChildren
                  ,
                  <| data2, "Extent" -> commaExtent |>
                ]
              },
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          groupExtent = computeExtent[groupChildren];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1, "Extent" -> groupExtent |>
              ]
            };
          
          extent = computeExtent[indentedHead ~Join~ children];
        ];

        If[extent[[1]] >= $CurrentStyle["LineWidth"],
          (*
          exceeding LineWidth, so re-indent with "NewlinesInScoping" -> Insert
          *)
          Throw[indent[node, "NewlinesInScoping" -> Insert]]
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
    ]
  ]
]


(*
special casing If | Switch

If[a,
  b
  ,
  c
]

Switch[a,
  b,
    c
  ,
  d,
    e
]

*)
indent[node:CallNode[head:{LeafNode[Symbol, "If" | "Switch" | {FragmentNode[Symbol, "If" | "Switch", _], ___}, _], ___}, {
      GroupNode[GroupSquare, {
          opener_,
          openerSeq:comment...,
          commaNode:InfixNode[Comma, {
              expr:Except[LeafNode[Token`Comma, _, _]],
              exprSeq:Except[LeafNode[Token`Comma, _, _]]...,
              comma1:LeafNode[Token`Comma, _, _],
              rest___
            }
            ,
            data2_
          ], 
          closerSeq:comment...,
          closer_
        }
        ,
        data1_
      ]
    }, data_], OptionsPattern[]] :=
Catch[
  Module[{indentedHead, indentedCommaNode,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    commaChildren, groupChildren, children,
    commaExtent, groupExtent, extent},

    definitelyDelete = OptionValue["NewlinesInControl"] === Delete || $CurrentStyle["NewlinesInControl"] === Delete;
    definitelyInsert = OptionValue["NewlinesInControl"] === Insert || $CurrentStyle["NewlinesInControl"] === Insert;

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True;
    ];

    If[$Debug,
      Print["NewlinesInControl choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}];
    ];

    Which[
      TrueQ[definitelyDelete],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyInsert],

        (*
          NewlinesInControl -> Insert is equivalent to:
            NewlinesBetweenCommas -> Insert for the Comma node child of the control structure
            And also block the Comma node child
        *)

        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          indentedCommaNode = indent[commaNode, "NewlinesBetweenCommas" -> Insert];

          groupChildren =
            Flatten[{
              indent[opener],
              indent[Insert[#, EndOfLine -> True, {3, 1}]]& /@ {openerSeq},
              block @ {
                indentedCommaNode
              },
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          groupExtent = computeExtent[groupChildren];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1, "Extent" -> groupExtent |>
              ]
            };
          
          extent = computeExtent[indentedHead ~Join~ children];
        ];
        
        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
      ,
      TrueQ[definitelyAutomatic],

        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          commaChildren =
            Flatten[{
              indent[expr],
              indent /@ {exprSeq},
              indent[comma1],
              betterRiffle[indent /@ {rest}, line[]]
            }];

          commaExtent = computeExtent[commaChildren];

          groupChildren =
            Flatten[{
              indent[opener],
              indent /@ {openerSeq},
              blockAfterFirstRator @
                InfixNode[Comma,
                  commaChildren
                  ,
                  <| data2, "Extent" -> commaExtent |>
                ],
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          groupExtent = computeExtent[groupChildren];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1, "Extent" -> groupExtent |>
              ]
            };
          
          extent = computeExtent[indentedHead ~Join~ children];
        ];

        If[extent[[1]] >= $CurrentStyle["LineWidth"],
          (*
          exceeding LineWidth, so re-indent with "NewlinesInControl" -> Insert
          *)
          Throw[indent[node, "NewlinesInControl" -> Insert]]
        ];
        
        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
    ]
  ]
]


indent[node:ClauseNode[tag_, {
    test:Except[LeafNode[Token`Comma, _, _]], testSeq:Except[LeafNode[Token`Comma, _, _]]...,
    comma1:LeafNode[Token`Comma, _, _],
    val:Except[LeafNode[Token`Comma, _, _]], valSeq:Except[LeafNode[Token`Comma, _, _]]...
  }, data_], OptionsPattern[]] :=
Catch[
Module[{indentedTest, indentedTestSeq, indentedComma1, indentedVal, indentedValSeq,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    children,
    extent},

  indentedTest = indent[test];
  indentedTestSeq = indent /@ {testSeq};
  indentedComma1 = indent[comma1];
  indentedVal = indent[val];
  indentedValSeq = indent /@ {valSeq};

  definitelyDelete = OptionValue["NewlinesInControl"] === Delete || $CurrentStyle["NewlinesInControl"] === Delete;
  definitelyInsert = OptionValue["NewlinesInControl"] === Insert || $CurrentStyle["NewlinesInControl"] === Insert;

  If[!TrueQ[(definitelyDelete || definitelyInsert)],
    definitelyAutomatic = True;
  ];

  If[$Debug,
    Print["NewlinesInControl choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}];
  ];

  Which[

    TrueQ[definitelyDelete],

      children =
        Flatten[{
          indentedTest, indentedTestSeq,
          indentedComma1, space[],
          indentedVal, indentedValSeq
        }];

      extent = computeExtent[children];

      ClauseNode[
        tag
        ,
        children
        ,
        <| data, "Extent" -> extent |>
      ]
    ,
    TrueQ[definitelyInsert],

      children =
        Flatten[betterRiffle[Flatten[{indentedTest, indentedTestSeq, indentedComma1, indentedVal, indentedValSeq}], line[]]];

      extent = computeExtent[children];

      ClauseNode[
        tag
        ,
        children
        ,
        <| data, "Multiline" -> True, "Extent" -> extent |>
      ]
    ,
    TrueQ[definitelyAutomatic],

      children =
        Flatten[{
          indentedTest, indentedTestSeq, indentedComma1, line[],
          indentedVal, indentedValSeq  
        }];

      extent = computeExtent[children];

      If[extent[[1]] >= $CurrentStyle["LineWidth"],
        (*
        exceeding LineWidth, so re-indent with "NewlinesInControl" -> Insert
        *)
        Throw[indent[node, "NewlinesInControl" -> Insert]]
      ];

      increment @
        ClauseNode[
          tag
          ,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
  ]
]]



(*
special casing Which

Which[
  a,
    b
  ,
  c,
    d
]

*)
indent[node:CallNode[head:{LeafNode[Symbol, "Which" | {FragmentNode[Symbol, "Which", _], ___}, _], ___}, {
      GroupNode[GroupSquare, {
          opener_,
          openerSeq:comment...,
          commaNode:InfixNode[Comma,
            commaChildrenIn_
            ,
            data2_
          ],
          closerSeq:comment...,
          closer_
        }
        ,
        data1_
      ]
    }, data_], OptionsPattern[]] :=
Catch[
  Module[{indentedHead, indentedCommaNode,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    commaChildren, groupChildren, children,
    commaExtent, groupExtent, extent},

    definitelyDelete = OptionValue["NewlinesInControl"] === Delete || $CurrentStyle["NewlinesInControl"] === Delete;
    definitelyInsert = OptionValue["NewlinesInControl"] === Insert || $CurrentStyle["NewlinesInControl"] === Insert;

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True
    ];

    If[$Debug,
      Print["NewlinesInControl choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}];
    ];

    Which[
      TrueQ[definitelyDelete],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyInsert],

        (*
          NewlinesInControl -> Insert is equivalent to:
            NewlinesBetweenCommas -> Insert for the Comma node child of the control structure
            And also block the Comma node child
        *)
        
        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          indentedCommaNode = indent[commaNode, "NewlinesBetweenCommas" -> Insert];

          groupChildren =
            Flatten[{
              indent[opener],
              indent[Insert[#, EndOfLine -> True, {3, 1}]]& /@ {openerSeq},
              block @ {
                indentedCommaNode
              },
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          groupExtent = computeExtent[groupChildren];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1, "Extent" -> groupExtent |>
              ]
            };
          
          extent = computeExtent[indentedHead ~Join~ children];
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
      ,
      TrueQ[definitelyAutomatic],

        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          commaChildren =
            Flatten[betterRiffle[indent /@ commaChildrenIn, line[]]];

          commaExtent = computeExtent[commaChildren];

          groupChildren =
            Flatten[{
              indent[opener],
              indent[Insert[#, EndOfLine -> True, {3, 1}]]& /@ {openerSeq},
              block @ {
                InfixNode[Comma,
                  commaChildren
                  ,
                  <| data2, "Extent" -> commaExtent |>
                ]
              },
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          groupExtent = computeExtent[groupChildren];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1, "Extent" -> groupExtent |>
              ]
            };
          
          extent = computeExtent[indentedHead ~Join~ children];
        ];

        If[extent[[1]] >= $CurrentStyle["LineWidth"],
          (*
          exceeding LineWidth, so re-indent with "NewlinesInControl" -> Insert
          *)
          Throw[indent[node, "NewlinesInControl" -> Insert]]
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
    ]
  ]
]


(*
special casing For

For[start, test, inc,
  body
]

completely redo newlines
*)
indent[node:CallNode[head:{LeafNode[Symbol, "For" | {FragmentNode[Symbol, "For", _], ___}, _], ___}, {
      GroupNode[GroupSquare, {
          opener_,
          openerSeq:comment...,
          commaNode:InfixNode[Comma, {
              start:Except[LeafNode[Token`Comma, _, _]],
              startSeq:Except[LeafNode[Token`Comma, _, _]]...,
              comma1:LeafNode[Token`Comma, _, _],
              test:Except[LeafNode[Token`Comma, _, _]],
              testSeq:Except[LeafNode[Token`Comma, _, _]]...,
              comma2:LeafNode[Token`Comma, _, _],
              inc:Except[LeafNode[Token`Comma, _, _]],
              incSeq:Except[LeafNode[Token`Comma, _, _]]...,
              comma3:LeafNode[Token`Comma, _, _],
              body:Except[LeafNode[Token`Comma, _, _]],
              bodySeq:Except[LeafNode[Token`Comma, _, _]]...
            }
            ,
            data2_
          ],
          closerSeq:comment...,
          closer_
        }
        ,
        data1_
      ]
    }, data_], OptionsPattern[]] :=
Catch[
  Module[{indentedHead, indentedCommaNode,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    commaChildren, groupChildren, children,
    commaExtent, groupExtent, extent},

    definitelyDelete = OptionValue["NewlinesInControl"] === Delete || $CurrentStyle["NewlinesInControl"] === Delete;
    definitelyInsert = OptionValue["NewlinesInControl"] === Insert || $CurrentStyle["NewlinesInControl"] === Insert;

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True;
    ];

    If[$Debug,
      Print["NewlinesInControl choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}];
    ];

    Which[
      TrueQ[definitelyDelete],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyInsert],

        (*
          NewlinesInControl -> Insert is equivalent to:
            NewlinesBetweenCommas -> Insert for the Comma node child of the control structure
            And also block the Comma node child
        *)

        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          indentedCommaNode = indent[commaNode, "NewlinesBetweenCommas" -> Insert];

          groupChildren =
            Flatten[{
              indent[opener],
              indent[Insert[#, EndOfLine -> True, {3, 1}]]& /@ {openerSeq},
              block @ {
                indentedCommaNode
              },
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          groupExtent = computeExtent[groupChildren];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1, "Extent" -> groupExtent |>
              ]
            };
          
          extent = computeExtent[indentedHead ~Join~ children];
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
      ,
      TrueQ[definitelyAutomatic],

        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          commaChildren =
            Flatten[{
              indent[start],
              indent /@ {startSeq},
              indent[comma1],
              space[],
              indent[test],
              indent /@ {testSeq},
              indent[comma2],
              space[],
              indent[inc],
              indent /@ {incSeq},
              indent[comma3],
              indent[body],
              indent /@ {bodySeq}
            }];

          commaExtent = computeExtent[commaChildren];

          groupChildren =
            Flatten[{
              indent[opener],
              indent /@ {openerSeq},
              blockAfterLastRator @
                InfixNode[Comma,
                  commaChildren
                  ,
                  <| data2, "Extent" -> commaExtent |>
                ],
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          groupExtent = computeExtent[groupChildren];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1, "Extent" -> groupExtent |>
              ]
            };
          
          extent = computeExtent[indentedHead ~Join~ children];
        ];

        If[extent[[1]] >= $CurrentStyle["LineWidth"],
          (*
          exceeding LineWidth, so re-indent with "NewlinesInControl" -> Insert
          *)
          Throw[indent[node, "NewlinesInControl" -> Insert]]
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True, "Extent" -> extent |>
        ]
    ]
  ]
]

indent[node:CallNode[head_, graphs_, data_], OptionsPattern[]] :=
  commonCallNodeIndent[node]



(*
g may be:

GroupNode[GroupSquare, ...]
GroupNode[GroupDoubleBracket, ...]

UnterminatedGroupNode[GroupSquare, ...]
UnterminatedGroupNode[GroupDoubleBracket, ...]

*)
commonCallNodeIndent[CallNode[head_, children_, data_]] :=
Module[{indentedHead, indentedChildren, anyIndentedChildrenMultiline,
  extent},

  indentedHead = indent[#,"NewlinesInGroups" -> Automatic]& /@ head;

  indentedChildren = indent[#, "NewlinesInGroups" -> Automatic]& /@ children;

  anyIndentedChildrenMultiline = AnyTrue[indentedChildren, Lookup[#[[3]], "Multiline", False]&];

  extent = computeExtent[indentedHead ~Join~ indentedChildren];

  CallNode[
    indentedHead
    ,
    indentedChildren
    ,
    <| data, "Multiline" -> anyIndentedChildrenMultiline, "Extent" -> extent |>
  ]
]




indent[SyntaxErrorNode[tag_, ts_, data_], OptionsPattern[]] :=
Module[{children,
  extent},

  children = indent /@ ts;

  extent = computeExtent[children];

  SyntaxErrorNode[tag, children, <| data, "Extent" -> extent |>]
]


indent[ContainerNode[tag_, graphs_, data_], OptionsPattern[]] :=
  Catch[
  Module[{indented,
    children,
    extent},

    indented = indent /@ graphs;

    If[AnyTrue[indented, FailureQ],
      Throw[FirstCase[indented, _?FailureQ]]
    ];

    indented = Flatten[betterRiffle[indented, {{line[], line[]}}]];

    If[Length[indented] >= 1 && !MatchQ[graphs[[-1]], nl],
      AppendTo[indented, line[]]
    ];

    extent = computeExtent[indented];

    ContainerNode[
      tag
      ,
      indented
      ,
      <| data, "Extent" -> extent |>
    ]
  ]]



indent[BoxNode[RowBox, {graphs_}, data_], OptionsPattern[]] :=
  Module[{children,
    extent},

    children = indent /@ graphs;

    extent = computeExtent[children];

    BoxNode[RowBox
      ,
      children
      ,
      <| data, "Extent" -> extent |>
    ]
  ]

indent[node:BoxNode[_, _, _], OptionsPattern[]] :=
  node


indent[args___] := Failure["InternalUnhandled", <| "Function" -> indent, "Args" -> {args} |>]




identityExtent = {0, 1, 0, 0}

combineExtents[{w1_, h1_, l1_, t1_}, {w2_, h2_, l2_, t2_}] := {
  (*
  width of result

  Conceptually, extent2 is positioned after t1 on the last line of extent1

  So the width of the resulting combined extent is either:
  w1 itself if w2 is too small, or
  t1 + l2 if they combine to be wider than either, or
  w2 itself if w1 is too small
  *)
  Max[w1, t1 + l2, w2]
  ,
  (*
  height of result

  heights add like:
  1 + 1 == 1
  1 + 2 == 2
  etc.
  *)
  h1 + h2 - 1
  ,
  (*
  leadingWidth of result
  
  If h1 == 1, then that means that there is no newline in extent1
  The leadingWidth of the result is l1 + l2
  Otherwise, extent1 DOES have a newline, so leadingWidth of result is just l1
  *)
  If[h1 == 1, l1 + l2, l1]
  ,
  (*
  trailingWidth of result
  
  If h2 == 1, then that means that there is no newline in extent2
  The trailingWidth of the result is t1 + t2
  Otherwise, extent2 DOES have a newline, so trailingWidth of result is just t2
  *)
  If[h2 == 1, t1 + t2, t2]
}

combineExtents[args___] :=
  Failure["InternalUnhandled", <| "Function" -> combineExtents, "Arguments" -> {args} |>]

computeExtent[children_] :=
  Fold[combineExtents, identityExtent, getExtent /@ children]

computeIndentedExtent[children_] :=
  Fold[combineExtents, identityExtent, getIndentedExtent /@ children]

(*
getExtent[line[]] := {0, 2, 0, 0}

getExtent[IndentationNode[type_, children_, data_]] := {a, b, c}
*)
getExtent[line[]] := {0, 2, 0, 0}

getExtent[n_] := n[[3, Key["Extent"]]]


getIndentedExtent[line[]] := {0, 2, 0, 0} + {$CurrentIndentationStringLength, 0, 0, $CurrentIndentationStringLength}

getIndentedExtent[n_] := If[#[[2]] == 1, #, # + {$CurrentIndentationStringLength, 0, 0, $CurrentIndentationStringLength}]&[getExtent[n]]


End[]

EndPackage[]
