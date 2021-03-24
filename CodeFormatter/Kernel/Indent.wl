(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

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

IndentCST[gst_] :=
Module[{indented},

  If[$Debug,
    Print["Inside IndentCST"];
  ];

  (*
  indented has symbolic expressions:
  IndentationNode[]
  line[]
  *)
  indented = indent[gst];

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
    Module[{children, blocked, len},

      Internal`InheritedBlock[{$CurrentIndentationLevelNodeList = $CurrentIndentationLevelNodeList ~Join~ $CurrentIndentationNodeList},

        children = childrenIn;

        (*
        There may be leading whitespace from breaking after an operator such as :=, so make sure to remove
        *)
        len = LengthWhile[children, MatchQ[#, ws]&];
        children = Drop[children, len];

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
      Sequence @@ ({LeafNode[Token`Newline, $CurrentNewlineString, <||>]} ~Join~ $CurrentIndentationLevelNodeList);
    
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
  {LeafNode[Token`Newline, $CurrentNewlineString, <||>], $CurrentIndentationLevelNodeList}
*)

space[] =
  LeafNode[Whitespace, " ", <||>]


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
Module[{head, children},

  If[$Debug,
    Print["calling increment: ", {CallNode, headIn}];
  ];

  head = headIn;

  children = childrenIn;

  head = Flatten[{increment /@ head}];

  children = Flatten[{increment /@ children}];

  CallNode[head, children, <| data |>]
] *)

(* increment[n:LeafNode[_, _, _]] :=
Module[{},
  n
] *)

(* increment[type_[tag_, childrenIn_, data_]] :=
Module[{children},

  If[$Debug,
    Print["calling increment: ", {type, tag}];
  ];

  children = childrenIn;

  children = Flatten[{increment /@ children}];

  type[tag, children, <| data |>]
] *)


(*
short-hand for IndentationNode[Increment, ...]
*)
increment[n_] :=
Catch[
Module[{aggs, rators, firstRatorPos, first, rest, children},

  If[$Debug,
    Print["inside increment"];
  ];

  IndentationNode[Increment, {n}, <||>]
]]


(*
short-hand for IndentationNode[Block, ...]
*)
block[children_List] :=
Module[{},

  If[$Debug,
    Print["inside block"];
    Print["children: ", children];
    Print["$Toplevel: ", $Toplevel];
  ];

  IndentationNode[Block, children, <| "Toplevel" -> $Toplevel |>]
]

blockAfterFirstRator[type_[tag_, graphs_, data_]] :=
Catch[
Module[{aggs, rators, firstRatorPos, first, rest, children},

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

  type[tag, children, <| data |>]
]]

blockAfterLastRator[type_[tag_, graphs_, data_]] :=
Catch[
Module[{aggs, rators, lastRatorPos, first, rest, children},

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

  type[tag, children, <| data |>]
]]



Options[indent] = {
  "NewlinesInGroups" -> Automatic
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
  children},

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

    LeafNode[
      Token`Comment
      ,
      children
      ,
      <| data |>
    ]
    ,

    children =
      Flatten[
        {indent[fs[[1]]]} ~Join~
        replacedOrigSpaces ~Join~
        betterRiffle[{fragGroups[[1]]} ~Join~ (Function[{replacedStr}, indent[FragmentNode[Token`Comment, #, <||>]]& /@ replacedStr] /@ replacedStrs), nlGroups]
      ];

    LeafNode[
      Token`Comment
      ,
      children
      ,
      <| data, "Multiline" -> True |>
    ]
  ]
]


indent[LeafNode[Token`Comment, str_String, data_], OptionsPattern[]] :=
With[{len = StringLength[str]},

  If[$Debug,
    Print["indent comment"];
  ];

  LeafNode[Token`Comment, str, <| data |>]
]

(*
newlines may be introduced and result in:

f[args___] :=
    $Failed /; (g[])
;

so keep track of top-level ; and absorb newlines later as needed

*)
indent[LeafNode[Token`Semi, str_String, data_], OptionsPattern[]] :=
With[{len = StringLength[str]},

  LeafNode[Token`Semi, str, <| data, "Toplevel" -> $Toplevel |>]
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
  LeafNode[tag, str, <| data |>]
]

indent[LeafNode[tag_, frags_, data_], OptionsPattern[]] :=
With[
  {children = indent /@ frags}
  ,
  LeafNode[tag, children, <| data |>]
]

(*
Temporary line continuation
*)
(*
needs to be s_ /; ... because we do not want to use the value of $CurrentNewlineString at definition time
*)
indent[FragmentNode[Token`Comment, s_ /; s == $CurrentLineContinuationString, data_]] :=
  FragmentNode[Token`Comment, s, <| data |>]

indent[FragmentNode[Token`Comment, s_ /; s == $CurrentNewlineString, data_]] :=
  FragmentNode[Token`Comment, s, <| data |>]

indent[FragmentNode[tag_, str_String, data_], OptionsPattern[]] :=
With[{len = StringLength[str]},
  FragmentNode[tag, str, <| data |>]
]

indent[ErrorNode[tag_, str_String, data_], OptionsPattern[]] :=
With[{len = StringLength[str]},
  ErrorNode[tag, str, <| data |>]
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
  children},

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

  PrefixNode[tag,
    children
    ,
    <| data, "Multiline" -> indentedRandMultiline |>
  ]
]

indent[PostfixNode[tag_, {rand_, randSeq:comment..., rator_}, data_], OptionsPattern[]] :=
Module[{indentedRand, indentedRandSeq, indentedRator, indentedRandMultiline,
  children},

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

  PostfixNode[tag,
    children
    ,
    <| data, "Multiline" -> indentedRandMultiline |>
  ]
]


indent[PrefixBinaryNode[tag_, {rator_, rand1Seq:comment..., rand1_, rand2Seq:comment..., rand2_}, data_], OptionsPattern[]] :=
Module[{indentedRator, indentedRand1Seq, indentedRand1, indentedRand2Seq, indentedRand2, indentedRandMultiline,
  children},

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
  
  PrefixBinaryNode[tag,
    children
    ,
    <| data, "Multiline" -> indentedRandMultiline |>
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
Module[{ratorsPat, definitelyDelete, definitelyInsert, split, indentedGraphs, anyIndentedGraphsMultiline},

  If[$Debug,
    Print["indent CompoundExpression"];
  ];

  indentedGraphs = indent /@ graphs;

  anyIndentedGraphsMultiline = AnyTrue[indentedGraphs, Lookup[#[[3]], "Multiline", False]&];

  ratorsPat = LeafNode[Token`Semi, _, _];

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
    definitelyDelete = $CurrentStyle["NewlinesBetweenSemicolons"] === Delete;
    definitelyInsert = $CurrentStyle["NewlinesBetweenSemicolons"] === Insert;
  ];

  If[!TrueQ[(definitelyDelete || definitelyInsert)],
    definitelyInsert = True
  ];

  Which[
    TrueQ[definitelyInsert],
      split = Split[indentedGraphs, MatchQ[#2, LeafNode[Token`Semi, _, _]]&];
    ,
    TrueQ[definitelyDelete],
      split = {indentedGraphs};
  ];

  baseOperatorNodeIndent[InfixNode, CompoundExpression, data, split, ratorsPat, anyIndentedGraphsMultiline, False]
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

indent[InfixNode[Comma, graphs_, data_], OptionsPattern[]] :=
  Catch[
  Module[{aggs, ratorsPat, split, definitelyDelete, definitelyInsert, definitelyAutomatic,
    indentedGraphs, anyIndentedGraphsMultiline},

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
      definitelyDelete = $CurrentStyle["NewlinesBetweenCommas"] === Delete;
      definitelyInsert = $CurrentStyle["NewlinesBetweenCommas"] === Insert;
    ];

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      Which[
        True,
          definitelyDelete = True
      ]
    ];

    If[$Debug,
      Print["comma choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}];
    ];

    Which[
      TrueQ[definitelyInsert],
        split = {#}& /@ indentedGraphs;
      ,
      TrueQ[definitelyDelete],
        split = {indentedGraphs};
      ,
      TrueQ[definitelyAutomatic],
        split = Split[indentedGraphs, (!MatchQ[#1, LeafNode[Token`Comment, _, _]] && MatchQ[#2, LeafNode[Token`Comma, _, _]])&]
    ];
    
    If[$Debug,
      Print["Comma split: ", split];
    ];

    baseOperatorNodeIndent[InfixNode, Comma, data, split, ratorsPat, anyIndentedGraphsMultiline, False]
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
no space before commas or semi
*)
indentInfixRator[Comma | CompoundExpression][rator_] :=
  {rator, space[]}

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
indent[(type:BinaryNode|InfixNode|TernaryNode|QuaternaryNode)[tag_, graphs_, data_], OptionsPattern[]] :=
  Catch[
  Module[{aggs, rators, ratorsPat, split,
    definitelyDelete, definitelyInsert, definitelyAutomatic, indentedGraphs, anyIndentedGraphsMultiline, lastRator, lastRatorPos,
    infixRatorSurroundedByLeafs},

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
        definitelyDelete = $CurrentStyle["NewlinesBetweenOperators"] === Delete;
      ,
      True,
        definitelyDelete = $CurrentStyle["NewlinesBetweenOperators"] === Delete;
        definitelyInsert = $CurrentStyle["NewlinesBetweenOperators"] === Insert;
    ];

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True;
    ];

    If[$Debug,
      Print["indent operator choice: ", {definitelyDelete, definitelyInsert, definitelyAutomatic}]
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
          ,
          True,
            increment @
              baseOperatorNodeIndent[type, tag, data, split, ratorsPat, anyIndentedGraphsMultiline, infixRatorSurroundedByLeafs]
        ]
    ]
  ]]


baseOperatorNodeIndent[type_, tag_, data_, split_, ratorsPat_, anyIndentedGraphsMultiline_, infixRatorSurroundedByLeafs_] :=
Catch[
Module[{children, grouped},

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

    Throw[
      type[tag,
        children
        ,
        <| data, "Multiline" -> anyIndentedGraphsMultiline |>
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

  type[tag,
    children
    ,
    <| data, "Multiline" -> True |>
  ]
]]


(*
Empty group
*)
indent[GroupNode[tag_, {
      opener_,
      closer_
    }, data_], OptionsPattern[]] :=
Module[{children},

  children =
    {
      indent[opener],
      indent[closer]
    };

  Block[{$Toplevel = False},
    GroupNode[tag,
      children
      ,
      <| data |>
    ]
  ]
]

indent[GroupNode[tag_, {
      opener_, 
      graphSeq___,
      closer_
    }, data_], OptionsPattern[]] :=
  Module[{graphs, aggs, x,
    definitelyDelete, definitelyInsert,
    indentedOpener, indentedGraphs, indentedCloser,
    anyIndentedGraphsMultiline,
    children},
  
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
      definitelyDelete = OptionValue["NewlinesInGroups"] === Delete;
      definitelyInsert = OptionValue["NewlinesInGroups"] === Insert;
    ];

    If[$Debug,
      Print["1 indent Group definitely: ", {definitelyDelete, definitelyInsert}];
    ];

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyDelete = $CurrentStyle["NewlinesInGroups"] === Delete;
      definitelyInsert = $CurrentStyle["NewlinesInGroups"] === Insert;
    ];

    If[$Debug,
      Print["2 indent Group definitely: ", {definitelyDelete, definitelyInsert}];
    ];

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      Which[
        True,
          definitelyDelete = True
      ];
    ];

    If[$Debug,
      Print["indent Group definitely: ", {definitelyDelete, definitelyInsert}];
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

        GroupNode[tag,
          children
          ,
          <| data, "Multiline" -> True |>
        ]
      ,
      TrueQ[definitelyDelete],

        children =
          Flatten[{
            indentedOpener,
            indentedGraphs,
            indentedCloser
          }];

        GroupNode[tag,
          children
          ,
          (*
          may want to do:
          <| data, "Multiline" -> anyIndentedGraphsMultiline |>
          *)
          <| data |>
        ]
    ]
  ]]

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
          InfixNode[Comma, {
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
  Module[{indentedHead,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    commaChildren, groupChildren, children},

    definitelyDelete = $CurrentStyle["NewlinesInScoping"] === Delete;
    definitelyInsert = $CurrentStyle["NewlinesInScoping"] === Insert;

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True
    ];

    Which[
      TrueQ[definitelyDelete],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyInsert],
        commonCallNodeIndent[node]
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

          groupChildren =
            Flatten[{
              indent[opener],
              indent /@ {openerSeq},
              blockAfterFirstRator @
                InfixNode[Comma,
                  commaChildren
                  ,
                  <| data2 |>
                ],
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1 |>
              ]
            };
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True |>
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
  Module[{indentedHead,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    commaChildren, groupChildren, children},

    definitelyDelete = $CurrentStyle["NewlinesInScoping"] === Delete;
    definitelyInsert = $CurrentStyle["NewlinesInScoping"] === Insert;

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True
    ];

    Which[
      TrueQ[definitelyDelete],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyInsert],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyAutomatic],

        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          commaChildren =
            Flatten[betterRiffle[indent /@ {varsFirstSeq, comma1, varsSecondSeq, comma2, varsRestSeq, bodySeq}, line[]]];

          groupChildren =
            Flatten[{
              indent[opener],
              indent[Insert[#, EndOfLine -> True, {3, 1}]]& /@ {openerSeq},
              block @ {
                InfixNode[Comma,
                  commaChildren
                  ,
                  <| data2 |>
                ]
              },
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1 |>
              ]
            };
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True |>
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
          InfixNode[Comma, {
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
  Module[{indentedHead,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    commaChildren, groupChildren, children},

    definitelyDelete = $CurrentStyle["NewlinesInControl"] === Delete;
    definitelyInsert = $CurrentStyle["NewlinesInControl"] === Insert;

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True;
    ];

    Which[
      TrueQ[definitelyDelete],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyInsert],
        commonCallNodeIndent[node]
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

          groupChildren =
            Flatten[{
              indent[opener],
              indent /@ {openerSeq},
              blockAfterFirstRator @
                InfixNode[Comma,
                  commaChildren
                  ,
                  <| data2 |>
                ],
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1 |>
              ]
            };
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True |>
        ]
    ]
  ]


indent[ClauseNode[tag_, {
    test:Except[LeafNode[Token`Comma, _, _]], testSeq:Except[LeafNode[Token`Comma, _, _]]...,
    comma1:LeafNode[Token`Comma, _, _],
    val:Except[LeafNode[Token`Comma, _, _]], valSeq:Except[LeafNode[Token`Comma, _, _]]...
  }, data_], OptionsPattern[]] :=
Module[{indentedTest, indentedTestSeq, indentedComma1, indentedVal, indentedValSeq,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    children},

  indentedTest = indent[test];
  indentedTestSeq = indent /@ {testSeq};
  indentedComma1 = indent[comma1];
  indentedVal = indent[val];
  indentedValSeq = indent /@ {valSeq};

  definitelyDelete = $CurrentStyle["NewlinesInControl"] === Delete;
  definitelyInsert = $CurrentStyle["NewlinesInControl"] === Insert;

  If[!TrueQ[(definitelyDelete || definitelyInsert)],
    definitelyAutomatic = True;
  ];

  Which[

    TrueQ[definitelyDelete],

      children =
        Flatten[{
          indentedTest, indentedTestSeq,
          indentedComma1, space[],
          indentedVal, indentedValSeq
        }];

      ClauseNode[
        tag
        ,
        children
        ,
        <| data |>
      ]
    ,
    TrueQ[definitelyInsert],

      children =
        Flatten[betterRiffle[{indentedTest, indentedTestSeq, indentedComma1, indentedVal,  indentedValSeq}, line[]]];

      ClauseNode[
        tag
        ,
        children
        ,
        <| data, "Multiline" -> True |>
      ]
    ,
    TrueQ[definitelyAutomatic],

      children =
        Flatten[{
          indentedTest, indentedTestSeq, indentedComma1, line[],
          indentedVal, indentedValSeq  
        }];

      increment @
        ClauseNode[
          tag
          ,
          children
          ,
          <| data, "Multiline" -> True |>
        ]
  ]
]



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
          InfixNode[Comma,
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
  Module[{indentedHead,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    commaChildren, groupChildren, children},

    definitelyDelete = $CurrentStyle["NewlinesInControl"] === Delete;
    definitelyInsert = $CurrentStyle["NewlinesInControl"] === Insert;

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True
    ];

    Which[
      TrueQ[definitelyDelete],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyInsert],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyAutomatic],

        indentedHead = indent /@ head;
        Block[{$Toplevel = False},

          commaChildren =
            Flatten[betterRiffle[indent /@ commaChildrenIn, line[]]];

          groupChildren =
            Flatten[{
              indent[opener],
              indent[Insert[#, EndOfLine -> True, {3, 1}]]& /@ {openerSeq},
              block @ {
                InfixNode[Comma,
                  commaChildren
                  ,
                  <| data2 |>
                ]
              },
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1 |>
              ]
            };
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True |>
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
          InfixNode[Comma, {
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
  Module[{indentedHead,
    definitelyDelete, definitelyInsert, definitelyAutomatic,
    commaChildren, groupChildren, children},

    definitelyDelete = $CurrentStyle["NewlinesInControl"] === Delete;
    definitelyInsert = $CurrentStyle["NewlinesInControl"] === Insert;

    If[!TrueQ[(definitelyDelete || definitelyInsert)],
      definitelyAutomatic = True;
    ];

    Which[
      TrueQ[definitelyDelete],
        commonCallNodeIndent[node]
      ,
      TrueQ[definitelyInsert],
        commonCallNodeIndent[node]
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

          groupChildren =
            Flatten[{
              indent[opener],
              indent /@ {openerSeq},
              blockAfterLastRator @
                InfixNode[Comma,
                  commaChildren
                  ,
                  <| data2 |>
                ],
              indent[Insert[#, StartOfLine -> True, {3, 1}]]& /@ {closerSeq},
              indent[closer]
            }];

          children = {
              GroupNode[GroupSquare,
                groupChildren
                ,
                <| data1 |>
              ]
            };
        ];

        CallNode[
          indentedHead
          ,
          children
          ,
          <| data, "Multiline" -> True |>
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
Module[{indentedHead, indentedChildren, anyIndentedChildrenMultiline},

  indentedHead = indent[#,"NewlinesInGroups" -> Delete]& /@ head;

  indentedChildren = indent[#, "NewlinesInGroups" -> Delete]& /@ children;

  anyIndentedChildrenMultiline = AnyTrue[indentedChildren, Lookup[#[[3]], "Multiline", False]&];

  CallNode[
    indentedHead
    ,
    indentedChildren
    ,
    <| data, "Multiline" -> anyIndentedChildrenMultiline |>
  ]
]




indent[SyntaxErrorNode[tag_, ts_, data_], OptionsPattern[]] :=
Module[{children},

  children = indent /@ ts;

  SyntaxErrorNode[tag, children, <| data |>]
]


indent[ContainerNode[tag_, graphs_, data_], OptionsPattern[]] :=
  Catch[
  Module[{indented,
    children},

    indented = indent /@ graphs;

    If[AnyTrue[indented, FailureQ],
      Throw[FirstCase[indented, _?FailureQ]]
    ];

    indented = Flatten[betterRiffle[indented, {{line[], line[]}}]];

    If[Length[indented] >= 1 && !MatchQ[graphs[[-1]], nl],
      AppendTo[indented, line[]]
    ];

    ContainerNode[
      tag
      ,
      indented
      ,
      <| data |>
    ]
  ]]



indent[BoxNode[RowBox, {graphs_}, data_], OptionsPattern[]] :=
  Module[{children},

    children = indent /@ graphs;

    BoxNode[RowBox
      ,
      children
      ,
      <| data |>
    ]
  ]

indent[node:BoxNode[_, _, _], OptionsPattern[]] :=
  node


indent[args___] := Failure["InternalUnhandled", <| "Function" -> indent, "Args" -> {args} |>]


End[]

EndPackage[]
