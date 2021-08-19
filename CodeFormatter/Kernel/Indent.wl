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
  indent[gst]



line[] :=
  LeafNode[Token`Newline, $CurrentNewlineString, <||>]

space[] =
  LeafNode[Whitespace, " ", <||>]

tab[] =
  LeafNode[Whitespace, "\t", <||>]


(*
Replace all newline leafs with the sequence: newline + indentation

Make sure to handle the cases of newlines inside of multiline comments
*)
incrementIndent[n_] :=
Module[{},

  If[$Debug,
    Print["calling incrementIndent: ", n];
  ];

  ReplaceAll[n,
    {
      nl:LeafNode[Token`Newline, $CurrentNewlineString, _] :>
        Sequence @@ ({nl} ~Join~ $CurrentIndentationNodeList)
      ,
      continuation:FragmentNode[Token`Comment, "\\" <> $CurrentNewlineString, _] :>
        Sequence @@ ({continuation} ~Join~ $CurrentIndentationCommentFragmentNodeList)
      ,
      nl:FragmentNode[Token`Comment, $CurrentNewlineString, _] :>
        Sequence @@ ({nl} ~Join~ $CurrentIndentationCommentFragmentNodeList)
    }
  ]
]





Options[indent] = {
  "NewlinesInGroups" -> Automatic
}



(* indent[LeafNode[Token`Newline, _, _], level_] :=
  line[level] *)

(*
Special case multiline comments

It is ok to change the internal indentation of comments, as long as everything is still aligned
*)
indent[LeafNode[Token`Comment, fs:{
  (*
  needs to be s_ /; ... because we do not want to use the value of $CurrentNewlineString at definition time
  *)
  FragmentNode[Token`Comment, s_ /; s == "\\" <> $CurrentNewlineString, _],
  ___,
  FragmentNode[Token`Comment, "(*", _],
  ___,
  FragmentNode[Token`Comment, "*)", _]}, data_], OptionsPattern[]] :=
Module[{min, replaced, origSpaces, strs, minStr, indentStr, inserted, split, fragGroups, nlGroups, firstStrs, replacedStrs, replacedFirstStrs,
  replacedOrigSpaces, level},

  If[$Debug,
    Print["calling indent multiline Comment"];
  ];

  origSpaces = Count[fs, FragmentNode[Token`Comment, " ", <|"Temporary" -> True|>]];

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
      Flatten[Table[FragmentNode[Token`Comment, #, <||>], level]& /@ Characters[$CurrentIndentationString]]
    ];

  If[$Debug,
    Print["minStr: ", minStr //InputForm];
    Print["indentStr: ", indentStr //InputForm];
    Print["replacedOrigSpaces: ", replacedOrigSpaces //InputForm];
    Print["replacedStrs: ", replacedStrs //InputForm];
  ];

  If[$CurrentStyle["NewlinesInComments"] === Delete,
    LeafNode[Token`Comment,
      Flatten[
        replacedOrigSpaces ~Join~
        {fragGroups[[1]]} ~Join~ (Function[{replacedStr}, FragmentNode[Token`Comment, #, <||>]& /@ replacedStr] /@ replacedStrs)
      ]
      ,
      data
    ]
    ,
    LeafNode[Token`Comment,
      Flatten[
        {fs[[1]]} ~Join~
        replacedOrigSpaces ~Join~
        betterRiffle[{fragGroups[[1]]} ~Join~ (Function[{replacedStr}, FragmentNode[Token`Comment, #, <||>]& /@ replacedStr] /@ replacedStrs), nlGroups]
      ]
      ,
      <|data, "Multiline" -> True|>
    ]
  ]
]


indent[n:LeafNode[Token`Comment, _, _], OptionsPattern[]] :=
Module[{},

  If[$Debug,
    Print["indent comment"];
  ];

  n
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
indent[n:LeafNode[_, _, _], OptionsPattern[]] :=
  n

indent[n:ErrorNode[_, _, _], OptionsPattern[]] :=
  n


(*
"FrontEnd"-style:
indentPostfixRator[Function][rator_, level_] :=
  {space[], indent[rator, level]}
*)

(* indentPostfixRator[_][rator_, level_] :=
  indent[rator, level] *)

indent[PrefixNode[tag_, {rator_, randSeq:comment..., rand_}, data_], OptionsPattern[]] :=
Module[{indentedRator, indentedRandSeq, indentedRand, indentedRandMultiline},

  indentedRator = indent[rator];
  indentedRandSeq = indent /@ {randSeq};
  indentedRand = indent[rand];

  indentedRandMultiline = Lookup[indentedRand[[3]], "Multiline", False];

  If[$Debug,
    Print["enter Prefix"];
    Print["rand: ", indentedRand];
  ];

  If[indentedRandMultiline,
    PrefixNode[tag,
      Flatten[{
        indentedRator,
        indentedRandSeq,
        indentedRand
      }]
      ,
      <|data, "Multiline" -> True|>
    ]
    ,
    PrefixNode[tag,
      Flatten[{
        indentedRator,
        indentedRandSeq,
        indentedRand
      }]
      ,
      data
    ]
  ]
]

indent[PostfixNode[tag_, {rand_, randSeq:comment..., rator_}, data_], OptionsPattern[]] :=
Module[{indentedRand, indentedRandSeq, indentedRator, indentedRandMultiline},

  indentedRand = indent[rand];
  indentedRandSeq = indent /@ {randSeq};
  indentedRator = indent[rator];

  indentedRandMultiline = Lookup[indentedRand[[3]], "Multiline", False];

  If[$Debug,
    Print["enter Postfix"];
    Print["rand: ", indentedRand];
  ];

  If[indentedRandMultiline,
    PostfixNode[tag,
      Flatten[{
        indentedRand,
        indentedRandSeq,
        indentedRator
      }]
      ,
      <|data, "Multiline" -> True|>
    ]
    ,
    PostfixNode[tag,
      Flatten[{
        indentedRand,
        indentedRandSeq,
        indentedRator
      }]
      ,
      data
    ]
  ]
]


indent[PrefixBinaryNode[tag_, {rator_, rand1Seq:comment..., rand1_, rand2Seq:comment..., rand2_}, data_], OptionsPattern[]] :=
Module[{indentedRator, indentedRand1Seq, indentedRand1, indentedRand2Seq, indentedRand2, indentedRandMultiline},

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

  If[indentedRandMultiline,
    PrefixBinaryNode[tag,
      Flatten[{
        indentedRator,
        indentedRand1Seq,
        indentedRand1,
        indentedRand2Seq,
        indentedRand2
      }]
      ,
      <|data, "Multiline" -> True|>
    ]
    ,
    PrefixBinaryNode[tag,
      Flatten[{
        indentedRator,
        indentedRand1Seq,
        indentedRand1,
        indentedRand2Seq,
        indentedRand2
      }]
      ,
      data
    ]
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

  indentedGraphs = Flatten[indent /@ graphs];

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

    indentedGraphs = Flatten[indent /@ graphs];

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

    indentedGraphs = Flatten[indent /@ graphs];

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
            lastRator = rators[[-1]];
            lastRatorPos = Position[indentedGraphs, lastRator][[1]];
            split = {Take[indentedGraphs, lastRatorPos[[1]]], Drop[indentedGraphs, lastRatorPos[[1]]]};
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
        incrementIndentAfterFirstRator @
          baseOperatorNodeIndent[type, tag, data, split, ratorsPat, anyIndentedGraphsMultiline, infixRatorSurroundedByLeafs]
    ]
  ]]


baseOperatorNodeIndent[type_, tag_, data_, split_, ratorsPat_, anyIndentedGraphsMultiline_, infixRatorSurroundedByLeafs_] :=
Catch[
Module[{},

  If[$Debug,
    Print["inside baseOperatorNodeIndent"];
    Print["type: ", type];
    Print["tag: ", tag];
    Print["split: ", split];
    Print["ratorsPat: ", ratorsPat];
  ];

  If[Length[split] == 1,
    (*
    There were no newline tokens
    *)
    Throw[
      type[tag,
        Flatten[
        Map[
          Function[{grouped},
            Replace[grouped, {
              rator:ratorsPat :>
                If[infixRatorSurroundedByLeafs,
                  indentInfixRatorSurroundedByLeafs[tag][rator]
                  ,
                  indentInfixRator[tag][rator]
                ]
            }, {1}]
          ]
          ,
          split
        ]]
        ,
        <|data, "Multiline" -> anyIndentedGraphsMultiline|>
      ]
    ]
  ];

  type[tag,
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
      {line[]}
    ]
    ]
    ,
    <|data, "Multiline" -> True|>
  ]
]]


incrementIndentAfterFirstRator[(type:BinaryNode|InfixNode|TernaryNode|QuaternaryNode)[tag_, graphs_, data_], OptionsPattern[]] :=
Catch[
Module[{aggs, rators, firstRatorPos, first, rest},

  If[$Debug,
    Print["inside incrementIndentAfterFirstRator"];
    Print["graphs: ", graphs]
  ];

  aggs = DeleteCases[graphs, comment];

  rators = aggs[[2;;;;2]];

  firstRatorPos = Position[graphs, rators[[1]]][[1]];

  {first, rest} = TakeDrop[graphs, firstRatorPos[[1]]];

  type[tag, Flatten[{first, incrementIndent /@ rest}], data]
]]


(*
Empty group
*)
indent[GroupNode[tag_, {
      opener_,
      closer_
    }, data_], OptionsPattern[]] :=
  Module[{},

  Block[{$Toplevel = False},
    GroupNode[tag,
      {
        indent[opener],
        indent[closer]
      }
      ,
      data
    ]
  ]]

indent[GroupNode[tag_, {
      opener_, 
      graphSeq___,
      closer_
    }, data_], OptionsPattern[]] :=
  Module[{graphs, aggs,
    definitelyDelete, definitelyInsert,
    indentedOpener, indentedGraphs, indentedCloser,
    anyIndentedGraphsMultiline},
  
  Block[{$Toplevel = False},

    graphs = {graphSeq};

    indentedOpener = indent[opener];
    indentedGraphs = Flatten[indent /@ graphs];
    indentedCloser = indent[closer];

    If[$Debug,
      Print["enter indent Group"];
      Print["graphs to test: ", indentedGraphs];
    ];

    anyIndentedGraphsMultiline = AnyTrue[indentedGraphs, Lookup[#[[3]], "Multiline", False]&];

    If[$Debug,
      Print["anyIndentedGraphsMultiline: ", anyIndentedGraphsMultiline];
    ];

    aggs = DeleteCases[graphs, comment];

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
        GroupNode[tag,
          Flatten[
          {
            indentedOpener,
            incrementIndent /@ surround[indentedGraphs, {line[]}, "AlreadyPresent" -> {After}],
            line[],
            indentedCloser
          }
          ]
          ,
          <|data, "Multiline" -> True|>
        ]
      ,
      TrueQ[definitelyDelete],
        GroupNode[tag,
          Flatten[
          {
            indentedOpener,
            indentedGraphs, 
            indentedCloser
          }
          ]
          ,
          (*
          may want to do:
          <|data, "Multiline" -> anyIndentedGraphsMultiline|>
          *)
          data
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
indent[node:CallNode[{head:LeafNode[Symbol, "Module" | "Block" | "With" | "Function" | {FragmentNode[Symbol, "Module" | "Block" | "With" | "Function", _], ___}, _], headSeq:comment...}, {
      GroupNode[GroupSquare, {
          opener_, 
          openerSeq:comment..., 
          InfixNode[Comma, {
              varsSeq:Except[LeafNode[Token`Comma, _, _]]...,
              comma1:LeafNode[Token`Comma, _, _],
              bodySeq:Except[LeafNode[Token`Comma, _, _]...]
            }
            , _],
          closerSeq:comment..., 
          closer_
        }, _]
    }, data_], OptionsPattern[]] :=
  Module[{indentedHead, indentedHeadSeq, indentedOpener, indentedOpenerSeq, indentedVars, indentedComma1, indentedBody, indentedCloserSeq, indentedCloser,
    definitelyDelete, definitelyInsert, definitelyAutomatic},

    indentedHead = indent[head];
    indentedHeadSeq = indent /@ {headSeq};
    Block[{$Toplevel = False},
      indentedOpener = indent[opener];
      indentedOpenerSeq = indent /@ {openerSeq};
      indentedVars = indent /@ {varsSeq};
      indentedComma1 = indent[comma1];
      indentedBody = indent /@ {bodySeq};
      indentedCloserSeq = indent /@ {closerSeq};
      indentedCloser = indent[closer];
    ];

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
        CallNode[
          Flatten[{
            indentedHead,
            indentedHeadSeq
          }],
          Block[{$Toplevel = False},
          Flatten[{
            indentedOpener,
            incrementIndent /@ indentedOpenerSeq,
            incrementIndent /@ indentedVars,
            incrementIndent[indentedComma1],
            incrementIndent[line[]],
            incrementIndent /@ indentedBody,
            incrementIndent /@ indentedCloserSeq,
            line[],
            indentedCloser
          }]
          ]
          ,
          <|data, "Multiline" -> True|>
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
indent[node:CallNode[{head:LeafNode[Symbol, "With" | {FragmentNode[Symbol, "With", _], ___}, _], headSeq:comment...}, {
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
            }, _],
          closerSeq:comment..., 
          closer_
        }, _]
    }, data_], OptionsPattern[]] :=
  Module[{indentedHead, indentedHeadSeq, indentedOpener, indentedOpenerSeq, indentedVarsFirst, indentedComma1, indentedVarsSecond,
    indentedComma2, indentedVarsRest, indentedBody, indentedCloserSeq, indentedCloser,
    definitelyDelete, definitelyInsert, definitelyAutomatic},

    indentedHead = indent[head];
    indentedHeadSeq = indent /@ {headSeq};
    Block[{$Toplevel = False},
      indentedOpener = indent[opener];
      indentedOpenerSeq = indent /@ {openerSeq};
      indentedVarsFirst = indent /@ {varsFirstSeq};
      indentedComma1 = indent[comma1];
      indentedVarsSecond = indent /@ {varsSecondSeq};
      indentedComma2 = indent[comma2];
      indentedVarsRest = indent /@ {varsRestSeq};
      indentedBody = indent /@ {bodySeq};
      indentedCloserSeq = indent /@ {closerSeq};
      indentedCloser = indent[closer];
    ];

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
        CallNode[
          Flatten[{
            indentedHead,
            indentedHeadSeq
          }],
          Block[{$Toplevel = False},
          Flatten[{
            indentedOpener,
            incrementIndent[line[]],
            incrementIndent /@ indentedOpenerSeq,
            incrementIndent /@ indentedVarsFirst,
            incrementIndent[line[]],
            incrementIndent[indentedComma1],
            incrementIndent[line[]],
            incrementIndent /@ indentedVarsSecond,
            incrementIndent[line[]],
            incrementIndent[indentedComma2],
            surround[incrementIndent /@ indentedVarsRest, {incrementIndent[line[]]}, "AlreadyPresent" -> {Before, After}],
            incrementIndent[line[]],
            incrementIndent /@ indentedBody,
            incrementIndent /@ indentedCloserSeq,
            line[],
            indentedCloser
          }]
          ]
          ,
          <|data, "Multiline" -> True|>
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
indent[node:CallNode[{head:LeafNode[Symbol, "If" | "Switch" | {FragmentNode[Symbol, "If" | "Switch", _], ___}, _], headSeq___}, {
      GroupNode[GroupSquare, {
          opener_,
          openerSeq___,
          InfixNode[
            Comma, {
              expr:Except[LeafNode[Token`Comma, _, _]],
              exprSeq:Except[LeafNode[Token`Comma, _, _]]...,
              comma1:LeafNode[Token`Comma, _, _],
              rest___
            },
            _
          ], 
          closerSeq___,
          closer_
        }, _]
    }, data_], OptionsPattern[]] :=
  Module[{indentedHead, indentedHeadSeq, indentedOpener, indentedOpenerSeq, indentedExpr, indentedExprSeq, indentedComma1, indentedRest,
    indentedCloserSeq, indentedCloser,
    definitelyDelete, definitelyInsert, definitelyAutomatic},

    indentedHead = indent[head];
    indentedHeadSeq = indent /@ {headSeq};
    Block[{$Toplevel = False},
      indentedOpener = indent[opener];
      indentedOpenerSeq = indent /@ {openerSeq};
      indentedExpr = indent[expr];
      indentedExprSeq = indent /@ {exprSeq};
      indentedComma1 = indent[comma1];
      indentedRest = indent /@ {rest};
      indentedCloserSeq = indent /@ {closerSeq};
      indentedCloser = indent[closer];
    ];

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
        CallNode[
          Flatten[{
            indentedHead,
            indentedHeadSeq
          }]
          ,
          Block[{$Toplevel = False},
          Flatten[{
            indentedOpener,
            indentedOpenerSeq,
            incrementIndent[indentedExpr],
            incrementIndent /@ indentedExprSeq,
            incrementIndent[indentedComma1],
            {incrementIndent[line[]], incrementIndent[#]}& /@ indentedRest,
            incrementIndent /@ indentedCloserSeq,
            line[],
            indentedCloser
          }]
          ]
          ,
          <|data, "Multiline" -> True|>
        ]
    ]
  ]


indent[ClauseNode[tag_, {
    test:Except[LeafNode[Token`Comma, _, _]], testSeq:Except[LeafNode[Token`Comma, _, _]]...,
    comma1:LeafNode[Token`Comma, _, _],
    val:Except[LeafNode[Token`Comma, _, _]], valSeq:Except[LeafNode[Token`Comma, _, _]]...
  }, data_], OptionsPattern[]] :=
Module[{indentedTest, indentedTestSeq, indentedComma1, indentedVal, indentedValSeq,
    definitelyDelete, definitelyInsert, definitelyAutomatic},

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
      ClauseNode[
        tag
        ,
        Flatten[{
          indentedTest, indentedTestSeq,
          indentedComma1, space[],
          indentedVal, indentedValSeq
        }]
        ,
        data
      ]
    ,
    TrueQ[definitelyInsert],
      ClauseNode[
        tag
        ,
        Flatten[{
          indentedTest,
          {line[], #}& /@ indentedTestSeq,
          line[],
          indentedComma1,
          line[],
          indentedVal,
          {line[], #}& /@ indentedValSeq
        }]
        ,
        <|data, "Multiline" -> True|>
      ]
    ,
    TrueQ[definitelyAutomatic],
      ClauseNode[
        tag
        ,
        Flatten[{
          indentedTest, indentedTestSeq, indentedComma1,
          incrementIndent[line[]],
          incrementIndent[indentedVal], indentedValSeq
        }]
        ,
        <|data, "Multiline" -> True|>
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
indent[node:CallNode[{head:LeafNode[Symbol, "Which" | {FragmentNode[Symbol, "Which", _], ___}, _], headSeq___}, {
      GroupNode[GroupSquare, {
          opener_,
          openerSeq___, 
          InfixNode[
            Comma,
            commaChildren_,
            _
          ], 
          closerSeq___, 
          closer_
        }, _]
    }, data_], OptionsPattern[]] :=
  Module[{indentedHead, indentedHeadSeq, indentedOpener, indentedOpenerSeq, indentedCommaChildren, indentedCloserSeq, indentedCloser,
    definitelyDelete, definitelyInsert, definitelyAutomatic},

    indentedHead = indent[head];
    indentedHeadSeq = indent /@ {headSeq};
    Block[{$Toplevel = False},
      indentedOpener = indent[opener];
      indentedOpenerSeq = indent /@ {openerSeq};
      indentedCommaChildren = indent /@ commaChildren;
      indentedCloserSeq = indent /@ {closerSeq};
      indentedCloser = indent[closer];
    ];

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
        CallNode[
          Flatten[{
            indentedHead,
            indentedHeadSeq
          }]
          ,
          Block[{$Toplevel = False},
          Flatten[{
            indentedOpener,
            indentedOpenerSeq,
            {incrementIndent[line[]], incrementIndent[#]}& /@ indentedCommaChildren,
            incrementIndent /@ indentedCloserSeq,
            line[],
            indentedCloser
          }]
          ]
          ,
          <|data, "Multiline" -> True|>
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
indent[node:CallNode[{head:LeafNode[Symbol, "For" | {FragmentNode[Symbol, "For", _], ___}, _], headSeq___}, {
      GroupNode[_, {
          opener_, 
          openerSeq___,
          InfixNode[
            Comma, {
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
            }, _
          ], 
          closerSeq___,
          closer_
        }, _]
    }, data_], OptionsPattern[]] :=
  Module[{indentedHead, indentedHeadSeq, indentedOpener, indentedOpenerSeq, indentedStart, indentedStartSeq, indentedComma1,
    indentedTest, indentedTestSeq, indentedComma2, indentedInc, indentedIncSeq, indentedComma3, indentedBody, indentedBodySeq, indentedCloserSeq, indentedCloser,
    definitelyDelete, definitelyInsert, definitelyAutomatic},

    indentedHead = indent[head];
    indentedHeadSeq = indent /@ {headSeq};
    Block[{$Toplevel = False},
      indentedOpener = indent[opener];
      indentedOpenerSeq = indent /@ {openerSeq};
      indentedStart = indent[start];
      indentedStartSeq = indent /@ {startSeq};
      indentedComma1 = indent[comma1];
      indentedTest = indent[test];
      indentedTestSeq = indent /@ {testSeq};
      indentedComma2 = indent[comma2];
      indentedInc = indent[inc];
      indentedIncSeq = indent /@ {incSeq};
      indentedComma3 = indent[comma3];
      indentedBody = indent[body];
      indentedBodySeq = indent /@ {bodySeq};
      indentedCloserSeq = indent /@ {closerSeq};
      indentedCloser = indent[closer];
    ];

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
        CallNode[
          Flatten[{
            indentedHead,
            indentedHeadSeq
          }]
          ,
          Block[{$Toplevel = False},
          Flatten[{
            indentedOpener,
            indentedOpenerSeq,
            incrementIndent[indentedStart],
            incrementIndent /@ indentedStartSeq,
            incrementIndent[indentedComma1],
            incrementIndent[space[]],
            incrementIndent[indentedTest],
            incrementIndent /@ indentedTestSeq,
            incrementIndent[indentedComma2],
            incrementIndent[space[]],
            incrementIndent[indentedInc],
            incrementIndent /@ indentedIncSeq,
            incrementIndent[indentedComma3],
            incrementIndent[line[]],
            incrementIndent[indentedBody],
            incrementIndent /@ indentedBodySeq,
            incrementIndent /@ indentedCloserSeq,
            line[],
            indentedCloser
          }]
          ]
          ,
          <|data, "Multiline" -> True|>
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
commonCallNodeIndent[CallNode[{head_, headSeq___}, {g_}, data_]] :=
Module[{indentedHead, indentedHeadSeq, indentedGroup, indentedGroupMultiline},

  indentedHead = indent[head, "NewlinesInGroups" -> Delete];
  indentedHeadSeq = indent /@ {headSeq};

  indentedGroup = indent[g, "NewlinesInGroups" -> Delete];

  indentedGroupMultiline = Lookup[indentedGroup[[3]], "Multiline", False];

  If[indentedGroupMultiline,
    CallNode[
      Flatten[{
        indentedHead,
        indentedHeadSeq
      }]
      ,
      {indentedGroup}
      ,
      <|data, "Multiline" -> True|>
    ]
    ,
    CallNode[
      Flatten[{
        indentedHead,
        indentedHeadSeq
      }]
      ,
      {indentedGroup}
      ,
      data
    ]
  ]
]




indent[SyntaxErrorNode[tag_, ts_, data_], OptionsPattern[]] :=
  SyntaxErrorNode[tag, indent /@ ts, data]


indent[ContainerNode[tag_, graphs_, data_], OptionsPattern[]] :=
  Catch[
  Module[{indented},

    indented = indent /@ graphs;

    If[AnyTrue[indented, FailureQ],
      Throw[FirstCase[indented, _?FailureQ]]
    ];

    indented = Flatten[betterRiffle[indented, {{line[], line[]}}]];

    If[Length[indented] >= 1 && !MatchQ[graphs[[-1]], nl],
      AppendTo[indented, LeafNode[Token`Newline, $CurrentNewlineString, <||>]]
    ];

    ContainerNode[
      tag
      ,
      indented
      ,
      data
    ]
  ]]



indent[BoxNode[RowBox, {graphs_}, data_], OptionsPattern[]] :=
  Module[{},

    BoxNode[RowBox
      ,
      Flatten[
        indent /@ graphs
      ]
      ,
      data
    ]
  ]

indent[node:BoxNode[_, _, _], OptionsPattern[]] :=
  node


indent[args___] := Failure["InternalUnhandled", <| "Function" -> indent, "Args" -> {args} |>]

End[]

EndPackage[]
