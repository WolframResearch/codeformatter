BeginPackage["CodeFormatter`Indent`"]

indent

Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"] (* for empty *)


line[level_] :=
  {LeafNode[Token`Newline, $CurrentNewline, <||>]} ~Join~
    Table[LeafNode[Whitespace, #, <||>]& /@ Characters[$CurrentIndentationString], level]

space[] = LeafNode[Whitespace, " ", <||>]

tab[] = LeafNode[Whitespace, "\t", <||>]

nil[] = {}



indent[LeafNode[Token`Newline, _, _], level_] :=
  line[level]

(*
Special case multiline comments

It is ok to change the internal indentation of comments, as long as everything is still aligned
*)
indent[LeafNode[Token`Comment, fs:{
  FragmentNode[Token`Comment, s_ /; s == "\\" <> $CurrentNewline, _],
  ___,
  FragmentNode[Token`Comment, "(*", _],
  ___,
  FragmentNode[Token`Comment, "*)", _]}, data_], level_] :=
Module[{min, replaced, origSpaces, strs, minStr, indentStr, frags, inserted, split, fragGroups, nlGroups, firstStrs, replacedStrs, replacedFirstStrs},

  inserted = Lookup[data, "InsertedFragmentNodes", 0];

  origSpaces = inserted - 1;

  If[$Debug,
    Print["origSpaces: ", origSpaces //InputForm];
  ];

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
    inserted - 1,
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

  minStr = StringJoin[Table[" ", min]];
  indentStr = StringJoin[Table[$CurrentIndentationString, level]];
  
  replacedFirstStrs = StringReplace[firstStrs[[2;;]], StartOfString ~~ minStr :> indentStr];

  replacedStrs = MapThread[{#1}~Join~Rest[#2] &, {replacedFirstStrs, strs[[2;;]]}];

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
        {fragGroups[[1]]} ~Join~ ((FragmentNode[Token`Comment, #, <||>]& /@ #)& /@ replacedStrs)
      ]
      ,
      data
    ]
    ,
    LeafNode[Token`Comment,
      Flatten[
        {fs[[1]]} ~Join~
        replacedOrigSpaces ~Join~
        betterRiffle[{fragGroups[[1]]} ~Join~ ((FragmentNode[Token`Comment, #, <||>]& /@ #)& /@ replacedStrs), nlGroups]
      ]
      ,
      data
    ]
  ]
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


(*
Boxes may come in such as:
RowBox[{"!", "a", " "}]

with trailing whitespace

CodeParser does not keep whitespace inside the PrefixNode, but the FE does
*)
indent[PrefixNode[tag_, {rator_, trivia..., rand_, trivia...}, data_], level_] :=
  PrefixNode[tag, {indent[rator, level], indent[rand, level]}, data]

indent[PostfixNode[tag_, {rand_, trivia..., rator_}, data_], level_] :=
  PostfixNode[tag, {indent[rand, level], indentPostfixRator[tag][rator, level]}, data]


(*
Special case CompoundExpression at top-level

Completely preserve newlines for CompoundExpression at top-level

Breaking up lines or gluing lines together may change the actual CompoundExpressions and
how expressions are parsed
*)
indent[InfixNode[CompoundExpression, ts_, data_], level_] /; TrueQ[$Toplevel] :=
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

  InfixNode[CompoundExpression
    ,
    Flatten[
    Replace[graphs, {
      lastRator /; MatchQ[lastRand, LeafNode[Token`Fake`ImplicitNull, _, _]] :> indent[lastRator, level], 
      rator : ratorsPat :> {indent[rator, level], space[]},
      rand : randsPat :> indent[rand, level], 
      other_ :> indent[other, level]
    }, {1}]]
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
indent[InfixNode[CompoundExpression, ts_, data_], level_] /; !TrueQ[$Toplevel] :=
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
    $CurrentStyle["NewlinesBetweenCompoundExpressions"] === Delete,
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

  InfixNode[CompoundExpression
    ,
    Flatten[
    Which[
      $CurrentStyle["NewlinesBetweenCompoundExpressions"] === Insert,
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
    ]]
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
indentInfixRator[Comma][rator_, level_] /; $CurrentStyle["NewlinesBetweenCommas"] === Insert :=
  {line[level], indent[rator, level], line[level]}

indentInfixRatorNoTrailingSpace[Comma][rator_, level_] /; $CurrentStyle["NewlinesBetweenCommas"] === Insert :=
  {line[level], indent[rator, level], line[level]}

indentInfixRator[Comma][rator_, level_] :=
  {indent[rator, level], space[]}

indentInfixRatorNoTrailingSpace[Comma][rator_, level_] :=
  indent[rator, level]

(*
special case implicit Times
*)
indentInfixRator[Times][l:LeafNode[Token`Fake`ImplicitTimes, _, _], level_] :=
  l

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

    Which[
      (*
      foo := symbol  =>  single line
      *)
      MatchQ[aggs, {_, _, LeafNode[Symbol, _, _]}],
        (*
        Single line, so delete newlines
        *)
        graphs = DeleteCases[ts, ws | nl];

        split = {graphs}
      ,
      MemberQ[$AlwaysLineBreak, tag],
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
        ]
      ,
      True,
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
    
    If[$Debug,
      Print["split: ", split];
    ];

    If[$CurrentStyle["NewlinesBetweenOperators"] === Delete || (Length[split] == 1 && $CurrentStyle["NewlinesBetweenOperators"] =!= Insert),
      (*
      There were no newline tokens
      *)
      Throw[
        head[tag,
          Flatten[
          Map[
            Function[{grouped},
              Replace[grouped, {
                rator : ratorsPat :> indentInfixRator[tag][rator, level], 
                randOrComment_ :> indent[randOrComment, level]
              }, {1}]
            ]
            ,
            split
          ]]
          ,
          data
        ]
      ]
    ];

    head[tag,
      With[{newLevel = indentIncrement[tag, level]},
        Flatten[
        betterRiffle[
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
        ]]
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
    LeafNode[Token`Newline, $CurrentNewline, <||>],
    ts,
    LeafNode[Token`Newline, $CurrentNewline, <||>],
    closer
  }, data]



indent[GroupNode[tag_, {
      opener_, 
      trivia1 : trivia...,
      closer_
    }, data_], level_] :=
  Module[{triviaAggs, triviaGraphs, condition},

    triviaAggs = DeleteCases[{trivia1}, ws];

    triviaGraphs = DeleteCases[{trivia1}, ws | nl];

    Which[
      $CurrentStyle["NewlinesInGroups"] === Insert,
        condition = MultiLineEnum
      ,
      $CurrentStyle["NewlinesInGroups"] === Delete,
        condition = SingleLineEnum
      ,
      !FreeQ[triviaAggs, LeafNode[Token`Newline, _, _]],
        condition = MultiLineEnum
      ,
      True,
        condition = SingleLineEnum
    ];

    Block[{$Toplevel = False},
    GroupNode[tag,
      Flatten[
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
      ]]
      ,
      data
    ]
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
      $CurrentStyle["NewlinesInGroups"] === Insert,
        condition = MultiLineEnum
      ,
      $CurrentStyle["NewlinesInGroups"] === Delete,
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

    Block[{$Toplevel = False},
    GroupNode[tag,
      Flatten[
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
      ]]
      ,
      data
    ]
    ]
  ]

indent[n:GroupMissingCloserNode[_, _, _], level_] :=
  n

indent[n:UnterminatedGroupNode[_, _, _], level_] :=
  n



(*
special casing Module | With | Block
*)
indent[CallNode[{head : LeafNode[Symbol, "Module" | "With" | "Block" | {FragmentNode[Symbol, "Module" | "With" | "Block", _], ___}, _], trivia1:trivia...}, {
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

    -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

    -3 is where LeafNode[xxx, xxx, <||>] is
    *)
    vars = DeleteCases[vars, LeafNode[Token`Newline, _, _], {-5, -3}];

    If[$CurrentStyle["NewlinesInScoping"] === Delete,
      (*
      no newlines
      *)
      CallNode[
        Flatten[{
          indent[head, level],
          indent[#, level + 1]& /@ comments1
        }],
        Block[{$Toplevel = False},
        Flatten[{
          indent[opener, level + 1],
          indent[#, level + 1]& /@ comments2,
          indent[vars, level + 1],
          indent[#, level + 1]& /@ comments3,
          indent[comma1, level + 1],
          indent[#, level + 1]& /@ comments4,
          space[],
          indent[body, level + 1],
          indent[#, level + 1]& /@ comments5,
          indent[closer, level]
        }]
        ]
        ,
        data
      ]
      ,
      (*
      Redo newlines
      *)
      CallNode[
        Flatten[{
          indent[head, level],
          surround[indent[#, level + 1]& /@ comments1, {line[level + 1]}]
        }],
        Block[{$Toplevel = False},
        Flatten[{
          indent[opener, level + 1],
          surround[indent[#, level + 1]& /@ comments2, {line[level + 1]}],
          indent[vars, level + 1],
          surround[indent[#, level + 1]& /@ comments3, {line[level + 1]}],
          indent[comma1, level + 1],
          {line[level + 1], indent[#, level + 1]}& /@ comments4,
          line[level + 1],
          indent[body, level + 1],
          surround[indent[#, level + 1]& /@ comments5, {line[level + 1]}],
          line[level],
          indent[closer, level]
        }]
        ]
        ,
        data
      ]
    ]
  ]


(*
special casing Function
*)
indent[CallNode[{head:LeafNode[Symbol, "Function" | {FragmentNode[Symbol, "Function", _], ___}, _], trivia1:trivia...}, {
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

    This is a kind of "pre-processing" that undoes previous line breaks that were inserted just because of hitting column limits
    
    -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

    -3 is where LeafNode[xxx, xxx, <||>] is
    *)
    vars = DeleteCases[{varsIn}, LeafNode[Token`Newline, _, _], {-5, -3}];

    CallNode[
      Flatten[{
        indent[head, level],
        indent[#, level]& /@ comments1}]
      ,
      Block[{$Toplevel = False},
      Flatten[{
        indent[GroupNode[GroupSquare,
            {opener} ~Join~
            comments2 ~Join~
            {InfixNode[Comma, vars ~Join~ {rest}, data2]} ~Join~
            {trivia4, closer}, data1], level]
      }]
      ]
      ,
      data
    ]
  ]


(*
special casing Switch

completely redo newlines
*)
indent[CallNode[{tag:LeafNode[Symbol, "Switch" | {FragmentNode[Symbol, "Switch", _], ___}, _], trivia1:trivia...}, {
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
    (* normally the end is -2, but we are working with MOST of the children, so make sure to grab the last element, which is a rator *)
    rators = aggs[[2 ;; (*-2*) ;; 2]];
    ratorsPat = Alternatives @@ rators;
    testsPat = Alternatives @@ tests;
    bodiesPat = Alternatives @@ bodies;

    If[$CurrentStyle["NewlinesInControl"] === Delete,
      (*
      no newlines
      *)
      CallNode[
        Flatten[{
          indent[tag, level + 1],
          indent[#, level + 1]& /@ comments1
        }]
        ,
        Block[{$Toplevel = False},
        Flatten[{
          indent[opener, level + 1],
          indent[#, level + 1]& /@ comments2,
          indent[firstRand, level + 1],
          indent[#, level + 1]& /@ comments3,
          indent[firstRator, level + 1],
          indent[#, level + 1]& /@ comments4,
          Replace[graphs, {
              rator : ratorsPat :> indent[rator, level + 1],
              test:testsPat :> indent[test, level + 1],
              body:bodiesPat :> indent[body, level + 2],
              other_ :> indent[other, level + 1]
            }, {1}],
          indent[#, level + 1]& /@ comments5,
          indent[lastRand, level + 2],
          indent[#, level + 1]& /@ comments6,
          indent[closer, level]
        }]
        ]
        ,
        data
      ]
      ,
      (*
      Redo newlines
      *)
      CallNode[
        Flatten[{
          indent[tag, level + 1],
          surround[indent[#, level + 1]& /@ comments1, {line[level + 1]}]
        }]
        ,
        Block[{$Toplevel = False},
        Flatten[{
          indent[opener, level + 1],
          surround[indent[#, level + 1]& /@ comments2, {line[level + 1]}],
          indent[firstRand, level + 1],
          surround[indent[#, level + 1]& /@ comments3, {line[level + 1]}],
          indent[firstRator, level + 1],
          surround[indent[#, level + 1]& /@ comments4, {line[level + 1]}],
          Replace[graphs, {
              rator : ratorsPat :> indent[rator, level + 1],
              test:testsPat :> {line[level + 1], indent[test, level + 1]},
              body:bodiesPat :> {line[level + 2], indent[body, level + 2], line[level + 1]},
              other_ :> {line[level + 1], indent[other, level + 1]}
            }, {1}],
          {line[level + 1], indent[#, level + 1]}& /@ comments5,
          line[level + 2],
          indent[lastRand, level + 2],
          {line[level + 1], indent[#, level + 1]}& /@ comments6,
          line[level],
          indent[closer, level]
        }]
        ]
        ,
        data
      ]
    ]
  ]


(*
special casing Which

completely redo newlines
*)
indent[CallNode[{tag:LeafNode[Symbol, "Which" | {FragmentNode[Symbol, "Which", _], ___}, _], trivia1:trivia...}, {
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
    (* normally the end is -2, but we are working with MOST of the children, so make sure to grab the last element, which is a rator *)
    rators = aggs[[2 ;; (*-2*) ;; 2]];
    ratorsPat = Alternatives @@ rators;
    testsPat = Alternatives @@ tests;
    bodiesPat = Alternatives @@ bodies;

    If[$CurrentStyle["NewlinesInControl"] === Delete,
      (*
      no newlines
      *)
      CallNode[
        Flatten[{
          indent[tag, level + 1],
          indent[#, level + 1]& /@ comments1
        }]
        ,
        Block[{$Toplevel = False},
        Flatten[{
          indent[opener, level + 1],
          indent[#, level + 1]& /@ comments2, 
          Replace[graphs, {
              rator : ratorsPat :> indent[rator, level + 1],
              test:testsPat :> indent[test, level + 1],
              body:bodiesPat :> indent[body, level + 2],
              other_ :> indent[other, level + 1]
            }, {1}],
          indent[lastRand, level + 2],
          indent[#, level + 1]& /@ comments3,
          indent[closer, level]
        }]
        ]
        ,
        data
      ]
      ,
      (*
      Redo newlines
      *)
      CallNode[
        Flatten[{
          indent[tag, level + 1],
          surround[indent[#, level + 1]& /@ comments1, {line[level + 1]}]
        }]
        ,
        Block[{$Toplevel = False},
        Flatten[{
          indent[opener, level + 1],
          surround[indent[#, level + 1]& /@ comments2, {line[level + 1]}],
          Replace[graphs, {
              rator : ratorsPat :> indent[rator, level + 1],
              test:testsPat :> {line[level + 1], indent[test, level + 1]},
              body:bodiesPat :> {line[level + 2], indent[body, level + 2], line[level + 1]},
              other_ :> {line[level + 1], indent[other, level + 1]}
            }, {1}],
          line[level + 2],
          indent[lastRand, level + 2],
          {line[level + 1], indent[#, level + 1]}& /@ comments3,
          line[level],
          indent[closer, level]
        }]
        ]
        ,
        data
      ]
    ]
  ]


(*
special casing If

completely redo newlines
*)
indent[CallNode[{tag : LeafNode[Symbol, "If" | {FragmentNode[Symbol, "If", _], ___}, _], trivia1 : trivia...}, {
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
      $CurrentStyle["NewlinesInControl"] === Delete,
        condition = SingleLine
      ,
      !empty[comments2],
        condition = RedoNewlines
      ,
      (*
      If[anything, symbol, symbol] => single line
      *)
      MatchQ[aggs, {PatternSequence[LeafNode[Symbol, _, _], LeafNode[Token`Comma, _, _]]..., LeafNode[Symbol, _, _]}],
        condition = SingleLine
      ,
      True,
        condition = RedoNewlines
    ];

    Switch[condition,
      SingleLine,
        CallNode[
          Flatten[{
            indent[tag, level + 1], 
            indent[#, level + 1]& /@ comments1
          }]
          ,
          Block[{$Toplevel = False},
          Flatten[{
            indent[opener, level + 1], 
            indent[#, level + 1]& /@ comments2, 
            indent[firstRand, level + 1], 
            indent[#, level + 1]& /@ comments3, 
            indent[firstRator, level + 1],
            space[],
            Replace[graphs, {
                rator : ratorsPat :> {indent[rator, level + 1], space[]},
                other_ :> indent[other, level + 1]
              }, {1}], 
            indent[#, level]& /@ comments4,
            indent[closer, level]
          }]
          ]
          ,
          data
        ]
      ,
      RedoNewlines,
        CallNode[
            Flatten[{
              indent[tag, level + 1], 
              surround[indent[#, level + 1]& /@ comments1, {line[level + 1]}]
            }]
            ,
            Block[{$Toplevel = False},
            Flatten[{
              indent[opener, level + 1],
              surround[indent[#, level + 1]& /@ comments2, {line[level + 1]}],
              indent[firstRand, level + 1],
              surround[indent[#, level + 1]& /@ comments3, {line[level + 1]}],
              indent[firstRator, level + 1],
              Replace[graphs, {
                  rator : ratorsPat :> {line[level + 1], indent[rator, level + 1]}, 
                  other_ :> {line[level + 1], indent[other, level + 1]}
                }, {1}], 
              surround[indent[#, level + 1]& /@ comments4, {line[level + 1]}],
              line[level],
              indent[closer, level]
            }]
            ]
            ,
            data
        ]
    ]
  ]

(*
special casing For

completely redo newlines
*)
indent[CallNode[{tag : LeafNode[Symbol, "For" | {FragmentNode[Symbol, "For", _], ___}, _], trivia1 : trivia...}, {
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

    If[$CurrentStyle["NewlinesInControl"] === Delete,
      (*
      no newlines
      *)
      CallNode[
        Flatten[{
          indent[tag, level],
          betterRiffle[indent[#, level]& /@ comments1, {space[]}]
        }]
        ,
        Block[{$Toplevel = False},
        Flatten[{
          indent[opener, level],
          betterRiffle[indent[#, level]& /@ comments2, {space[]}],
          indent[rand1, level],
          betterRiffle[indent[#, level]& /@ comments3, {space[]}],
          indent[rator1, level], space[],
          betterRiffle[indent[#, level]& /@ comments4, {space[]}],
          indent[rand2, level],
          betterRiffle[indent[#, level]& /@ comments5, {space[]}],
          indent[rator2, level], space[],
          betterRiffle[indent[#, level]& /@ comments6, {space[]}],
          indent[rand3, level],
          betterRiffle[indent[#, level]& /@ comments7, {space[]}],
          indent[rator3, level], space[],
          betterRiffle[indent[#, level]& /@ comments8, {space[]}],
          indent[rand4, level + 1],
          betterRiffle[indent[#, level + 1]& /@ comments9, {space[]}],
          indent[closer, level]
        }]
        ]
        ,
        data
      ]
      ,
      (*
      Redo newlines
      *)
      CallNode[
        Flatten[{
          indent[tag, level],
          betterRiffle[indent[#, level]& /@ comments1, {space[]}]
        }]
        ,
        Block[{$Toplevel = False},
        Flatten[{
          indent[opener, level],
          betterRiffle[indent[#, level]& /@ comments2, {space[]}],
          indent[rand1, level],
          betterRiffle[indent[#, level]& /@ comments3, {space[]}],
          indent[rator1, level], space[],
          betterRiffle[indent[#, level]& /@ comments4, {space[]}],
          indent[rand2, level],
          betterRiffle[indent[#, level]& /@ comments5, {space[]}],
          indent[rator2, level], space[],
          betterRiffle[indent[#, level]& /@ comments6, {space[]}],
          indent[rand3, level],
          betterRiffle[indent[#, level]& /@ comments7, {space[]}],
          indent[rator3, level], space[],
          betterRiffle[indent[#, level]& /@ comments8, {space[]}],
          line[level + 1],
          indent[rand4, level + 1],
          If[!empty[comments9], line[level + 1], nil[]],
          betterRiffle[indent[#, level + 1]& /@ comments9, {space[]}],
          line[level], 
          indent[closer, level]
        }]
        ]
        ,
        data
      ]
    ]
  ]

indent[CallNode[{tag_, trivia...}, ts_, data_], level_] :=
  CallNode[{indent[tag, level]}
    ,
    Block[{$Toplevel = False},
    indent[#, level]& /@ ts
    ]
    ,
    data
  ]


indent[SyntaxErrorNode[tag_, ts_, data_], level_] :=
  SyntaxErrorNode[tag, indent[#, level]& /@ ts, data]


indent[ContainerNode[tag_, ts_, data_], level_] :=
  Catch[
  Module[{graphs, indented},

    graphs = DeleteCases[ts, ws];

    If[Length[graphs] >= 1 && !MatchQ[graphs[[-1]], nl],
      AppendTo[graphs, LeafNode[Token`Newline, "", <||>]]
    ];

    indented = Flatten[
      indent[#, level]& /@ graphs
    ];

    If[AnyTrue[indented, FailureQ],
      Throw[FirstCase[indented, _?FailureQ]]
    ];

    ContainerNode[
      tag
      ,
      indented
      ,
      data
    ]
  ]]



indent[BoxNode[RowBox, {ts_}, data_], level_] :=
  Module[{graphs},

    graphs = DeleteCases[ts, ws];

    BoxNode[RowBox
      ,
      Flatten[
        indent[#, level]& /@ graphs
      ]
      ,
      data
    ]
  ]

indent[node:BoxNode[_, _, _], level_] :=
  node


indent[args___] := Failure["InternalUnhandled", <| "Function" -> indent, "Args" -> {args} |>]

End[]

EndPackage[]
