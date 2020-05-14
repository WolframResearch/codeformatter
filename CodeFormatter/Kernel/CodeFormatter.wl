BeginPackage["CodeFormatter`"]

(*
Functions
*)
CodeFormat



RemoveLineContinuations

StandardizeNewlines

StandardizeTabs



(*
Options
*)
AirynessLevel



(*
Undocumented functions
*)
CodeFormatCST



Begin["`Private`"]

Needs["CodeFormatter`Utils`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["CodeInspector`Utils`"] (* for replaceTabs and tabReplacementFunc *)


If[PacletFind["Format"] != {},
  Message[General::obspkg, "Format`"]
]



$DefaultAirynessLevel = 1.0

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
Module[{cst, tabWidth},

  tabWidth = OptionValue["TabWidth"];

  cst = CodeConcreteParse[file, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
   ];

  CodeFormatCST[cst, opts]
]]


CodeFormat[str_String, opts:OptionsPattern[]] :=
Module[{cst, tabWidth},

  tabWidth = OptionValue["TabWidth"];

  cst = CodeConcreteParse[str, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
   ];

   CodeFormatCST[cst, opts]
]


CodeFormat[bytes_List, opts:OptionsPattern[]] :=
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

   CodeFormatCST[cst, opts]
]]



Options[CodeFormatCST] = Options[CodeFormat]

CodeFormatCST[cstIn_, opts:OptionsPattern[]] :=
Catch[
Module[{indentationString, cst, newline, tabWidth, indented},

  indentationString = OptionValue["IndentationString"];
  newline = OptionValue["Newline"];
  tabWidth = OptionValue["TabWidth"];

  cst = cstIn;

  cst = RemoveLineContinuations[cst];

  cst = StandardizeNewlines[cst, newline];

  cst = StandardizeTabs[cst, newline, tabWidth];

  cst = IntroduceRowNodes[cst];

  cst = AbstractFormatNodes[cst];

  Block[{blockedIndentationString, blockedNewline},

    blockedIndentationString[] = indentationString;

    blockedNewline[] = newline;

    indented = indent[cst, 0];
  ];

  indented
]]




trivia = LeafNode[Whitespace | Token`Boxes`MultiWhitespace | Token`Comment | Token`Newline, _, _]

ws = LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]

nl = LeafNode[Token`Newline, _, _]

comment = LeafNode[Token`Comment, _, _]

matchNewlineQ = MatchQ[nl]




RemoveLineContinuations::usage = "RemoveLineContinuations[cst] removes line continuations from cst."

(*
Level -5 is where LeafNodes[] are
*)
RemoveLineContinuations[cst_] :=
  Replace[cst, {
    (*
    FIXME: multiline string/comments with extraneous line continuations NOT IMPLEMENTED YET

    keep line continuations INSIDE OF strings and comments: may have been intentionally formatted
    But make sure to remove line continuations outside of the actual token

    Need to do:
      after removing the extraneous line continuations, recalculate the correct Source
      i.e., there may be whitespace after the line continuation that contributes to indentation
      The spacing before the string/comment need to be preserved
    *)
    (*
    LeafNode[tag: String | Token`Comment, s_ /; hasExtraneousLineContinuationQ[s], data_] :>
      Module[{replacedS, recalculatedStartCol},
        replacedS = NestWhile[removeExtraneousLineContinuations, s, hasExtraneousLineContinuationQ];
        recalculatedStartCol = StringCases[replacedS, StartOfString ~~ w:(WhitespaceCharacter ...) ~~ __ :> StringLength[w]][[1]];
        recalculatedStartCol++;
        If[$Debug,
          Print["inside removeLineContinuations"];
          Print["replacedS: ", replacedS];
          Print["recalculatedStartCol: ", recalculatedStartCol];
          Print[];
        ];
        LeafNode[tag,
          replacedS
          ,
          <|Source -> {{Indeterminate, recalculatedStartCol}, {Indeterminate, Indeterminate}}|>
        ]
      ],
    *)
    LeafNode[tag: String | Token`Comment, s_, data_] :>
      LeafNode[tag, s, data]
    ,
    (*
    quickly filter down to LeafNodes that may have line continuations

    remove from all other leafs:
    Integers, Reals, etc.: line continuations are not "intentional"; purely for line breaking
    white space, etc.: line continuations serve no purpose
    *)
    LeafNode[tag_, s_ /; hasLineContinuationQ[s], data_] :>
      LeafNode[tag, 
        NestWhile[removeLineContinuations, s, hasLineContinuationQ]
        ,
        data
      ]
  }, {-5}]


(*
extraneous line continuations are at the start or at the end
*)
hasExtraneousLineContinuationQ[s_String] :=
  StringMatchQ[s, 
    (StartOfString ~~ "\\" ~~ ("\r\n" | "\n" | "\r") ~~ __) |
    (__ ~~ "\\" ~~ ("\r\n" | "\n" | "\r") ~~ EndOfString)]

(*
do not remove trailing whitespace from extraneous line continuations
the whitesace indicates intentional formatting in tokens where it matters: strings and comments
*)
removeExtraneousLineContinuations[s_String] :=
  StringReplace[s, {
    StartOfString ~~ "\\" ~~ ("\r\n" | "\n" | "\r") -> "",
    "\\" ~~ ("\r\n" | "\n" | "\r") ~~ EndOfString -> ""}
  ]


hasLineContinuationQ[s_String] :=
  StringContainsQ[s, "\\" ~~ ("\r\n" | "\n" | "\r") ~~ WhitespaceCharacter...]

(*
Remove both extraneous and internal line continuations
*)
removeLineContinuations[s_String] :=
  StringReplace[s, {"\\" ~~ ("\r\n" | "\n" | "\r") ~~ WhitespaceCharacter... -> ""}]



StandardizeNewlines::usage = "StandardizeNewlines[cst, newline] standardizes the newlines in cst."

(*
Level -5 is where LeafNodes[] are

Do not need to recalculate Source data: replacing 1 Source character with 1 Source character
*)
StandardizeNewlines[cst_, newline_String] :=
  Replace[cst, {
    LeafNode[tag : String | Token`Comment, s_, data_] :>
     LeafNode[tag, 
      StringReplace[s, {"\r\n" -> newline, "\n" -> newline, "\r" -> newline}], data]}, {-5}]



StandardizeTabs::usage = "StandardizeTabs[cst, newline, tabWidth] standardizes tabs in cst."

(*
Level -5 is where LeafNodes[] are

Do not need to recalculate Source data: Source columns were already calculated accounting for tab stops
*)
StandardizeTabs[cst_, newline_String, tabWidth_Integer] :=
  Replace[cst, {
    LeafNode[Whitespace, "\t", data_] :> 
      LeafNode[Whitespace, tabReplacementFunc[data[[Key[Source], 1, 2]], tabWidth], data],
    LeafNode[tag : String | Token`Comment, s_ /; StringContainsQ[s, "\t"], data_] :> 
      LeafNode[tag, replaceTabs[s, data[[Key[Source], 1, 2]], newline, tabWidth], data]}, {-5}]



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
    
    commentPoss = Position[cst, LeafNode[Token`Comment, _, _]];
    
    isCommentOrWhitespacePos[pos1_] :=
      ListQ[pos1] && MatchQ[Extract[cst, pos1], LeafNode[Token`Comment | Whitespace, _, _]];

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
        last[[2]] = last[[2]] <> StringJoin[ToSourceCharacterString /@ extracted[[2;;]]];
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

abstractFormatNodes[head_[tag_, ts_, data_]] := head[tag, abstractFormatNodes /@ ts, data]







cat[docs___] :=
  StringJoin[docs]

line[level_] := blockedNewline[] <> StringJoin[Table[blockedIndentationString[], level]]

space[] = " "

tab[] = "\t"

nil[] = ""



indent[LeafNode[Token`Newline, s_, _], level_] :=
  line[level]

(*
Special case multiline strings

Must preserve the original number of columns preceding the string

We do this by inserting a newline, then the original number of spaces.

A line continuation is inserted to help with semantics of inserting a newline (this may not be needed)
Also, the line continuation helps to communicate the "separateness" of the string
*)
indent[LeafNode[String, s_ /; StringContainsQ[s, blockedNewline[]], data_], level_] :=
Module[{origSpaces},
  origSpaces = data[[Key[Source], 1, 2]]-1;
  cat["\\", blockedNewline[], Table[" ", origSpaces], s]
]

(*
Special case multiline comments

It is ok to change the internal indentation of comments
*)
indent[LeafNode[Token`Comment, s_ /; StringContainsQ[s, blockedNewline[]], data_], level_] :=
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
  StringReplace[replaced, blockedNewline[] -> blockedNewline[] <> StringJoin[Table[blockedIndentationString[], level]]]
]


(*
All other leafs: integers, reals, symbols, 1D strings, 1D comments, etc.
*)
indent[LeafNode[_, s_, _], level_] :=
  s


indent[ErrorNode[_, s_, _], level_] :=
  s


indent[CompoundNode[_, {rand1_, rand2_}, _], level_] :=
  cat[indent[rand1, level], indent[rand2, level]]






(*
"FrontEnd"-style:
indentPostfixRator[Function][rator_, level_] :=
  cat[space[], indent[rator, level]]
*)

indentPostfixRator[_][rator_, level_] :=
  indent[rator, level]



indent[PrefixNode[_, {rator_, trivia..., rand_}, _], level_] :=
  cat[indent[rator, level], indent[rand, level]]

indent[PostfixNode[tag_, {rand_, trivia..., rator_}, _], level_] :=
  cat[indent[rand, level], indentPostfixRator[tag][rator, level]]



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

  Which[
    shouldStayOnSingleLine,
      cat[Replace[graphs, {
          lastRator /; MatchQ[lastRand, LeafNode[Token`Fake`ImplicitNull, _, _]] :> indent[lastRator, level], 
          rator : ratorsPat :> cat[indent[rator, level], space[]],
          rand : randsPat :> indent[rand, level], 
          comm_ :> indent[comm, level]
        }, {1}]
      ]
    ,
    True,
      cat[Replace[graphs, {
          lastRator /; MatchQ[lastRand, LeafNode[Token`Fake`ImplicitNull, _, _]] :> indent[lastRator, level], 
          rator : ratorsPat :> cat[indent[rator, level], line[level]], 
          rand : randsPat :> indent[rand, level], 
          comm_ :> cat[indent[comm, level], line[level]]
        }, {1}]
      ]
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
indentInfixRator[Comma][rator_, level_] :=
  cat[indent[rator, level], space[]]

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
  cat[space[], indent[rator, level], space[]]

indentInfixRatorNoTrailingSpace[_][rator_, level_] :=
  cat[space[], indent[rator, level]]



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
indent[(BinaryNode|InfixNode|TernaryNode|QuaternaryNode)[tag_, ts_, _], level_] :=
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

    If[Length[split] == 1,
      (*
      There were no newline tokens
      *)
      Throw[
        cat[
          Map[
            Function[{grouped},
              cat[Replace[grouped, {
                rator : ratorsPat :> indentInfixRator[tag][rator, level], 
                randOrComment_ :> indent[randOrComment, level]
              }, {1}]]
            ]
            ,
            split
          ]
        ]
      ]
    ];

    With[{newLevel = indentIncrement[tag, level]},
    cat[
    Riffle[
      Map[
        (*
        grouped is a list of tokens with no newline tokens
        *)
        Function[{grouped},
          cat[
            cat[Replace[Most[grouped], {
              rator : ratorsPat :> indentInfixRator[tag][rator, newLevel], 
              randOrComment_ :> indent[randOrComment, newLevel]
            }, {1}]]
            ,
            cat[Replace[{Last[grouped]}, {
              (* do not insert space after rator if immediately followed by newline *)
              rator : ratorsPat :> indentInfixRatorNoTrailingSpace[tag][rator, newLevel], 
              randOrComment_ :> indent[randOrComment, newLevel]
            }, {1}]]
          ]
        ]
        ,
        split
      ]
      ,
      line[newLevel]
    ]]
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
    }, _], level_] :=
  Module[{triviaAggs, triviaGraphs, condition},

    triviaAggs = DeleteCases[{trivia}, ws];

    triviaGraphs = DeleteCases[{trivia}, ws | nl];

    Which[
      !FreeQ[triviaAggs, LeafNode[Token`Newline, _, _]],
        condition = MultiLineEnum
      ,
      True,
        condition = SingleLineEnum
    ];

    Which[
      condition === MultiLineEnum,
      (*
      if multiline, then keep as multiline, but redo newlines
      *)
      cat[
        indent[opener, level], 
        indent[#, level + 1]& /@ triviaGraphs,
        line[level],
        indent[closer, level]
      ]
      ,
      condition === SingleLineEnum,
      (*
      if will NOT make multiline, then keep everything on 1 line,
      and do not indent
      *)
      cat[
        indent[opener, level], 
        indent[#, level]& /@ triviaGraphs, 
        indent[closer, level]
      ]
    ]
  ]

indent[GroupNode[tag_, {
      opener_, 
      trivia1 : trivia...,
      ts:PatternSequence[Except[trivia], ___, Except[trivia]] | Except[trivia],
      trivia2 : trivia...,
      closer_
    }, _], level_] :=
  Module[{aggs, trivia1Aggs, trivia2Aggs, trivia1Graphs, trivia2Graphs, condition, x},

    trivia1Aggs = DeleteCases[{trivia1}, ws];
    aggs = DeleteCases[{ts}, ws];
    trivia2Aggs = DeleteCases[{trivia2}, ws];

    trivia1Graphs = DeleteCases[{trivia1}, ws | nl];
    trivia2Graphs = DeleteCases[{trivia2}, ws | nl];

    Which[
      !FreeQ[trivia1Aggs, LeafNode[Token`Newline, _, _]],
        condition = MultiLineEnum
      ,
      FreeQ[aggs, LeafNode[Token`Newline, _, _]],
        condition = SingleLineEnum
      ,
      MatchQ[aggs, {GroupNode[_, _, _]}],
        condition = SingleLineEnum
      ,
      MatchQ[aggs, {InfixNode[Comma, {GroupNode[_, _, _], ___}, _]}] && FreeQ[trivia2Aggs, LeafNode[Token`Newline, _, _]],
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
      cat[
        indent[opener, level], 
        indent[#, level]& /@ trivia1Graphs, 
        indent[#, level + x]& /@ aggs, 
        indent[#, level]& /@ trivia2Graphs,
        line[level],
        indent[closer, level]
      ]
      ,
      condition === MultiLineEnum,
      (*
      if multiline, then keep as multiline, but redo newlines
      *)
      cat[
        indent[opener, level], 
        indent[#, level + 1]& /@ trivia1Graphs,
        line[level + 1],
        indent[#, level + 1]& /@ aggs, 
        indent[#, level]& /@ trivia2Graphs,
        line[level],
        indent[closer, level]
      ]
      ,
      condition === SingleLineEnum,
      (*
      if will NOT make multiline, then keep everything on 1 line,
      and do not indent
      *)
      cat[
        indent[opener, level], 
        indent[#, level]& /@ trivia1Graphs, 
        indent[#, level]& /@ aggs, 
        indent[#, level]& /@ trivia2Graphs, 
        indent[closer, level]
      ]
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
    }, _], level_] :=
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

    cat[
      indent[head, level],
      indent[#, level + 1]& /@ comments1,
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

    cat[
      indent[head, level],
      indent[#, level]& /@ comments1,
      indent[GroupNode[GroupSquare,
          {opener} ~Join~
          comments2 ~Join~
          {InfixNode[Comma, vars ~Join~ {rest}, data2]} ~Join~
          {trivia4, closer}, data1], level]
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
    }, _], level_] :=
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
    cat[
      indent[tag, level + 1],
      indent[#, level + 1]& /@ comments1,
      indent[opener, level + 1],
      indent[#, level + 1]& /@ comments2, 
      indent[firstRand, level + 1],
      indent[#, level + 1]& /@ comments3,
      indent[firstRator, level + 1],
      indent[#, level + 1]& /@ comments4,
      Replace[graphs, {
          rator : ratorsPat :> indent[rator, level + 1],
          test:testsPat :> cat[line[level + 1], indent[test, level + 1]],
          body:bodiesPat :> cat[line[level + 2], indent[body, level + 2], line[level + 1]],
          other_ :> indent[other, level + 1]
        }, {1}],
      line[level + 2],
      indent[#, level + 1]& /@ comments5,
      indent[lastRand, level + 2],
      indent[#, level + 1]& /@ comments6,
      line[level],
      indent[closer, level]
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
    }, _], level_] :=
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
    cat[
      indent[tag, level + 1],
      indent[#, level + 1]& /@ comments1,
      indent[opener, level + 1],
      indent[#, level + 1]& /@ comments2, 
      Replace[graphs, {
          rator : ratorsPat :> indent[rator, level + 1],
          test:testsPat :> cat[line[level + 1], indent[test, level + 1]],
          body:bodiesPat :> cat[line[level + 2], indent[body, level + 2], line[level + 1]],
          other_ :> indent[other, level + 1]
        }, {1}],
      line[level + 2],
      indent[lastRand, level + 2],
      indent[#, level + 1]& /@ comments3,
      line[level],
      indent[closer, level]
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
    }, _], level_] :=
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
      indent[tag, level + 1], 
      indent[#, level + 1]& /@ comments1, 
      indent[opener, level + 1], 
      indent[#, level + 1]& /@ comments2, 
      indent[firstRand, level + 1], 
      indent[#, level + 1]& /@ comments3, 
      indent[firstRator, level + 1], 
      line[level + 1], 
      Replace[graphs, {
          rator : ratorsPat :> cat[line[level + 1], indent[rator, level + 1], line[level + 1]], 
          other_ :> indent[other, level + 1]
        }, {1}], 
      indent[#, level]& /@ comments4, 
      line[level], 
      indent[closer, level]
    ]
  ]

indent[CallNode[{tag_, trivia...}, ts_, _], level_] :=
  cat[indent[tag, level], indent[#, level]& /@ ts]


indent[SyntaxErrorNode[_, ts_, _], level_] :=
  cat[indent[#, level]& /@ ts]


(*
special casing ContainerNode

redo newlines
*)
indent[ContainerNode[_, ts_, _], level_] :=
  Module[{graphs},
    graphs = DeleteCases[ts, ws | nl];
    cat[
      cat[indent[#, level], line[level], line[level]]& /@ graphs
    ]
  ]


End[]

EndPackage[]
