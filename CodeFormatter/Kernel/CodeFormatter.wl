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




$DefaultAirynessLevel




Begin["`Private`"]

Needs["CodeFormatter`Utils`"]

Needs["CodeParser`"]
Needs["CodeParser`Folds`"] (* for linearize *)
Needs["CodeParser`Utils`"]


If[PacletFind["Format"] != {},
  Message[General::obspkg, "Format`"]
]



$DefaultAirynessLevel = 0

$DefaultIndentationString := StringRepeat[" ", $DefaultTabWidth]

$DefaultNewline = "\n"

$DefaultTabWidth = 4

$DefaultLineWidth = 120



CodeFormat::usage = "CodeFormat[code] returns a string of formatted WL code."

Options[CodeFormat] = {
  AirynessLevel :> $DefaultAirynessLevel,
  "IndentationString" :> $DefaultIndentationString,
  "Newline" :> $DefaultNewline,
  "TabWidth" :> $DefaultTabWidth,
  "LineWidth" :> $DefaultLineWidth
}


CodeFormat[file_File, opts:OptionsPattern[]] :=
Catch[
Module[{cst, tabWidth, formattedStr, agg, cst2, agg2, aggToCompare, agg2ToCompare, newline},

  tabWidth = OptionValue["TabWidth"];
  newline = OptionValue["Newline"];

  cst = CodeConcreteParse[file, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
  ];

  formattedStr = CodeFormatCST[cst, opts];

  If[$DisableSanityChecking,
    Throw[formattedStr]
  ];

  agg = CodeParser`Abstract`Aggregate[cst];
  agg = normalizeTokens[agg, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  cst2 = CodeConcreteParse[formattedStr];
  agg2 = CodeParser`Abstract`Aggregate[cst2];
  agg2 = normalizeTokens[agg2, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  agg2[[1]] = File;

  aggToCompare = agg /. _Association -> <||>;
  agg2ToCompare = agg2 /. _Association -> <||>;

  If[aggToCompare =!= agg2ToCompare,
    Throw[Failure["SanityCheckFailed", <||>]]
  ];

  formattedStr
]]


CodeFormat[str_String, opts:OptionsPattern[]] :=
Catch[
Module[{cst, tabWidth, formattedStr, agg, cst2, agg2, aggToCompare, agg2ToCompare},

  tabWidth = OptionValue["TabWidth"];
  newline = OptionValue["Newline"];

  cst = CodeConcreteParse[str, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
  ];

  formattedStr = CodeFormatCST[cst, opts];

  formattedStr = StringTrim[formattedStr];

  If[$DisableSanityChecking,
    Throw[formattedStr]
  ];

  agg = CodeParser`Abstract`Aggregate[cst];
  agg = normalizeTokens[agg, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  cst2 = CodeConcreteParse[formattedStr];
  agg2 = CodeParser`Abstract`Aggregate[cst2];
  agg2 = normalizeTokens[agg2, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  aggToCompare = agg /. _Association -> <||>;
  agg2ToCompare = agg2 /. _Association -> <||>;

  If[aggToCompare =!= agg2ToCompare,
    If[$Debug,
      Print["aggToCompare: ", aggToCompare];
      Print["agg2ToCompare: ", agg2ToCompare];
    ];
    Throw[Failure["SanityCheckFailed", <||>]]
  ];

  formattedStr
]]


CodeFormat[bytes_List, opts:OptionsPattern[]] :=
Catch[
Module[{cst, tabWidth, formattedStr, agg, cst2, agg2, aggToCompare, agg2ToCompare, newline},

  tabWidth = OptionValue["TabWidth"];
  newline = OptionValue["Newline"];

  If[bytes == {},
    Throw[""]
  ];

  cst = CodeConcreteParse[bytes, "TabWidth" -> tabWidth];

  If[FailureQ[cst],
    Throw[cst]
  ];

  formattedStr = CodeFormatCST[cst, opts];

  formattedStr = StringTrim[formattedStr];

  If[$DisableSanityChecking,
    Throw[formattedStr]
  ];

  agg = CodeParser`Abstract`Aggregate[cst];
  agg = normalizeTokens[agg, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  cst2 = CodeConcreteParse[formattedStr];
  agg2 = CodeParser`Abstract`Aggregate[cst2];
  agg2 = normalizeTokens[agg2, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth];

  agg2[[1]] = Byte;

  aggToCompare = agg /. _Association -> <||>;
  agg2ToCompare = agg2 /. _Association -> <||>;

  If[aggToCompare =!= agg2ToCompare,
    Throw[Failure["SanityCheckFailed", <||>]]
  ];

  formattedStr
]]



Options[CodeFormatCST] = Options[CodeFormat]

CodeFormatCST[cstIn_, opts:OptionsPattern[]] :=
Catch[
Module[{indentationString, cst, newline, tabWidth, indented, airyness, formattedStr, merged,
  linearized, strs, spaced, breaked, lineWidth},

  indentationString = OptionValue["IndentationString"];
  newline = OptionValue["Newline"];
  tabWidth = OptionValue["TabWidth"];
  airyness = OptionValue[AirynessLevel];
  lineWidth = OptionValue["LineWidth"];

  cst = cstIn;

  If[$Debug,
    Print["CodeFormatCST: ", cst];
  ];

  Block[{$CurrentIndentationString, $CurrentNewline, $CurrentAiryness},

    $CurrentIndentationString = indentationString;

    $CurrentNewline = newline;

    $CurrentAiryness = airyness;

    (*
    Must be before StandardizeEmbeddedNewlines
    depends on "EmbeddedNewlines" data
    *)
    cst = RemoveSimpleLineContinuations[cst];

    If[$Debug,
      Print["after RemoveSimpleLineContinuations: ", cst];
    ];

    cst = RemoveComplexLineContinuations[cst];

    If[$Debug,
      Print["after RemoveComplexLineContinuations: ", cst];
    ];

    cst = RemoveRemainingSimpleLineContinuations[cst];

    If[$Debug,
      Print["after RemoveRemainingSimpleLineContinuations: ", cst];
    ];

    (*
    Post condition: "EmbeddedNewlines" data is removed
    *)
    cst = StandardizeEmbeddedNewlines[cst, newline];

    If[$Debug,
      Print["after StandardizeEmbeddedNewlines: ", cst];
    ];

    cst = StandardizeEmbeddedTabs[cst, newline, tabWidth];

    If[$Debug,
      Print["after StandardizeEmbeddedTabs: ", cst];
    ];

    cst = Fragmentize[cst];

    If[$Debug,
      Print["after Fragmentize: ", cst];
    ];

    cst = IntroduceRowNodes[cst];

    If[$Debug,
      Print["after IntroduceRowNodes: ", cst];
    ];
    
    cst = AbstractFormatNodes[cst];

    If[$Debug,
      Print["after AbstractFormatNodes: ", cst];
    ];

    indented = indent[cst, 0];

    If[$Debug,
      Print["after indent: ", indented];
    ];

    linearized = linearize[indented];

    If[$Debug,
      Print["after linearize: ", linearized];
    ];

    merged = mergeLineContinuations[linearized];

    If[$Debug,
      Print["after mergeLineContinuations: ", merged];
    ];

    spaced = insertNecessarySpaces[merged];

    If[$Debug,
      Print["after insertNecessarySpaces: ", spaced];
    ];

    breaked = breakLines[spaced, lineWidth];

    If[$Debug,
      Print["after breaked: ", breaked];
    ];

    strs = breaked[[All, 2]];

    formattedStr = StringJoin[strs];

    formattedStr
  ]
]]




trivia = LeafNode[Whitespace | Token`Boxes`MultiWhitespace | Token`Comment | Token`Newline, _, _]

ws = LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]

nl = LeafNode[Token`Newline, _, _]

comment = LeafNode[Token`Comment, _, _]

matchNewlineQ = MatchQ[nl]

matchCommentFragmentNewlineQ := MatchQ[FragmentNode[Token`Comment, $CurrentNewline, _]]




RemoveSimpleLineContinuations::usage = "RemoveSimpleLineContinuations[cst] removes simple line continuations from cst."

RemoveSimpleLineContinuations[cstIn_] :=
  Module[{data, cst, tokStartLocs, simpleLineContinuations, grouped, poss, tuples, mapSpecs,
    embeddedNewlineStartLocs, extracted, complexLineContinuations},

    cst = cstIn;

    data = cst[[3]];

    If[KeyExistsQ[data, "SimpleLineContinuations"],

      (*
      -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

      -3 is where LeafNode[xxx, xxx, <||>] is

      There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
      poss = Cases[poss, {___Integer}];

      extracted = Extract[cst, poss];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ extracted;

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];



      (*
      Filter out multiline strings and multiline comments: they are not simple
      *)
      If[KeyExistsQ[data, "EmbeddedNewlines"],

        embeddedNewlineStartLocs = data["EmbeddedNewlines"];

        KeyDropFrom[grouped, embeddedNewlineStartLocs]
      ];

      If[KeyExistsQ[data, "ComplexLineContinuations"],

        complexLineContinuations = data["ComplexLineContinuations"];

        KeyDropFrom[grouped, complexLineContinuations]
      ];



      simpleLineContinuations = data["SimpleLineContinuations"];

      mapSpecs = Map[
        Function[{contLoc},

          tuples = grouped[contLoc];

          (*
          The token associated with this location may have been processed away and now missing
          *)
          If[!MissingQ[tuples],
            tuples
            ,
            Nothing
          ]
        ]
        ,
        simpleLineContinuations
      ];

      mapSpecs = Flatten[mapSpecs, 1];

      (*
      There may be more than 1 simple line continuation, so use FixedPoint
      *)
      cst = MapAt[FixedPoint[removeSimpleLineContinuation, #]&, cst, mapSpecs[[All, 2]]];

      If[empty[simpleLineContinuations],
        KeyDropFrom[data, "SimpleLineContinuations"]
        ,
        data["SimpleLineContinuations"] = simpleLineContinuations
      ];

      cst[[3]] = data;
    ];

    cst
  ]

RemoveComplexLineContinuations::usage = "RemoveComplexLineContinuations[cst] removes complex line continuations from cst."

RemoveComplexLineContinuations[cstIn_] :=
  Module[{data, cst, tokStartLocs, grouped, poss, tuples, mapSpecs,
    extracted, complexLineContinuations},

    cst = cstIn;

    data = cst[[3]];

    If[KeyExistsQ[data, "ComplexLineContinuations"],

      (*
      -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

      -3 is where LeafNode[xxx, xxx, <||>] is

      There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
      poss = Cases[poss, {___Integer}];

      extracted = Extract[cst, poss];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ extracted;

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

      complexLineContinuations = data["ComplexLineContinuations"];

      mapSpecs = Map[
        Function[{contLoc},

          tuples = grouped[contLoc];

          (*
          The token associated with this location may have been processed away and now missing
          *)
          If[!MissingQ[tuples],
            tuples
            ,
            Nothing
          ]
        ]
        ,
        complexLineContinuations
      ];

      mapSpecs = Flatten[mapSpecs, 1];

      cst = MapAt[removeComplexLineContinuations, cst, mapSpecs[[All, 2]]];

      KeyDropFrom[data, "ComplexLineContinuations"];

      cst[[3]] = data;
    ];

    cst
  ]

(*
There may be "simple line continuations" that were left behind because they belonged to a multiline string or comment

Ex:

x + \
 "abc
 def"

We remove the simple line continuation here, but we DO NOT remove the preceding whitespace

Note: we only care about the LAST preceding whitespace

Ex:

x + \
 \
   "abc
   def"

the last preceding whitespace is 3 spaces, so keep that

Also recompute the Source of the multiline token to reflect the new whitespace start

Need to recompute Source because it is used later
*)
RemoveRemainingSimpleLineContinuations::usage =
  "RemoveRemainingSimpleLineContinuations[cst] removes simple line continuations from cst."

RemoveRemainingSimpleLineContinuations[cstIn_] :=
  Module[{data, cst, tokStartLocs, simpleLineContinuations, grouped, poss, tuples, mapSpecs, extracted},

    cst = cstIn;

    data = cst[[3]];

    If[KeyExistsQ[data, "SimpleLineContinuations"],

      (*
      -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

      -3 is where LeafNode[xxx, xxx, <||>] is

      There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
      poss = Cases[poss, {___Integer}];

      extracted = Extract[cst, poss];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ extracted;

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

      simpleLineContinuations = data["SimpleLineContinuations"];

      mapSpecs = Map[
        Function[{contLoc},

          tuples = grouped[contLoc];

          (*
          The token associated with this location may have been processed away and now missing
          *)
          If[!MissingQ[tuples],
            tuples
            ,
            Nothing
          ]
        ]
        ,
        simpleLineContinuations
      ];

      mapSpecs = Flatten[mapSpecs, 1];

      (*
      There may be more than 1 simple line continuation, so use FixedPoint
      *)
      cst = MapAt[FixedPoint[removeRemainingSimpleLineContinuation, #]&, cst, mapSpecs[[All, 2]]];

      KeyDropFrom[data, "SimpleLineContinuations"];

      cst[[3]] = data;
    ];

    cst
  ]


StandardizeEmbeddedNewlines::usage = "StandardizeEmbeddedNewlines[cst, newline] standardizes the newlines in cst."

StandardizeEmbeddedNewlines[cstIn_, newline_String] :=
  Module[{cst, data, embeddedNewlines, mapSpecs, tuples, poss, tokStartLocs, grouped},

    cst = cstIn;

    data = cst[[3]];

    If[KeyExistsQ[data, "EmbeddedNewlines"],

      (*
      -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

      -3 is where LeafNode[xxx, xxx, <||>] is

      There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
      poss = Cases[poss, {___Integer}];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ Extract[cst, poss];

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

      embeddedNewlines = data["EmbeddedNewlines"];

      mapSpecs = Map[
        Function[{newlineLoc},

          tuples = grouped[newlineLoc];

          (*
          The token associated with this location may have been abstracted away and now missing
          *)
          If[!MissingQ[tuples],
            tuples
            ,
            Nothing
          ]
        ]
        ,
        embeddedNewlines
      ];

      mapSpecs = Flatten[mapSpecs, 1];

      cst = MapAt[convertEmbeddedNewlines[#, "FormatOnly" -> True, "Newline" -> newline]&, cst, mapSpecs[[All, 2]]];

      KeyDropFrom[data, "EmbeddedNewlines"];

      cst[[3]] = data;
    ];

    cst
  ]



StandardizeEmbeddedTabs::usage = "StandardizeEmbeddedTabs[cst, newline, tabWidth] standardizes tabs in cst."

StandardizeEmbeddedTabs[cstIn_, newline_String, tabWidth_Integer] :=
  Module[{cst, data, embeddedTabs, mapSpecs, tuples, poss, tokStartLocs, grouped},

    cst = cstIn;

    data = cst[[3]];

    If[KeyExistsQ[data, "EmbeddedTabs"],

      (*
      -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

      -3 is where LeafNode[xxx, xxx, <||>] is

      There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those

      Line continuations in multiline strings and multiline comments will be handled later
      *)
      poss = Position[cst, LeafNode[_, _, _], {-5, -3}];
      poss = Cases[poss, {___Integer}];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ Extract[cst, poss];

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

      embeddedTabs = data["EmbeddedTabs"];

      mapSpecs = Map[
        Function[{newlineLoc},

          tuples = grouped[newlineLoc];

          (*
          The token associated with this location may have been abstracted away and now missing
          *)
          If[!MissingQ[tuples],
            tuples
            ,
            Nothing
          ]
        ]
        ,
        embeddedTabs
      ];

      mapSpecs = Flatten[mapSpecs, 1];

      cst = MapAt[convertEmbeddedTabs[#, "FormatOnly" -> True, "Newline" -> newline, "TabWidth" -> tabWidth]&, cst, mapSpecs[[All, 2]]];

      KeyDropFrom[data, "EmbeddedTabs"];

      cst[[3]] = data;
    ];

    cst
  ]


(*
Fragmentize leaf nodes

Fragmentizing is converting:

LeafNode[String, "abc", <||>]

into

LeafNode[String, {FragmentNode[String, "abc", <||>]}, <||>]

We do NOT want to fragmentize ALL LeafNodes, that would not be intuitive

Actual leaves will be fragmentized in various ways:

overlong leafs split on line breaks

multiline strings have temporary line continuations introduced

multiline comments have temporary line continuations introduced

others?

*)
Fragmentize[cstIn_] :=
  Module[{poss, cst},

    cst = cstIn;

    (*
    Special case multiline strings and multiline comments with LineColumn convention

    Must preserve the original number of columns preceding the string

    We do this by inserting a newline, then the original number of spaces.

    A line continuation is inserted to help with semantics of inserting a newline (this may not ultimately be needed)
    Also, the line continuation helps to communicate the "separateness" of the string or comment
    *)
    poss = Position[cst, LeafNode[String | Token`Comment, s_ /; StringContainsQ[s, $CurrentNewline], KeyValuePattern[Source -> {{_, _}, {_, _}}]]];

    cst = MapAt[fragmentizeMultilineLeafNode, cst, poss];

    (*
    Now also fragmentize the remaining comments
    *)
    poss = Position[cst, LeafNode[Token`Comment, _String, _]];

    cst = MapAt[fragmentizeComment, cst, poss];

    cst
  ]

fragmentizeMultilineLeafNode[LeafNode[String, s_, data_]] :=
Module[{origSpaces},
  origSpaces = data[[Key[Source], 1, 2]]-1;
  LeafNode[String,
    Flatten[{
      FragmentNode[String, "\\" <> $CurrentNewline, <||>],
      Table[FragmentNode[String, " ", <||>], origSpaces],
      Riffle[FragmentNode[String, #, <||>]& /@ StringSplit[s, $CurrentNewline, All], FragmentNode[String, $CurrentNewline, <||>]]
    }], <|data, "InsertedFragmentNodes" -> 1 + origSpaces|>]
]

(*
Make sure to fragmentize ( * and * ) here
*)
fragmentizeMultilineLeafNode[LeafNode[Token`Comment, s_, data_]] :=
Module[{origSpaces},
  origSpaces = data[[Key[Source], 1, 2]]-1;
  LeafNode[Token`Comment,
    Flatten[{
      FragmentNode[Token`Comment, "\\" <> $CurrentNewline, <||>],
      Table[FragmentNode[Token`Comment, " ", <||>], origSpaces],
      FragmentNode[Token`Comment, "(*", <||>],
      Riffle[FragmentNode[Token`Comment, #, <||>]& /@ StringSplit[StringTake[s, {3, -3}], $CurrentNewline, All], FragmentNode[Token`Comment, $CurrentNewline, <||>]],
      FragmentNode[Token`Comment, "*)", <||>]
    }], <|data, "InsertedFragmentNodes" -> 1 + origSpaces|>]
]

fragmentizeComment[LeafNode[Token`Comment, s_, data_]] :=
  LeafNode[Token`Comment,
    FragmentNode[Token`Comment, #, <||>]& /@ ({"(*"} ~Join~ {StringTake[s, {3, -3}]} ~Join~ {"*)"})
    ,
    data
  ]



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
    
    commentPoss = Position[cst, LeafNode[Token`Comment, _, _] | GroupNode[Comment, _, _]];
    
    isCommentOrWhitespacePos[pos1_] :=
      ListQ[pos1] && MatchQ[Extract[cst, pos1], LeafNode[Token`Comment | Whitespace | Token`Boxes`MultiWhitespace, _, _] | GroupNode[Comment, _, _]];

    ranges = Function[{commentPos},
      Reverse[NestWhileList[ Join[Most[#], {Last[#] - 1}]&, commentPos, (Last[#] >= 1 && isCommentOrWhitespacePos[#])&]]
    ] /@ commentPoss;

    If[$Debug2,
      Print["ranges: ", ranges];
    ];

    ranges = DeleteCases[ranges, {{_, 0}, ___}];

    If[$Debug2,
      Print["ranges: ", ranges];
    ];

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

    If[$Debug2,
      Print["ranges: ", ranges];
    ];

    (*
    if comments are on their own newlines, then do not introduce a RowNode
    *)
    commentsOnNewlinesOrStartOfFile = Select[ranges, MatchQ[Extract[cst, #[[1]]], LeafNode[Token`Newline, _, _]]&];

    ranges = Complement[ranges, commentsOnNewlinesOrStartOfFile];

    If[$Debug2,
      Print["ranges: ", ranges];
    ];

    (*
    Complement[] sorts in standard order (where smaller ranges come before larger ranges, regardless of their positions),
    which is not what we want

    Resort into lex order
    *)
    ranges = lexSort[ranges];

    If[$Debug2,
      Print["ranges: ", ranges];
    ];

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

        If[$Debug,
          Print["extracted: ", extracted];
        ];

        If[ListQ[last[[2]]],
          (*
          May have already been fragmentized
          *)
          last[[2]] = last[[2]] ~Join~
            Flatten[extractFragmentNodes /@ extracted[[2;;]]];
          ,
          last[[2]] = {FragmentNode[last[[1]], last[[2]], <||>]} ~Join~
            Flatten[extractFragmentNodes /@ extracted[[2;;]]];
        ];

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

extractFragmentNodes[LeafNode[_, fs_List, _]] := fs

extractFragmentNodes[LeafNode[tag_, s_String, data_]] := {FragmentNode[tag, s, data]}


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
      rhs_}, _]}, data_]] /; DeclarationName[lhs1] === DeclarationName[lhs2] :=
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
      rhs_}, _]}, data_]] /; DeclarationName[lhs2] === DeclarationName[lhs3] :=
  QuaternaryNode[MemoizedTagSetDelayed, abstractFormatNodes /@ {lhs1, trivia1, op1, trivia2, lhs2, trivia3, op2, trivia4, lhs3, trivia5, op3, trivia6, rhs}, data]

abstractFormatNodes[node_LeafNode] := node

abstractFormatNodes[CallNode[head_, ts_, data_]] := CallNode[abstractFormatNodes /@ head, abstractFormatNodes /@ ts, data]

abstractFormatNodes[BoxNode[RowBox, {ts_}, data_]] := BoxNode[RowBox, {abstractFormatNodes /@ ts}, data]

abstractFormatNodes[node:CodeNode[_, _, _]] := node

abstractFormatNodes[head_[tag_, ts_, data_]] := head[tag, abstractFormatNodes /@ ts, data]



line[level_] :=
  {LeafNode[Token`Newline, $CurrentNewline, <||>]} ~Join~
    Table[LeafNode[Token`Whitespace, #, <||>]& /@ Characters[$CurrentIndentationString], level]

space[] = LeafNode[Token`Whitespace, " ", <||>]

tab[] = LeafNode[Token`Whitespace, "\t", <||>]

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
  
  origSpaces = data[[Key[Source], 1, 2]]-1;

  If[$Debug,
    Print["origSpaces: ", origSpaces //InputForm];
  ];

  inserted = data["InsertedFragmentNodes"];

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

  LeafNode[Token`Comment,
    Flatten[
      {fs[[1]]} ~Join~
      replacedOrigSpaces ~Join~
      Riffle[{fragGroups[[1]]} ~Join~ ((FragmentNode[Token`Comment, #, <||>]& /@ #)& /@ replacedStrs), nlGroups]
    ]
    ,
    data
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


indent[PrefixNode[tag_, {rator_, trivia..., rand_}, data_], level_] :=
  PrefixNode[tag, {indent[rator, level], indent[rand, level]}, data]

indent[PostfixNode[tag_, {rand_, trivia..., rator_}, data_], level_] :=
  PostfixNode[tag, {indent[rand, level], indentPostfixRator[tag][rator, level]}, data]


(*
Special case CompoundExpression at top-level

Completely preserve newlines for CompoundExpression at top-level

Breaking up lines or gluing lines together may change the actual CompoundExpressions and
how expressions are parsed
*)
indent[InfixNode[CompoundExpression, ts_, data_], level:0] :=
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
    $CurrentAiryness <= -0.25,
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
      $CurrentAiryness >= 0.25,
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
indentInfixRator[Comma][rator_, level_] /; $CurrentAiryness >= 0.85 :=
  {line[level], indent[rator, level], line[level]}

indentInfixRatorNoTrailingSpace[Comma][rator_, level_] /; $CurrentAiryness >= 0.85 :=
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
    
    If[$CurrentAiryness <= -0.5 || (Length[split] == 1 && $CurrentAiryness <= 0.5),
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
        Riffle[
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


(*
Do not format linear syntax right now
*)
indent[n:GroupNode[GroupLinearSyntaxParen, _, _], _] :=
  n

indent[GroupNode[tag_, {
      opener_, 
      trivia : trivia...,
      closer_
    }, data_], level_] :=
  Module[{triviaAggs, triviaGraphs, condition},

    triviaAggs = DeleteCases[{trivia}, ws];

    triviaGraphs = DeleteCases[{trivia}, ws | nl];

    Which[
      $CurrentAiryness >= 0.75,
        condition = MultiLineEnum
      ,
      $CurrentAiryness <= -0.75,
        condition = SingleLineEnum
      ,
      !FreeQ[triviaAggs, LeafNode[Token`Newline, _, _]],
        condition = MultiLineEnum
      ,
      True,
        condition = SingleLineEnum
    ];

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
      $CurrentAiryness >= 0.75,
        condition = MultiLineEnum
      ,
      $CurrentAiryness <= -0.75,
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

indent[n:GroupMissingCloserNode[_, _, _], level_] :=
  n

indent[n:UnterminatedGroupNode[_, _, _], level_] :=
  n



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

    CallNode[
      Flatten[{
        indent[head, level],
        indent[#, level + 1]& /@ comments1
      }],
      Flatten[{
        indent[opener, level + 1],
        indent[#, level + 1]& /@ comments2,
        indent[vars, level + 1],
        indent[#, level + 1]& /@ comments3,
        indent[comma1, level + 1],
        If[!empty[comments4], line[level + 1], nil[]],
        indent[#, level + 1]& /@ comments4,
        line[level + 1],
        indent[body, level + 1],
        If[!empty[comments5], line[level + 1], nil[]],
        Riffle[indent[#, level + 1]& /@ comments5, {space[]}],
        line[level],
        indent[closer, level]
      }]
      ,
      data
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
    
    -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

    -3 is where LeafNode[xxx, xxx, <||>] is
    *)
    vars = DeleteCases[{varsIn}, LeafNode[Token`Newline, _, _], {-5}];

    CallNode[
      Flatten[{
        indent[head, level],
        indent[#, level]& /@ comments1}]
      ,
      Flatten[{
        indent[GroupNode[GroupSquare,
            {opener} ~Join~
            comments2 ~Join~
            {InfixNode[Comma, vars ~Join~ {rest}, data2]} ~Join~
            {trivia4, closer}, data1], level]
      }]
      ,
      data
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
    rators = aggs[[2 ;; -2 ;; 2]];
    ratorsPat = Alternatives @@ rators;
    testsPat = Alternatives @@ tests;
    bodiesPat = Alternatives @@ bodies;

    CallNode[
      Flatten[{
        indent[tag, level + 1],
        indent[#, level + 1]& /@ comments1
      }]
      ,
      Flatten[{
        indent[opener, level + 1],
        indent[#, level + 1]& /@ comments2, 
        indent[firstRand, level + 1],
        indent[#, level + 1]& /@ comments3,
        indent[firstRator, level + 1],
        indent[#, level + 1]& /@ comments4,
        Replace[graphs, {
            rator : ratorsPat :> indent[rator, level + 1],
            test:testsPat :> {line[level + 1], indent[test, level + 1]},
            body:bodiesPat :> {line[level + 2], indent[body, level + 2], line[level + 1]},
            other_ :> indent[other, level + 1]
          }, {1}],
        line[level + 2],
        indent[#, level + 1]& /@ comments5,
        indent[lastRand, level + 2],
        indent[#, level + 1]& /@ comments6,
        line[level],
        indent[closer, level]
      }]
      ,
      data
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
    rators = aggs[[2 ;; -2 ;; 2]];
    ratorsPat = Alternatives @@ rators;
    testsPat = Alternatives @@ tests;
    bodiesPat = Alternatives @@ bodies;

    CallNode[
      Flatten[{
        indent[tag, level + 1],
        indent[#, level + 1]& /@ comments1
      }]
      ,
      Flatten[{
        indent[opener, level + 1],
        indent[#, level + 1]& /@ comments2, 
        Replace[graphs, {
            rator : ratorsPat :> indent[rator, level + 1],
            test:testsPat :> {line[level + 1], indent[test, level + 1]},
            body:bodiesPat :> {line[level + 2], indent[body, level + 2], line[level + 1]},
            other_ :> indent[other, level + 1]
          }, {1}],
        line[level + 2],
        indent[lastRand, level + 2],
        indent[#, level + 1]& /@ comments3,
        line[level],
        indent[closer, level]
      }]
      ,
      data
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
      (*
      If[anything, symbol, symbol] => single line
      *)
      MatchQ[aggs, {PatternSequence[LeafNode[Symbol, _, _], LeafNode[Token`Comma, _, _]]..., LeafNode[Symbol, _, _]}],
        condition = SingleLine
      ,
      True,
        condition = RedoNewlines
    ];

    CallNode[
      Flatten[{
        indent[tag, level + 1], 
        indent[#, level + 1]& /@ comments1
      }]
      ,
      Flatten[
      Switch[condition,
        SingleLine,
          {
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
          }
        ,
        RedoNewlines,
          {
            indent[opener, level + 1], 
            indent[#, level + 1]& /@ comments2, 
            indent[firstRand, level + 1], 
            indent[#, level + 1]& /@ comments3, 
            indent[firstRator, level + 1], 
            line[level + 1], 
            Replace[graphs, {
                rator : ratorsPat :> {line[level + 1], indent[rator, level + 1], line[level + 1]}, 
                other_ :> indent[other, level + 1]
              }, {1}], 
            indent[#, level]& /@ comments4, 
            line[level], 
            indent[closer, level]
          }
      ]]
      ,
      data
    ]
  ]

(*
special casing For

completely redo newlines
*)
indent[CallNode[{tag : LeafNode[Symbol, "For", _], trivia1 : trivia...}, {
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

    CallNode[
      Flatten[{
        indent[tag, level],
        Riffle[indent[#, level]& /@ comments1, {space[]}]
      }]
      ,
      Flatten[{
        indent[opener, level],
        Riffle[indent[#, level]& /@ comments2, {space[]}],
        indent[rand1, level],
        Riffle[indent[#, level]& /@ comments3, {space[]}],
        indent[rator1, level], space[],
        Riffle[indent[#, level]& /@ comments4, {space[]}],
        indent[rand2, level],
        Riffle[indent[#, level]& /@ comments5, {space[]}],
        indent[rator2, level], space[],
        Riffle[indent[#, level]& /@ comments6, {space[]}],
        indent[rand3, level],
        Riffle[indent[#, level]& /@ comments7, {space[]}],
        indent[rator3, level], space[],
        Riffle[indent[#, level]& /@ comments8, {space[]}],
        line[level + 1],
        indent[rand4, level + 1],
        If[!empty[comments9], line[level + 1], nil[]],
        Riffle[indent[#, level + 1]& /@ comments9, {space[]}],
        line[level], 
        indent[closer, level]
      }]
      ,
      data
    ]
  ]

indent[CallNode[{tag_, trivia...}, ts_, data_], level_] :=
  CallNode[{indent[tag, level]}
    ,
    indent[#, level]& /@ ts
    ,
    data
  ]


indent[SyntaxErrorNode[tag_, ts_, data_], level_] :=
  SyntaxErrorNode[tag, indent[#, level]& /@ ts, data]


indent[ContainerNode[tag_, ts_, data_], level_] :=
  Module[{graphs},

    graphs = DeleteCases[ts, ws];

    If[Length[graphs] >= 1 && !MatchQ[graphs[[-1]], nl],
      AppendTo[graphs, LeafNode[Token`Newline, "", <||>]]
    ];

    ContainerNode[
      tag
      ,
      Flatten[
        indent[#, level]& /@ graphs
      ]
      ,
      data
    ]
  ]



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



mergeLineContinuations[fs_] :=
  Module[{poss, lc, onePast, numberOfOriginalSpaces, numberOfBeforeChars, originalSpacesSpec, dropSpecs, takeSpec,
    newFs, commentReplaceSpecs},

    If[$Debug,
      Print["fs: ", fs];
    ];

    lc = FragmentNode[_, "\\" <> $CurrentNewline, _];

    poss = Position[fs, lc];

    dropSpecs = {};
    commentReplaceSpecs = {};
    Function[{pos},

      If[$Debug,
        Print["pos: ", pos];
      ];

      (*
      Count how many spaces after the line continuation
      *)
      onePast = NestWhile[# + 1 &, pos[[1]] + 1, # <= Length[fs] && MatchQ[fs[[#]], (LeafNode|FragmentNode)[_, " ", _]] &];

      originalSpacesSpec = {pos[[1]] + 1, onePast};

      If[$Debug,
        Print["originalSpacesSpec: ", originalSpacesSpec];
      ];

      numberOfOriginalSpaces = (onePast) - (pos[[1]] + 1);

      (*
      Count how many characters before the line continuation (but after any previous newline)
      *)
      onePast = NestWhile[# - 1 &, pos[[1]] - 1, # >= 1 && !MatchQ[fs[[#]], (LeafNode|FragmentNode)[_, $CurrentNewline, _]] &];

      takeSpec = {onePast + 1, pos[[1]] - 1};

      If[$Debug,
        Print["takeSpec: ", takeSpec];
      ];

      numberOfBeforeChars =
        Total[
          (*
          Make sure to treat implicit Times as " "
          *)
          StringLength /@ (Take[fs, takeSpec] /. LeafNode[Token`Fake`ImplicitTimes, _, _] -> LeafNode[Whitespace, " ", <||>])[[All, 2]]
        ];

      If[$Debug,
        Print["numberOfBeforeChars: ", numberOfBeforeChars];
        Print["numberOfOriginalSpaces: ", numberOfOriginalSpaces];
      ];

      Which[
        (*
        numberOfBeforeChars == 0 && numberOfOriginalSpaces == 0,
          (*
          not mergeable; the line continuation will be dropped later 
          *)
          If[$Debug,
            Print["not mergeable 1"];
          ];
          Nothing
        ,
        *)
        (*
        either there are more original spaces (so fine to merge back)

        a::usage = \
                   "text
        text"

        turns into =>

        a::usage = "text
        text"

        *)
        numberOfBeforeChars <= numberOfOriginalSpaces,
          (*
          make sure to include the line continuation itself to be removed
          *)
          If[$Debug,
            Print["mergeable 1"];
          ];

          AppendTo[dropSpecs, {pos[[1]], originalSpacesSpec[[2]] - (numberOfOriginalSpaces - numberOfBeforeChars) - 1}]
        ,
        (*
        or whatever is overlapping is spaces, so also fine to merge back

        a::usage =
              \
        "text
        text"

        turns into =>

        a::usage =
        "text
        text"

        *)
        MatchQ[Take[fs, takeSpec], {(LeafNode|FragmentNode)[_, " ", _]...}],
          If[$Debug,
            Print["mergeable 2"];
          ];
          (*
          make sure to include the line continuation itself
          *)
          AppendTo[dropSpecs, {takeSpec[[1]], pos[[1]]}]
        ,
        (*

        If the string itself starts with " and then immediately a newline, then it is fine to merge
        (we will not mess up any alignment, because there is no alignment on the first line)

        a::usage = \
                 "
        xxx"

        turns into =>

        a::usage = "
        xxx"

        *)
        MatchQ[fs[[originalSpacesSpec[[2]]]], FragmentNode[String, "\"", _] | FragmentNode[Token`Comment, "(*", _]],
          (*
          make sure to include the line continuation itself to be removed
          *)
          If[$Debug,
            Print["mergeable 3"];
          ];
          AppendTo[dropSpecs, {pos[[1]], originalSpacesSpec[[2]] - 1}]
        ,
        MatchQ[Take[fs, {Max[pos[[1]] - (numberOfBeforeChars - numberOfOriginalSpaces), 1], pos[[1]] - 1}], {(LeafNode|FragmentNode)[_, " ", _]...}],
          (*
          
          If[  \
              "x
          x"
          ]

          turns into

          If[ "x
          x"
          ]

          *)
          If[$Debug,
            Print["mergeable 4"];
          ];
          AppendTo[dropSpecs, {Max[pos[[1]] - (numberOfBeforeChars - numberOfOriginalSpaces), 1], pos[[1]] + numberOfOriginalSpaces}]
        ,
        MatchQ[fs[[pos[[1]]]], FragmentNode[Token`Comment, "\\" <> $CurrentNewline, _]],
          (*
          Always ok to remove line continuation from a comment
          But make sure to leave a newline
          *)
          (*
          AppendTo[commentReplaceSpecs, pos]
          *)
          Null
        ,
        MatchQ[fs[[pos[[1]]]], FragmentNode[Token`String, "\\" <> $CurrentNewline, _]],
          (*

          Something like:

          a::usage = \
                   "xx
          xx"

          turns into

          a::usage = "xx
          xx"

          The internal alignment of the string is changed.
          Give a message.

          *)
          If[TrueQ[$DisableBadMerging],
            Message[CodeFormat::multiline];
            ,
            AppendTo[dropSpecs, {pos[[1]], originalSpacesSpec[[2]] - 1}]
          ]
      ]

    ] /@ poss;

    If[$Debug,
      Print["commentReplaceSpecs: ", commentReplaceSpecs];
      Print["dropSpecs: ", dropSpecs];
    ];

    newFs = fs;
    newFs = ReplacePart[newFs, commentReplaceSpecs -> FragmentNode[Token`Comment, $CurrentNewline, <||>]];
    newFs = Fold[Drop, newFs, dropSpecs // Reverse];

    If[$Debug,
      Print["newFs: ", newFs];
    ];

    (*
    cleanup any remaining line continuations
    *)
    (*
    newFs = newFs /. {
      lc :> Sequence @@ line[0],
      (*
      space fragments are now orphaned, so need to convert back to LeafNodes
      *)
      FragmentNode[_, " ", data_] :> LeafNode[Token`Whitespace, " ", data],
      FragmentNode[tag_, str_, data_] :> LeafNode[tag, str, data]
    };
    *)

    newFs
  ]



breakLines[tokensIn_, lineWidth_] :=
  Module[{tokens, lines},

    tokens = tokensIn;

    lines = Most /@ Split[tokens, !MatchQ[#1, LeafNode[Token`Newline, _, _] | FragmentNode[Token`Comment, $CurrentNewline, _]]&];

    If[$Debug,
      Print["lines: ", lines];
    ];

    lines = breakLine[#, lineWidth]& /@ lines;

    If[$Debug,
      Print["lines: ", lines];
    ];

    tokens = Flatten[{#, {LeafNode[Token`Newline, $CurrentNewline, <||>]}}& /@ lines];

    tokens
  ]

breakLine[tokensIn_, lineWidth_] :=
  Module[{tokens, width, tok, toSplit, takeSpecs, kTmp},

    tokens = tokensIn;

    width = 0;
    toSplit = <||>;
    Do[
      If[$Debug,
        Print["starting loop ", i];
        Print["width is: ", width];
      ];
      tok = tokens[[i]];

      width += StringLength[tok[[2]]];
      If[$Debug,
        Print["width is (tentatively) now 1: ", width];
      ];

      While[True,

        If[width <= lineWidth,
          Break[]
        ];

        If[KeyExistsQ[toSplit, i],
          toSplit[i] = Join[toSplit[i], { StringLength[tok[[2]]] - (width - lineWidth)  }]
          ,
          toSplit[i] = { StringLength[tok[[2]]] - (width - lineWidth) }
        ];

        width = StringLength[tok[[2]]] - (StringLength[tok[[2]]] - (width - lineWidth));
        If[$Debug,
          Print["toSplit: ", toSplit];
          Print["width is (tentatively) now 2: ", width];
        ]
      ]
      ,
      {i, 1, Length[tokens]}
    ];

    If[$Debug,
      Print["toSplit 1: ", toSplit];
    ];

    (*
    Adjust toSplit as needed to not split \[Alpha] or \"
    *)
    toSplit = Association[KeyValueMap[
      Function[{key, val},
        tok = tokens[[key]];

        Which[
          StringContainsQ[tok[[2]], "\\"],
            (*
            splitting at k means to return substrings with indices {1, k} and {k+1, 20}
            *)
            key -> (Function[k,
              kTmp = k;
              Catch[
              (*
              need to loop because there may be mutiple escapes to handle in a row
              *)
              While[True,
                If[$Debug,
                  Print["kTmp: ", kTmp];
                  Print["examing: ", StringTake[tok[[2]], kTmp]];
                ];
                Which[
                  (*
                  reached beginning of token
                  *)
                  kTmp == -1,
                    Throw[0]
                  ,
                  (*
                  ends with unfinished longname
                  *)
                  StringEndsQ[StringTake[tok[[2]], kTmp], RegularExpression["\\\\\\[([a-zA-Z0-9]*)"]],
                    If[$Debug,
                      Print["unfinished longname"];
                    ];
                    kTmp = kTmp - StringLength[StringCases[StringTake[tok[[2]], kTmp], RegularExpression["\\\\\\[([a-zA-Z0-9]*)$"]][[1]]] - 1
                  ,
                  (*
                  ends with unfinished single escaped character
                  *)
                  StringEndsQ[StringTake[tok[[2]], kTmp], RegularExpression["\\\\"]],
                    If[$Debug,
                      Print["unfinished single character"];
                    ];
                    kTmp = kTmp - StringLength[StringCases[StringTake[tok[[2]], kTmp], RegularExpression["\\\\$"]][[1]]] - 1
                  ,
                  True,
                    If[$Debug,
                      Print["nothing unfinished"];
                    ];
                    Throw[kTmp]
                ]
              ]]
            ] /@ val)
          ,
          True,
            key -> val
        ]
      ]
      ,
      toSplit
    ]];

    If[$Debug,
      Print["toSplit 2: ", toSplit];
    ];

    (*
    Now split each token accordingly
    *)
    KeyValueMap[
      Function[{key, val},
        tok = tokens[[key]];
        (*
        scan for fragments such as * ) that should NOT be broken

        but line break BEFORE this fragment
        *)
        Which[
          (*
          comments have been fragmentized

          there may be more fragments to check in the future
          *)
          MatchQ[tok, FragmentNode[Token`Comment, "*)", _]],
            tokens[[key]] = {Fragment[Token`Comment, "\\" <> $CurrentNewline, tok[[3]]], tok}
          ,
          MatchQ[tok, LeafNode[String, s_ /; !StringStartsQ[s, "\""], _]],
            (*
            just ignore the line break for now
            FIXME: yes this is wrong
            *)
            tokens[[key]] = tok
          ,
          MatchQ[tok, LeafNode[Token`Fake`ImplicitTimes, _, _]],
            Message[CodeFormat::implicittimesaftercontinuation];
            tokens[[key]] = {Fragment[Token`Fake`ImplicitTimes, "\\" <> $CurrentNewline, tok[[3]]], FragmentNode[Token`Star, "*", <||>]}
          ,
          (*
          No special cases, can use the original split value
          *)
          True,
            takeSpecs = Partition[{0} ~Join~ val ~Join~ {StringLength[tok[[2]]]}, 2, 1];
            If[$Debug,
              Print["takeSpecs 1: ", takeSpecs];
            ];
            takeSpecs = Replace[takeSpecs, {
                {0, 0} -> {},
                {start_, end_} :> {start+1, end}
              }
              ,
              {1}
            ];
            If[$Debug,
              Print["takeSpecs 2: ", takeSpecs];
            ];

            tokens[[key]] = FragmentNode[tok[[1]], #, tok[[3]]]& /@ Riffle[StringTake[tok[[2]], takeSpecs], "\\" <> $CurrentNewline]
        ];
      ]
      ,
      toSplit
    ];

    If[$Debug,
      Print["tokens: ", tokens];
    ];

    Flatten[tokens]
  ]


End[]

EndPackage[]
