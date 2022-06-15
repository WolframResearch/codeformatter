(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

BeginPackage["CodeFormatter`Fragmentize`"]

Fragmentize

mergeTemporaryLineContinuations

Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


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
Fragmentize[cstIn:CallNode[_, _, _]] :=
Catch[
Module[{cst, data},

  cst = cstIn;
  
  cst = fragmentizeCommentGroups[cst];
  
  data = cst[[3]];

  (*
  Only proceed if LineColumn convention
  *)
  If[!MatchQ[data, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
    Throw[cst]
  ];

  fragmentize[cst]
]]

Fragmentize[cstIn:_[_, _String, _]] :=
Catch[
Module[{cst, data},

  cst = cstIn;
  
  cst = fragmentizeCommentGroups[cst];
  
  data = cst[[3]];

  (*
  Only proceed if LineColumn convention
  *)
  If[!MatchQ[data, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
    Throw[cst]
  ];

  fragmentize[cst]
]]

Fragmentize[cstIn:_[_, _List, _]] :=
Catch[
Module[{cst, data, firstChild, firstChildData},

  cst = cstIn;
  
  cst = fragmentizeCommentGroups[cst];
  
  data = cst[[3]];

  If[empty[cst[[2]]],
    Throw[cst]
  ];

  firstChild = cst[[2, 1]];

  If[MissingQ[firstChild],
    Throw[cst]
  ];
  
  (*
  cst may be a ContainerNode with no source info
  so look at first child to determine source convention
  *)
  firstChildData = firstChild[[3]];

  (*
  Only proceed if LineColumn convention
  *)
  If[!MatchQ[firstChildData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
    Throw[cst]
  ];

  fragmentize[cst]
]]

fragmentize[cstIn_] :=
Catch[
Module[{commentPoss, nonCommentPoss, poss, cst, tokStartLocs, grouped, data, embeddedNewlines, mapSpecs, embeddedNewlinePoss, tuples,
  allOtherCommentPoss},

  cst = cstIn;
  
  data = cst[[3]];

  (*
  Special case multiline strings and multiline comments with LineColumn convention

  Must preserve the original number of columns preceding the string

  We do this by inserting a newline, then the original number of spaces.

  A line continuation is inserted to help with semantics of inserting a newline (this may not ultimately be needed)
  Also, the line continuation helps to communicate the "separateness" of the string or comment
  *)

  (*
  Used to be just:
  
  poss = Position[]
  
  but was split into commentPoss and nonCommentPoss for efficiency purposes later
  *)
  commentPoss = Position[cst, LeafNode[Token`Comment, _, _], {-5, -3}];
  nonCommentPoss = Position[cst, LeafNode[Except[Token`Comment], _, _], {-5, -3}];
  commentPoss = Cases[commentPoss, {___Integer}];
  nonCommentPoss = Cases[nonCommentPoss, {___Integer}];
  
  poss = commentPoss ~Join~ nonCommentPoss;

  tokStartLocs = #[[3, Key[Source], 1]]& /@ Extract[cst, poss];

  (*
  Group by starting SourceLocation
  *)
  grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

  (*
  "EmbeddedNewlines" may not exist
  *)
  embeddedNewlines = Lookup[data, "EmbeddedNewlines", {}];

  (*
  FIXME: use Lookup[]
  *)
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
  embeddedNewlinePoss =  mapSpecs[[All, 2]];

  cst = MapAt[fragmentizeMultilineLeafNode, cst, embeddedNewlinePoss];

  If[$Debug,
    Print["after fragmentizeMultilineLeafNode: ", cst];
  ];

  (*
  This used to be:

  allOtherPoss = Complement[poss, embeddedNewlinePoss];

  but we only care about comments, so only process comments
  *)
  allOtherCommentPoss = Complement[commentPoss, embeddedNewlinePoss];

  If[$Debug,
    Print["allOtherCommentPoss: ", allOtherCommentPoss];
  ];

  (*
  Now also fragmentize the remaining comments
  *)
  cst = MapAt[fragmentizeComment, cst, allOtherCommentPoss];

  If[$Debug,
    Print["after fragmentizeComment: ", cst];
  ];

  cst
]]


fragmentizeMultilineLeafNode[LeafNode[String, s_, data:KeyValuePattern[Source -> {{_, _}, {_, _}}]]] :=
Module[{origSpaces},
  origSpaces = data[[Key[Source], 1, 2]] - 1;
  LeafNode[
    String
    ,
    Flatten[{
      FragmentNode[String, "\\" <> $CurrentNewlineString, <|"Temporary" -> True|>],
      Table[FragmentNode[String, " ", <|"Temporary" -> True|>], origSpaces],
      Function[{cases}, {
          FragmentNode[String, #, <| "LastFragmentInString" -> False |>]& /@ Most[cases],
          FragmentNode[String, Last[cases], <| "LastFragmentInString" -> True |>]
        }][
        DeleteCases[StringSplit[s, x:$CurrentNewlineString :> x], ""]
      ]
    }]
    ,
    data
  ]
]

(*
Make sure to fragmentize ( * and * ) here

And fragmentize nested comments also
*)
fragmentizeMultilineLeafNode[LeafNode[Token`Comment, s_, data:KeyValuePattern[Source -> {{_, _}, {_, _}}]]] :=
Module[{origSpaces},
  origSpaces = data[[Key[Source], 1, 2]] - 1;
  LeafNode[
    Token`Comment
    ,
    Flatten[{
      FragmentNode[Token`Comment, "\\" <> $CurrentNewlineString, <| "Temporary" -> True |>],
      Table[FragmentNode[Token`Comment, " ", <| "Temporary" -> True |>], origSpaces],
      FragmentNode[Token`Comment, #, <||>]& /@ DeleteCases[StringSplit[s, x:"(*"|"*)"|$CurrentNewlineString :> x], ""]
    }]
    ,
    data
  ]
]

(*
Could be processed because it has the same Source as a multiline leaf node
*)
fragmentizeMultilineLeafNode[n:LeafNode[Token`Fake`ImplicitNull, _, _]] :=
  n

fragmentizeMultilineLeafNode[args___] :=
  Failure["Unhandled", <| "Function" -> fragmentizeMultilineLeafNode, "Arguments" -> HoldForm[{args}] |>]


(*
Make sure to fragmentize ( * and * ) here

And fragmentize nested comments also
*)
fragmentizeComment[LeafNode[Token`Comment, s_, data_]] :=
  LeafNode[Token`Comment,
    FragmentNode[Token`Comment, #, <||>]& /@ DeleteCases[StringSplit[s, x:"(*"|"*)" :> x], ""]
    ,
    data
  ]



(*
Flatten comment groups
*)

fragmentizeCommentGroups[cstIn_] :=
Module[{cst, poss},

  cst = cstIn;

  poss = Position[cst, GroupNode[Comment, _, _]];

  cst = MapAt[convertCommentGroups, cst, poss];

  cst
]

convertCommentGroups[c:GroupNode[Comment, boxs_, data_]] :=
Module[{leafs, src, origSpaces},
  src = data[Source];
  leafs = Cases[boxs, LeafNode[_, _, _], Infinity];

  If[MemberQ[leafs, LeafNode[String, "\[IndentingNewLine]" | "\n", _]], 
    origSpaces = 0;
    (*
    Do a little hack here

    It is hard to tell how many spaces were in front of the multiline comment

    so count how many spaces were before the closer and use that
    *)
    Do[
      If[MatchQ[leafs[[i]], LeafNode[String, "\[IndentingNewLine]" | "\n", _]],
        Break[]
      ];
      If[MatchQ[leafs[[i]], LeafNode[String, s_ /; StringMatchQ[s, " "..], _]],
        origSpaces += StringLength[leafs[[i]][[2]]]
      ];
      ,
      {i, Length[leafs] - 1, 2, -1}
    ];

    LeafNode[
      Token`Comment
      ,
      {FragmentNode[Token`Comment, "\\" <> $CurrentNewlineString, <| "Temporary" -> True |>]} ~Join~
        Table[FragmentNode[String, " ", <| "Temporary" -> True |>], origSpaces] ~Join~
        (FragmentNode[Token`Comment, #[[2]], <||>]& /@ leafs)
      ,
      data
    ]
    ,
    (* single line *)
    LeafNode[
      Token`Comment
      ,
      FragmentNode[Token`Comment, #[[2]], <||>]& /@ leafs
      ,
      data
    ]
  ]
]




mergeTemporaryLineContinuations[fsIn_] :=
Module[{fs, poss, lc, onePast, numberOfOriginalSpaces, numberOfBeforeChars, originalSpacesSpec, deleteSpecs, takeSpec,
  commentReplaceSpecs},

  fs = fsIn;

  If[$Debug,
    Print["fs: ", fs];
  ];

  lc = FragmentNode[_, "\\" <> $CurrentNewlineString, _];

  poss = Position[fs, lc, {1}];

  deleteSpecs = Internal`Bag[];
  commentReplaceSpecs = {};
  Function[{pos},

    If[$Debug,
      Print["pos: ", pos];
    ];

    (*
    Count how many spaces after the line continuation
    *)
    onePast = NestWhile[(# + 1)&, pos[[1]] + 1, (# <= Length[fs] && MatchQ[fs[[#]], (LeafNode|FragmentNode)[_, " ", _]])&];

    originalSpacesSpec = {pos[[1]] + 1, onePast};

    If[$Debug,
      Print["originalSpacesSpec: ", originalSpacesSpec];
    ];

    numberOfOriginalSpaces = (onePast) - (pos[[1]] + 1);

    (*
    Count how many characters before the line continuation (but after any previous newline)
    *)
    onePast = NestWhile[(# - 1)&, pos[[1]] - 1, (# >= 1 && !MatchQ[fs[[#]], (LeafNode|FragmentNode)[_, $CurrentNewlineString, _]])&];

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

        Scan[Internal`StuffBag[deleteSpecs, {#}]&, Range[pos[[1]], originalSpacesSpec[[2]] - (numberOfOriginalSpaces - numberOfBeforeChars) - 1]];
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
        Scan[Internal`StuffBag[deleteSpecs, {#}]&, Range[takeSpec[[1]], pos[[1]]]];
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
        Scan[Internal`StuffBag[deleteSpecs, {#}]&,  Range[pos[[1]], originalSpacesSpec[[2]] - 1]];
      ,
      MatchQ[Take[fs, {Max[pos[[1]] - (numberOfBeforeChars - numberOfOriginalSpaces), 1], pos[[1]] - 1}], {(LeafNode|FragmentNode)[Whitespace, _, _]...}],
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
        Scan[Internal`StuffBag[deleteSpecs, {#}]&, Range[Max[pos[[1]] - (numberOfBeforeChars - numberOfOriginalSpaces), 1], pos[[1]] + numberOfOriginalSpaces]];
      ,
      MatchQ[fs[[pos[[1]]]], FragmentNode[Token`Comment, "\\" <> $CurrentNewlineString, _]],
        (*
        Always ok to remove line continuation from a comment
        But make sure to leave a newline
        *)
        (*
        AppendTo[commentReplaceSpecs, pos]
        *)
        Null
      ,
      MatchQ[fs[[pos[[1]]]], FragmentNode[String, "\\" <> $CurrentNewlineString, _]],
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
          Scan[Internal`StuffBag[deleteSpecs, {#}]&, Range[pos[[1]], originalSpacesSpec[[2]] - 1]];
        ]
    ]

  ] /@ poss;

  deleteSpecs = Internal`BagPart[deleteSpecs, All];

  If[$Debug,
    Print["commentReplaceSpecs: ", commentReplaceSpecs];
    Print["deleteSpecs: ", deleteSpecs];
  ];

  fs = ReplacePart[fs, commentReplaceSpecs -> FragmentNode[Token`Comment, $CurrentNewlineString, <||>]];
  fs = Delete[fs, deleteSpecs];

  If[$Debug,
    Print["newFs: ", fs];
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
    FragmentNode[_, " ", data_] :> LeafNode[Whitespace, " ", data],
    FragmentNode[tag_, str_, data_] :> LeafNode[tag, str, data]
  };
  *)

  fs
]


End[]

EndPackage[]
