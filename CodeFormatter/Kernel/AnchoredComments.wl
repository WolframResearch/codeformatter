BeginPackage["CodeFormatter`AnchoredComments`"]

InsertNewlineAnchorInformationIntoComments

absorbNewlinesIntoComments

Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

(*

inserts this metadata into comments:

StartOfLine -> True|False
EndOfLine -> True|False
"Multiline" -> True|False


"Multiline" is used by Indent

It is not meant to mean that the comment takes up multiple lines


*)

InsertNewlineAnchorInformationIntoComments[cstIn_] :=
Catch[
Module[{cst, isWhitespacePos, isNewlinePos, commentPoss, commentMap, newPos, len, startOfLine, endOfLine, comment, commentData},

  If[$Debug,
    Print["enter InsertNewlineAnchorInformationIntoComments"];
  ];

  cst = cstIn;

  isWhitespacePos[pos1_] :=
    ListQ[pos1] && MatchQ[Extract[cst, pos1], LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]];
  
  isNewlinePos[pos1_] :=
    ListQ[pos1] && MatchQ[Extract[cst, pos1], LeafNode[Token`Newline, _, _]];

  commentPoss = Position[cst, LeafNode[Token`Comment, _, _]];

  If[$Debug,
    Print["commentPoss: ", commentPoss]
  ];

  commentMap = <||>;

  Scan[
    Function[{pos},
      
      comment = Extract[cst, pos];

      startOfLine = False;
      endOfLine = False;

      newPos = pos;
      newPos = Most[newPos] ~Join~ {Last[newPos] - 1};
      While[Last[newPos] >= 1 && isWhitespacePos[newPos],
        newPos = Most[newPos] ~Join~ {Last[newPos] - 1};
      ];
      If[Last[newPos] >= 1 && isNewlinePos[newPos],
        startOfLine = True
      ];

      newPos = pos;
      len = Length[Extract[cst, Most[newPos]]];
      newPos = Most[newPos] ~Join~ {Last[newPos] + 1};
      While[Last[newPos] <= len && isWhitespacePos[newPos],
        newPos = Most[newPos] ~Join~ {Last[newPos] + 1};
      ];
      If[Last[newPos] <= len && isNewlinePos[newPos],
        endOfLine = True
      ];

      commentData = comment[[3]];
      commentData = <|commentData, <|StartOfLine -> startOfLine, EndOfLine -> endOfLine, "Multiline" -> (startOfLine || endOfLine)|>|>;
      comment[[3]] = commentData;

      commentMap[pos] = comment
    ]
    ,
    commentPoss
  ];

  cst = ReplacePart[cst, commentMap];

  cst
]]



absorbNewlinesIntoComments[fsIn_] :=
Module[{fs, poss, toDelete, i, isWhitespacePos, isNewlinePos, len, toInsert, lastInserted1, lastInserted2},

  fs = fsIn;

  isWhitespacePos[pos1_] :=
    ListQ[pos1] && MatchQ[Extract[fs, pos1], LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]];
  
  isNewlinePos[pos1_] :=
    ListQ[pos1] && MatchQ[Extract[fs, pos1], LeafNode[Token`Newline, _, _]];

  lastInserted1 = Null;
  lastInserted2 = Null;

  toDelete = Internal`Bag[];
  toInsert = Internal`Bag[];


  poss = Position[fs, FragmentNode[Token`Comment, _, KeyValuePattern[StartOfLine -> False]], {1}];

  If[$Debug,
    Print["poss (StartOfLine): ", poss];
  ];

  (*
  Eat all of the whitespace and newlines before a StartOfLine -> False comment fragment
  *)
  Scan[
    Function[{pos},
      i = Last[pos];
      i--;
      While[i >= 1 && (isWhitespacePos[Most[pos] ~Join~ {i}] || isNewlinePos[Most[pos] ~Join~ {i}]),
        Internal`StuffBag[toDelete, Most[pos] ~Join~ {i}];
        lastInserted1 = Most[pos] ~Join~ {i};
        i--;
      ]
    ]
    ,
    poss
  ];


  poss = Position[fs, FragmentNode[Token`Comment, _, KeyValuePattern[EndOfLine -> False]], {1}];

  If[$Debug,
    Print["poss (EndOfLine): ", poss];
  ];

  (*
  Eat all of the whitespace and newlines after a EndOfLine -> False comment fragment
  *)
  Scan[
    Function[{pos},
      len = Length[fs];
      i = Last[pos];
      i++;
      While[i <= len && (isWhitespacePos[Most[pos] ~Join~ {i}] || isNewlinePos[Most[pos] ~Join~ {i}]),
        Internal`StuffBag[toDelete, Most[pos] ~Join~ {i}];
        lastInserted2 = Most[pos] ~Join~ {i};
        i++;
      ]
    ]
    ,
    poss
  ];


  toDelete = Internal`BagPart[toDelete, All];

  (*
  Make sure to insert a single space back before / after the comment
  *)
  If[lastInserted1 =!= Null,
    toDelete = DeleteCases[toDelete, lastInserted1];
    fs = ReplacePart[fs, lastInserted1 -> LeafNode[Whitespace, " ", <||>]];
  ];
  If[lastInserted2 =!= Null,
    toDelete = DeleteCases[toDelete, lastInserted2];
    fs = ReplacePart[fs, lastInserted2 -> LeafNode[Whitespace, " ", <||>]];
  ];

  If[$Debug,
    Print["toDelete: ", toDelete];
  ];

  fs = Delete[fs, toDelete];

  fs
]


End[]

EndPackage[]
