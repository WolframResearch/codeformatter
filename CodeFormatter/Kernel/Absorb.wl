BeginPackage["CodeFormatter`Absorb`"]

absorbNewlinesIntoComments

absorbNewlinesIntoSemis

Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


absorbNewlinesIntoComments[fsIn_] :=
Module[{fs, poss, toDelete, i, isWhitespacePos, isNewlinePos, len, toInsert, lastInserted1, lastInserted2, changed},

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

  changed = !empty[toDelete];

  {fs, changed}
]


absorbNewlinesIntoSemis[fsIn_] :=
Module[{fs, poss, toDelete, i, isWhitespacePos, isNewlinePos, len},

  fs = fsIn;

  isWhitespacePos[pos1_] :=
    ListQ[pos1] && MatchQ[Extract[fs, pos1], LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]];
  
  isNewlinePos[pos1_] :=
    ListQ[pos1] && MatchQ[Extract[fs, pos1], LeafNode[Token`Newline, _, _]];

  toDelete = Internal`Bag[];


  poss = Position[fs, LeafNode[Token`Semi, _, KeyValuePattern["Toplevel" -> True]], {1}];

  If[$Debug,
    Print["poss: ", poss];
  ];

  (*
  Eat all of the whitespace and newlines before a "Toplevel" -> True Token`Semi leaf
  *)
  Scan[
    Function[{pos},
      i = Last[pos];
      i--;
      While[i >= 1 && (isWhitespacePos[Most[pos] ~Join~ {i}] || isNewlinePos[Most[pos] ~Join~ {i}]),
        Internal`StuffBag[toDelete, Most[pos] ~Join~ {i}];
        i--;
      ]
    ]
    ,
    poss
  ];


  toDelete = Internal`BagPart[toDelete, All];

  If[$Debug,
    Print["toDelete: ", toDelete];
  ];

  fs = Delete[fs, toDelete];

  fs
]


End[]

EndPackage[]
