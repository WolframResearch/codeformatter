BeginPackage["CodeFormatter`Standardize`"]

StandardizeEmbeddedNewlines

StandardizeEmbeddedTabs

Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


StandardizeEmbeddedNewlines::usage = "StandardizeEmbeddedNewlines[cst, newline] standardizes the newlines in cst."

StandardizeEmbeddedNewlines[cstIn_, newline_String] :=
Catch[
Module[{cst, data, embeddedNewlines, mapSpecs, tuples, poss, tokStartLocs, grouped, childData},

  cst = cstIn;

  data = cst[[3]];

  If[empty[cst[[2]]],
    Throw[cst]
  ];

  childData = cst[[2, 1, 3]];

  (*
  Only proceed if LineColumn convention
  *)
  If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
    Throw[cst]
  ];

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

    cst = MapAt[convertEmbeddedNewlines[#, "FormatOnly" -> True, "NewlineString" -> newline]&, cst, mapSpecs[[All, 2]]];

    (*

    To be used later

    KeyDropFrom[data, "EmbeddedNewlines"];
    *)

    cst[[3]] = data;
  ];

  cst
]]



StandardizeEmbeddedTabs::usage = "StandardizeEmbeddedTabs[cst, newline, tabWidth] standardizes tabs in cst."

StandardizeEmbeddedTabs[cstIn_, newline_String, tabWidth_Integer] :=
Catch[
Module[{cst, data, embeddedTabs, mapSpecs, tuples, poss, tokStartLocs, grouped, childData},

  cst = cstIn;

  data = cst[[3]];

  If[empty[cst[[2]]],
    Throw[cst]
  ];

  childData = cst[[2, 1, 3]];

  (*
  Only proceed if LineColumn convention
  *)
  If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
    Throw[cst]
  ];

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
      embeddedTabs
    ];

    mapSpecs = Flatten[mapSpecs, 1];

    cst = MapAt[convertEmbeddedTabs[#, "FormatOnly" -> True, "NewlineString" -> newline, "TabWidth" -> tabWidth]&, cst, mapSpecs[[All, 2]]];

    (*
    
    To be used later

    KeyDropFrom[data, "EmbeddedTabs"];
    *)

    cst[[3]] = data;
  ];

  cst
]]


End[]

EndPackage[]
