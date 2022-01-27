BeginPackage["CodeFormatter`RemoveLineContinuations`"]

RemoveSimpleLineContinuations

RemoveComplexLineContinuations

RemoveRemainingSimpleLineContinuations

Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*

Line continuations inside of strings or comments are "complex":
Formatting matters

All other line continuations are simple:
inside or outside of other tokens
outside of strings or comments

*)




RemoveSimpleLineContinuations::usage = "RemoveSimpleLineContinuations[cst] removes simple line continuations from cst."

RemoveSimpleLineContinuations[cstIn_] :=
Catch[
Module[{data, cst, tokStartLocs, simpleLineContinuations, grouped, poss, tuples, mapSpecs,
  embeddedNewlineStartLocs, extracted, complexLineContinuations, childData},

  cst = cstIn;

  data = cst[[3]];

  If[empty[cst[[2]]],
    Throw[cst]
  ];

  If[MissingQ[cst[[2, 1]]],
    Throw[cst]
  ];
  
  childData = cst[[2, 1, 3]];

  (*
  Only proceed if LineColumn convention
  *)
  If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
    Throw[cst]
  ];

  If[KeyExistsQ[data, "SimpleLineContinuations"],

    (*
    -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

    -3 is where LeafNode[xxx, xxx, <||>] is

    There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those
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

    (*
    FIXME: use Lookup[]
    *)
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
]]

RemoveComplexLineContinuations::usage = "RemoveComplexLineContinuations[cst] removes complex line continuations from cst."

RemoveComplexLineContinuations[cstIn_] :=
Catch[
Module[{data, cst, tokStartLocs, grouped, poss, tuples, mapSpecs,
  extracted, complexLineContinuations, childData},

  cst = cstIn;

  data = cst[[3]];

  If[empty[cst[[2]]],
    Throw[cst]
  ];

  If[MissingQ[cst[[2, 1]]],
    Throw[cst]
  ];

  childData = cst[[2, 1, 3]];

  (*
  Only proceed if LineColumn convention
  *)
  If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
    Throw[cst]
  ];

  If[KeyExistsQ[data, "ComplexLineContinuations"],

    (*
    -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

    -3 is where LeafNode[xxx, xxx, <||>] is

    There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those
    *)

    (*
    Only search for strings

    for comments, just leave alone

    it's not a "real" continuation; it could be the result of ASCII art or something
    *)
    poss = Position[cst, LeafNode[String, _, _], {-5, -3}];
    poss = Cases[poss, {___Integer}];

    extracted = Extract[cst, poss];

    tokStartLocs = #[[3, Key[Source], 1]]& /@ extracted;

    (*
    Group by starting SourceLocation
    *)
    grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

    complexLineContinuations = data["ComplexLineContinuations"];

    (*
    FIXME: use Lookup[]
    *)
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
]]

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
Catch[
Module[{data, cst, tokStartLocs, simpleLineContinuations, grouped, poss, tuples, mapSpecs, extracted, childData},

  cst = cstIn;

  data = cst[[3]];

  If[empty[cst[[2]]],
    Throw[cst]
  ];

  If[MissingQ[cst[[2, 1]]],
    Throw[cst]
  ];

  childData = cst[[2, 1, 3]];

  (*
  Only proceed if LineColumn convention
  *)
  If[!MatchQ[childData, KeyValuePattern[Source -> {{_, _}, {_, _}}]],
    Throw[cst]
  ];

  If[KeyExistsQ[data, "SimpleLineContinuations"],

    (*
    -5 is where LeafNode[xxx, xxx, <|Source->{{1,1},{1,1}}|>] is

    -3 is where LeafNode[xxx, xxx, <||>] is

    There may be LeafNodes in metadata such as SyntaxIssues or AbstractSyntaxIssues, so remove those
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

    (*
    FIXME: use Lookup[]
    *)
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
]]

End[]

EndPackage[]
