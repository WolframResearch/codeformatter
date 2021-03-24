BeginPackage["CodeFormatter`AnchoredComments`"]

InsertNewlineAnchorInformationIntoComments

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


End[]

EndPackage[]
