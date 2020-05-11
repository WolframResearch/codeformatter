BeginPackage["CodeFormatter`Utils`"]


replaceTabs

tabReplacementFunc


lexSort


firstToken
lastToken


firstLeafNode
lastLeafNode


isOpener
isCloser



Begin["`Private`"]

Needs["CodeParser`"]


(*
Memoizing function that returns the number of spaces that a tab should be replaced with

tabReplacementFunc[1, 4] => "    "
tabReplacementFunc[2, 4] => "   "
tabReplacementFunc[3, 4] => "  "
tabReplacementFunc[4, 4] => " "
tabReplacementFunc[5, 4] => "    "
tabReplacementFunc[6, 4] => "   "
tabReplacementFunc[7, 4] => "  "
tabReplacementFunc[8, 4] => " "
tabReplacementFunc[9, 4] => "    "

*)
tabReplacementFunc[col_Integer, tabWidth_Integer] :=
  tabReplacementFunc[col, tabWidth] =
  StringJoin[Table[" ", Mod[1 - col, tabWidth, 1]]]


replaceTabs[str_String, startingColumn_Integer, newline_String, tabWidth_Integer] :=
Module[{lines},
  lines = StringSplit[str, newline, All];
  (*
  Pad the first line with the correct number of characters from its origin
  *)
  lines[[1]] = StringJoin[Table["!", startingColumn - 1], lines[[1]]];
  lines = Map[
    Function[{line},
      (*
      for each line, accumulate a string by replacing each tab with its equivalent spaces,
      working from left to right
      *)
      NestWhile[
        Function[{lineAccum},
          With[{pos = StringPosition[lineAccum, "\t"][[1, 1]]},
            StringReplacePart[lineAccum, tabReplacementFunc[pos, tabWidth], {pos, pos}]]
        ]
        ,
        line
        ,
        StringContainsQ[#, "\t"]&
      ]
    ]
    ,
    lines
  ];
  (*
  Remove padding
  *)
  lines[[1]] = StringDrop[lines[[1]], startingColumn - 1];
  StringJoin[Riffle[lines, newline]]
]







(*
Define lexical ordering to use for lists

The default Wolfram Language ordering is not the same as common lexical ordering
*)
lexOrderingForLists[{}, {}] := 0
lexOrderingForLists[{}, b_] := 1
lexOrderingForLists[a_, {}] := -1
lexOrderingForLists[a_, b_] :=
  Order[Take[a, 1], Take[b, 1]] /. 
    0 :> lexOrderingForLists[Drop[a, 1], Drop[b, 1]]

(*

Unreported bug in v11.0:

In[77]:= Sort[{{1, 1}, {1, 3}, {1, 2}}, lexOrderingForLists]

Out[77]= {{1, 1}, {1, 3}, {1, 2}}

Fixed in 11.1

checkUnreportedSortBug1[] sets the flag $WorkaroundUnreportedSortBug1 to True if we still need to workaround unreported Sort bug 1
*)
checkUnreportedSortBug1[] :=
Module[{res},
  res = Sort[{{1, 1}, {1, 3}, {1, 2}}, lexOrderingForLists];
  Switch[res,
    {{1, 1}, {1, 3}, {1, 2}},
      True
    ,
    {{1, 1}, {1, 2}, {1, 3}},
      False
  ]
]



$WorkaroundUnreportedSortBug1 = checkUnreportedSortBug1[]


(*
Yes, this slower than it needs to be
But it is simple and reliable
*)
bubbleLexSort[listIn_] :=
Module[{list, len, tmp},
  list = listIn;
  len = Length[list];
  Do[
    If[lexOrderingForLists[list[[i]], list[[j]]] == -1,
      tmp = list[[i]]; 
      list[[i]] = list[[j]];
      list[[j]] = tmp
    ];
    ,
    {i, 1, len}
    ,
    {j, i, len}
  ];
  list
]


If[$WorkaroundUnreportedSortBug1,
  lexSort = bubbleLexSort
  ,
  (*
  TODO: v12.0 introduced SortBy[list, f, p]
  when targeting v12.0 as a minimum, then can use SortBy[list, ToCharacterCode, lexOrderingForLists]
  *)
  lexSort = Sort[#, lexOrderingForLists]&
]




firstToken[LeafNode[tok_, _, _]] := tok

firstToken[CallNode[{first_, ___}, _, _]] := firstToken[first]

firstToken[_[_, {first_, ___}, _]] := firstToken[first]



lastToken[LeafNode[tok_, _, _]] := tok

lastToken[_[_, {___, last_}, _]] := lastToken[last]



firstLeafNode[leaf:LeafNode[_, _, _]] := 

firstLeafNode[CallNode[{first_, ___}, _, _]] := firstLeafNode[first]

firstLeafNode[_[_, {first_, ___}, _]] := firstLeafNode[first]



lastLeafNode[leaf:LeafNode[_, _, _]] := leaf

lastLeafNode[_[_, {___, last_}, _]] := lastLeafNode[last]



isOpener[Token`LessBar] = True
isOpener[Token`OpenCurly] = True
isOpener[Token`OpenParen] = True
isOpener[Token`OpenSquare] = True
isOpener[Token`LongName`OpenCurlyDoubleQuote] = True
isOpener[Token`LongName`OpenCurlyQuote] = True
isOpener[Token`LongName`LeftAngleBracket] = True
isOpener[Token`LongName`LeftAssociation] = True
isOpener[Token`LongName`LeftBracketingBar] = True
isOpener[Token`LongName`LeftCeiling] = True
isOpener[Token`LongName`LeftDoubleBracket] = True
isOpener[Token`LongName`LeftDoubleBracketingBar] = True
isOpener[Token`LongName`LeftFloor] = True
isOpener[Token`LinearSyntax`OpenParen] = True

isOpener[_] = False


isCloser[Token`BarGreater] = True
isCloser[Token`CloseCurly] = True
isCloser[Token`CloseParen] = True
isCloser[Token`CloseSquare] = True
isCloser[Token`LongName`CloseCurlyDoubleQuote] = True
isCloser[Token`LongName`CloseCurlyQuote] = True
isCloser[Token`LongName`RightAngleBracket] = True
isCloser[Token`LongName`RightAssociation] = True
isCloser[Token`LongName`RightBracketingBar] = True
isCloser[Token`LongName`RightCeiling] = True
isCloser[Token`LongName`RightDoubleBracket] = True
isCloser[Token`LongName`RightDoubleBracketingBar] = True
isCloser[Token`LongName`RightFloor] = True
isCloser[Token`LinearSyntax`CloseParen] = True

isCloser[_] = False







End[]

EndPackage[]
